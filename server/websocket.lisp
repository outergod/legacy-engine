;;;; Engine - websocket.lisp Hunchentoot-based WebSocket (draft) implementation
;;;; Copyright (C) 2011  Alexander Kahl <e-user@fsfe.org>
;;;; This file is part of Engine.
;;;; Engine is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU Affero General Public License as
;;;; published by the Free Software Foundation; either version 3 of the
;;;; License, or (at your option) any later version.
;;;;
;;;; Engine is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :websocket)

(defclass websocket-request (request)
  ((handler :accessor websocket-request-handler
            :initform nil)))

(defclass websocket-reply (reply) ())

(defmethod initialize-instance :after ((reply websocket-reply) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (reply-external-format reply) (make-external-format :utf8 :eol-style :lf)))

(defclass websocket-acceptor (acceptor)
  ((websocket-timeout :initarg :websocket-timeout
                      :accessor websocket-timeout
                      :initform 60))
  (:default-initargs :request-class 'websocket-request :reply-class 'websocket-reply))

(define-condition websocket-unsupported-version (condition)
  ((version :initarg :version :reader websocket-unsupported-version-of
            :initform (required-argument :version))))

(define-condition websocket-illegal-key (condition)
  ((key :initarg :key :reader websocket-illegal-key-of
        :initform (required-argument :key))))

(define-condition websocket-illegal-frame-type (condition)
  ((type :initarg :type :reader websocket-illegal-frame-type-of
         :initform (required-argument :type))))

(defconstant +websocket-terminator+ '(#x00 #xff))
(defconstant +websocket-magic-key+ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11") ; this is a fixed uuid v4 value by specification

(defvar *websocket-stream*)
(defvar *websocket-stream-mutex*)
(defvar *websocket-socket*)

(defparameter *websocket-handlers* nil)

(defun integer-octets-32be (number)
  (let ((result (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (index 4 result)
      (let ((position #+little-endian (- 24 (* 8 index)) #-little-endian (* 8 index)))
        (setf (aref result index)
              (ldb (byte 8 position) number))))))

(let ((digit-scanner (create-scanner "[^\\d]"))
      (space-scanner (create-scanner "[^ ]")))
  (defun websocket-keyhash (key)
    (let ((number (parse-integer (regex-replace-all digit-scanner key "")
                                 :junk-allowed nil))
          (spaces (length (regex-replace-all space-scanner key ""))))
      (if (or (zerop spaces)
              (not (zerop (mod number spaces))))
          (error 'socket.io-websocket-illegal-key :initarg key)
          (integer-octets-32be (/ number spaces)))))) ; crack-smoking mac pussies at google want big endian

(defun read-key3 (request)
  (let ((key (make-array 8 :element-type '(unsigned-byte 8))))
    (dotimes (index 8 key)
      (setf (aref key index)
            (char-int (chunga:read-char* (content-stream request) t))))))

(defun digest-key (key)
  (ironclad:digest-sequence :md5 key))

(defun websocket-uri (request host &optional ssl)
  (format nil "~:[ws~;wss~]://~a~a" ssl host (script-name request)))

(defun handle-handshake-legacy (request reply)
  (prog1
      (let* ((stream (make-in-memory-output-stream)))
        (mapc #'(lambda (key)
                  (write-sequence key stream))
              (list (websocket-keyhash (header-in :sec-websocket-key1 request))
                    (websocket-keyhash (header-in :sec-websocket-key2 request))
                    (read-key3 request)))
        (digest-key (get-output-stream-sequence stream)))
    (setf (header-out :sec-websocket-origin reply) (header-in :origin request)
          (header-out :sec-websocket-location reply) (or (websocket-uri request (header-in :host request)
                                                                        (ssl-p (request-acceptor request))))
          (header-out :sec-websocket-protocol reply) (header-in :sec-websocket-protocol request))))

(defun websocket-handle-handshake (request reply)
  (handler-case
      (prog1
          (cond ((header-in :sec-websocket-version request) ; >= draft-hybi-04 FIXME
                 (error 'websocket-unsupported-version (header-in :sec-websocket-version request)))
                ((header-in :sec-websocket-draft request) ; (and (>= draft-hybi-02) (<= draft-hybi-03)) FIXME
                 (error 'websocket-unsupported-version (header-in :sec-websocket-draft request)))
                ((and (header-in :sec-websocket-key1 request) ; < draft-hybi-02
                      (header-in :sec-websocket-key2 request))
                 (handle-handshake-legacy request reply))
                (t (error 'websocket-unsupported-version :unknown)))
        (setf (return-code* reply) +http-switching-protocols+
              (header-out :upgrade reply) "WebSocket"
              (header-out :connection reply) "Upgrade"
              (content-type* reply) "application/octet-stream"))
    (websocket-illegal-key (condition)
      (hunchentoot-error "Illegal key ~a encountered" (websocket-illegal-key-of condition)))
    (websocket-unsupported-version ()
      (hunchentoot-error "WebSocket handshake failed because of unsupported protocol version"))))

(defun websocket-send-term (&optional (stream *websocket-stream*) (mutex *websocket-stream-mutex*))
  (with-lock-held (mutex)
    (write-sequence +websocket-terminator+ stream)
    (force-output stream)))

(defun websocket-send-message (message &optional (stream *websocket-stream*) (mutex *websocket-stream-mutex*))
  (when (> (length message) 0) ; empty message would send terminator
    (with-lock-held (mutex)
      (write-byte #x00 stream)
      (write-utf-8-bytes message stream)
      (write-byte #xff stream)
      (force-output stream))))

(defun skip-bytes (stream number)
  (dotimes (num number)
    (read-byte stream)))

(defun websocket-process-connection (stream message-handler &optional (version :draft-hixie-76))
  (ecase version
    ((:draft-hixie-76 :draft-hybi-00)
     (loop for type = (read-byte stream) do 
          (cond ((= #x00 type)
                 (do ((reader (make-in-memory-output-stream))
                      (data (read-byte stream) (read-byte stream)))
                     ((= #xff data)
                      (funcall message-handler (utf-8-bytes-to-string (get-output-stream-sequence reader))))
                   (write-byte data reader)))
                ((= #xff type)
                 (let ((data (read-byte stream)))
                   (if (= #x00 data)
                       (return) ; regular termination
                       (do* ((data data (read-byte stream))
                             (length (logand #x7f data) (+ (* 128 length) (logand #x7f data))))
                            ((= #x80 (logand #x80 data))
                             (skip-bytes stream length))))))
                (t (error 'websocket-illegal-frame-type ; irregular termination
                          :type type))))))) 

(defmethod process-connection :around ((*acceptor* websocket-acceptor) (socket t))
  (let ((*websocket-socket* socket)
        (hunchentoot-asd:*hunchentoot-version* (format nil "~a cl-websocket 0" hunchentoot-asd:*hunchentoot-version*)))
    (call-next-method)))

(defmethod process-connection ((*acceptor* websocket-acceptor) (socket t))
  (handler-case
      (call-next-method)
    (error (condition)
      (log-message :error "WebSocket connection terminated unexpectedly: ~a" condition))))

(defmethod process-request :around ((request websocket-request))
  "I *do* know what I'm doing, Mister!"
  (let ((*approved-return-codes* (cons +http-switching-protocols+
                                       *approved-return-codes*)))
    (let ((stream (call-next-method)))
      (prog1 stream
        (when (= +http-switching-protocols+ (return-code*))
          (force-output stream)
          (let ((timeout (websocket-timeout (request-acceptor request))))
            (set-timeouts *websocket-socket* timeout timeout))
          (handler-case
              (let ((*websocket-stream* stream)
                    (*websocket-stream-mutex* (make-lock)))
                (websocket-process-connection stream
                                              (funcall (websocket-request-handler request) stream *websocket-stream-mutex*))) ; custom handshake
            (end-of-file ()
              (log-message :debug "WebSocket connection terminated"))
            (websocket-illegal-frame-type (condition)
              (log-message :error "WebSocket illegal frame type 0x~x encountered, terminating"
                           (websocket-illegal-frame-type-of condition)))))))))

(defun dispatch-websocket-handlers (request)
  (some #'(lambda (handler)
            (funcall handler request))
        *websocket-handlers*))

(defmethod handle-request ((*acceptor* websocket-acceptor) (*request* websocket-request))
  (if (and (string= "upgrade" (string-downcase (header-in* :connection)))
           (string= "websocket" (string-downcase (header-in* :upgrade))))
      (if-let ((handler (dispatch-websocket-handlers *request*)))
        (prog1 (websocket-handle-handshake *request* *reply*)
          (setf (websocket-request-handler *request*) handler))
        (hunchentoot-error "WebSocket request rejected (no suitable handler found)"))
      (call-next-method)))
