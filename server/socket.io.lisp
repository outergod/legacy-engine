;;;; Engine - socket.io.lisp Hunchentoot-based socket.io implementation
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

(in-package :socket.io)

; FIXME (maybe) use hunchtoot session management?
(defparameter *sessions* (list))
(defparameter *close-timeout* 25)
(defparameter *heartbeat-interval* 20)

; should really be in the uuid package and use accessors
(defun uuid-equal-p (uuid-1 uuid-2)
  (every #'=
         (uuid:uuid-to-byte-array uuid-1)
         (uuid:uuid-to-byte-array uuid-2)))

(defun uuid-session (uuid)
  (assoc uuid *sessions* :test #'uuid-equal-p))

(defun getf-session (session indicator)
  (getf (cdr session) indicator))

(defun (setf getf-session) (val session var)
  (log-message :debug "Setting ~a:~a to ~a" (car session) var val)
  (setf (getf (cdr session) var) val))

(defun getf-uuid-session (uuid indicator)
  (getf-session (uuid-session uuid) indicator))

(defun (setf getf-uuid-session) (val uuid var)
  (setf (getf-session (uuid-session uuid) var) val))

(defun delete-session (session)
  (log-message :debug "Going to delete session ~a" (car session))
  (setq *sessions* (delete session *sessions* :test #'eq))
  (setq *websocket-handlers* (delete (getf-session session :dispatcher) *websocket-handlers* :test #'eq)))

(defun send-disconnect ()
  (websocket-send-message "0::"))

(defun send-connect ()
  (websocket-send-message "1::"))

(defun send-heartbeat ()
  (websocket-send-message "2::"))

(defun send-message (message)
  (let ((message-id (incf (getf-session *session* :message-id))))
    (websocket-send-message (apply #'format nil "~d:~d::~a"
                                   (if (consp message)
                                       (list 4 message-id (encode-json-to-string message))
                                       (list 3 message-id message))))))

(defun send-event (name lambda-list &optional (callback #'(lambda (&rest args) (declare (ignore args)))))
  (let* ((message-id (incf (getf-session *session* :message-id)))
         (handler #'(lambda (&rest args)
                      (prog1 (apply callback args)
                        (setf (getf-session *session* :callbacks)
                              (delete message-id (getf-session *session* :callbacks) :key #'car :test #'=))))))
    (push (cons message-id handler)
          (getf-session *session* :callbacks))
    (websocket-send-message (apply #'format nil "5:~d~:[~;+~]::~a"
                                   (list message-id callback
                                         (encode-json-to-string (list (cons :name name)
                                                                      (cons :args lambda-list))))))))

(defun send-ack (id &rest data)
  (websocket-send-message (apply #'concatenate 'string "6:::" id (if data (list "+" (encode-json-to-string data)) (list "")))))

(defun send-error (message &optional advice)
  (websocket-send-message (apply #'concatenate 'string "7:::" message (if advice (list "+" advice) (list "")))))

(defun terminate (session)
  (let ((stream (getf-session session :stream)))
    (delete-session session)
    (when (and (streamp stream) (open-stream-p stream))
      (send-disconnect)
      (websocket-send-term)
      (close stream))))

(defun heartbeat ()
  (log-message :debug "Bubump")
  (mapc #'(lambda (session)
            (destructuring-bind (session-id &key stream mutex heartbeat-timeout open
                                            (send-timestamp 0) (receive-timestamp heartbeat-timeout) &allow-other-keys)
                session
              (let ((*websocket-stream* stream)
                    (*websocket-stream-mutex* mutex)
                    (stream-open-p (and (streamp stream) (open-stream-p stream))))
                (cond ((not open) nil)
                      ((or (not stream-open-p)
                           (> send-timestamp
                              (+ heartbeat-timeout receive-timestamp)))
                       (log-message :debug "Heartbeat timeout for ~a: ~:[not ~;~]open / ~d > ~d"
                                    session-id stream-open-p send-timestamp (+ heartbeat-timeout receive-timestamp))
                       (terminate session))
                      (t (log-message :debug "Sending heartbeat to ~a" session-id)
                         (send-heartbeat)
                         (setf (getf-uuid-session session-id :send-timestamp) (get-universal-time)))))))
        *sessions*))

; (sb-ext:list-all-timers)
; (sb-ext:unschedule-timer *heartbeat-timer*)
(defparameter *heartbeat-timer* (sb-ext:make-timer #'heartbeat :name "session.io-heartbeat" :thread t))

(defun log-request (request)
  (let ((pathinfo (cdddr (split "/" (script-name request)))))
    (log-message :debug (format nil "path ~a~%headers~%~a~%get-parameters~%~a"
                                pathinfo
                                (mapcar #'(lambda (parameter)
                                            (format nil "~a: ~a" (car parameter) (cdr parameter)))
                                      (headers-in request))
                                (mapcar #'(lambda (parameter)
                                            (format nil "~a: ~a" (car parameter) (cdr parameter)))
                                      (get-parameters request))))))

(defun timeout-heartbeat (timeout)
  (let ((heartbeat (* 2 (/ timeout 3))))
    (if (> heartbeat 1)
        (round heartbeat)
        1)))

(defun translate-message-type (type)
  (ecase (parse-integer type :junk-allowed nil)
    (0 :disconnect)
    (1 :connect)
    (2 :heartbeat)
    (3 :message)
    (4 :json-message)
    (5 :event)
    (6 :ack)
    (7 :error)
    (8 :noop)))

(defun handle-ack (id data data-ack-p)
  (cond ((and data-ack-p (not id))
         (log-message :warning "Data acknowledgement requested without id, ignoring"))
        (data-ack-p (send-ack id (encode-json-to-string data)))
        (id (send-ack id))))

; [message type] ':' [message id ('+')] ':' [message endpoint] (':' [message data])
(let ((message-scanner (create-scanner #>eof>^(\d):(\d+)?(\+)?:([^:]+)?(?::(.+))?eof))
      (message-splitter (create-scanner #>eof>^([^\+]*)(?:\+?(.*))eof)))
  (defun handle-message-frame (message message-handler)
    (multiple-value-bind (match matches)
        (scan-to-strings message-scanner message)
      (if match
          (destructuring-bind (type id data-ack endpoint data)
              (coerce matches 'list)
            (declare (ignore endpoint)) ; TODO
            (ecase (translate-message-type type)
              (:disconnect
               (log-message :debug "Client signalled termination")
               (delete-session *session*))
              (:connect
               (log-message :info "Client tried to connect to additional endpoint")
               (send-error "Additional endpoints not supported" "Just don't try again, bitch!"))
              (:heartbeat
               (log-message :debug "Client sent heartbeat")
               (setf (getf-session *session* :receive-timestamp) (get-universal-time)))
              (:message
               (log-message :debug "Client sent message ~s" data)
               (handle-ack id (funcall message-handler data) data-ack))
              (:json-message
               (log-message :debug "Client sent JSON message ~s" data)
               (handle-ack id (funcall message-handler (decode-json-from-string data)) data-ack))
              (:event
               (log-message :debug "Client sent event message ~s" data)
               (handle-ack id (funcall message-handler (decode-json-from-string data) t) data-ack))
              (:ack
               (destructuring-bind (id data)
                   (coerce (nth-value 1 (scan-to-strings message-splitter data)) 'list)
                 (log-message :debug "Client acknowledged ~d~%Data: ~a" id data)
                 (alexandria:when-let ((callback (cdr (assoc (parse-integer id :junk-allowed nil)
                                                             (getf-session *session* :callbacks) :test #'=))))
                   (apply callback (decode-json-from-string (or data "[]"))))))
              (:error
               (destructuring-bind (reason advice)
                   (coerce (nth-value 1 (scan-to-strings message-splitter message)) 'list)
                 (log-message :error "Client signalled an error: ~a~%Advice: ~a" reason advice)))
              (:noop (log-message :debug "Client sent noop (whatever)"))))
          (log-message :error "Message is malfomed: ~s" message)))))

(defun socket.io-session ()
  (let* ((session-id (uuid:make-v4-uuid))
         (timeout *close-timeout*)
         (heartbeat (timeout-heartbeat timeout)))
    (list session-id :heartbeat-timeout heartbeat :close-timeout timeout :message-id 0)))

(defun socket.io-dispatcher (session scanner message-handler)
  #'(lambda (request) ; dispatcher
      (and (scan scanner (script-name request))
           (uuid-equal-p (uuid:make-uuid-from-string (cadr (path-info* request)))
                         (car session))
           #'(lambda (stream mutex) ; handshake handler
               (send-connect)
               (setf (getf-session session :stream) stream
                     (getf-session session :mutex) mutex
                     (getf-session session :open) t)
               #'(lambda (message) ; message handler
                   (let ((*session* session))
                     (handle-message-frame message message-handler)))))))

(defun handle-handshake (scanner message-handler)
  (let* ((session (socket.io-session))
         (dispatcher (socket.io-dispatcher session scanner message-handler)))
    (setf (getf-session session :dispatcher) dispatcher)
    (push session *sessions*)
    (push dispatcher *websocket-handlers*)
    (format nil "~a:~d:~d:~{~a~^,~}" (car session) (getf-session session :heartbeat-timeout) (getf-session session :close-timeout) (list "websocket"))))

(defun handle-disconnect (reply session-id)
  (let* ((uuid (make-uuid-from-string session-id))
         (session (uuid-session uuid)))
    (if session
        (delete-session session)
        (setf (return-code reply) +http-internal-server-error+))))

(defun path-info* (&optional (request *request*))
  (cdddr (split "/" (script-name* request))))

(defun handle-socket.io-request (pathinfo scanner message-handler)
  (unless (find *heartbeat-timer* (sb-ext:list-all-timers) :test #'eq)
    (sb-ext:schedule-timer *heartbeat-timer* 0 :repeat-interval *heartbeat-interval* :absolute-p nil))
  (setf (content-type*) "text/plain")
  (if pathinfo
      ; v1 spec [scheme] '://' [host] '/' [namespace] '/' [protocol version] '/' [transport id] '/' [session id] '/' ( '?' [query] )
      (destructuring-bind (transport-id session-id)
          pathinfo
        (declare (ignore transport-id)) ; support for websocket only
        (cond ((assoc "disconnect" (get-parameters*) :test #'string=) ; FIXME more cases or remove cond
               (prog1 nil (handle-disconnect *reply* session-id)))))
      (handle-handshake scanner message-handler)))

(defmacro define-socket.io-handler (message-handler &key (name 'socket.io) (prefix "/socket.io"))
  `(let ((scanner (create-scanner (concatenate 'string "^" ,prefix "/1(/.*)?")))) ; v1
     (define-easy-handler (,name :uri #'(lambda (request)
                                          (scan scanner (script-name request)))) ()
       (handle-socket.io-request (path-info*) scanner ,message-handler))))
