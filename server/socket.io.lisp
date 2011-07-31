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

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

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
    (apply #'websocket-send-message "~d:~d::~a"
           (if (consp message)
               (list 4 message-id (encode-json message))
               (list 3 message-id message)))))

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
            (destructuring-bind (session-id &key stream mutex heartbeat-timeout
                                            (send-timestamp 0) (receive-timestamp heartbeat-timeout) &allow-other-keys)
                session
              (let ((*websocket-stream* stream)
                    (*websocket-stream-mutex* mutex)
                    (open (and (streamp stream) (open-stream-p stream))))
                (if (or (not open)
                        (> send-timestamp
                           (+ heartbeat-timeout receive-timestamp)))
                    (progn
                      (log-message :debug "Heartbeat timeout for ~a: ~:[not ~;~]open / ~d > ~d"
                                   session-id open send-timestamp (+ heartbeat-timeout receive-timestamp))
                      (terminate session))
                    (progn
                      (log-message :debug "Sending heartbeat to ~a" session-id)
                      (send-heartbeat)
                      (setf (getf-uuid-session session-id :send-timestamp) (get-universal-time)))))))
        *sessions*))

; (sb-ext:list-all-timers)
(sb-ext:unschedule-timer *heartbeat-timer*)
(defparameter *heartbeat-timer* (sb-ext:make-timer #'heartbeat :name "session.io-heartbeat" :thread t))
(sb-ext:schedule-timer *heartbeat-timer* 0 :repeat-interval *heartbeat-interval* :absolute-p nil)

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

(defun handle-message (message message-handler)
  (declare (ignore message-handler)) ; TODO
  (log-message :debug "Received socket.io message ~s~%" message)
  (multiple-value-bind (match matches)
      ; [message type] ':' [message id ('+')] ':' [message endpoint] (':' [message data])
      (scan-to-strings #>eof>^(\d):(\d+)?(\+)?:([^:]+)?(?::(.+))?eof message)
    (if match
        (destructuring-bind (type id handle-id endpoint data)
            (coerce matches 'list)
          (declare (ignore id handle-id endpoint data)) ; TODO
          (ecase (translate-message-type type)
            (:disconnect
             (log-message :debug "Client signalled termination")
             (delete-session *session*))
            (:connect ; connect TODO
             (log-message :info "Client tried to connect to additional endpoint"))
            (:heartbeat
             (log-message :debug "Client sent heartbeat")
             (setf (getf-session *session* :receive-timestamp) (get-universal-time)))))
        (log-message :error "Message is malfomed"))))

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
                     (getf-session session :mutex) mutex)
               #'(lambda (message) ; message handler
                   (let ((*session* session))
                     (handle-message message message-handler)))))))

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
    (if uuid-session
        (delete-session session)
        (setf (return-code reply) +http-internal-server-error+))))

(defun path-info* (&optional (request *request*))
  (cdddr (split "/" (script-name* request))))

(defun handle-socket.io-request (pathinfo scanner message-handler)
  (setf (content-type*) "text/plain")
  (if pathinfo
      ; v1 spec [scheme] '://' [host] '/' [namespace] '/' [protocol version] '/' [transport id] '/' [session id] '/' ( '?' [query] )
      (destructuring-bind (transport-id session-id)
          pathinfo
        (declare (ignore transport-id)) ; support for websocket only
        (cond ((assoc "disconnect" (get-parameters*) :test #'string=)
               (prog1 nil (handle-disconnect *reply* session-id)))))
      (handle-handshake scanner message-handler)))

(defmacro define-socket.io-handler (message-handler &key (name 'socket.io) (prefix "/socket.io"))
  `(let ((scanner (create-scanner (concatenate 'string "^" ,prefix "/1(/.*)?")))) ; v1
     (define-easy-handler (,name :uri #'(lambda (request)
                                          (scan scanner (script-name request)))) ()
       (handle-socket.io-request (path-info*) scanner ,message-handler))))
