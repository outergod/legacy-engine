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

(defparameter *socket.io-sessions* (list))

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

; should really be in the uuid package and use accessors
(defun uuid-equal-p (uuid-1 uuid-2)
  (every #'=
         (uuid:uuid-to-byte-array uuid-1)
         (uuid:uuid-to-byte-array uuid-2)))

(defun socket.io-handle-handshake ()
  (let ((session-id (uuid:make-v4-uuid)))
    (push (list session-id :hearbeat-timeout 15 :close-timeout 25 :timestamp (get-universal-time)) *socket.io-sessions*)
    (format nil "~a:~d:~d:~{~a~^,~}" session-id 15 25 (list "websocket"))))

(defun socket.io-handle-disconnect (reply session-id)
  (let* ((uuid (uuid:make-uuid-from-string session-id))
         (uuid-session (car (assoc uuid *socket.io-sessions* :test #'uuid-equal-p))))
    (if uuid-session
        (setq *socket.io-sessions* (delete uuid-session *socket.io-sessions* :test #'eq :key #'car))
        (setf (return-code reply) +http-internal-server-error+))))

(let ((scanner (create-scanner "^/socket.io/1(/.*)?"))) ; v1
  (define-easy-handler (socket.io :uri #'(lambda (request)
                                           (scan scanner (script-name request)))) ()
    (log-request *request*)
    (setf (content-type*) "text/plain")
    (let ((pathinfo (cdddr (split "/" (script-name*)))))
      ; v1 spec [scheme] '://' [host] '/' [namespace] '/' [protocol version] '/' [transport id] '/' [session id] '/' ( '?' [query] )
      (if pathinfo
          (destructuring-bind (transport-id session-id)
              pathinfo
            (declare (ignore transport-id))
            (cond ((assoc "disconnect" (get-parameters*) :test #'string=)
                   (prog1 nil (socket.io-handle-disconnect *reply* session-id)))))
          (socket.io-handle-handshake)))))
