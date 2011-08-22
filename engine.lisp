;;;; Engine - engine.lisp Hunchentoot-based Common Lisp server for Engine
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

(let ((asdf:*asdf-verbose*))
  (require :engine))

(in-package :engine)

(define-constant +hunchentoot-port+ 8888 :test #'=)
(define-constant +swank-port+ (1+ +hunchentoot-port+) :test #'=)
(define-constant +startup-buffer+ "*scratch*" :test #'string=)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '*cwd*)
    (defparameter *cwd* (pathname-directory-pathname (compile-file-pathname "")))))

(defun in-project-path (&rest paths)
  (labels ((rec (acc rest)
             (if rest
                 (let ((file (if (cdr rest)
                                 (pathname-as-directory (car rest))
                                 (car rest))))
                   (rec (merge-pathnames file acc) (cdr rest)))
                 acc)))
    (rec *cwd* paths)))

(defparameter *acceptor* (make-instance 'websocket-acceptor :port +hunchentoot-port+))
;; (defparameter *acceptor*
;;   (make-instance 'websocket-ssl-acceptor :port +hunchentoot-port+
;;                  :ssl-certificate-file (in-project-path "cert" "engine_cert.pem")
;;                  :ssl-privatekey-file (in-project-path "cert" "engine_key.pem")
;;                  :ssl-privatekey-password "engine"))

(start *acceptor*)

(setq *prologue* "<!DOCTYPE html>" ; html5, bitch!
      *default-content-type* "text/html; charset=utf-8")

(setq *message-log-pathname* (in-project-path "log" "message.log")
      *access-log-pathname* (in-project-path "log" "access.log"))

(setq *dispatch-table* (list 'dispatch-easy-handlers
                             (create-folder-dispatcher-and-handler "/client/ace/"
                                                                   (pathname-as-directory (in-project-path "support" "ace" "build" "src")))                             
                             (create-folder-dispatcher-and-handler "/client/socket.io/"
                                                                   (pathname-as-directory (in-project-path "support" "socket.io-client" "dist")))
                             (create-folder-dispatcher-and-handler "/client/"
                                                                   (pathname-as-directory (in-project-path "client")))
                             'default-dispatcher))


(create-buffer +startup-buffer+)

(define-easy-handler (index :uri "/") ()
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :lang "en"
     (:head
      (:title "Engine")
      (:meta :charset "utf-8")
      (:meta :http-equiv "X-UA-Compatible" :content "chrome=1")
      (:style :type "text/css" :media "screen"
              (str (css (("body") (:overflow "none"))
                        (("#editor") (:margin 0 :position "absolute" :top 0 :bottom 0 :left 0 :right 0)))))
      (:script :data-main "client/main" :src "client/require.js"))
     (:body (:pre :id "editor")))))

(defmacro define-memoized-js-handler (description lambda-list &body body)
  `(let ((time (get-universal-time))
         (result))
     (define-easy-handler ,description ,lambda-list
       (handle-if-modified-since time)
       (setf (content-type*) "text/javascript"
             (header-out :last-modified) (rfc-1123-date time))
       (or result
           (setq result
                 (with-html-output-to-string (string)
                   (str ,@body)))))))

(defmacro define-memoized-ps-handler (description lambda-list &body body)
  `(define-memoized-js-handler ,description ,lambda-list
     (ps ,@body)))


(define-memoized-ps-handler (client/main :uri "/client/main.js") ()
  (require (list "ace/ace-uncompressed" "parenscript" "socket.io/socket.io")
           (lambda ()
             (require (list "engine/keyboard" "ace/edit_session" "engine/commands/default_commands" "ace/theme-twilight")
                      (lambda (keyboard edit)
                        (let ((editor (chain ace (edit "editor")))
                              (session (@ edit "EditSession")))
                          (setf (@ editor io) (chain io (connect))
                                (@ editor buffer-name) (lisp +startup-buffer+))
                          (chain editor (set-theme "ace/theme/twilight"))
                          (chain editor (set-keyboard-handler (keyboard editor)))
                          (chain editor renderer (set-show-gutter false))
                          (chain editor renderer (set-show-print-margin false))
                          (chain editor io (emit "load-buffer" (lisp +startup-buffer+)
                                                 (lambda (contents position)
                                                   (chain editor (set-session (new (session contents))))
                                                   (chain editor (move-cursor-to (@ position row) (@ position column))))))))))))

(define-memoized-ps-handler (client/engine/commands/default_commands :uri "/client/engine/commands/default_commands.js") ()
  (define (list "pilot/canon" "pilot/lang" "parenscript")
      (lambda (canon lang ps)
        (flet ((bind-key (key)
                 (create win key mac key sender "editor")))
          (macrolet ((add-command-args (name &body body)
                       `(create name ,name bind-key (bind-key nil)
                                exec (lambda (env args request)
                                       ,@body))))
            (chain ps (map (chain canon add-command)
                           (list (add-command-args "self-insert-command"
                                                   (chain env editor (insert (@ args text))))
                                 (add-command-args "backward-char"
                                                   (chain env editor (navigate-left 1)))
                                 (add-command-args "forward-char"
                                                   (chain env editor (navigate-right 1)))
                                 (add-command-args "move-to-position"
                                                   (chain env editor (move-cursor-to (@ args row) (@ args column))))
                                 (add-command-args "backward-delete-char"
                                                   (chain env editor (remove-left)))
                                 (add-command-args "delete-char"
                                                   (chain env editor (remove-right)))))))))))

(define-memoized-ps-handler (client/parenscript :uri "/client/parenscript.js") ()
  (define (lambda ()
            (lisp *ps-lisp-library*)
            (lisp (cons 'create (mapcan #'(lambda (item)
                                            (list item item))
                                        (mapcar #'cadr (cdr *ps-lisp-library*))))))))

(define-memoized-ps-handler (client/engine/keyboard :uri "/client/engine/keyboard.js") ()
  (define (list "pilot/canon")
      (lambda (canon)
        (lambda (editor)
          (create handle-keyboard (lambda (data hash-id text-or-key key-code)
                                    (chain editor io (emit "keyboard" hash-id text-or-key key-code (@ editor buffer-name)
                                                           (lambda (response)
                                                             (chain canon (exec (@ response command) (create editor editor) "editor" (@ response args))))))
                                    (create command "noop")))))))

(define-socket.io-handler #'(lambda (message)
                              (declare (ignore message))))

(defun synchronized-insert-sequence (cursor sequence)
  (multiple-value-prog1 (values "self-insert-command" (list (cons "text" sequence)))
    (insert-sequence cursor sequence)))

(defmacro define-synchronized-operation (name args &body body)
  `(defun ,name ,args
     (handler-case
         (progn ,@body)
       (flexi-position-error ()
         "noop"))))

(define-synchronized-operation synchronized-delete< (cursor)
  (prog1 "backward-delete-char"
    (delete< cursor)))

(define-synchronized-operation synchronized-delete> (cursor)
  (prog1 "delete-char"
    (delete> cursor)))

(define-synchronized-operation synchronized-cursor-left (cursor)
  (prog1 "backward-char"
    (buffer-cursor-lineward cursor -1)))

(define-synchronized-operation synchronized-cursor-right (cursor)
  (prog1 "forward-char"
    (buffer-cursor-lineward cursor 1)))

(define-synchronized-operation synchronized-cursor-up (cursor)
  (buffer-cursor-columnward cursor -1)
  (multiple-value-bind (row column)
      (cursor-2d-position cursor)
    (values "move-to-position" (list (cons :row row) (cons :column column)))))

(define-synchronized-operation synchronized-cursor-down (cursor)
  (buffer-cursor-columnward cursor 1)
  (multiple-value-bind (row column)
      (cursor-2d-position cursor)
    (values "move-to-position" (list (cons :row row) (cons :column column)))))

(defun key-code-case (code cursor)
  (case code
    (8 (synchronized-delete< cursor))       ; backspace
    (13 (synchronized-insert-sequence cursor (string #\Newline))) ; return
    (37 (synchronized-cursor-left cursor))  ; cursor left
    (38 (synchronized-cursor-up cursor))    ; cursor up
    (39 (synchronized-cursor-right cursor)) ; cursor right
    (40 (synchronized-cursor-down cursor))  ; cursor down
    (64 (synchronized-delete> cursor))      ; delete
    (t "noop")))

(socket.io-on "connection" (session)
  (setf (getf-session session :buffer-cursors) (list)))

(defun session-buffer-cursor (session buffer)
  (or (cdr (assoc buffer (getf-session session :buffer-cursors) :test #'eq))
      (prog1-let cursor (create-buffer-cursor buffer)
        (push (cons buffer cursor)
              (getf-session session :buffer-cursors)))))

(socket.io-on "keyboard" (hash-id key key-code buffer-name)
  (let ((cursor (session-buffer-cursor *socket.io-session* (buffer-named buffer-name)))
        (key (string-trim (list #\Null #\Newline #\Return) key)))
    (prog1 (multiple-value-bind (command args)
               (cond ((and (zerop key-code)
                           (not (zerop (length key))))
                      (synchronized-insert-sequence cursor key))
                     ((not (zerop key-code))
                      (key-code-case key-code cursor))
                     (t "noop"))
             (list (cons :command command) (cons :args args)))
      (log-message :debug "got event keyboard args ~s ~s ~s ~s" hash-id key key-code buffer-name))))

(socket.io-on "load-buffer" (buffer-name)
  (let* ((buffer (buffer-named buffer-name))
         (cursor (session-buffer-cursor *socket.io-session* buffer)))
    (multiple-value-bind (row column)
        (cursor-2d-position cursor)
      (values (buffer-string buffer) (list (cons :row row) (cons :column column))))))

(create-buffer "*scratch*")

;; (add-command-args "forward-char" "Ctrl-f"
;;                   (chain env editor (navigate-right 1)))
;;                                  (add-command-args "backward-char" "Ctrl-b"
;;                                                    (chain env editor (navigate-left 1)))
;;                                  (add-command-args "forward-word" "Alt-f"
;;                                                    (chain env editor selection (move-cursor-word-right)))
;;                                  (add-command-args "backward-word" "Alt-b"
;;                                                    (chain env editor selection (move-cursor-word-left)))
;;                                  (add-command-args "move-beginning-of-line" "Ctrl-a"
;;                                                    (chain env editor (move-cursor-to (chain env editor selection (get-selection-lead) row) 0)))
;;                                  (add-command-args "move-end-of-line" "Ctrl-e"
;;                                                    (chain env editor selection (move-cursor-line-end)))
;;                                  (add-command-args "back-to-indentation" "Alt-m"
;;                                                    (chain env editor selection (move-cursor-line-end))
;;                                                    (chain env editor selection (move-cursor-line-start)))
;;                                  (add-command-args "beginning-of-buffer" "Alt-Shift-," ; ouch, Alt-< doesn't work instead
;;                                                    (chain env editor (navigate-file-start)))
;;                                  (add-command-args "end-of-buffer" "Alt-Shift-." ; ditto for Alt->
;;                                                    (chain env editor (navigate-file-end)))
;;                                  (add-command-args "delete-char" "Ctrl-d"
;;                                                    (chain env editor (remove-right)))
;;                                  (add-command-args "kill-word" "Alt-d" ; TODO send to server
;;                                                    (chain env editor (remove-word-right)))
;;                                  (add-command-args "undo" "Alt-_" ; TODO send to server; FIXME; UndoManager?
;;                                                    (chain env editor (undo)))

(let ((swank:*use-dedicated-output-stream* nil)
      (swank:*communication-style*
       #+ccl  :spawn
       #+sbcl :spawn
       #+ecl  :spawn))
  (swank:create-server :coding-system "utf-8-unix" :port +swank-port+ :dont-close t))

(format t "~&Ready.~%Press return to stop~%")
(read-line)
(stop *acceptor*)
(exit 0)
