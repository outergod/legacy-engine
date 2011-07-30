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

(defpackage engine
  (:use :cl :hunchentoot :osicat :osicat-sys :cl-who :parenscript :websocket)
  (:import-from :css-lite :css))

(in-package :engine)

(defconstant +hunchentoot-port+ 8888)
(defconstant +swank-port+ (1+ +hunchentoot-port+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '*cwd*)
    (defparameter *cwd* (pathname-directory-pathname (compile-file-pathname "")))))

(defparameter *acceptor* (make-instance 'websocket-acceptor :port +hunchentoot-port+))
(start *acceptor*)

(setq *prologue* "<!DOCTYPE html>" ; html5, bitch!
      *default-content-type* "text/html; charset=utf-8")

(defun in-project-path (&rest paths)
  (labels ((rec (acc rest)
             (if rest
                 (let ((file (if (cdr rest)
                                 (pathname-as-directory (car rest))
                                 (car rest))))
                   (rec (merge-pathnames file acc) (cdr rest)))
                 acc)))
    (rec *cwd* paths)))

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
             (require (list "engine/commands/default_commands" "ace/theme-twilight")
                      (lambda ()
                        (let ((editor (chain ace (edit "editor"))))
                          (chain editor (set-theme "ace/theme/twilight"))
                          (chain editor renderer (set-show-gutter false))
                          (chain editor renderer (set-show-print-margin false))))))))

(define-memoized-ps-handler (client/engine/commands/default_commands :uri "/client/engine/commands/default_commands.js") ()
  (define (list "pilot/canon" "parenscript")
      (lambda (canon ps)
        (flet ((bind-key (key)
                 (create win key mac key sender "editor")))
          (macrolet ((add-command-args (name key &body body)
                       `(create name ,name bind-key (bind-key ,key)
                                exec (lambda (env args request)
                                       ,@body))))
            (chain ps (map (chain canon add-command)
                           (list (add-command-args "forward-char" "Ctrl-f"
                                                   (chain env editor (navigate-right 1)))
                                 (add-command-args "forward-char" "Ctrl-b"
                                                   (chain env editor (navigate-left 1)))
                                 (add-command-args "forward-char" "Alt-f"
                                                   (chain env editor selection (move-cursor-word-right)))
                                 (add-command-args "forward-char" "Alt-b"
                                                   (chain env editor selection (move-cursor-word-left)))
                                 (add-command-args "move-beginning-of-line" "Ctrl-a"
                                                   (chain env editor (move-cursor-to (chain env editor selection (get-selection-lead) row) 0)))
                                 (add-command-args "move-end-of-line" "Ctrl-e"
                                                   (chain env editor selection (move-cursor-line-end)))
                                 (add-command-args "back-to-indentation" "Alt-m"
                                                   (chain env editor selection (move-cursor-line-end))
                                                   (chain env editor selection (move-cursor-line-start)))
                                 (add-command-args "beginning-of-buffer" "Alt-Shift-," ; ouch, Alt-< doesn't work instead
                                                   (chain env editor (navigate-file-start)))
                                 (add-command-args "end-of-buffer" "Alt-Shift-." ; ditto for Alt->
                                                   (chain env editor (navigate-file-end)))
                                 (add-command-args "delete-char" "Ctrl-d"
                                                   (chain env editor (remove-right)))
                                 (add-command-args "kill-word" "Alt-d" ; TODO send to server
                                                   (chain env editor (remove-word-right)))
                                 (add-command-args "undo" "Alt-_" ; TODO send to server; FIXME; UndoManager?
                                                   (chain env editor (undo)))))))))))

(define-memoized-ps-handler (client/parenscript :uri "/client/parenscript.js") ()
  (define (lambda ()
            (lisp *ps-lisp-library*)
            (lisp (cons 'create (mapcan #'(lambda (item)
                                            (list item item))
                                        (mapcar #'cadr (cdr *ps-lisp-library*))))))))

(socket.io:define-socket.io-handler)

(let ((swank:*use-dedicated-output-stream* nil)
      (swank:*communication-style*
       #+ccl  :spawn
       #+sbcl :spawn
       #+ecl  :spawn))
  (swank:create-server :coding-system "utf-8-unix" :port +swank-port+ :dont-close t))

(format t "~&Ready.~%Press return to stop~%")
(read-line)
(stop *acceptor*)
(osicat-posix:exit 0)
