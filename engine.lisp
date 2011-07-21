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
  (mapc #'require (list :hunchentoot :osicat :swank :cl-who :css-lite :parenscript)))

(defpackage engine
  (:use :cl :hunchentoot :osicat :osicat-sys :cl-who :parenscript)
  (:import-from :css-lite :css))

(in-package :engine)

(set-dispatch-macro-character #\# #\> #'cl-heredoc:read-heredoc)

(defconstant +hunchentoot-port+ 8888)
(defconstant +swank-port+ (1+ +hunchentoot-port+))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (boundp '*cwd*)
    (defparameter *cwd* (pathname-directory-pathname (compile-file-pathname "")))))

(defparameter *acceptor* (make-instance 'acceptor :port +hunchentoot-port+))
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
       (print (or result
                  (setq result
                        (with-html-output-to-string (string)
                          (str ,@body))))))))

(defmacro define-memoized-ps-handler (description lambda-list &body body)
  `(define-memoized-js-handler ,description ,lambda-list
     (ps ,@body)))


(define-memoized-ps-handler (client/main :uri "/client/main.js") ()
  (chain require (ready (lambda ()
                          (require (list "ace/ace-uncompressed" "parenscript")
                                   (lambda ()
                                     (require (list "ace/theme-twilight" "engine/commands/default_commands")
                                              (lambda ()
                                                (let ((editor (chain ace (edit "editor"))))
                                                  (chain editor (set-theme "ace/theme/twilight"))
                                                  (chain editor renderer (set-show-gutter false))
                                                  (chain editor renderer (set-show-print-margin false)))))))))))

(define-memoized-ps-handler (client/engine/commands/default_commands :uri "/client/engine/commands/default_commands.js") ()
  (define (list "pilot/canon" "parenscript")
      (lambda (canon ps)
        (flet ((bind-key (key)
                 (create win key mac key sender "editor")))
          (chain ps (map (chain canon add-command)
                         (list (create name "move-beginning-of-line" bind-key (bind-key "Ctrl-a")
                                       exec (lambda (env args request)
                                              (chain env editor (move-cursor-to-position 0))))
                               (create name "move-end-of-line" bind-key (bind-key "Ctrl-e")
                                       exec (lambda (env args request)
                                              (chain env editor selection (move-cursor-line-end))))
                               (create name "back-to-indentation" bind-key (bind-key "Alt-m")
                                       exec (lambda (env args request)
                                              (chain env editor selection (move-cursor-line-end))
                                              (chain env editor selection (move-cursor-line-start)))))))))))

(define-memoized-ps-handler (client/parenscript :uri "/client/parenscript.js") ()
  (define (lambda ()
            (lisp *ps-lisp-library*)
            (lisp (cons 'create (mapcan #'(lambda (item)
                                            (list item item))
                                        (mapcar #'cadr (cdr *ps-lisp-library*))))))))

(setq *dispatch-table* (list 'dispatch-easy-handlers
                             (create-folder-dispatcher-and-handler "/client/ace/"
                                                                   (pathname-as-directory (in-project-path "support" "ace" "build" "src")))
                             (create-folder-dispatcher-and-handler "/client/canon/"
                                                                   (pathname-as-directory (in-project-path "support" "ace" "support" "canon")))
                             (create-folder-dispatcher-and-handler "/client/"
                                                                   (pathname-as-directory (in-project-path "client")))                             
                             'default-dispatcher))

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
