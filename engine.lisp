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
      (:script :type "text/javascript" :src "ace/ace-uncompressed.js")
      (:script :type "text/javascript" :src "ace/theme-twilight.js")
      (:script :type "text/javascript"
               (str (ps (chain window (add-event-listener "load"
                                                            (lambda ()
                                                              (let ((editor (chain ace (edit "editor"))))
                                                                (chain editor (set-theme "ace/theme/twilight"))))
                                                            false)))))
      (:body (:pre :id "editor"))))))

(push (create-folder-dispatcher-and-handler "/ace/"
                                            (pathname-as-directory (in-project-path "support" "ace" "build" "src")))
      *dispatch-table*)

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
