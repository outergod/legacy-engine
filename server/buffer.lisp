;;;; Engine - buffer.lisp Engine buffer capabilities
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

(in-package :engine)

(define-constant +base-buffer-size+ 1024 :test #'=)

(defclass buffer (flexichain:cursorchain)
  ((mutex :reader buffer-mutex-of
          :initform (make-lock))
   (position :reader buffer-position-of
             :initform (make-instance 'flexichain:right-sticky-flexicursor)))
  (:default-initargs :element-type 'character :fill-element #\Null))

(defparameter *buffers* (list))

(defmethod initialize-instance :before ((buffer buffer) &rest initargs &key &allow-other-keys)
  (push (cons (or (getf initargs :name)
                  (required-argument :name))
              buffer)
        *buffers*))

(defmethod initialize-instance :after ((buffer buffer) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (flexichain:chain (buffer-position-of buffer)) buffer))

(mapc #'(lambda (method)
          (eval `(defmethod ,method :around
                     ,(cons (list 'buffer 'buffer)
                            (cdr (closer-mop:generic-function-lambda-list (symbol-function method))))
                   (with-lock-held ((buffer-mutex-of buffer))
                     (call-next-method)))))
      (list 'nb-elements 'insert* 'delete* 'delete-elements*
            'element* 'push-start))

(defmethod (setf element*) :around (object (buffer buffer) position)
  (declare (ignore object position))
  (with-lock-held ((buffer-mutex-of buffer))
    (call-next-method)))

;; (mapc #'unintern (list 'nb-elements 'insert* 'delete* 'delete-elements* 'element* 'push-start))
;; (intern (symbol-name 'flexichain:nb-elements))
;; (unintern 'nb-elements :keyword)
; (closer-mop:generic-function-lambda-list #'flexichain:delete*)





