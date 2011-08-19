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

; FIXME upstream
(in-package :flexichain)
(export 'cursors)

(defmethod delete* :before ((chain standard-cursorchain) position)
  (with-slots (cursors) chain
     (let* ((old-index (position-index chain position)))
       (loop for cursor-wp in cursors
             as cursor = (weak-pointer-value cursor-wp)
             when (and cursor (= old-index (flexicursor-index cursor)))
               do (cond ((subtypep (class-of cursor) (find-class 'right-sticky-flexicursor))
                         (incf (cursor-pos cursor)))
                        ((subtypep (class-of cursor) (find-class 'left-sticky-flexicursor))
                         (decf (cursor-pos cursor))))))))

(defmethod delete-elements* :before ((chain standard-cursorchain) position n)
  (with-slots (cursors) chain
    (when (minusp n)
      (incf position n)
      (setf n (* -1 n)))
    (unless (zerop n)
      (loop for cursor-wp in cursors
         as cursor = (weak-pointer-value cursor-wp)
         when (and cursor (<= position (cursor-pos cursor)
                              (+ position n)))
         do (cond ((subtypep (class-of cursor) (find-class 'right-sticky-flexicursor))
                   (setf (cursor-pos cursor)
                         (+ position n)))
                  ((subtypep (class-of cursor) (find-class 'left-sticky-flexicursor))
                   (setf (cursor-pos cursor)
                         position)))))))

(in-package :engine)

(defclass buffer (standard-cursorchain)
  ((mutex :reader buffer-mutex-of
          :initform (make-recursive-lock)))
  (:default-initargs :element-type 'character :fill-element #\Null))

(defclass buffer-cursor (right-sticky-flexicursor)
  ((line-position :accessor cursor-line-position-of
                  :initform 0)))

(defparameter *buffers* (list))

(defun buffer-named (name)
  (cdr (assoc name *buffers* :test #'string=)))

(defun create-buffer (name)
  (make-instance 'buffer :name name :initial-element #\Null))

(defgeneric create-buffer-cursor (buffer)
  (:method ((buffer buffer))
    (make-instance 'buffer-cursor :chain buffer)))

(defmethod initialize-instance :before ((buffer buffer) &rest initargs &key &allow-other-keys)
  (push (cons (or (getf initargs :name)
                  (required-argument :name))
              buffer)
        *buffers*))

(defmethod initialize-instance :after ((buffer buffer) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs)))


(mapc #'(lambda (method)
          (eval `(defmethod ,method :around
                     ,(cons (list 'buffer 'buffer)
                            (cdr (closer-mop:generic-function-lambda-list (symbol-function method))))
                   (with-recursive-lock-held ((buffer-mutex-of buffer))
                     (call-next-method)))))
      (list 'nb-elements 'insert* 'delete* 'delete-elements*
            'element* 'push-start 'push-end 'pop-start 'pop-end
            'rotate 'clone-cursor 'cursor-pos 'at-beginning-p 'at-end-p
            'insert 'insert-sequence 'delete< 'delete> 'element< 'element>))

(defmethod (setf element*) :around (object (buffer buffer) position)
  (declare (ignore object position))
  (with-recursive-lock-held ((buffer-mutex-of buffer))
    (call-next-method)))

(defmethod (setf cursor-pos) :around (position (buffer buffer))
  (declare (ignore position))
  (with-recursive-lock-held ((buffer-mutex-of buffer))
    (call-next-method)))

(defmethod (setf element<) :around (object (buffer buffer))
  (declare (ignore object))
  (with-recursive-lock-held ((buffer-mutex-of buffer))
    (call-next-method)))

(defmethod (setf element>) :around (object (buffer buffer))
  (declare (ignore object))
  (with-recursive-lock-held ((buffer-mutex-of buffer))
    (call-next-method)))

(defmethod insert* :before ((buffer buffer) position object)
  (when (char= #\Newline object)
    (with-slots (cursors)
        buffer
      (mapc #'(lambda (cursor-wp)
                (let ((cursor (weak-pointer-value cursor-wp)))
                  (with-accessors ((cursor-position cursor-pos)
                                   (line-position cursor-line-position-of))
                      cursor
                    (when (<= position cursor-position)
                      (incf line-position)))))
            cursors))))

(defmethod delete* :before ((buffer buffer) position)
  (when (char= #\Newline (element* buffer position))
    (with-slots (cursors)
        buffer
      (mapc #'(lambda (cursor-wp)
                (let ((cursor (weak-pointer-value cursor-wp)))
                  (with-accessors ((cursor-position cursor-pos)
                                   (line-position cursor-line-position-of))
                      cursor
                    (when (<= position cursor-position)
                      (decf line-position)))))
            cursors))))

(defgeneric buffer-string (buffer)
  (:method ((buffer buffer))
    (coerce
     (loop for index from 0 below (nb-elements buffer) collect (element* buffer index))
     'string)))

(defgeneric cursor-2d-position (buffer-cursor)
  (:method ((cursor buffer-cursor))
    (with-accessors ((position cursor-pos)
                     (row cursor-line-position-of)
                     (chain flexichain:chain))
        cursor
      (values row
              (do ((position position (1- position))
                   (column 0 (1+ column)))
                  ((or (<= position 0)
                       (char= #\Newline (element* chain (1- position)))) ; FIXME use buffer-local line encoding
                   column))))))
