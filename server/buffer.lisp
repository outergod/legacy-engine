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
          :initform (make-recursive-lock))
   (newlines :reader buffer-newlines-of
             :initform (make-container 'sorted-dlist-container)))
  (:default-initargs :element-type 'character :fill-element #\Null))

(defclass buffer-cursor (right-sticky-flexicursor)
  ((virtual-column :accessor buffer-cursor-virtual-column-of
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
  (declare (ignore initargs))
  (with-accessors ((newlines buffer-newlines-of))
      buffer
; fake newlines at -1 and (initially) 0 are extremely helpful to prevent annoying IFs
    (insert-list newlines (list -1 0))))


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

(defgeneric cursor-next-newline-node (buffer-cursor &optional position)
  (:method ((cursor buffer-cursor) &optional (position (cursor-pos cursor)))
    (with-accessors ((buffer flexichain:chain))
        cursor
      (with-accessors ((newlines buffer-newlines-of))
          buffer
        (search-for-matching-node newlines #'(lambda (node)
                                               (>= (element node) position)))))))

(defun adjust-right-newlines (container position function)
  (when-let ((node (search-for-matching-node container #'(lambda (node)
                                                           (>= (element node) position)))))
    (funcall function node)
    (iterate-right-nodes container node function)))

(defmethod insert* :around ((buffer buffer) position object)
  (with-accessors ((newlines buffer-newlines-of))
      buffer
    (call-next-method) ; don't execute if error is signalled here
    (adjust-right-newlines newlines position #'(lambda (node)
                                                 (incf (element node))))
    (when (char= #\Newline object)
      (insert-item newlines position))))

(defmethod delete* :around ((buffer buffer) position)
  (with-accessors ((newlines buffer-newlines-of))
      buffer
    (let ((newline-p (char= #\Newline (element* buffer position))))
      (call-next-method) ; don't execute if error is signalled here
      (when newline-p
        (delete-item newlines position))
      (adjust-right-newlines newlines position #'(lambda (node)
                                                   (decf (element node)))))))

(defmethod insert :after ((cursor buffer-cursor) object)
  (declare (ignore object))
  (with-accessors ((column buffer-cursor-virtual-column-of))
      cursor
    (setq column (nth-value 1 (cursor-2d-position cursor)))))

(defmethod delete< :after ((cursor buffer-cursor) &optional n)
  (declare (ignore n))
  (with-accessors ((column buffer-cursor-virtual-column-of))
      cursor
    (setq column (nth-value 1 (cursor-2d-position cursor)))))

(defgeneric buffer-string (buffer)
  (:method ((buffer buffer))
    (coerce
     (loop for index from 0 below (nb-elements buffer) collect (element* buffer index))
     'string)))


(defgeneric cursor-next-newline-node (buffer-cursor &optional position)
  (:method ((cursor buffer-cursor) &optional (position (cursor-pos cursor)))
    (with-accessors ((buffer flexichain:chain))
        cursor
      (with-accessors ((newlines buffer-newlines-of))
          buffer
        (search-for-matching-node newlines #'(lambda (node)
                                               (>= (element node) position)))))))

(defgeneric cursor-previous-newline-node (buffer-cursor &optional position)
  (:method ((cursor buffer-cursor) &optional (position (cursor-pos cursor)))
    (when-let ((node (cursor-next-newline-node cursor position)))
      (previous-item node))))

(defgeneric cursor-2d-position (buffer-cursor)
  (:method ((cursor buffer-cursor))
    (with-accessors ((position cursor-pos)
                     (buffer flexichain:chain))
        cursor
      (with-accessors ((newlines buffer-newlines-of))
          buffer
        (let* ((node (cursor-previous-newline-node cursor))
               (row (element-position newlines (element node)))
               (row-position (1+ (element node))))
          (values row (- position row-position)))))))

(defgeneric update-virtual-column (buffer-cursor)
  (:method ((cursor buffer-cursor))
    (with-accessors ((column buffer-cursor-virtual-column-of))
        cursor
      (setq column (nth-value 1 (cursor-2d-position cursor))))))

(defgeneric buffer-cursor-lineward (buffer-cursor delta)
  (:method ((cursor buffer-cursor) delta)
    (with-accessors ((position cursor-pos))
        cursor
      (incf position delta)))
  (:method :after ((cursor buffer-cursor) delta)
           (declare (ignore delta))
           (update-virtual-column cursor)))

(defgeneric buffer-cursor-columnward (buffer-cursor delta)
  (:method ((cursor buffer-cursor) delta)
    (with-accessors ((position cursor-pos)
                     (buffer flexichain:chain)
                     (column buffer-cursor-virtual-column-of))
        cursor
      (with-accessors ((newlines buffer-newlines-of))
          buffer
        (let ((row (+ delta
                      (nth-value 0 (cursor-2d-position cursor))))
              (size (- (cl-containers:size newlines) 2)))
          (when (and (>= row 0) (<= row size))
            (let ((row-position (1+ (nth-element newlines row))))
              (setq position (+ row-position
                                (min column
                                     (- (nth-element newlines (1+ row))
                                        row-position)))))))))))
