#|
  This file is a part of optima-immutable-struct project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage optima-immutable-struct
  (:use :cl :optima :alexandria)
  (:shadow :defstruct :ftype)
  (:export
   :defstruct
   :ftype
   :id-mixin))
(in-package :optima-immutable-struct)

;; blah blah blah.

(defmacro defstruct (name-and-options &rest slots)
  "A variation of defstruct, with read-only slots and automatically defined constructor.
+ The constructor name has the different convention compared to the default naming convention in cl.
  It has (<name> &optional <slots...>) and has no keyword argument.
+ It adds read-only option in cl:defstruct to each slot definition.
+ It uses the noninterned symbols for the name of each slot, disallowing the use of slot-value.
+ It also defines a pattern matcher clause in exactly the same form as the constructor.
"
  (let ((slots (mapcar (lambda (slot)
                         (ematch slot
                           ((list* (structure symbol (-name name)) initform options)
                            (list* (make-symbol name) initform :read-only t options))
                           ((structure symbol (-name name))
                            (list (make-symbol name) nil :read-only t))))
                       slots)))
    (ematch (ensure-list name-and-options)
      ((list* name options)
       `(eval-when (:compile-toplevel :load-toplevel :execute)
          (cl:defstruct (,name
                         ,@options
                         (:constructor ,name (&optional ,@(mapcar #'car slots))))
            ,@slots)
          ,(%defpattern name slots))))))

(defun %defpattern (name slots)
  (let ((slots-optional-args
         (mapcar (lambda (slot)
                   (ematch slot
                     ((list* slot _) `(,slot '_))))
                 slots)))
    `(defpattern ,name (&optional ,@slots-optional-args)
       (list 'structure ',(symbolicate name '-)
             ,@(mapcar (lambda (slot)
                         (ematch (ensure-list slot)
                           ((list* slot _ (property :type type))
                            ``(,',slot (and ,,slot (type ,',type))))
                           ((list* slot _) ;; see below
                            ``(,',slot ,,slot))))
                       slots)))))

#+nil
(defstruct rb-node
  (color :red :type symbol)
  (left (leaf) :type rb-tree)
  (label 0 :type real)
  content ;; <--------  non-typed slot in the middle
  (right (leaf) :type rb-tree))

(defmacro ftype (name-or-names &rest types)
  "abbreviation of (declaim (ftype (function (<types...>) <type>)
<name>)). the last type is used for the return type."
  `(declaim (cl:ftype (function ,(butlast types) ,(lastcar types))
                      ,@(match name-or-names
                          ((list 'setf _) (list name-or-names))
                          (_ (ensure-list name-or-names))))))
