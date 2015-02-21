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
   :ftype))
(in-package :optima-immutable-struct)

;; blah blah blah.

(defmacro defstruct (name-and-options &rest slots)
  (ematch name-and-options
    ((list* name _)
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (cl:defstruct (,@name-and-options
                       (:constructor ,name ,(mapcar #'car (mapcar #'ensure-list slots))))
          ,@slots)
        ,(%defpattern name slots)))
    ((and (symbol) name)
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (cl:defstruct (,name
                       (:constructor ,name ,(mapcar #'car (mapcar #'ensure-list slots))))
          ,@slots)
        ,(%defpattern name slots)))))

(defun %defpattern (name slots)
  (let ((slots-optional-args
         (mapcar (lambda (slot)
                   (match slot
                     ((or (symbol) (list* slot _)) `(,slot '_))))
                 slots)))
    `(defpattern ,name (&optional ,@slots-optional-args)
       (list ',name
             ,@(mapcar (lambda (slot)
                         (match slot
                           ((or (symbol) (list* slot _))
                            ``(,',slot ,,slot))))
                       slots)))))

(defmacro ftype (name &rest types)
  "abbreviation of (declaim (ftype (function (<types...>) <type>)
<name>)). the last type is used for the return type."
  `(declaim (cl:ftype (function ,(butlast types) ,(lastcar types)) ,name)))
