#|
  This file is a part of optima-immutable-struct project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage optima-immutable-struct
  (:use :cl :optima :alexandria)
  (:shadow :defstruct)
  (:export
   #:defstruct))
(in-package :optima-immutable-struct)

;; blah blah blah.

(defmacro defstruct (name-and-options &rest slots)
  (ematch name-and-options
    ((or (list* name _)
         (and (symbol) name))
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (cl:defstruct (,@name-and-options
                       (:constructor ,name ,@(mapcar #'car (mapcar #'ensure-list slots))))
          ,@slots)
        (defpattern ,name (&optional
                           ,@(mapcar (lambda (slot)
                                       (match slot
                                         ((symbol) `(,slot '_))
                                         ((list* slot _)
                                          `(,slot '_))))
                                     slots))
          `(,name ,@(mapcar (lambda (slot)
                              (match slot
                                ((symbol) `(,slot '_))
                                ((list* slot _)
                                 `(,slot '_))))
                            slots)))))))

