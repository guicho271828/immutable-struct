#|
  This file is a part of optima-immutable-struct project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :cl-user)
(defpackage :optima-immutable-struct.test
  (:use :cl
        :optima-immutable-struct
        :fiveam
        :optima :alexandria :iterate)
  (:shadowing-import-from
   :optima-immutable-struct
   :ftype :defstruct)
  (:shadow :fail))
(in-package :optima-immutable-struct.test)



(def-suite :optima-immutable-struct)
(in-suite :optima-immutable-struct)

;; run test with (run! test-name) 
;;   test as you like ...

(test no-typevar-used
  (finishes ; signals warning
    (define-with-typevar (<s>)
      (defstruct coordinate
        (x 0)
        (y 0))
      (ftype my-+ fixnum fixnum fixnum))))

(test optima-immutable-struct
  (define-with-typevar (<s> <t>)
    (defstruct coordinate2
      (x 0 :type <s>)
      (y 0 :type <t>)
      (z 0 :type <s>))
    (ftype vector-+ (/ coordinate2 <s> <t>) (/ coordinate2 <s> <t>) (/ coordinate2 <s> <t>))
    (defun vector-+ (v1 v2)
      (declare (optimize (speed 3) (space 0) (safety 0) (debug 0)))
      (coordinate2
       (+ (coordinate2-x v1)
          (coordinate2-x v2))
       (+ (coordinate2-y v1)
          (coordinate2-y v2))
       (+ (coordinate2-z v1)
          (coordinate2-z v2)))))

  (signals error
    ;; insufficient number of arguments
    (instantiate-structure-form 'coordinate2 'fixnum))
  
  (finishes
   (instantiate-structure 'coordinate2 'fixnum 'float))

  (finishes
   (instantiate-ftype 'vector-+ 'fixnum 'float))

  (finishes
   (instantiate-ftype 'vector-+ 'fixnum 'fixnum)))


