
(in-package :optima-immutable-struct)

;;; typevar example

(define-with-typevar (<s>)
  (defstruct coordinate
    (x 0)
    (y 0)))

(define-with-typevar (<s>)
  (defstruct coordinate
    (x 0 :type <s>)
    (y 0 :type <s>)))

(define-with-typevar (<s> <t>)
  (defstruct coordinate2
    (x 0 :type <s>)
    (y 0 :type <t>)))


(print (instantiate-structure-form 'coordinate 'fixnum))
(instantiate-structure 'coordinate 'fixnum)
(print (instantiate-structure-form 'coordinate2 'fixnum))

(print (instantiate-structure-form 'coordinate2 'fixnum 'float))

SB-INT:SIMPLE-PROGRAM-ERROR
