
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


(instantiate-structure-form 'coordinate 'fixnum)
