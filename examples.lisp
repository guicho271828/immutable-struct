
(in-package :optima-immutable-struct)

;;; typevar example

(define-with-typevar (<s>)
  (defstruct coordinate
    (x 0)
    (y 0))
  (ftype my-+ fixnum fixnum fixnum))

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


(print (instantiate-structure-form 'coordinate 'fixnum))
(instantiate-structure 'coordinate 'fixnum)

;; error
(print (instantiate-structure-form 'coordinate2 'fixnum))

(print (instantiate-structure-form 'coordinate2 'fixnum 'float))
(instantiate-structure 'coordinate2 'fixnum 'float)

(print (multiple-value-list (instantiate-ftype-form 'vector-+ 'fixnum 'float)))
(instantiate-ftype 'vector-+ 'fixnum 'float)
