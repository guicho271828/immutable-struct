
;;;; the sexp below is an API design experiment, not a valid lisp source!

(in-package :optima-immutable-struct)


(defmacro define-with-typevar (name typevars &body body)
  (let ((body-template `(progn ,@body)))
    `(setf (symbol-typevar-closure ',name)
           (lambda ,typevars ; list of types etc.
             (eval 
              (print
               (subst-all (list ,@typevars) ',typevars
                          ',body-template)))))))

(defun subst-all (news olds tree)
  (reduce (lambda (tree pair) (subst (car pair) (cdr pair) tree))
          (mapcar #'cons news olds)
          :initial-value tree))

(defun ensure-typevar (name &rest args)
  (apply (symbol-typevar-closure name) args))

;; example

(define-with-typevar heap (<foo> <s>)
  (defstruct <foo>
    (bar 0 :type <s>)))

(define-with-typevar heap (<foo> <s>)
  `(defstruct ,<foo>
     (bar 0 :type ,<s>)))

(define-with-typevar heap (foo s)
  `(defstruct ,foo
     (bar 0 :type ,s)))

;; (setf *break-on-signals* t)
;; (setf *break-on-signals* nil)
(defstruct <foo>
  (bar 0 :type <s>))


;; SB-KERNEL:PARSE-UNKNOWN-TYPE
;; SB-KERNEL:PARSE-UNKNOWN-TYPE-SPECIFIER

(ensure-typevar 'heap 'foo-fixnum 'fixnum)

;; example2

(defmacro define-with-typevar (name typevars affected &body body)
  (let ((body-template `(progn ,@body)))
    `(setf (symbol-typevar-closure ',name)
           (lambda ,typevars ; list of types etc.
             (eval 
              (print
               (subst-all (list ,@typevars) ',typevars
                          ',body-template)))))))

(define-with-typevar <coordinate> (<t>) (<vector-+>)
  (defstruct <coordinate>
    (x 0 :type <t>)
    (y 0 :type <t>))

  (deftype coordinate (<t>)
    `<coordinate>)
  
  (declaim (ftype (function (<coordinate> <coordinate>) <coordinate>) <vector-+>))
  (defun <vector-+> (v w)
    (match* (v w)
      (((<coordinate> x1 y1) (<coordinate> x2 y2))
       (<coordinate> (+ x1 x2) (+ y1 y2))))))

(instantiate '<coordinate> fixnum)

;;; expected-result:

(defstruct coordinate/fixnum
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(declaim (ftype (function (coordinate/fixnum coordinate/fixnum) coordinate/fixnum) vector-+/fixnum))
(defun vector-+/fixnum (v w)
  (match* (v w)
    (((coordinate/fixnum x1 y1) (coordinate/fixnum x2 y2))
     (coordinate/fixnum (+ x1 x2) (+ y1 y2)))))

;; another try

(defstruct (coordinate (:typevar (<t>)))
  (x 0 :type <t>)
  (y 0 :type <t>))

;; -> expands into

(deftype coordinate (<t>)
  (combine-names 'coordinate <t>))

;; type (coordinate fixnum) -> coordinate/fixnum



(coordinate 1.0 1)
;; --> implicitly calls below, on compile time

(instantiate-struct 'coordinate 'real)
;; which is equal to
(defstruct coordinate/real
  (x 0 :type real)
  (y 0 :type real))

;; also, type (coordinate real) expands into coordinate/real

;; then
(coordinate/real 1.0 1)



(define-with-typevar coordinate (s) (<vector-+>)
  (defstruct <coordinate s>
    (x 0 :type <s>)
    (y 0 :type <s>))
  ;; defines deftype (coordinate <s>) implicitly
  ;; defines defpattern (coordinate <s>) implicitly?
  
  (ftype <vector-+> <coordinate s> <coordinate s> <coordinate s>)
  (defun <vector-+> (v w)
    (match* (v w)
      (((<coordinate s> x1 y1)
        (<coordinate s> x2 y2))
       (<coordinate s> (+ x1 x2) (+ y1 y2))))))

;;;; 

(defpattern (coordinate real) (x y)
  `(coordinate/real (x ,x) (y ,y)))

;; optima throws error:  (coordinate real) not symbol

;;;; 

(define-with-typevar coordinate (<s>) (vector-+)
  (defstruct coordinate
    (x 0 :type <s>)
    (y 0 :type <s>))
  ;; defines deftype (coordinate <s>) implicitly?
  ;; defines defpattern (coordinate <s>) implicitly?
  ;; defines defun (coordinate x y) [dispatching] implicitly
  
  (ftype vector-+ (coordinate <s>) (coordinate <s>) (coordinate <s>))
  (defun vector-+ (v w)
    (match* (v w)
      (((coordinate <s> x1 y1)
        (coordinate <s> x2 y2))
       (coordinate <s> (+ x1 x2) (+ y1 y2))))))

(define-with-typevar (<s>)
  (defstruct coordinate
    (x 0 :type <s>)
    (y 0 :type <s>))
  ;; defines deftype (coordinate <s>) implicitly?
  ;; defines defpattern (coordinate <s>) implicitly?
  ;; defines defun (coordinate x y) [dispatching] implicitly
  
  (ftype vector-+ (coordinate <s>) (coordinate <s>) (coordinate <s>))
  (defun vector-+ (v w)
    (match* (v w)
      (((coordinate <s> x1 y1)
        (coordinate <s> x2 y2))
       (coordinate <s> (+ x1 x2) (+ y1 y2))))))



