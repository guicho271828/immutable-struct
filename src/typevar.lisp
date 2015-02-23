#|
  This file is a part of optima-immutable-struct project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :optima-immutable-struct)

;;; utilities
(defmacro getenv (&environment env) env)
(defun expand-with-local-macros (definitions body)
  (let ((local-env
         (#-sbcl progn #+sbcl sb-ext:without-package-locks
                 (eval `(macrolet ,definitions
                          (getenv))))))
    (macroexpand `(%macroexpand ,local-env ,body))))
(defmacro %macroexpand (env forms)
  `(progn ,@(%%macroexpand env forms)))
(defun %%macroexpand (env forms)
  (mapcar (lambda (form) (macroexpand form env)) forms))

;;; define-with-typevar

(defmacro define-with-typevar (typevars &body body)
  (expand-with-local-macros
   `((defstruct (name-and-options &optional documentation &rest slots)
       (%defstruct-with-typevar ',typevars name-and-options documentation slots))
     (ftype (name-or-names &rest types)
            (%ftype-with-typevar ',typevars name-or-names types))
     ;; (defun (name args &body body)
     ;;     (%defun-with-typevar ',typevars name args body))
     )
   body))

;; prevent recursive expansion

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (macro-function 'safe-defstruct) (macro-function 'defstruct))
  (setf (macro-function 'safe-ftype) (macro-function 'ftype))
  (setf (macro-function 'safe-defun) (macro-function 'defun)))

;;; %defstruct-with-typevar

(defun slot-type (slotform)
  (match slotform
    ((list* _ _ (property :type type))
     type)))

(defun instantiate-slot-type (slotform typevar-typeval)
  (match typevar-typeval
    ((cons typevar typeval)
     (match slotform
       ((list* name default (and options (property :type (eq typevar))))
        (let ((options (copy-list options)))
          (setf (getf options :type) typeval)
          (list* name default options)))
       (_ slotform)))))



(defun instantiate-name (name typevals)
  (reduce (lambda (name type)
            (symbolicate name '/ type))
          typevals :initial-value name))

(defun instantiate-name-and-options (name-and-options ground-name)
  (ematch name-and-options
    ((list* name (and options (assoc :constructor
                                     (list* (eq name) args))))
     `(,ground-name
       (:include name)
       (:constructor ,ground-name ,@args)
       ,@(remove :constructor options :key #'car)))))

(lispn:define-namespace typevar-structure function)

(defun %defstruct-with-typevar (typevars name-and-options documentation slots)
  (multiple-value-match (canonicalize-defstruct-form name-and-options documentation slots)
    (((and name-and-options (list* name _))  documentation slots)
     (let ((typevars (intersection typevars (mapcar #'slot-type slots))))
       ;; ignore typevars which are not used in this structure
       (unless typevars
         ;; no typevars
         (warn 'simple-style-warning
               :format-control "Structure ~a inside define-with-typevar does not use any typevar"
               :format-arguments (list name))
         (return-from %defstruct-with-typevar
           `(safe-defstruct ,name-and-options ,documentation ,@slots)))
       (%%defstruct typevars name documentation name-and-options slots)))))

(defun %%defstruct (typevars name documentation name-and-options slots)
  (let ((tb-name (symbolicate '* name '-types-table '*)))
    (with-gensyms (slot)
      `(progn
         (defvar ,tb-name (make-hash-table :test #'equal))
         (deftype ,name (,@typevars)
           (gethash (list ,@typevars) ,tb-name))
         (setf (symbol-typevar-structure ',name)
               (lambda ,typevars ; (<s>)
                 (let ((ground-slots
                        (mapcar (lambda (,slot)
                                  (reduce #'instantiate-slot-type
                                          (mapcar #'cons ',typevars (list ,@typevars))
                                          :initial-value ,slot))
                                ',slots))
                       (ground-name-and-options
                        (instantiate-name-and-options
                         ',name-and-options
                         (instantiate-name ',name (list ,@typevars)))))
                   (match ground-name-and-options
                     ((list* gname _)
                      `(progn
                         (setf (gethash '(,,@typevars) ,',tb-name) ',gname)
                         (defstruct
                             ,ground-name-and-options
                           ,',documentation
                           ,@ground-slots)))))))))))

;;; %ftype-with-typevar

(lispn:define-namespace typevar-ftype function)

(defun %ftype-with-typevar (typevars name-or-names types)
  (let ((typevars (intersection typevars (remove-duplicates
                                          (flatten types))))
        ;; WARN: do not forget there is #'(setf XXX) !!
        (names (canonicalize-name-or-names name-or-names)))
    ;; ignore typevars which are not used in this structure
    (unless typevars
      ;; no typevars
      (warn 'simple-style-warning
            :format-control "FTYPE of ~a inside define-with-typevar does not use any typevar"
            :format-arguments (list names))
      (return-from %ftype-with-typevar
        `(safe-ftype ,names ,@types)))
    
    `(progn
       ,@(mapcar (lambda (name)
                   `(setf (symbol-typevar-ftype ',name)
                          (lambda ,typevars
                            `(ftype ,(instantiate-name ',name (list ,@typevars))
                                    ,@(subst-all (list ,@typevars)
                                                 ',typevars
                                                 ',types)))))
                 names))))

(defun subst-all (news olds tree)
  (reduce (lambda (tree pair)
            (subst (car pair) (cdr pair) tree))
          (mapcar #'cons news olds)
          :initial-value tree))

;;; %defun-with-typevar

(defun %defun-with-typevar (typevars name args body)
  
  (warn "no impl yet"))

;;; instantiation

(defun instantiate-structure-form (name &rest types)
  (handler-bind ((program-error (lambda (c)
                                  (declare (ignore c))
                                  (error "insufficient number of typevar"))))
    (apply (symbol-typevar-structure name) types)))

(defun instantiate-structure (name &rest types)
  (eval (apply #'instantiate-structure-form name types)))


(defun instantiate-ftype-form (name &rest types)
  (handler-bind ((program-error (lambda (c)
                                  (declare (ignore c))
                                  (error "insufficient number of typevar"))))
    (apply (symbol-typevar-ftype name) types)))

(defun instantiate-ftype (name &rest types)
  (eval (apply #'instantiate-ftype-form name types)))

;; (defun instantiate-defun-form (name &rest types)
;;   (handler-bind ((program-error (lambda (c)
;;                                   (declare (ignore c))
;;                                   (error "insufficient number of typevar"))))
;;     (apply (symbol-typevar-defun name) types)))
;; 
;; (defun instantiate-defun (name &rest types)
;;   (eval (apply #'instantiate-defun-form name types)))

