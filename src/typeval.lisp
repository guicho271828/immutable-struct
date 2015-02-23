#|
  This file is a part of optima-immutable-struct project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :optima-immutable-struct)

;;; utilities
(defmacro getenv (&environment env) env)
(defmacro %macroexpand (env forms)
  `(progn ,@(%%macroexpand env forms)))
(defun %%macroexpand (env forms)
  (mapcar (lambda (form) (macroexpand form env)) forms))
(defun expand-with-local-macros (definitions body)
  (let ((local-env
         (#-sbcl progn #+sbcl sb-ext:without-package-locks
                 (eval `(macrolet ,definitions
                          (getenv))))))
    `(%macroexpand ,local-env ,body)))

;;; define-with-typevar

(defmacro define-with-typevar (typevars &body body)
  (expand-with-local-macros
   `((defstruct (name-and-options &optional documentation &rest slots)
       (%defstruct-with-typevar ',typevars name-and-options documentation slots))
     (ftype (name-or-names &rest types)
            (%ftype-with-typevar ',typevars name-or-names types))
     (defun (name args &body body)
         (%defun-with-typevar ',typevars name args body)))
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


(defun instantiate-name-and-options (name-and-options typevar-typeval)
  (ematch typevar-typeval
    ((cons _ typeval)
     (ematch name-and-options
       ((list* name (and options (assoc :constructor
                                        (list* (eq name) args))))
        (let* ((ground-name (symbolicate name '/ typeval))
               (new-options (cons (list* :constructor ground-name args)
                                  (remove :constructor options :key #'car))))
          (list* ground-name new-options)))))))

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

       (with-gensyms (slot)
         `(setf (symbol-typevar-structure ',name)
                (lambda ,typevars ; (<s>)
                  (let ((ground-slots
                         (mapcar (lambda (,slot)
                                   (reduce #'instantiate-slot-type
                                           (mapcar #'cons ',typevars (list ,@typevars))
                                           :initial-value ,slot))
                                 ',slots))
                        (ground-name-and-options
                         (reduce #'instantiate-name-and-options
                                 (mapcar #'cons ',typevars (list ,@typevars))
                                 :initial-value ',name-and-options)))
                    (list* 'defstruct
                           ground-name-and-options
                           ,documentation
                           ground-slots)))))))))

;;; %ftype-with-typevar

(defun %ftype-with-typevar (typevars name-or-names types)
  (warn "no impl yet"))

;;; %defun-with-typevar

(defun %defun-with-typevar (typevars name args body)
  (warn "no impl yet"))

;;; instantiation

(defun instantiate-structure-form (name &rest types)
  (apply (symbol-typevar-structure name) types))

(defun instantiate-structure (name &rest types)
  (eval (apply #'instantiate-structure-form name types)))

