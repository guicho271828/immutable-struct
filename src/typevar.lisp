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
  `(locally ,@(%%macroexpand env forms)))
(defun %%macroexpand (env forms)
  (mapcar (lambda (form) (macroexpand form env)) forms))

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

(defun instantiate-slot (slotform typevar-typeval)
  (match typevar-typeval
    ((cons typevar typeval)
     (match slotform
       ((list* name default (and options (property :type (eq typevar))))
        (let ((options (copy-list options)))
          (setf (getf options :type) typeval)
          (list* name (coerce default typeval) options)))
       (_ slotform)))))

(defun instantiate-name-and-options (name-and-options typevals ground-slots)
  (ematch name-and-options
    ((list* name options)
     `(,(instantiate-name name typevals)
        (:include ,name ,@ground-slots
                  ;;,@(mapcar #'car ground-slots)
                  )
        ,@options))))

(defun instantiate-name (name typevals)
  (reduce (lambda (name type)
            (symbolicate name '/ type))
          typevals :initial-value name))

(defun %defstruct-with-typevar (typevars name-and-options documentation slots)
  (multiple-value-match (canonical-defstruct name-and-options documentation slots)
    (((and name-and-options (list* name _)) documentation slots)
     (let ((typevars (intersection typevars (mapcar #'slot-type slots))))
       ;; ignore typevars which are not used in this structure
       (unless typevars
         ;; no typevars
         (warn 'simple-style-warning
               :format-control "Structure ~a inside define-with-typevar does not use any typevar"
               :format-arguments (list name))
         (return-from %defstruct-with-typevar
           `(safe-defstruct ,name-and-options ,documentation ,@slots)))
       (%%defstruct typevars name-and-options documentation slots)))))

(defvar *typevar-types* (make-hash-table :test #'equal))
(#-sbcl progn #+sbcl sb-ext:without-package-locks
 (deftype / (name &rest args)
   (or (gethash (cons name args) *typevar-types*)
       (progn
         (apply #'instantiate-structure name args)
         (gethash (cons name args) *typevar-types*))
       (error "failed to expand the lifted structure type ~a with arguments ~a" name args))))

(lispn:define-namespace typevar-structure struct-info)
(cl:defstruct (struct-info (:constructor struct-info (expander slots)))
  "defstruct infomation"
  expander slots)

(defun %%defstruct (typevars name-and-options documentation slots)
  (with-gensyms (slot)
    (ematch name-and-options
      ((list* name _)
       `(progn
          (defstruct ,name-and-options
            ,documentation
            ,@(mapcar (lambda (slot)
                        (reduce #'instantiate-slot
                                (mapcar #'cons typevars (mapcar (constantly t) typevars))
                                :initial-value slot))
                      slots))
          (setf (symbol-typevar-structure ',name)
                (struct-info
                 (lambda ,typevars ; (<s>)
                   (let* ((ground-slots
                           (mapcar (lambda (,slot)
                                     (reduce #'instantiate-slot
                                             (mapcar #'cons ',typevars (list ,@typevars))
                                             :initial-value ,slot))
                                   ',slots))
                          (ground-name-and-options
                           (append-constructor
                            (instantiate-name-and-options
                             ',name-and-options
                             (list ,@typevars)
                             ground-slots)
                            ground-slots)))
                     (match ground-name-and-options
                       ((list* gname _)
                        `(progn
                           (setf (gethash '(,',name ,,@typevars) *typevar-types*) ',gname)
                           (defstruct ,ground-name-and-options
                             #+nil ,@ground-slots))))))
                 ',slots)))))))

;;; %ftype-with-typevar

(cl:defstruct (ft-info (:constructor ft-info (ftype defun source)))
  "ftype infomation"
  ftype defun source)

(lispn:define-namespace typevar-function ft-info)
(defun ensure-typevar-function (name)
  (unless (typevar-function-boundp name)
    (setf (symbol-typevar-function name) (ft-info nil nil nil))))


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
                   `(progn
                      (ensure-typevar-function ',name)
                      (setf (ft-info-source (symbol-typevar-function ',name))
                            ',types
                            (ft-info-ftype (symbol-typevar-function ',name))
                            (lambda ,typevars
                              `(ftype ,(instantiate-name ',name (list ,@typevars))
                                      ,@(subst-all (list ,@typevars)
                                                   ',typevars
                                                   ',types))))))
                 names))))

(defun subst-all (news olds tree)
  (reduce (lambda (tree pair)
            (subst (car pair) (cdr pair) tree))
          (mapcar #'cons news olds)
          :initial-value tree))

;;; %defun-with-typevar

(defun %defun-with-typevar (typevars name args body)
  `(progn
     (ensure-typevar-function ',name)
     (setf (ft-info-defun (symbol-typevar-function ',name))
           (lambda ,typevars ; (<s>)
             `(macrolet ,(%generate-constructor-transformer ',name ',typevars (list ,@typevars))
                (defun ,(instantiate-name ',name (list ,@typevars)) ,',args
                  ;; ,(expand-with-local-macros
                  ;;   (%generate-constructor-transformer ',name ',typevars (list ,@typevars))
                  ;;   ',body)
                  ,@',body))))))

(defun %generate-constructor-transformer (name typevars typevals)
  (let ((types (ft-info-source (symbol-typevar-function name)))
        (type-alist (mapcar #'cons typevars typevals)))
    ;; e.g. ((/ coordinate2 <s> <t>) (/ coordinate2 <s> <t>) (/ coordinate2 <s> <t>))
    (let (found)
      (labels ((rec (tree)
                 (match tree
                   ((list* '/ args) (push args found))
                   ((list* _ args)
                    (map nil #'rec args)))))
        (map nil #'rec types))
      ;; found : ((coordinate2 <s> <t>) (oordinate2 <s> <t>) (coordinate2 <s> <t>))
      (setf found (remove-duplicates found))
      ;; found : ((coordinate2 <s> <t>))
      (mappend (lambda (type)
                 (match type
                   ((list* lifted parameters)
                    ;; (coordinate2 args...) -> (coordinate/fixnum/float args...)
                    ;; (coordinate2-x arg) -> (coordinate/fixnum/float-x arg)
                    ;; (coordinate2-y arg) -> (coordinate/fixnum/float-y arg)
                    ;; (coordinate2-z arg) -> (coordinate/fixnum/float-z arg)
                    (let* ((ground (instantiate-name
                                    lifted
                                    (mapcar (lambda (p) (cdr (assoc p type-alist)))
                                            parameters)))
                           (slots (struct-info-slots (symbol-typevar-structure lifted))))
                      `((,lifted (&rest args) `(,',ground ,@args))
                        ,@(mapcar (lambda (slot)
                                    `(,(symbolicate lifted '- slot) (arg)
                                       `(,',(symbolicate ground '- slot) ,arg)))
                                  (mapcar #'car slots)))))))
               found))))

;;; instantiation

(defun instantiate-structure-form (name &rest types)
  (handler-bind ((program-error (lambda (c)
                                  (declare (ignore c))
                                  (error "insufficient number of typevar"))))
    (apply (struct-info-expander (symbol-typevar-structure name)) types)))

(defun instantiate-structure (name &rest types)
  (format *trace-output* "~&; Instantiating a structure ~A with typevars ~A" name types)
  (eval (apply #'instantiate-structure-form name types)))


(defun instantiate-ftype-form (name &rest types)
  (handler-bind ((program-error (lambda (c)
                                  (declare (ignore c))
                                  (error "insufficient number of typevar"))))
    (values (apply (ft-info-ftype (symbol-typevar-function name)) types)
            (apply (ft-info-defun (symbol-typevar-function name)) types))))

(defun instantiate-ftype (name &rest types)
  (format *trace-output* "~&; Instantiating a function ~A with typevars ~A" name types)
  (multiple-value-bind (ftype defun)
      (apply #'instantiate-ftype-form name types)
    (handler-bind ((error (lambda (c) (signal c))))
      (eval (print ftype))
      (eval defun))))

;;; 
