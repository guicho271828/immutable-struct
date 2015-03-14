#|
  This file is a part of optima-immutable-struct project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

(in-package :optima-immutable-struct)

(cl:defstruct id-mixin
  "A mix-in that adds a slot named `id', which is useful for computing a hash value."
  (id (random MOST-POSITIVE-FIXNUM) :type fixnum))

;; (canonicalize-defstruct-form
;;  'id-mixin
;;  "A mix-in that adds a slot named `id', which is useful for computing a hash value."
;;  '((id (random MOST-POSITIVE-FIXNUM) :type fixnum)))

