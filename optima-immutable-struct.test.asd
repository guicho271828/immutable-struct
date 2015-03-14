#|
  This file is a part of optima-immutable-struct project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage optima-immutable-struct.test-asd
  (:use :cl :asdf))
(in-package :optima-immutable-struct.test-asd)


(defsystem optima-immutable-struct.test
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:optima-immutable-struct
               :fiveam)
  :components ())

;; now removed
