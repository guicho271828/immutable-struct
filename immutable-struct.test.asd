#|
  This file is a part of immutable-struct project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#


(in-package :cl-user)
(defpackage immutable-struct.test-asd
  (:use :cl :asdf))
(in-package :immutable-struct.test-asd)


(defsystem immutable-struct.test
  :author "Masataro Asai"
  :license "LLGPL"
  :depends-on (:immutable-struct
               :fiveam)
  :components ())

;; now removed
