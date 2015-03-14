#|
  This file is a part of immutable-struct project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage immutable-struct-asd
  (:use :cl :asdf))
(in-package :immutable-struct-asd)


(defsystem immutable-struct
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:optima :alexandria :lisp-namespace :compiler-macro)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "id"))))
  :description "Library that encourage the use of functional programming + pattern matching"
  :in-order-to ((test-op (load-op immutable-struct.test))))
