#|
  This file is a part of optima-immutable-struct project.
  Copyright (c) 2015 Masataro Asai (guicho2.71828@gmail.com)
|#

#|
  Author: Masataro Asai (guicho2.71828@gmail.com)
|#



(in-package :cl-user)
(defpackage optima-immutable-struct-asd
  (:use :cl :asdf))
(in-package :optima-immutable-struct-asd)


(defsystem optima-immutable-struct
  :version "0.1"
  :author "Masataro Asai"
  :mailto "guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:optima :alexandria :lisp-namespace :compiler-macro)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "id"))))
  :description ""
  :in-order-to ((test-op (load-op optima-immutable-struct.test))))
