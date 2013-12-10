;;; -*- mode: lisp -*-
(in-package :cl-user)

(defpackage :with-star-asd
  (:use :cl :asdf))

(in-package :with-star-asd)

(defsystem :with.star
  :version "13.12.0"
  :author "Masatoshi SANO"
  :components ((:module "src"
                :components 
                ((:file "package")
                 (:file "utils" :depends-on ("package"))
                 (:file "defmacro" :depends-on ("package"))
                 #+not-yet-defined(:file "defun" :depends-on ("defmacro"))
                 #+not-yet-defined(:file "defmethod" :depends-on ("defmacro"))
                 #+not-yet-defined(:file "defgeneric" :depends-on ("defmacro"))
                 #+not-yet-defined(:file "defclass" :depends-on ("defmacro"))
                 #+not-yet-defined(:file "defvar" :depends-on ("defmacro"))
                 #+not-yet-defined(:file "defparameter" :depends-on ("defmacro"))
                 #+not-yet-defined(:file "defconstant" :depends-on ("defmacro")))))
  :depends-on ())
