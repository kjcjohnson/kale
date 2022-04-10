;;;;
;;;; System definition for Keith's Encapsulated Classes
;;;;
(asdf:defsystem "com.kjcjohnson.kale.encapsulated-classes"
  :description "Encapsulated classes for Common Lisp"
  :version "0.0.1"
  :author "Keith Johnson <kjcjohnson@ymail.com>"
  :license "MIT or Apache 2"
  :depends-on ("trivial-package-local-nicknames")
  :components ((:file "encapsulated")))
