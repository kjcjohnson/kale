;;;;
;;;; System definition for the Kale Veneer
;;;;
(asdf:defsystem "com.kjcjohnson.kale"
  :description "A standard library veneer for Common Lisp"
  :version "0.0.1"
  :author "Keith Johnson <kjcjohnson@ymail.com>"
  :license "MIT or Apache 2"
  :depends-on ("trivial-package-local-nicknames" "closer-mop")
  :components ((:file "package")
               (:module "general"
                :components ((:file "foreach")
                             (:file "comparison")))
               (:module "oo"
                :components ((:file "encapsulated")))
               (:module "collections"
                :components ((:file "package")
                             (:file "enumerator" :depends-on ("package"))
                             (:file "dictionary" :depends-on ("package")))
                :depends-on ("general"))))
               
