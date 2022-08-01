;;;;
;;;; Comparison functions
;;;;
(in-package #:com.kjcjohnson.kale)

(defgeneric equals (a b)
  (:documentation "Checks if A and B equal each other (specific to types)")
  (:method (a b) (equal a b)))

(defgeneric get-hash-code (obj)
  (:documentation "Computes a hash code for OBJ")
  (:method (obj) (sxhash obj)))

#+sbcl
(sb-ext:define-hash-table-test equals get-hash-code)
