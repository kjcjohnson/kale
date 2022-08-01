;;;;
;;;; Top-level Kale package
;;;;
(defpackage #:com.kjcjohnson.kale
  (:use #:cl)

  (:export #:equals
           #:get-hash-code
           #:foreach
           #:enumerator-get-enumerator
           #:enumerator-move-next
           #:enumerator-current))
