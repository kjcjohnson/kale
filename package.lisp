;;;;
;;;; Top-level Kale package
;;;;
(defpackage #:com.kjcjohnson.kale
  (:use #:cl)

  (:export #:foreach
           #:enumerator-get-enumerator
           #:enumerator-move-next
           #:enumerator-current))
