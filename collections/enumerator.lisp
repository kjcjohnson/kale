(in-package #:com.kjcjohnson.kale.collections)

;;;
;;; Interface definitions for enumerators and enumerables
;;;
(kl/oo:define-encapsulated-class &enumerator

  (public property current :get (error "Not implemented"))
  (public move-next () (error "Not implemented"))
  (public reset () (error "Not implemented")))

(kl/oo:define-encapsulated-class &enumerable
    
  (public get-enumerator () (error "Not implemented")))

;;;
;;; Implement the generic enumerator protocol
;;;
(defmethod kl:enumerator-get-enumerator ((thing &enumerable))
  (&enumerable:get-enumerator thing))

(defmethod kl:enumerator-move-next ((enumerator &enumerator))
  (&enumerator:move-next enumerator))

(defmethod kl:enumerator-current ((enumerator &enumerator))
  (&enumerator:current enumerator))