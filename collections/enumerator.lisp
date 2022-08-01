(in-package #:com.kjcjohnson.kale.collections)

;;;
;;; Interface definitions for enumerators and enumerables
;;;
(kl/oo:define-encapsulated-class &enumerator

  (public property current :get (error "Not implemented"))
  (public move-next () :prototype)
  (public reset () :prototype))

(kl/oo:define-encapsulated-class &enumerable
    
  (public get-enumerator () :prototype))

;;;
;;; Implement the generic enumerator protocol
;;;
(defmethod kl:enumerator-get-enumerator ((thing &enumerable))
  (&enumerable:get-enumerator thing))

(defmethod kl:enumerator-move-next ((enumerator &enumerator))
  (&enumerator:move-next enumerator))

(defmethod kl:enumerator-current ((enumerator &enumerator))
  (&enumerator:current enumerator))
