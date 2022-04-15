(in-package #:com.kjcjohnson.kale.collections)

;;;
;;; Dictionaries
;;;
(kl/oo:define-encapsulated-class &dictionary
  :documentation "Dictionary abstraction."
  (public property key-list :get (error "Not implemented") :set (error "RO"))
  (public property value-list :get (error "Not implemented") :set (error "RO"))
  (public add (key value) (error "Not implemented"))
  (public contains-key (key) (error "Not implemented"))
  (public remove (key) (error "Not implemented"))
  (public try-get-value (key) (error "Not implemented")))

;;;
;;; Default implementation with hashtable backing
;;;
(kl/oo:define-encapsulated-class dictionary :extends &dictionary

  (private field _hashtable)

  (public constructor (&key test)
          (setf _hashtable (if (null test)
                               (make-hash-table)
                               (make-hash-table :test test))))

  (public add (key value)
          (setf (gethash key _hashtable) value))

  (public contains-key (key)
          (nth-value 1 (gethash key _hashtable)))

  (public remove (key)
          (remhash key _hashtable))

  (public try-get-value (key)
          (gethash key _hashtable))

  (public property key-list
          :get (let ((ks))
                 (maphash #'(lambda (k v) (push k ks)) _hashtable)
                 (nreverse ks))
          :set (error "RO"))

  (public property value-list
          :get (let ((vs))
                 (maphash #'(lambda (k v) (push v vs)) _hashtable)
                 (nreverse vs))
          :set (error "RO")))
