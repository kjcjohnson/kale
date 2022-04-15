(in-package #:com.kjcjohnson.kale)

;;;
;;; Base enumerator generic functions
;;;
(defgeneric enumerator-get-enumerator (thing)
  (:documentation "Returns an enumerator for the given thing."))

(defgeneric enumerator-move-next (enumerator)
  (:documentation "Advances the enumerator to the next element. Returns NIL if no elements left."))

(defgeneric enumerator-current (enumerator)
  (:documentation "Gets the current element of the enumerator."))

;;;
;;; For-each loop over a generic collection (implementing ENUMERATOR-GET-ENUMERATOR)
;;;
(defmacro foreach ((var in collection) &body body)
  "Iterates over the given enumerator."
  (declare (ignore in))
  `(let ((enumerator (enumerator-get-enumerator ,collection)))
     (loop while (enumerator-move-next enumerator)
           for ,var = (enumerator-current enumerator)
           do
              (progn ,@body))))

;;;
;;; Definitions for lists. Only a single cons is allocated for a list enumerator, but note that no
;;; other enumerator can be just a list (since the generic functions are specialized on that)
;;;
(defmethod enumerator-get-enumerator ((thing list))
  "Gets a list enumerator. We tack an extra cons on the front, as the enumerator is positioned before
the first element of the list."
  (cons nil thing))

(defmethod enumerator-move-next ((enumerator cons))
  "Advances a list enumerator to the next position."
  (if (null (car enumerator))
      (setf (car enumerator) t) ; Initial movement. Set flag that iteration has started
      (setf (cdr enumerator) (rest (cdr enumerator)))) ; Advance the enumerator

  (not (endp (cdr enumerator))))

(defmethod enumerator-current ((enumerator cons))
  "Gets the next element of a list enumerator."
  (first (cdr enumerator)))
