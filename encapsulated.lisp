;;;;
;;;; encapsulated classes for Common Lisp
;;;;
(defpackage #:com.kjcjohnson.kale.encapsulated-classes
  (:use #:cl)
  (:import-from #:trivial-package-local-nicknames
                #:add-package-local-nickname)
  (:export #:define-encapsulated-class
           #:method-invoke
           #:property-invoke))

(in-package #:com.kjcjohnson.kale.encapsulated-classes)

#|
Goal: define an encapsulated class like:

(define-encapsulated-class MyClass :extends (class-a class-b)

  (public property ThisThing)
  (public property OtherThing :get (code) :set (code))

  (private field _x)

  (public Method1 (args)
    "This is a method that does something"
    ...)

  (private Method2 (args)
    "This is a private method"
    ...)

  (xxx static xxx ...)
)

What this does:
 * Defines a new package for the class: (current-package).MyClass
 ** Adds MyClass as a package-local-nickname in (current-package)
 ** This gives the public members class-level scope
 * Exports public symbols from *.MyClass
 * Defines a new class MyClass with class-a and class-b as parents
 ** This is in the parent package
 * Adds slots for every declared field and auto-property
 * Adds specializations to method-invoke and property-invoke as needed
 * Defines convenience macros in *.MyClass for the public members

|#

(defgeneric method-invoke (instance name &rest arguments)
  (:documentation "Invokes the given method on the instance."))

(defgeneric property-invoke (instance name)
  (:documentation "Selects the given property on the instance."))

(defgeneric (setf property-invoke) (value instance name)
  (:documentation "Sets the given property on the instance."))

(defun try-add-hash (table key value)
  "Tries to add the given key and value to the hashtable, if it doesn't exist"
  (multiple-value-bind (val exists?) (gethash key table)
    (declare (ignore val))
    (if exists?
        nil
        (progn
          (setf (gethash key table) value)
          t))))

(defun parse-leading-options (definitions &optional (options (make-hash-table)))
  "Parses leading options (e.g., keywords like :extends) from the class definition"
  (if (endp definitions)
      options

      (let ((opt (first definitions)))
        (cond
          ((eql opt :extends)
           (when (null (rest definitions)) (error "Missing arguments to :extends"))
           (let ((superclasses (second definitions)))
             (when (atom superclasses) (setf superclasses (list superclasses)))
             (unless (try-add-hash options :extends superclasses)
               (error "Duplicate :extends clause in class definition"))))

          ((eql opt :metaclass)
           (let ((metaclass (second definitions)))
             (unless (or (null metaclass) (atom metaclass))
               (error "Invalid metaclass: must be a single classname"))
             (unless (try-add-hash options :metaclass metaclass)
               (error "Duplicate :metaclass clause in class definition"))))

          ((keywordp opt)
           (error "Unknown option in class definition: ~s" opt))

          ((atom opt)
           (error "Class options must be keywords, but got: ~s" opt))

          (t
           (dolist (e definitions)
             (unless (listp e)
               (error "Class options must come before definitions: ~s" e)))))
        (parse-leading-options (cddr definitions) options))))

(defstruct member-declaration
  name
  kind
  visibility
  static
  rest)

(defun parse-member-declarations (definitions &optional (decls (list)))
  "Parses member declarations from the class definition"

  (loop until (listp (first definitions)) doing
    (setf definitions (cddr definitions))) ; Skip over prefix options


  (if (endp definitions)
      (nreverse decls)
      (progn

        (let ((raw-decl (first definitions))
              (decl (make-member-declaration :kind :method
                                             :visibility :private
                                             :static nil)))

          ;; Valid prefixes: public private static property field method
          (let (end-of-prefixes)
            (dolist (v raw-decl)
              (cond
                (end-of-prefixes
                 (push v (member-declaration-rest decl)))

                ((and (symbolp v)
                      (string= (symbol-name v) "PUBLIC"))
                 (setf (member-declaration-visibility decl) :public))

                ((and (symbolp v)
                      (string= (symbol-name v) "PRIVATE"))
                 (setf (member-declaration-visibility decl) :private))

                ((and (symbolp v)
                      (string= (symbol-name v) "STATIC"))
                 (setf (member-declaration-static decl) t))

                ((and (symbolp v)
                      (string= (symbol-name v) "PROPERTY"))
                 (setf (member-declaration-kind decl) :property))

                ((and (symbolp v)
                      (string= (symbol-name v) "FIELD"))
                 (setf (member-declaration-kind decl) :field))

                ((and (symbolp v)
                      (string= (symbol-name v) "METHOD"))
                 (setf (member-declaration-kind decl) :method))

                ((symbolp v)
                 (setf (member-declaration-name decl) (symbol-name v)
                       end-of-prefixes t))

                (t
                 (error "Invalid component of member declaration: ~s" v)))))
          (setf (member-declaration-rest decl)
                (nreverse (member-declaration-rest decl)))
          (parse-member-declarations (rest definitions) (cons decl decls))))))

(defun compute-encapsulated-class-slots (options declarations)
  "Computes a list of slots needed to implement this class."
  (declare (ignore options))
  (let (slots)
    (dolist (decl declarations)
      (cond
        ;; Fields are direct declaration of slots
        ((eql (member-declaration-kind decl) :field)
         (push (member-declaration-name decl) slots))

        ;; Properties without getter/setter have a generated backing slot
        ;; Slots are in a different namespace, so we don't have to mangle the name
        ((eql (member-declaration-kind decl) :property)
         (when (null (member-declaration-rest decl))
           (push (member-declaration-name decl) slots)))))
    (map 'list #'intern slots)))


(defun compute-encapsulated-class-options (options declarations)
  "Computes the class options."
  (declare (ignore declarations))
  (let ((metaclass (gethash :metaclass options)))
    (unless (null metaclass)
      `((:metaclass , metaclass)))))

(defun compute-encapsulated-class-package (name options declarations)
  "Computes the steps needed to (re-)create the class package and symbols."
  (declare (ignore options))
  (let ((class-package-name (concatenate 'string
                                         (package-name *package*)
                                         "."
                                         (string-upcase name))))


    (when (find-package class-package-name)
      (delete-package class-package-name))
    (let ((class-package (make-package class-package-name)))
      `(progn
         (add-package-local-nickname ,(string-upcase name) ,class-package)
         ,@(let (exports wrappers)
             (dolist (member declarations)
               (when (eql (member-declaration-visibility member) :public)
                 (let ((name (member-declaration-name member)))
                   (push `(export ',(intern name class-package)
                                  ,class-package)
                         exports)
                   (case (member-declaration-kind member)
                     (:field
                      (push `(defmacro ,(intern name class-package) (instance)
                               (slot-value instance ',name))
                            wrappers))
                     (:property
                      (push `(defmacro ,(intern name class-package) (instance)
                               (property-invoke instance ',name))
                            wrappers))
                     (:method
                         (let ((form-var (gensym "form")))
                           (push
                            `(defmacro ,(intern name class-package)
                                 (&whole
                                    ,form-var
                                    instance
                                    ,@(first (member-declaration-rest member)))
                               (declare (ignorable
                                         ,@(first
                                            (member-declaration-rest member))))
                               `(funcall #'method-invoke
                                         ,instance
                                         ,'',(intern name)
                                         ,@(cddr ,form-var)))
                            wrappers)))))))

             (nconc exports wrappers))))))


(defun expand-member-definitions (name options declarations)
  "Expands definitions into actual methods."
  (declare (ignore options))
  (let (symbol-bindings
        macro-bindings
        (this-var (intern "THIS"))
        (value-var (intern "VALUE")))
    (dolist (decl declarations)
      (let ((name (member-declaration-name decl)))
        (cond
          ((eql (member-declaration-kind decl) :field)
           (push (list (intern name)
                       `(slot-value ,this-var ',(intern name)))
                 symbol-bindings)
           (push (list (intern (concatenate 'string "THIS." (string-upcase name)))
                       `(slot-value ,this-var ',(intern name)))
                 symbol-bindings))

          ((eql (member-declaration-kind decl) :property)
           (push (list (intern name)
                       `(property-invoke ,this-var ',(intern name)))
                 symbol-bindings)
           (push (list (intern (concatenate 'string "THIS." (string-upcase name)))
                       `(property-invoke ,this-var ',(intern name)))
                 symbol-bindings))

          ((eql (member-declaration-kind decl) :method)
           (let ((form-var (gensym "form")))
             (push (list (intern name)
                         `(&whole ,form-var
                                  ,@(first (member-declaration-rest decl)))
                         `(declare (ignorable ,@(first (member-declaration-rest decl))))
                         ``(funcall #'method-invoke
                                    ,',this-var
                                    ,'',(intern name)
                                    ,@(cdr ,form-var)))
                   macro-bindings))))))
    `(symbol-macrolet (,@symbol-bindings)
       (macrolet (,@macro-bindings)
         ,@(map 'list
                #'(lambda (decl)
                    `(defmethod method-invoke
                         ((,this-var ,name)
                          (name (eql ',(intern (member-declaration-name decl))))
                          &rest arguments)
                       (apply #'(lambda ,(first (member-declaration-rest decl))
                                  ,@(rest (member-declaration-rest decl)))
                              arguments)))
                (remove-if-not #'(lambda (d)
                                   (eql (member-declaration-kind d) :method))
                               declarations))
         ,@(map 'list
                #'(lambda (decl)
                    `(progn
                       (defmethod property-invoke
                           ((,this-var ,name)
                            (name (eql ',(intern (member-declaration-name decl)))))
                         ,(if (not
                               (null (getf (member-declaration-rest decl) :get)))
                              (getf (member-declaration-rest decl) :get)
                              `(slot-value ,this-var ',(intern (member-declaration-name decl)))))
                       (defmethod (setf property-invoke)
                           (,value-var
                            (,this-var ,name)
                            (name (eql ',(intern (member-declaration-name decl)))))
                         ,(if (not
                               (null (getf (member-declaration-rest decl) :set)))
                              (getf (member-declaration-rest decl) :set)
                              `(setf (slot-value ,this-var ',(intern (member-declaration-name decl)))
                                     ,value-var)))))
                (remove-if-not #'(lambda (d)
                                   (eql (member-declaration-kind d) :property))
                               declarations))))))

(defun expand-encapsulated-class-definition (name definitions)
  "Expands an encapsulated class definition into the real deal."
  (let ((options (parse-leading-options definitions))
        (declarations (parse-member-declarations definitions)))

    `(progn
       (defclass ,name ,(gethash :extends options)
         ,(compute-encapsulated-class-slots options declarations)
         ,@(compute-encapsulated-class-options options declarations))

       ,(compute-encapsulated-class-package name options declarations)
       ,(expand-member-definitions name options declarations))))

(defmacro define-encapsulated-class (name &rest definitions)
  "Defines an encapsulated class."
  (let ((options (parse-leading-options definitions))
        (decls (parse-member-declarations definitions)))
    (format t "For class ~a~%" name)
    (format t "Got options:~%")
    (maphash #'(lambda (k v) (format t " ~s = ~s~%" k v)) options)
    (format t "Got declarations:~%")
    (map nil #'(lambda (s) (format t " ~s~%" s)) decls))
  (expand-encapsulated-class-definition name definitions))
