;;;;
;;;; encapsulated classes for Common Lisp
;;;;
(defpackage #:com.kjcjohnson.kale.oo
  (:use #:cl)
  (:import-from #:trivial-package-local-nicknames
                #:add-package-local-nickname)
  (:export #:define-encapsulated-class
           #:method-invoke
           #:property-invoke
           #:import-class-from
           #:import-classes-from
           #:forward
           #:class))

(in-package #:com.kjcjohnson.kale.oo)

#|
Goal: define an encapsulated class like:

(define-encapsulated-class MyClass :extends (class-a class-b)

  (constructor () ...)

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

(defun class-package-name (class package)
  (concatenate 'string
               package
               "."
               (string-upcase class)))


(defvar *package-classes* (make-hash-table))

(defun register-class (class package)
  (unless (packagep package)
    (setf package (find-package package)))
  (push class (gethash package *package-classes*)))

(defun classes-in-package (package)
  (unless (packagep package)
    (setf package (find-package package)))
  (gethash package *package-classes*))

(defun %filter-ignorable (vars)
  "Filters a list of variables to ignore, removing special lambda list symbols."
  (remove-if #'(lambda (s)
                   (find s lambda-list-keywords))
               vars))

(defmacro forward (name &rest exports)
  "Forward declares a class and needed public symbols."
  (let* ((class-package-name (class-package-name name (package-name *package*)))
         (package (find-package class-package-name)))

    (unless package
      (setf package (make-package class-package-name))
      (format *trace-output* "~&; Created package: ~a~%" class-package-name))
    (add-package-local-nickname (string-upcase name) class-package-name)
    (register-class name *package*)
    (dolist (sym exports)
      (export (intern (symbol-name sym) package) package))))

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

          ((eql opt :documentation)) ;; void for now

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

                ((and (symbolp v)
                      (string= (symbol-name v) "CONSTRUCTOR"))
                 (setf (member-declaration-kind decl) :constructor
                       end-of-prefixes t))

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
         (if (member-declaration-static decl)
             (push `(,(intern (member-declaration-name decl)) :allocation :class) slots)
             (push (intern (member-declaration-name decl)) slots)))

        ;; Properties without getter/setter have a generated backing slot
        ;; Slots are in a different namespace, so we don't have to mangle the name
        ((eql (member-declaration-kind decl) :property)
         (when (null (member-declaration-rest decl))
           (if (member-declaration-static decl)
               (push `(,(intern (member-declaration-name decl)) :allocation :class) slots)
               (push (intern (member-declaration-name decl)) slots))))))
    slots))


(defun compute-encapsulated-class-options (options declarations)
  "Computes the class options."
  (declare (ignore declarations))
  (let ((metaclass (gethash :metaclass options)))
    (unless (null metaclass)
      `((:metaclass , metaclass)))))

(defun compute-encapsulated-class-package (name class-package-name options declarations)
  "Computes the steps needed to (re-)create the class package and symbols."
  (declare (ignore options))

  (let ((class-package (find-package class-package-name)))
    `(progn
       ,@(let (exports wrappers)
           (dolist (member declarations)
             (when (eql (member-declaration-visibility member) :public)
               (let ((m-name (member-declaration-name member)))
                 (when (eql (member-declaration-kind member) :constructor)
                   (setf m-name "NEW"))
                 (push `(export ',(intern m-name class-package)
                                ,class-package)
                       exports)
                 (case (member-declaration-kind member)
                   (:constructor
                       (let ((form-var (gensym "form")))
                         (push
                          `(defmacro ,(intern "NEW" class-package)
                               (&whole
                                  ,form-var
                                  ,@(first (member-declaration-rest member)))
                             (declare (ignorable
                                       ,@(%filter-ignorable
                                          (first
                                           (member-declaration-rest member)))))
                             `(funcall #'make-instance
                                       ,'',name
                                       :constructor-args (list ,@(cdr ,form-var))))

                          wrappers)))
                   (:field
                    (if (member-declaration-static member)
                        (push `(defmacro ,(intern m-name class-package) ()
                                 `(slot-value
                                   (closer-mop:class-prototype
                                    (find-class ,'',name))))
                              wrappers)
                        (push `(defmacro ,(intern m-name class-package) (instance)
                                 `(slot-value ,instance ,'',(intern m-name)))
                              wrappers)))
                   (:property
                    (if (member-declaration-static member)
                        (push `(defmacro ,(intern m-name class-package) ()
                                 `(property-invoke
                                   (closer-mop:class-prototype
                                    (find-class ,'',name))
                                   ,'',(intern m-name :keyword)))
                              wrappers)
                        (push `(defmacro ,(intern m-name class-package) (instance)
                                 `(property-invoke ,instance ,'',(intern m-name :keyword)))
                              wrappers)))
                   (:method
                       (let ((form-var (gensym "form")))
                         (if (member-declaration-static member)
                             (push
                              `(defmacro ,(intern m-name class-package)
                                   (&whole
                                      ,form-var
                                      ,@(first (member-declaration-rest member)))
                                 (declare (ignorable
                                           ,@(%filter-ignorable
                                              (first
                                               (member-declaration-rest member)))))
                                 `(funcall #'method-invoke
                                           (closer-mop:class-prototype
                                            (find-class ,'',name))
                                           ,'',(intern m-name :keyword)
                                           ,@(cddr ,form-var)))
                              wrappers)
                             (push
                              `(defmacro ,(intern m-name class-package)
                                   (&whole
                                      ,form-var
                                      instance
                                      ,@(first (member-declaration-rest member)))
                                 (declare (ignorable
                                           ,@(%filter-ignorable
                                              (first
                                               (member-declaration-rest member)))))
                                 `(funcall #'method-invoke
                                           ,instance
                                           ,'',(intern m-name :keyword)
                                           ,@(cddr ,form-var)))
                              wrappers))))))))

           (nconc exports wrappers)))))


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
                       `(property-invoke ,this-var ',(intern name :keyword)))
                 symbol-bindings)
           (push (list (intern (concatenate 'string "THIS." (string-upcase name)))
                       `(property-invoke ,this-var ',(intern name :keyword)))
                 symbol-bindings))

          ((eql (member-declaration-kind decl) :method)
           (let ((form-var (gensym "form")))
             (push (list (intern name)
                         `(&whole ,form-var
                                  ,@(first (member-declaration-rest decl)))
                         `(declare (ignorable
                                    ,@(%filter-ignorable
                                       (first (member-declaration-rest decl)))))
                         ``(funcall #'method-invoke
                                    ,',this-var
                                    ,'',(intern name :keyword)
                                    ,@(cdr ,form-var)))
                   macro-bindings))))))
    `(symbol-macrolet (,@symbol-bindings)
       (macrolet (,@macro-bindings)
         ,@(map 'list
                #'(lambda (decl)
                    `(defmethod method-invoke
                         ((,this-var ,name)
                          (name (eql ',(intern (member-declaration-name decl)
                                        :keyword)))
                          &rest arguments)
                       ,@(if (eql (first (rest (member-declaration-rest decl)))
                                 :prototype)
                             `((declare (ignore arguments))
                               (error "Not implemented."))
                             `((apply #'(lambda ,(first
                                                  (member-declaration-rest decl))
                                          ,@(rest (member-declaration-rest decl)))
                                      arguments)))))
                (remove-if-not #'(lambda (d)
                                   (eql (member-declaration-kind d) :method))
                               declarations))
         ,@(map 'list
                #'(lambda (decl)
                    `(progn
                       (defmethod property-invoke
                           ((,this-var ,name)
                            (name (eql ',(intern (member-declaration-name decl)
                                          :keyword))))
                         ,(if (not
                               (null (getf (member-declaration-rest decl) :get)))
                              (getf (member-declaration-rest decl) :get)
                              `(slot-value ,this-var ',(intern (member-declaration-name decl)))))
                       (defmethod (setf property-invoke)
                           (,value-var
                            (,this-var ,name)
                            (name (eql ',(intern (member-declaration-name decl)
                                          :keyword))))
                         ,(if (not
                               (null (getf (member-declaration-rest decl) :set)))
                              (getf (member-declaration-rest decl) :set)
                              `(setf (slot-value ,this-var ',(intern (member-declaration-name decl)))
                                     ,value-var)))))
                (remove-if-not #'(lambda (d)
                                   (eql (member-declaration-kind d) :property))
                               declarations))
         ,@(map 'list
                #'(lambda (decl)
                    `(defmethod initialize-instance :after
                         ((,this-var ,name) &rest initargs
                          &key constructor-args &allow-other-keys)
                       (declare (ignore initargs))
                       (apply #'(lambda ,(first (member-declaration-rest decl))
                                  ,@(rest (member-declaration-rest decl)))
                              constructor-args)))
                (remove-if-not #'(lambda (d)
                                   (eql (member-declaration-kind d) :constructor))
                               declarations))))))

(defun expand-encapsulated-class-definition (name definitions)
  "Expands an encapsulated class definition into the real deal."
  (let ((options (parse-leading-options definitions))
        (declarations (parse-member-declarations definitions))
        (class-package-name (class-package-name name (package-name *package*))))

    (unless (find-package class-package-name)
      (make-package class-package-name)
      (format *trace-output* "~&; Created package: ~a~%" class-package-name))
    (add-package-local-nickname (string-upcase name) class-package-name)
    (register-class name *package*)

    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (unless (find-package ,class-package-name)
           (make-package ,class-package-name)
           (format *trace-output* "~&; Created package: ~a~%" ,class-package-name))
         (add-package-local-nickname ,(string-upcase name) ,class-package-name)
         (register-class ',name *package*))

       (defclass ,name ,(gethash :extends options)
         ,(compute-encapsulated-class-slots options declarations)
         ,@(compute-encapsulated-class-options options declarations))

       (eval-when (:compile-toplevel :load-toplevel :execute)
         ,(compute-encapsulated-class-package name
                                              class-package-name
                                              options
                                              declarations))
       ,(expand-member-definitions name options declarations)
       (find-class ',name))))

(defmacro define-encapsulated-class (name &rest definitions)
  "Defines an encapsulated class."
  (expand-encapsulated-class-definition name definitions))

(defun import-class-from (class package &key as into)
  (when (null into) (setf into *package*))
  (let* ((nickname (if (null as)
                       (string-upcase class)
                       (concatenate 'string (string-upcase as) "." (string-upcase class))))
         (realname (class-package-name class (package-name (find-package package))))
         (existing (assoc nickname
                          (trivial-package-local-nicknames:package-local-nicknames into)
                          :test #'string=)))
    (if (null existing)
        (progn
          (format *trace-output* "; Importing ~s from ~s~%" class package)
          (trivial-package-local-nicknames:add-package-local-nickname
           nickname
           realname
           into))
        (unless (string= (package-name (cdr existing)) realname)
          (warn "Cannot import ~s as ~s into ~s due to existing nickname for ~s"
                class nickname into (cdr existing))))))

(defmacro import-classes-from (package &key as into)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let ((mapping (classes-in-package ',package)))
       (dolist (c mapping)
         (import-class-from c ',package :as ',as :into ,(if (null into) '*package* into))))))
