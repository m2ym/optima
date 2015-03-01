

(in-package :optima)

(defstruct (<>-pattern
            (:include constructor-pattern)
            (:constructor make-<>-pattern
                          (name arg value &aux (subpatterns (list name)))))
  name arg value)

(defmethod constructor-pattern-destructor-sharable-p
    ((x <>-pattern) (y <>-pattern))
  nil)

(defmethod constructor-pattern-make-destructor
    ((pattern <>-pattern) matched)
  (with-slots (arg value) pattern
     (make-destructor
      :bindings `((,arg ,matched))
      :predicate-form t
      :accessor-forms `(,value))))

(defmethod unparse-pattern ((pattern <>-pattern))
  (with-slots (name arg value) pattern
     `(<> ,name ,arg ,value)))

(defmethod parse-constructor-pattern ((name (eql '<>)) &rest args)
  (match args
    ((list name (and arg (symbol)) value)
     (make-<>-pattern (parse-pattern name) arg value))
    (otherwise
     (error "Bad binding pattern: ~S" (list* name args)))))

#+nil
(match 'x
  ((<> a 3)
   (print a)))

#+nil
(defpattern cons (a b)
  (let ((x gensym))
    `(and (type cons)
          (<> a ,x (car ,x))
          (<> b ,x (cdr ,x)))))

#+nil
(defpattern function-type (return-type)
  `(or (and 'function (<> ,return-type _ '*))
       (and (list 'function _ ,return-type))))

