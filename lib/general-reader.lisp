(defpackage :optima.general-accessor
  (:use :cl :optima)
  (:export #:accessor))
(in-package :optima.general-accessor)

(defstruct (accessor-pattern (:include optima::constructor-pattern)
                             (:constructor make-accessor-pattern
                                           (accessor &rest optima::subpatterns)))
  accessor)

(defmethod optima::destructor-equal ((x accessor-pattern) (y accessor-pattern))
  (equal (accessor-pattern-accessor x)
         (accessor-pattern-accessor y)))

(defmethod optima::destructor-predicate-form ((pattern accessor-pattern) var)
  (values var t))

(defmethod optima::destructor-forms ((pattern accessor-pattern) var)
  (with-slots (accessor) pattern
    (list `(funcall ,accessor ,var)))) ;; only assumes 1-value

(defmethod optima::parse-constructor-pattern ((name (eql 'accessor)) &rest args)
  (assert (= 2 (length args)))
  (make-accessor-pattern
   (first args) ;; accessor function
   (optima::parse-pattern (second args))))

(defmethod optima::unparse-pattern ((pattern accessor-pattern))
  `(accessor ,(accessor-pattern-accessor pattern)
             ,(first (accessor-pattern-subpatterns pattern))))

(match (list 1 2 3)
  ((and (type cons)
        (accessor #'car x)
        (accessor #'cdr y))
   (list x y)))

;; (LET ((#:VAR1254 (LIST 1 2 3)))
;;   (DECLARE (IGNORABLE #:VAR1254))
;;   (OPTIMA::%OR
;;    (LET ((#:G1257 #:VAR1254))
;;      (OPTIMA::%IF #:G1257
;;                   (LET ((#:G1256 (FUNCALL #'CAR #:G1257)))
;;                     (DECLARE (IGNORABLE #:G1256))
;;                     (OPTIMA::%MATCH (#:G1256 #:VAR1254)
;;                                     (((X (ACCESSOR #'CDR Y))
;;                                       (LET ((#:IT1255 #:VAR1254))
;;                                         (IF (AND (TYPEP #:IT1255 'CONS))
;;                                             (LIST X Y)
;;                                             (FAIL)))))
;;                                     (FAIL)))
;;                   (FAIL)))
;;    NIL))

(defpattern mycons (a b)
  `(and (type cons)
        (accessor #'car ,a)
        (accessor #'cdr ,b)))


(match (list 1 2 3)
  ((mycons a b)
   (list a b)))

;; --> (1 (2 3))