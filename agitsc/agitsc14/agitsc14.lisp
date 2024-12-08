(defpackage :agitsc14
  (:use :cl)
  (:import-from :pcl9 :check :deftest))

(in-package :agitsc14)

(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
          (exp (macroexpand exp1))
          (*print-circle* nil))
     (cond ((equal exp exp1)
            (format t "~&Macro expansion:")
            (pprint exp))
           (t (format t "~&First step of expansion:")
              (pprint exp1)
              (format t "~%~%Final expansion:")
              (pprint exp)))
     (format t "~%~%")
     (values)))

;;; Ex. 14.1

(defun ex14.1 ()
  (ppmx (pop x)))

;; Macro expansion:
;; (LET* ((#:LIST X) (#:CAR (CAR #:LIST)) (#:NEW327 (CDR #:LIST)))
;;   (SETQ X #:NEW327)
;;   #:CAR)

;;; Ex. 14.2

(defun ex14.2 ()
  (ppmx (defstruct starship
          (name nil)
          (condition 'green))))

;; Jesus!
;;
;; Macro expansion:
;; (PROGN
;;  (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
;;    (SB-KERNEL::%DEFSTRUCT-PACKAGE-LOCKS
;;     '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {100215E983}>))
;;  (SB-KERNEL::%DEFSTRUCT
;;   '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {100215E983}>
;;   '#(#<SB-KERNEL:LAYOUT for T {50901003}>
;;      #<SB-KERNEL:LAYOUT (ID=1) for STRUCTURE-OBJECT {50901073}>)
;;   (SB-C:SOURCE-LOCATION))
;;  (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)
;;    (SB-KERNEL::%COMPILER-DEFSTRUCT
;;     '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {100215E983}>
;;     '#(#<SB-KERNEL:LAYOUT for T {50901003}>
;;        #<SB-KERNEL:LAYOUT (ID=1) for STRUCTURE-OBJECT {50901073}>)))
;;  (SB-C:XDEFUN COPY-STARSHIP
;;      :COPIER
;;      NIL
;;      (SB-KERNEL:INSTANCE)
;;    (COPY-STRUCTURE (THE STARSHIP SB-KERNEL:INSTANCE)))
;;  (SB-C:XDEFUN STARSHIP-P
;;      :PREDICATE
;;      NIL
;;      (SB-KERNEL::OBJECT)
;;    (TYPEP SB-KERNEL::OBJECT 'STARSHIP))
;;  (SB-C:XDEFUN (SETF STARSHIP-NAME)
;;      :ACCESSOR
;;      (NAME NIL)
;;      (SB-KERNEL::VALUE SB-KERNEL:INSTANCE)
;;    (LET ((#:INSTANCE (THE STARSHIP SB-KERNEL:INSTANCE))
;;          (#:VAL SB-KERNEL::VALUE))
;;      (SB-KERNEL:%INSTANCE-SET #:INSTANCE 0 #:VAL)
;;      #:VAL))
;;  (SB-C:XDEFUN STARSHIP-NAME
;;      :ACCESSOR
;;      (NAME NIL)
;;      (SB-KERNEL:INSTANCE)
;;    (SB-KERNEL:%INSTANCE-REF (THE STARSHIP SB-KERNEL:INSTANCE) 0))
;;  (SB-C:XDEFUN (SETF STARSHIP-CONDITION)
;;      :ACCESSOR
;;      (CONDITION ’GREEN)
;;      (SB-KERNEL::VALUE SB-KERNEL:INSTANCE)
;;    (LET ((#:INSTANCE (THE STARSHIP SB-KERNEL:INSTANCE))
;;          (#:VAL SB-KERNEL::VALUE))
;;      (SB-KERNEL:%INSTANCE-SET #:INSTANCE 1 #:VAL)
;;      #:VAL))
;;  (SB-C:XDEFUN STARSHIP-CONDITION
;;      :ACCESSOR
;;      (CONDITION ’GREEN)
;;      (SB-KERNEL:INSTANCE)
;;    (SB-KERNEL:%INSTANCE-REF (THE STARSHIP SB-KERNEL:INSTANCE) 1))
;;  (SB-C:XDEFUN MAKE-STARSHIP
;;      :CONSTRUCTOR
;;      NIL
;;      (&KEY ((:NAME #:NAME) NIL)
;;       ((:CONDITION #:CONDITION)
;;        (SB-KERNEL:THE*
;;         (T :SOURCE-FORM (CONDITION ’GREEN) :CONTEXT
;;          #<SB-KERNEL:DEFSTRUCT-SLOT-DESCRIPTION CONDITION> :USE-ANNOTATIONS T)
;;         ’GREEN)))
;;    (DECLARE (SB-INT:EXPLICIT-CHECK)
;;             (SB-C::LAMBDA-LIST
;;              (&KEY ((:NAME #:NAME) NIL) ((:CONDITION #:CONDITION) ’GREEN))))
;;    (SB-KERNEL::%MAKE-STRUCTURE-INSTANCE-MACRO
;;     #<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {100215E983}>
;;     '((:SLOT T . 0) (:SLOT T . 1)) #:NAME #:CONDITION))
;;  (SB-KERNEL::%TARGET-DEFSTRUCT
;;   '#<SB-KERNEL:DEFSTRUCT-DESCRIPTION STARSHIP {100215E983}>
;;   (SB-INT:NAMED-LAMBDA "STARSHIP-EQUALP"
;;       (SB-KERNEL::A SB-KERNEL::B)
;;     (DECLARE (OPTIMIZE (SB-C:STORE-SOURCE-FORM 0) (SAFETY 0))
;;              (TYPE STARSHIP SB-KERNEL::A SB-KERNEL::B)
;;              (IGNORABLE SB-KERNEL::A SB-KERNEL::B))
;;     (AND
;;      (EQUALP (SB-EXT:TRULY-THE T (SB-KERNEL:%INSTANCE-REF SB-KERNEL::A 0))
;;              (SB-EXT:TRULY-THE T (SB-KERNEL:%INSTANCE-REF SB-KERNEL::B 0)))
;;      (EQUALP (SB-EXT:TRULY-THE T (SB-KERNEL:%INSTANCE-REF SB-KERNEL::A 1))
;;              (SB-EXT:TRULY-THE T (SB-KERNEL:%INSTANCE-REF SB-KERNEL::B 1)))))
;;   '(SETF STARSHIP-NAME) 'STARSHIP-NAME '(SETF STARSHIP-CONDITION)
;;   'STARSHIP-CONDITION))

;;; Ex. 14.3
;;;
;;; Write a SET-NIL macro that sets a variable to NIL.

(defmacro set-nil (var)
  `(setf ,var nil))

(deftest ex14.3-set-nil ()
  (check
    (let ((x 2)) (set-nil x) (not x))))

;;; Ex. 14.4
;;;
;;; Write a macro called SIMPLE-ROTATEF that switches the value of two
;;; variables. For example, if A is two and B is seven, then (SIMPLE-ROTATEF A
;;; B) should make A seven and B two. Obviously, setting A to B first, and then
;;; setting B to A won’t work. Your macro should expand into a LET expression
;;; that holds on to the original values of the two variables and then assigns
;;; them their new values in its body

(defmacro simple-rotatef (var1 var2)
  (let ((tmp (gensym)))
    `(let ((,tmp ,var1))
       (setf ,var1 ,var2)
       (setf ,var2 ,tmp))))

(deftest ex14.4-simple-rotatef ()
  (check
    (let ((x 2) (y 7) (expected '(7 2)))
      (equal (progn (simple-rotatef x y) (list x y)) expected))
    (let ((tmp 2) (y 7) (expected '(7 2)))
      (equal (progn (simple-rotatef tmp y) (list tmp y)) expected))))

;; For the sake of simplicity, the version from the book is:
;;
;; (defmacro simple-rotatef (var1 var2)
;;   `(let ((temp1 ,var1)
;;          (temp2 ,var2))
;;      (setf ,var1 temp2)
;;      (setf ,var2 temp1)))
;;
;; But it wouldn't work for:
;;
;; (let ((temp1 2)
;;       (temp2 7))
;;   (simple-rotatef temp1 temp2)
;;   (values temp1 temp2))
;;
;; => 2, 7

;;; Chapter tests

(deftest test-chapter-14 ()
  (ex14.3-set-nil)
  (ex14.4-simple-rotatef))

(test-chapter-14)
