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

;; Use PPMX to find the expression to which (POP X) expands.

(defun ex14.1 ()
  (ppmx (pop x)))

;; Macro expansion:
;; (LET* ((#:LIST X) (#:CAR (CAR #:LIST)) (#:NEW327 (CDR #:LIST)))
;;   (SETQ X #:NEW327)
;;   #:CAR)

;;; Ex. 14.2

;; Use PPMX to see to what expression the following DEFSTRUCT expands. (The
;; results will be highly implementation dependent.)

(defun ex14.2 ()
  (ppmx (defstruct starship
          (name nil)
          (condition 'green))))

;;; Ex. 14.3

;; Write a SET-NIL macro that sets a variable to NIL.

(defmacro set-nil (var)
  `(setf ,var nil))

(deftest ex14.3-set-nil ()
  (check
    (let ((x 2)) (set-nil x) (not x))))

;;; Ex. 14.4

;; Write a macro called SIMPLE-ROTATEF that switches the value of two
;; variables. For example, if A is two and B is seven, then (SIMPLE-ROTATEF A B)
;; should make A seven and B two. Obviously, setting A to B first, and then
;; setting B to A won’t work. Your macro should expand into a LET expression
;; that holds on to the original values of the two variables and then assigns
;; them their new values in its body

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

;; (defmacro simple-rotatef (var1 var2)
;;   `(let ((temp1 ,var1)
;;          (temp2 ,var2))
;;      (setf ,var1 temp2)
;;      (setf ,var2 temp1)))

;; But it wouldn't work for:

;; (let ((temp1 2)
;;       (temp2 7))
;;   (simple-rotatef temp1 temp2)
;;   (values temp1 temp2))

;; => 2, 7

;;; Ex. 14.5

;; Write a macro SET-MUTUAL that takes two variable names as input and expands
;; into an expression that sets each variable to the name of the
;; other. (SET-MUTUAL A B) should set A to ’B, and B to ’A

(defmacro set-mutual (var1 var2)
  `(progn
     (setf ,var1 ',var2)
     (setf ,var2 ',var1)))

(deftest ex14.5-set-mutual ()
  (check
    (equal (let (x y) (set-mutual x y) (list x y)) '(y x))))

;;; Chapter tests

(deftest test-chapter-14 ()
  (ex14.3-set-nil)
  (ex14.4-simple-rotatef)
  (ex14.5-set-mutual))

(test-chapter-14)
