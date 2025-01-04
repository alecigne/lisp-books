(defpackage :pcl3-nomac
  (:use :cl))

(in-package :pcl3-nomac)

(defvar *db* nil)

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun where-loop (&rest args)
  (lambda (rec)
    (loop for (k v) on args by #'cddr
          always (equal (getf rec k) v))))

(defun where-recur (&rest args)
  (lambda (rec)
    (labels ((check (lst)
               (or (null lst)
                   (and (equal (getf rec (first lst)) (second lst))
                        (check (cddr lst))))))
      (check args))))
