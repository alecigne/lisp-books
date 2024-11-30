;;;; This chapter is actually about managing a 'lispy' DB in which records are
;;;; plists. Only a few functions are about a CD collection.
;;;;
;;;; The update function is thus generalized to a macro, just like `where',
;;;; because, hey, why not. Don't do this at home.

;;; DB

(defvar *db* nil)

(defun add-record (rec)
  (push rec *db*))

(defun dump-db ()
  (dolist (rec *db*)
    (format t "~{~a:~10t~a~%~}~%" rec)))

(defun save-db (filename)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun clear-db ()
  (setq *db* nil))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun make-exps (exp-maker fields)
  (loop while fields
        collecting (funcall exp-maker (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  (flet ((make-where-expr (field value)
           `(equal (getf rec ,field) ,value)))
    `#'(lambda (rec) (and ,@(make-exps #'make-where-expr clauses)))))

(defmacro update (selector-fn &rest clauses)
  (flet ((make-update-expr (field value)
           `(setf (getf rec ,field) ,value)))
    `(setf *db* (mapcar
                 #'(lambda (rec)
                     (when (funcall ,selector-fn rec)
                       ,@(make-exps #'make-update-expr clauses))
                     row) *db*))))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

;;; User interaction

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;;; CDs

(defun make-cd (artist title rating ripped)
  (list :artist artist :title title :rating rating :ripped ripped))

(defun prompt-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped: ")))

(defun add-cds ()
  (loop
    (add-record (prompt-for-cd))
    (unless (y-or-n-p "Another?") (return))))
