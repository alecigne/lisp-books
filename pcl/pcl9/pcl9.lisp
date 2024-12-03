(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))

(defun check (form)
  `(report-result ,form ',form))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))
