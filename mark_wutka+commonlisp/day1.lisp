(load "mwlib.lisp")

(defun day1a ()
  (let ((numbers (mapcar #'parse-integer (read-file "day1a.txt"))))
    (length
     (remove-if-not #'identity
		    (mapcar #'< numbers (cdr numbers))))))

(defun sliding-window (l)
  (mapcar #'+ l (cdr l) (cddr l)))

(defun day1b ()
  (let ((numbers (mapcar #'parse-integer (read-file "day1a.txt"))))
    (length
     (remove-if-not #'identity
		    (mapcar #'< (sliding-window numbers)
			     (sliding-window (cdr numbers)))))))
