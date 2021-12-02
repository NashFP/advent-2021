(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
       while line
       collect line)))

(defun split-groups (lines groups group)
  (cond ((null lines) (reverse (if (null group) groups (cons (reverse group) groups))))
	((= (length (car lines)) 0) (split-groups (cdr lines) (cons (reverse group) groups) '()))
	(t (split-groups (cdr lines) groups (cons (car lines) group)))))

(defun take (n list)
  (if (or (= n 0) (null list)) '()
      (cons (car list) (take (1- n) (cdr list)))))

(defun take-while (f l)
  (if (null l) '()
      (if (apply f (car l)) (cons (car l) (take-while f (cdr l)))
	  (take-while f (cdr l)))))

(defun drop (n list)
  (if (or (= n 0) (null list)) list
      (drop (1- n) (cdr list))))

(defun tr-sub (ch char-map)
  (let ((match (assoc ch char-map)))
    (if match (cadr match) ch)))

(defun tr (source char-map)
  (map 'string
       (lambda (ch) (tr-sub ch char-map)) source))

(defun iota (n &key (start 0))
  (loop for i from start below (+ n start) collect i))

(defun repeat (x n)
  (labels ((rec (x n acc)
	     (if (= n 0) acc (rec x (1- n) (cons x acc)))))
    (rec x n '())))

(defun cartesian-product (&rest lists)
  (labels ((prod (a b)
	     (mapcan (lambda (bx) (mapcar (lambda (ax)
					    (if (listp bx) (cons ax bx)
						(list ax bx)))
					  a))
		     b)))
    (reduce #'prod lists :from-end t)))

(defun partial (f &rest args)
  (lambda (&rest others) (apply f (append args others))))

(defun intern-uc (sym)
  (intern (string-upcase sym)))
