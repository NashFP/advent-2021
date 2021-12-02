(ql:quickload :cl-ppcre)
(load "mwlib.lisp")

;;; Split the string by space, turn the command into a symbol,
;;; parse the amount as an integer. This uses intern-uc in
;;; mwlib that converts the sym to upper case before calling
;;; the CL intern function. Otherwise, the lowercase symbol
;;; names would have to be pipe-delimited like '|forward|
(defun parse-command (l)
  (ppcre:register-groups-bind
      ((#'intern-uc dir) (#'parse-integer amt))
      ("([^ ]*) *([0-9]*)" l)
    (cons dir amt)))

;;; Return only the items for a particular command
(defun get-command (dir l)
  (mapcar #'cdr
	  (remove-if-not (lambda (x) (eq (car x) dir)) l)))

(defun day2a ()
  ;;; Parse the file into a list of commands
  (let* ((commands (mapcar #'parse-command (read-file "day2.txt")))
	 ;;; sum the commands in each direction
	 (forwards (apply #'+ (get-command 'forward commands)))
	 (ups (apply #'+ (get-command 'up commands)))
	 (downs (apply #'+ (get-command 'down commands))))
    ;;; compute the result
    (* forwards (- downs ups))))

;;; Modifies the depth and aim according to the received command
(defun process-command (state cmd)
  (let ((command (car cmd))
	(amt (cdr cmd))
	(depth (car state))
	(aim (cdr state)))
    (cond
      ((eq command 'forward) (cons (+ depth (* aim amt)) aim))
      ((eq command 'down) (cons depth (+ aim amt)))
      ((eq command 'up) (cons depth (- aim amt))))))

(defun day2b ()
  ;;; Parse the file into a list of commands
  (let* ((commands (mapcar #'parse-command (read-file "day2.txt")))
	 ;;; Sum the forwards to get the horizontal pos
	 (forwards (apply #'+ (get-command 'forward commands)))
	 ;;; Start with depth & aim of 0, process commands sequentially
	 (depth-aim (reduce #'process-command commands :initial-value (cons 0 0))))
    (* forwards (car depth-aim))))
