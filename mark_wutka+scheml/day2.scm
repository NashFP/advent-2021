;;; Define type constructors for a command
(type command
      (Forward int)
      (Up int)
      (Down int))

;;; Define type constructor for state
(type state
      (State int int int int))

;;; Process a command and compute the depth
;;; for both part A and part B
;;; where old-depth is the calculation for
;;; part A.
(define (process-command curr-state command)
  (let (((State pos old-depth depth aim) curr-state))
    (match command
          ((Forward amt) (State (+ pos amt)
                                old-depth
                                (+ depth (* aim amt))
                                aim))
          ((Up amt) (State pos
                           (- old-depth amt)
                           depth
                           (- aim amt)))
          ((Down amt) (State pos
                             (+ old-depth amt)
                             depth
                             (+ aim amt))))))

;;; Convert a command string into a command value
(define (parse-command cmd-str)
  ;;; Split on space
  (let* ((parts (split cmd-str " "))
         (cmd (head parts))
         (amt (string->int (head (tail parts)))))
    (if (equals? cmd "forward")
        (Forward amt)
        (if (equals? cmd "up")
            (Up amt)
            (Down amt))))) ;;; If it isn't forward or up, assume it was down

(define (day2a)
  (let* ((commands (map parse-command (read-lines "day2.txt")))
         ((State pos old-depth depth aim)
          (fold process-command (State 0 0 0 0) commands)))
    (* pos old-depth)))

(define (day2b)
  (let* ((commands (map parse-command (read-lines "day2.txt")))
         ((State pos old-depth depth aim)
          (fold process-command (State 0 0 0 0) commands)))
    (* pos depth)))
