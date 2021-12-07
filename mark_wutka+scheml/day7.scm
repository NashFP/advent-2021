(load "mwlib.scm")

(define (find-best-pos fuel-func max-pos crabs pos best-fuel best-pos)
  (if (> pos max-pos) best-fuel
      (let ((curr-fuel (fold (lambda (f c) (+ f (fuel-func c pos))) 0 crabs)))
        (if (< curr-fuel best-fuel)
            (find-best-pos fuel-func max-pos crabs (+ pos 1) curr-fuel pos)
            (find-best-pos fuel-func max-pos crabs (+ pos 1) best-fuel best-pos)))))

(define (linear-fuel c pos)
  (abs (- c pos)))

(define (increase-fuel c pos)
  (let ((dist (abs (- c pos))))
    (/ (* dist (+ dist 1)) 2)))

(define (day7a)
  (let ((crabs (map string->int (split (head (read-lines "day7.txt")) ","))))
    (find-best-pos linear-fuel (list-max crabs) crabs (list-min crabs) 9999999999 -1)))
        
(define (day7b)
  (let ((crabs (map string->int (split (head (read-lines "day7.txt")) ","))))
    (find-best-pos increase-fuel (list-max crabs) crabs (list-min crabs) 9999999999 -1)))
        
