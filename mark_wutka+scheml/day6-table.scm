;;; This version of the day 6 problem uses a pre-computed lookup
;;; table and sums the offsets 
(load "day6-table-data.scm")

;;; For each day number for a fish, add 1 to the count for that
;;; slot in the counts array, then return the array as a list
;;; Rather than iterating n days, it just iterates for the first
;;; 7 day offsets. The table was generated just to handle 256
;;; days.

(define (make-initial-counts fish counts)
  (if (empty? fish) counts
      (let* ((curr-fish (head fish))
             (curr-count (@ counts curr-fish)))
      (make-initial-counts (tail fish)
                           (@<- counts curr-fish (+ curr-count 1))))))

(define (compute-count-after-days days initial-counts)
  (let ((days-offset (+ days 5)))
    (fold + 0 (map 
                (lambda (i) (* (@ initial-counts i) (@ count-table (- days-offset i))))
                (range 0 6)))))

(define (day6a)
  (let* ((fish (map string->int (split (head (read-lines "day6.txt")) ",")))
         (initial-counts (make-initial-counts fish (make-array-with-default 9 0))))
    (compute-count-after-days 80 initial-counts)))

(define (day6b)
  (let* ((fish (map string->int (split (head (read-lines "day6.txt")) ",")))
         (initial-counts (make-initial-counts fish (make-array-with-default 9 0))))
    (compute-count-after-days 256 initial-counts)))
