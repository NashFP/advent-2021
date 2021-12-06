;;; I thought I would need a bignum for this, so I
;;; initially coded it that way, but then when the
;;; number would fit in an int (64-bit) I went back
;;; to integer arithmetic.

;;; The head of the list is the number of fish that
;;; give birth today. That count gets added to the
;;; count of fish in the current day 7 slot, and
;;; also appended as the fish that give birth in 9
;;; days (representing the newly-born fish)
(define (iterate-day day counts)
  (if (= day 0) counts
      (let* ((curr-count (head counts))
             (day-7-count (nth 7 counts))
             (day-8-count (nth 8 counts))
             (day1-6 (take 6 (tail counts))))
        (iterate-day (- day 1)
            ;;; Shift the counts left by 1, add the curr count
            ;;; to the old day-7 fish, also append the curr count
            ;;; to the end of the list
            (append day1-6 (list (+ curr-count day-7-count)
                                 day-8-count
                                 curr-count))))))

;;; For each day number for a fish, add 1 to the count for that
;;; slot in the counts array, then return the array as a list
(define (make-initial-counts fish counts)
  (if (empty? fish) (array->list counts)
      (let* ((curr-fish (head fish))
             (curr-count (@ counts curr-fish)))
      (make-initial-counts (tail fish)
                           (@<- counts curr-fish (+ curr-count 1))))))

(define (day6a)
  (let* ((fish (map string->int (split (head (read-lines "day6.txt")) ",")))
         (initial-counts (make-initial-counts fish (make-array-with-default 9 0)))
         (final-counts (iterate-day 80 initial-counts)))
    (fold + 0 final-counts)))

(define (day6b)
  (let* ((fish (map string->int (split (head (read-lines "day6.txt")) ",")))
         (initial-counts (make-initial-counts fish (make-array-with-default 9 0)))
         (final-counts (iterate-day 256 initial-counts)))
    (fold + 0 final-counts)))
