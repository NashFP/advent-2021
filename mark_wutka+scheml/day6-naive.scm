;;; This was my first stab at the problem, decrementing the
;;; day number for each individual fish and adding a new fish
;;; when a fish reproduced. It worked for the first part, but
;;; I had to rethink for the second part.

(define (iterate-fish fish acc)
  (if (empty? fish) acc
      (if (= 0 (head fish))
          (iterate-fish (tail fish) (cons 6 (cons 8 acc)))
          (iterate-fish (tail fish) (cons (- (head fish) 1) acc)))))

(define (iterate-days n fish)
  (if (= n 0) fish
      (iterate-days (- n 1) (iterate-fish fish nil))))

(define (day6a)
  (let ((fish (map string->int
                   (split (head (read-lines "day6.txt")) ","))))
    (length (iterate-days 80 fish))))
