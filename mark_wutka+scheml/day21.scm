(load "mwlib.scm")

(define (add3 l) (+ (head l) (+ (nth 1 l) (nth 2 l))))

(define (mod10 n) (let ((nm (% n 10))) (if (= nm 0) 10 nm)))
(define (mod100 n) (let ((nm (% n 100))) (if (= nm 0) 100 nm)))

(define (sum3 start l acc)
  (if (empty? l) (reverse acc)
      (sum3 (mod10 (+ start (add3 (take 3 l)))) (drop 6 l) (cons start acc))))

(define (make-table-1)
  (list->array 
    (map (lambda (start) (fold + 0 (take 10 (sum3 start (range 1 300) nil)))) (range 1 10))))

(define (make-table-2)
  (list->array 
    (map (lambda (start) (fold + 0 (take 10 (sum3 start (range 4 300) nil)))) (range 1 10))))

(define (play p1-score p1-pos p2-score p2-pos rolls die)
  (let* ((new-pos-1 (mod10 (+ p1-pos (+ (* die 3) 3))))
         (new-score-1 (+ p1-score new-pos-1))
         (die-2 (mod100 (+ die 3))))
    (if (>= new-score-1 1000) (* p2-score (+ rolls 3))
        (let* ((new-pos-2 (mod10 (+ p2-pos (+ (* die-2 3) 3))))
               (new-score-2 (+ p2-score new-pos-2))
               (die-next (mod100 (+ die-2 3))))
          (if (> new-score-2 1000) (* p1-score (+ rolls 6))
              (play new-score-1 new-pos-1 new-score-2 new-pos-2 (+ rolls 6) die-next))))))

(define (start-play p1-start p2-start)
  ;;; Since the tables repeat (player 1 actually repeats after 5 turns, but since player 2
  ;;; repeats after 10, I just do the calculation based on 10 turns. The entry in the
  ;;; table is how many points the player makes after 10 turns. By dividing 1000 by this,
  ;;; we get within 10 turns of the end, and then just let the play routine play it out
  (let ((p1-incr (@ (make-table-1) (- p1-start 1)))
        (p2-incr (@ (make-table-2) (- p2-start 1))))
    (let ((num-10rounds (/ 1000 (if (> p1-incr p2-incr) p1-incr p2-incr))))
      (play (* p1-incr num-10rounds) p1-start (* p2-incr num-10rounds) p2-start
              (* 60 num-10rounds) (mod100 (+ 1 (* 60 num-10rounds)))))))

(define (day21a)
  (start-play 6 9))

(define (add-pairs p1 p2)
  (let (((Pair x1 y1) p1)
        ((Pair x2 y2) p2))
    (Pair (+ x1 x2) (+ y1 y2))))

(type dirac-key (DiracKey int int int int))

(define (psum p) (+ (fst p) (snd p)))

;;; Generate all possible rolls of 3 3-sided dice
(define all-rolls (map psum (cartesian-product (range 1 3) (map psum (cartesian-product (range 1 3) (range 1 3))))))

;;; Generate the next position for each player given a pair of rolls
(define (generate-roll p1-score p1-pos p2-score p2-pos rolls)
  (let* (((Pair roll1 roll2) rolls)
         (new-p1-pos (mod10 (+ p1-pos roll1)))
         (new-p2-pos (mod10 (+ p2-pos roll2))))
    (DiracKey (+ p1-score new-p1-pos) new-p1-pos (+ p2-score new-p2-pos) new-p2-pos)))

;;; Generate all the possible next outcomes for a given score and position
;;; for each player
(define (generate-rolls key)
  (let (((DiracKey p1-score p1-pos p2-score p2-pos) key))
    (map (generate-roll p1-score p1-pos p2-score p2-pos) (cartesian-product all-rolls all-rolls))))

;;; Compute the possible wins for a given position
(define (dirac-wins d key)
  (let (((DiracKey p1-score p1-pos p2-score p2-pos) key))
    ;;; If p1 or p2 wins, just return the win value
    (if (> p1-score 20) (Pair 1 0)
        (if (> p2-score 20) (Pair 0 1)
          ;; If we have cached this result, return it
          (if (dict-has-key? d key) (dict-lookup d key)
            ;;; otherwise calculate the wins
            (let ((wins
                (fold add-pairs (Pair 0 0)
                          (map (dirac-wins d)
                               (generate-rolls key)))))
              ;;; and cache them (dict in Scheml is stateful)
              (dict-put d key wins)
              wins))))))

(define (day21b)
  ;;; 0 4 0 8 represents the test data starting case, I just change 4 and 8
  ;;; to the initial positions I was given
  (let (((Pair p1wins p2wins) (dirac-wins (make-dict) (DiracKey 0 4 0 8))))
    ;;; Because I have already generated the rolls for player b,
    ;;; the player 1 score is 27 times what it should be (because I
    ;;; add 1 for the win for each player b roll even though those rolls
    ;;; wouldn't take place
    (max (/ p1wins 27) p2wins)))
