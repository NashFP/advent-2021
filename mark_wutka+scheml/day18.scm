;;; I hated this one and had pretty much given up on it because
;;; it just wasn't clear to me about the ordering of the reducing.
;;; It would pass all the basic tests, but then the first example
;;; of adding two long numbers it failed.

(load "mwlib.scm")

(type numlist (Single int) (NumPair numlist numlist))

(define (numlist->string nl)
  (match nl
    ((Single n) (sprintf "%d" n))
    ((NumPair v1 v2) (sprintf "[%s,%s]" (numlist->string v1) (numlist->string v2)))))

(define (digit? ch) (and (char>= ch #\0) (char<= ch #\9)))

(define (read-number str acc)
  (if (end-of-stream? str) acc
    (if (digit? (just (peek-stream str)))
       (read-number str (+ (* 10 acc) (- (char->int (just (read-stream str))) 48)))
        acc)))

(define (parse-number str)
  (let ((ch (just (peek-stream str))))
    (if (char= ch #\[) 
        (let ((ignore0 (read-stream str)) ;;; ignore the opening [ (which was peeked)
              (val1 (parse-number str))
              (ignore (read-stream str))  ;;; ignore the ,
              (val2 (parse-number str))
              (ignore2 (read-stream str)))  ;;; ignore the closing ]
          (NumPair val1 val2))
        (Single (read-number str 0)))))

(define (parse-line l) (parse-number (make-stream (string->list l))))

(define (single? v)
  (match v
    ((Single _) #t)
    ((NumPair _ _) #f)))

(define (single v)
  (match v
    ((Single num) num)
    ((NumPair _ _) (fail "tried to get Single from NumPair"))))

(type reduce-action None Done (Explode int int) (ExplodeLeft int)
      (ExplodeRight int))

;;; Splits a single by creating a pair of n/2 and n/2+n%2 (i.e. n/2 rounded up)
(define (do-split n)
  (NumPair (Single (/ n 2)) (Single (+ (/ n 2) (% n 2)))))

(define (replace-leftmost target v)
  (match v
    ((Single n) (Single (+ n target)))
    ((NumPair v1 v2) (NumPair (replace-leftmost target v1) v2))))

(define (replace-rightmost target v)
  (match v
    ((Single n) (Single (+ n target)))
    ((NumPair v1 v2) (NumPair v1 (replace-rightmost target v2)))))

(define (none? v)
  (match v
    (None #t)
    (_ #f)))

(define (reduce splits? v depth)
  (match v
    ;;; If this is a single and we are reducing singles,
    ;;; see if it should split. If the depth is >= 4, go
    ;;; ahead and explode it right here
    ((Single n) (if (not splits?) (Pair (None) v)
                    (if (>= n 10) 
                        (if (>= depth 4) (Pair (Explode (/ n 2) (+ (/ n 2) (% n 2))) (Single 0))
                            (Pair (Done) (do-split n)))
                        (Pair (None) v))))
    ((NumPair v1 v2)
        ;;; if the pair is nested >= 4, it explodes
        (if (and (>= depth 4)
                 (and (single? v1) (single? v2)))
            (Pair (Explode (single v1) (single v2)) (Single 0))

            ;;; Reduce the left side of the pair
            (let (((Pair res1 val1) (reduce splits? v1 (+ depth 1))))
              (match res1
                ;;; If done, go ahead and return, v2 will eventually reduce
                (Done (Pair (Done) (NumPair val1 v2)))
                ;;; If the left hand exploded, we can go ahead and replace the
                ;;; leftmost value in the right-hand part of the pair now,
                ;;; and then return a result indicating that the explosion still
                ;;; needs to proceed to the left
                ((Explode e1 e2) (Pair (ExplodeLeft e1) (NumPair val1 (replace-leftmost e2 v2))))

                ;;; If propagating the left side of an explosion, just return it
                ((ExplodeLeft e1) (Pair (ExplodeLeft e1) (NumPair val1 v2)))

                ;;; If propagating the right side of an explosion, replace the
                ;;; leftmost number in v2 with the propagated value
                ((ExplodeRight e2) (Pair (Done) (NumPair val1
                                                         (replace-leftmost e2 v2))))
                (None
                  ;;; if nothing happened in v1, try reducing v2
                  (let (((Pair res2 val2) (reduce splits? v2 (+ depth 1))))
                    (match res2
                      (Done (Pair (Done) (NumPair v1 val2)))
                      ;;; If v2 exploded, we can immediately replace the rightmost
                      ;;; number in the lefthand side, but then must return a value
                      ;;; to propagate the explosion to the right
                      ((Explode e1 e2) (Pair (ExplodeRight e2)
                                             (NumPair (replace-rightmost e1 v1)
                                                      val2)))
                      ;;; if propagating the left side of an explosion, we can
                      ;;; do that replacement here
                      ((ExplodeLeft e1) (Pair (Done)
                                              (NumPair (replace-rightmost e1 v1)
                                                       val2)))
                      ;;; otherwise if propagating the righthand side of an explosion
                      ;;; we just need to return it up the stack
                      ((ExplodeRight e2) (Pair (ExplodeRight e2)
                                               (NumPair v1 val2)))
                      (None (Pair (None) (NumPair v1 v2))))))))))))

(define (reduce-loop v)
  ;;; First try reducing only explosions
  (let (((Pair res val) (reduce #f v 0)))
    ;;; If something happened in the reduce, reduce again
    (if (not (none? res)) (reduce-loop val)
        ;;; Otherwise, reduce numbers (and explosions)
        (let (((Pair res2 val2) (reduce #t val 0)))
          ;;; If nothing happened in the reduce, we're done
          (if (none? res2) val2
            ;;; otherwise go back and reduce starting again
            ;;; with only explosions
            (reduce-loop val2))))))

(define (magnitude v)
  (match v
    ((Single n) n)
    ((NumPair v1 v2) (+ (* 3 (magnitude v1)) (* 2 (magnitude v2))))))

(define (add-lists v1 v2)
  (reduce-loop (NumPair v1 v2)))

(define (mag-add-pair p)
  (let (((Pair v1 v2) p))
    (magnitude (add-lists v1 v2))))

(define (day18a)
  (let* ((numbers (map parse-line (read-lines "day18.txt")))
         (result (fold add-lists (head numbers) (tail numbers))))
    (magnitude result)))

(define (day18b)
  (let* ((numbers (map parse-line (read-lines "day18.txt"))))
    (list-max (map mag-add-pair (cartesian-product numbers numbers)))))
