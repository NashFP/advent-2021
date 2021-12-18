;;; Solves the problem by enumerating all possible x values at a time t and
;;; eliminating the ones that aren't within the x bounds, then enumerating
;;; all the y values at a time t and eliminating the ones that aren't valid,
;;; then pairing valid x and y velocity values that have the same time t

(load "mwlib.scm")

(define (parse-box l)
  (just
    (regex-bind l "target area: x=([0-9]*)[.][.]([0-9]*), y=([-0-9]*)[.][.]([-0-9]*)"
                ((string->int min-x) (string->int max-x)
                 (string->int min-y) (string->int max-y))
                (Pair (Pair min-x max-x) (Pair min-y max-y)))))

(define (sum-int n) (/ (* n (+ n 1)) 2))

(define (compute-x-pos vel t)
  (if (< t vel) (- (sum-int vel) (sum-int (- vel t)))
      (sum-int vel)))

(define (valid-t-x-vel? min-val max-val vel-t-pair)
  (let* (((Pair vel t) vel-t-pair)
         (pos (compute-x-pos vel t)))
    (and (>= pos min-val) (<= pos max-val))))

(define (compute-y-pos vel t)
    (if (< t vel) (- (sum-int vel) (sum-int (- vel t)))
        (if (< vel 0) (* -1 (- (sum-int (+ (* -1 vel) (- t 1))) (sum-int (- (* -1 vel) 1))))
            (- (sum-int vel) (sum-int (- (- t 1) vel))))))

(define (valid-t-y-vel? min-val max-val vel-t-pair)
  (let* (((Pair vel t) vel-t-pair)
         (pos (compute-y-pos vel t)))
    (and (>= pos min-val) (<= pos max-val))))

(define (valid-t-vel-pair? xy-vel)
  (let (((Pair x-pair y-pair) xy-vel)
        ((Pair x-vel xt) x-pair)
        ((Pair y-vel yt) y-pair))
    (= yt xt)))

(define (t-vel-pair-to-vel-pair p)
  (Pair (fst (fst p)) (fst (snd p))))

(define (enumerate-x-vels min-x max-x)
    (filter (valid-t-x-vel? min-x max-x) (cartesian-product (range 1 200) (range 1 600))))

(define (enumerate-y-vels min-y max-y)
    (filter (valid-t-y-vel? min-y max-y) (cartesian-product (range -200 200) (range 1 600))))

(define (day17a)
  (let* ((l (head (read-lines "day17.txt")))
         ((Pair x-bounds y-bounds) (parse-box l))
         ((Pair min-x max-x) x-bounds)
         ((Pair min-y max-y) y-bounds)
         (x-vels (enumerate-x-vels min-x max-x))
         (y-vels (enumerate-y-vels min-y max-y))
         (best-y (list-max (map snd (set->list (list->set (map t-vel-pair-to-vel-pair (filter valid-t-vel-pair? (cartesian-product x-vels y-vels)))))))))
    (/ (* best-y (+ best-y 1)) 2)))

(define (day17b)
  (let* ((l (head (read-lines "day17.txt")))
         ((Pair x-bounds y-bounds) (parse-box l))
         ((Pair min-x max-x) x-bounds)
         ((Pair min-y max-y) y-bounds)
         (x-vels (enumerate-x-vels min-x max-x))
         (y-vels (enumerate-y-vels min-y max-y))
         (all-pairs (set->list (list->set (map t-vel-pair-to-vel-pair (filter valid-t-vel-pair? (cartesian-product x-vels y-vels)))))))
    (length all-pairs)))
