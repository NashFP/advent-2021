(load "mwlib.scm")

(define (make-grid lines)
  (let ((width (length (string->list (head lines))))
        (height (length lines))
        (data (map (lambda (ch) (- (char->int ch) 48))
                   (fold append nil
                         (map string->list lines)))))
    (Grid width height (list->array data))))

(define (greater-or-nothing val target)
  (if (nothing? target) #t
      (< val (just target))))

(define (is-low? g xy)
  (let* (((Pair x y) xy)
         (val (just (grid-fetch g x y))))
    (all (greater-or-nothing val)
          (list (grid-fetch g (- x 1) y)
                (grid-fetch g (+ x 1) y)
                (grid-fetch g x (- y 1))
                (grid-fetch g x (+ y 1))))))

(define (find-low g xy)
  (let (((Pair x y) xy))
    (if (is-low? g xy) (Just (Pair (just (grid-fetch g x y)) xy))
      (Nothing))))

(define (coord-pair g i)
  (let (((Grid width _ _) g))
    (Pair (% i width) (/ i width))))

(define (day9a)
  (let* ((g (make-grid (read-lines "day9.txt")))
         ((Grid _ _ data) g)
         (coords (map (coord-pair g) (range 0 (- (array-length data) 1))))
         (low-points (map fst (map-optional (find-low g) coords))))
    (fold (lambda (s p) (+ s (+ p 1))) 0 low-points)))

;;; This is just ugly, using references and some imperative forms
;;; The checked set specifically checks to see if a position has
;;; been checked relative to another position, so a position could
;;; be checked multiple times approaching from different directions
(define (check-in-basin grid val basin checked xy)
  (let* ((basin-set (! basin))
         ((Pair x y) xy)
         (target (grid-fetch grid x y)))
    (if (nothing? target) basin
        (progn
            (if (or (= (just target) 9) (< (just target) val))
                basin
                (progn
                  (<- basin (set-put basin-set xy))
                  (when (not (set-contains? (! checked) (Pair xy (Pair (- x 1) y))))
                    (<- checked (set-put (! checked) (Pair xy (Pair (- x 1) y))))
                    (check-in-basin grid (just target) basin checked
                                   (Pair (- x 1) y)))
                  (when (not (set-contains? (! checked) (Pair xy (Pair (+ x 1) y))))
                    (<- checked (set-put (! checked) (Pair xy (Pair (+ x 1) y))))
                    (check-in-basin grid (just target) basin checked
                                   (Pair (+ x 1) y)))
                  (when (not (set-contains? (! checked) (Pair xy (Pair x (- y 1)))))
                    (<- checked (set-put (! checked) (Pair xy (Pair x (- y 1)))))
                    (check-in-basin grid (just target) basin checked
                                   (Pair x (- y 1))))
                  (when (not (set-contains? (! checked) (Pair xy (Pair x (+ y 1)))))
                    (<- checked (set-put (! checked) (Pair xy (Pair x (+ y 1)))))
                    (check-in-basin grid (just target) basin checked
                                   (Pair x (+ y 1))))
                  basin))))))

(define (find-basin grid low-point)
  (let (((Pair val xy) low-point))
    (check-in-basin grid val (Ref (make-set)) (Ref (make-set)) xy)))

(define (top-3 l)
  (let* ((top1 (fold max (head l) (tail l)))
         (rest1 (remove top1 l))
         (top2 (fold max (head rest1) (tail rest1)))
         (rest2 (remove top2 rest1))
         (top3 (fold max (head rest2) (tail rest2))))
    (* top1 (* top2 top3))))

(define (day9b)
  (let* ((g (make-grid (read-lines "day9.txt")))
         ((Grid _ _ data) g)
         (coords (map (coord-pair g) (range 0 (- (array-length data) 1))))
         (low-points (map-optional (find-low g) coords))
         (basins (map (find-basin g) low-points)))
    (top-3 (map set-len (map ! basins)))))


