(load "mwlib.scm")

(type fold (X-Fold int) (Y-Fold int))

(define (parse-coord l)
  (let ((coords (map string->int (split l ","))))
    (Pair (head coords) (head (tail coords)))))

(define (parse-fold l)
  (just (regex-bind l "fold along (.)=([0-9]*)" (axis (string->int fold-point))
    (if (equals? axis "x") (X-Fold fold-point) (Y-Fold fold-point)))))

(define (fold-val v fold-point)
  (if (< v fold-point) v
      (- (* fold-point 2) v)))

(define (fold-coord fold-point coord)
  (let (((Pair x y) coord))
    (match fold-point
      ((X-Fold x-fold) (Pair (fold-val x x-fold) y))
      ((Y-Fold y-fold) (Pair x (fold-val y y-fold))))))

(define (do-fold coords fold-point)
  (list->set (map (fold-coord fold-point) (set->list coords))))

(define (day13a)
  (let* ((parts (split-groups (read-lines "day13.txt")))
         (coords (map parse-coord (head parts)))
         (folds (map parse-fold (head (tail parts))))
         (paper (list->set coords)))
    (set-len (do-fold (list->set coords) (head folds)))))

(define (make-grid coords)
  (let* ((coord-list (set->list coords))
         ;; Add 1 to x-max to make room for newline
         (x-max (+ 1 (list-max (map fst coord-list))))
         (y-max (list-max (map snd coord-list)))
         (grid-len (* (+ x-max 1) (+ y-max 1))))
    (make-array-with-function grid-len
      (lambda (i)
        (let ((x (% i x-max))
              (y (/ i x-max)))
          (if (= x (- x-max 1)) "\n"
              (if (set-contains? coords (Pair x y)) "#" " ")))))))

(define (print-grid coord-grid)
  (map print (array->list coord-grid)))

(define (day13b)
  (let* ((parts (split-groups (read-lines "day13.txt")))
         (coords (map parse-coord (head parts)))
         (folds (map parse-fold (head (tail parts))))
         (paper (list->set coords))
         (folded (fold do-fold (list->set coords) folds))
         (coord-grid (make-grid folded)))
    (print-grid coord-grid)))
