(load "mwlib.scm")

(type octopus (Charging int) Flash)

(define (char->num ch)
  (- (char->int ch) 48))

(define (increment-charge octo)
  (<- octo
      (match (! octo)
        ((Charging c) (Charging (+ c 1)))
        (Flash (Flash)))))

(define (increment-and-check g check-flash coord)
  (let ((octo (grid-fetch-coord g coord)))
    (if (nothing? octo) octo
        (progn
          (increment-charge (just octo))
          (check-flash g coord)))))

(define (check-flash g coord)
  (let (((Pair x y) coord)
        (octo (grid-fetch g x y)))
    (if (nothing? octo) octo
        (match (! (just octo))
           (Flash octo)
           ((Charging charge)
            (if (<= charge 9) octo
                (progn
                  (<- (just octo) (Flash))
                  (increment-and-check g check-flash (Pair (- x 1) (- y 1)))
                  (increment-and-check g check-flash (Pair x (- y 1)))
                  (increment-and-check g check-flash (Pair (+ x 1) (- y 1)))
                  (increment-and-check g check-flash (Pair (- x 1) y))
                  (increment-and-check g check-flash (Pair (+ x 1) y))
                  (increment-and-check g check-flash (Pair (- x 1) (+ y 1)))
                  (increment-and-check g check-flash (Pair x (+ y 1)))
                  (increment-and-check g check-flash (Pair (+ x 1) (+ y 1)))
                  octo)))))))

(define (count-flash octo)
  (match (! octo)
         (Flash (progn
                  (<- octo (Charging 0))
                  1))
         ((Charging _) 0)))
    
(define (iterate-day g)
  (let* (((Grid width height grid-data) g)
         (charged (array-map increment-charge grid-data))
         (coords (coord-pairs g)))
    (map (check-flash g) coords)
    (fold + 0 (map count-flash (array->list grid-data)))))

(define (day11a)
  (let* ((lines (read-lines "day11.txt"))
         (as-nums (map (lambda (l) (map char->num (string->list l))) lines))
         (g (list->grid as-nums))
         (octo-grid (map-grid (lambda (n) (Ref (Charging n))) g)))
    (fold + 0 (map (lambda (n) (iterate-day octo-grid)) (range 0 99)))))

(define (find-first-all-flash g step)
  (let (((Grid width height _) g)
        (flashing (iterate-day g)))
    (if (= flashing (* width height)) step
        (find-first-all-flash g (+ step 1)))))

(define (day11b)
  (let* ((lines (read-lines "day11.txt"))
         (as-nums (map (lambda (l) (map char->num (string->list l))) lines))
         (g (list->grid as-nums))
         (octo-grid (map-grid (lambda (n) (Ref (Charging n))) g)))
    (find-first-all-flash octo-grid 1)))
