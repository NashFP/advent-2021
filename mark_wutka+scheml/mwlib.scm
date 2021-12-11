(define (zip-with' f l1 l2 acc)
  (if (empty? l1) (reverse acc)
      (if (empty? l2) (reverse acc)
          (zip-with' f (tail l1) (tail l2) 
                     (cons (f (head l1) (head l2)) acc)))))

(define (zip-with f l1 l2) (zip-with' f l1 l2 nil))

(define (zip-with3' f l1 l2 l3 acc)
  (if (empty? l1) (reverse acc)
      (if (empty? l2) (reverse acc)
          (if (empty? l3) (reverse acc)
            (zip-with3' f (tail l1) (tail l2) (tail l3)
                     (cons (f (head l1) (head l2) (head l3)) acc))))))

(define (zip-with3 f l1 l2 l3) (zip-with3' f l1 l2 l3 nil))

(define (split-groups' lines group-acc groups-acc)
  (if (empty? lines)
      (if (empty? group-acc) (reverse groups-acc)
          (reverse (cons (reverse group-acc) groups-acc)))
      (if (= 0 (string-length (head lines)))
          (split-groups' (tail lines) nil 
                         (cons (reverse group-acc) 
                               groups-acc))
          (split-groups' (tail lines)
                         (cons (head lines) group-acc)
                         groups-acc))))

(define (split-groups lines)
  (split-groups' lines nil nil))

(define (list-min l)
  (fold min (head l) (tail l)))

(define (list-max l)
  (fold max (head l) (tail l)))

(type grid ('a) (Grid int int (array 'a)))

(define (grid-fetch g x y)
  (let (((Grid width height data) g))
    (if (and (and (>= x 0) (< x width))
             (and (>= y 0) (< y height)))
        (Just (@ data (+ x (* y width))))
        (Nothing))))

(define (grid-fetch-coord g coord)
  (let (((Grid width height data) g)
        ((Pair x y) coord))
    (if (and (and (>= x 0) (< x width))
             (and (>= y 0) (< y height)))
        (Just (@ data (+ x (* y width))))
        (Nothing))))

(define (map-grid f g)
  (let (((Grid width height data) g))
    (Grid width height (array-map f data))))

(define (offset-to-coord width off)
  (let ((x (% off width))
        (y (/ off width)))
    (Pair x y)))

(define (coord-pairs g)
  (let* (((Grid width height _) g)
         (num-coords (* width height)))
    (map (offset-to-coord width) (range 0 (- num-coords 1)))))


(define (list->grid l)
  (let ((width (length (head l)))
        (height (length l))
        (values (list->array (fold append (head l) (tail l)))))
    (Grid width height values)))
