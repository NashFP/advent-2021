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
