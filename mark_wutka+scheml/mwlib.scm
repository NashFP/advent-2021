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
