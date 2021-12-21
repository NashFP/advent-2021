(load "mwlib.scm")

(define (parse-algorithm line)
  (list->array (string->list line)))

(define (make-dict-entries y line)
  (let* ((xs (range 0 (- (string-length line) 1)))
         (chars (string->list line))
         (keys (map (lambda (x) (Pair x y)) xs)))
    (zip-with Pair keys chars)))
        
(define (parse-image lines y acc)
  (if (empty? lines) (list->dict acc)
      (parse-image (tail lines) (+ y 1)
        (append (make-dict-entries y (head lines)) acc))))

(define (get-pixel d default x y)
  (let* ((pixel (dict-lookup-with-default d (Pair x y) default)))
    (if (char= pixel #\#) 1 0)))

(define offsets (list (Pair -1 -1) (Pair 0 -1) (Pair 1 -1)
                      (Pair -1 0) (Pair 0 0) (Pair 1 0)
                      (Pair -1 1) (Pair 0 1) (Pair 1 1)))

(define (alg-pixel d default x y acc offset)
  (let (((Pair xoff yoff) offset)
        (pixel-value (get-pixel d default (+ x xoff) (+ y yoff))))
    (+ (* 2 acc) pixel-value)))
        
(define (process-pixel algorithm d default new-dict coord)
  (let* (((Pair x y) coord)
         (new-value (fold (alg-pixel d default x y) 0 offsets))
         (new-pixel (@ algorithm new-value)))
    (dict-put new-dict (Pair x y) new-pixel)))

(define (min-x d)
  (list-min (map fst (map fst (dict->list d)))))

(define (min-y d)
  (list-min (map snd (map fst (dict->list d)))))

(define (max-x d)
  (list-max (map fst (map fst (dict->list d)))))

(define (max-y d)
  (list-max (map snd (map fst (dict->list d)))))

(define (do-round algorithm default d)
  (let ((startx (- (min-x d) 1))
        (starty (- (min-y d) 1))
        (endx (+ (max-x d) 1))
        (endy (+ (max-y d) 1)))
    (fold (process-pixel algorithm d default) (make-dict) (cartesian-product (range startx endx) (range starty endy)))))

(define (do-round-pair algorithm d)
  (do-round algorithm #\# 
      (do-round algorithm #\. d)))
                        
(define (day20a)
  (let* ((lines (read-lines "day20.txt"))
         (algorithm (parse-algorithm (head lines)))
         (pixel-dict (parse-image (tail (tail lines)) 0 nil)))
    (length (filter (lambda (p) (char= #\# (snd p)))
                    (dict->list (do-round-pair algorithm pixel-dict))))))

(define (day20b)
  (let* ((lines (read-lines "day20.txt"))
         (algorithm (parse-algorithm (head lines)))
         (pixel-dict (parse-image (tail (tail lines)) 0 nil)))
    (length (filter (lambda (p) (char= #\# (snd p)))
                    (dict->list 
                      (fold (lambda (d i) (do-round-pair algorithm d)) pixel-dict (range 1 25)))))))

