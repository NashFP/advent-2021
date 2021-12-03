(define (char-to-bit ch)
  (- (char->int ch) 48))

(define (string-to-bits str)
  (map char-to-bit (string->list str)))

(define (bits-to-int bits acc)
  (if (empty? bits) acc
      (bits-to-int (tail bits) (+ (* acc 2) (head bits)))))

(define (most-common-head-bit bit-lists)
  (let* ((bit-sum (fold + 0 (map head bit-lists)))
         (other-sum (- (length bit-lists) bit-sum)))
    (if (> bit-sum other-sum) 1
        (if (= bit-sum other-sum) 1 0))))

(define (most-common-bits bit-lists acc)
  (if (empty? (head bit-lists)) (reverse acc)
      (most-common-bits (map tail bit-lists) 
                        (cons (most-common-head-bit bit-lists) acc))))

(define (common-bits-reduce most-frequent? bit-lists acc)
  ;;; as we accumulate the common bits in reverse order, when we find
  ;;; a single number left, we probably find it before we reach the
  ;;; last bit, so reverse the accumulated bits and then append
  ;;; whatever bits are left in the last number
  (if (= 1 (length bit-lists)) (append (reverse acc) (head bit-lists))

      ;;; Compute the target bit, invert if we want least-frequent bit
      (let* ((target-bit (if most-frequent?
                             (most-common-head-bit bit-lists)
                             (- 1 (most-common-head-bit bit-lists)))))
      ;;; add the target bit to the accumulated bits list,
      ;;; filter out the bit lists that don't start with the target bit
      ;;; and then just process the tails of each of those
      (common-bits-reduce 
        most-frequent?
        (map tail (filter (lambda (l) (= target-bit (head l))) bit-lists))
        (cons target-bit acc)))))

(define (invert-bits bits)
  ;;; Partial application of - this way causes the
  ;;; function to be (1 - x) which is what we want
  (map (- 1) bits))

(define (day3a)
  (let* ((bits (map string-to-bits (read-lines "day3.txt")))
         (most-common (most-common-bits bits nil))
         (gamma (bits-to-int most-common 0))
         (epsilon (bits-to-int (invert-bits most-common) 0)))
    (* gamma epsilon)))

(define (day3b)
  (let* ((bits (map string-to-bits (read-lines "day3.txt")))
         (oxygen (bits-to-int (common-bits-reduce #t bits nil) 0))
         (co2 (bits-to-int (common-bits-reduce #f bits nil) 0)))
    (* oxygen co2)))
