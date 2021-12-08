(define (parse-line line)
  (let ((parts (map (swap split " ") (split line " \\| "))))
    (Pair (head parts) (head (tail parts)))))

(define (is-unique? s)
  (let ((l (string-length s)))
    (or (or (= l 2) (= l 3))
        (or (= l 4) (= l 7)))))

(define (count-unique segs)
  (length (filter is-unique? segs)))

(define (day8a)
  (let ((output-segs (map snd (map parse-line (read-lines "day8.txt")))))
    (fold + 0 (map count-unique output-segs))))

;;; From a list of digit sets, find one with a specific size
;;; (expecting there to be exactly one)
(define (find-with-size l size)
  (let* ((result (filter (lambda (s) (= (set-len s) size)) l)))
    (head result)))

;;; Find the one digit set that has all the segments in target and a
;;; specific size
(define (find-matching-with-size l target size)
  (let ((result (filter (lambda (s) (= 0 (set-len (set-difference target s)))) l)))
    (find-with-size result size)))

;;; Looks for six by examining all the 6-segment digits and ignoring the ones
;;; where either the middle-seg (0) or the left-bottom-seg (9) is not present
(define (matches-six? middle-seg left-bottom-seg s)
  (and (= (set-len s) 6)
       (and (set-contains? s middle-seg)
            (set-contains? s left-bottom-seg))))

;;; Find the digit set that matches 6
(define (find-six digits-list middle-seg left-bottom-seg)
  (head (filter (matches-six? middle-seg left-bottom-seg) digits-list)))

(define (deduce-digits digits-list)
  ;;; 1, 7, 4, and 8 are immediately known
  (let* ((one-set (find-with-size digits-list 2))
         (seven-set (find-with-size digits-list 3))
         (four-set (find-with-size digits-list 4))
         (eight-set (find-with-size digits-list 7))
         ;;; We can deduce the top segment by subtracting 1's segments from 7
         (top-seg (head (set->list (set-difference seven-set one-set))))
         ;;; We can find 9 by adding the top segment to 4 and looking for
         ;;; a 6-segment digit that has those segments
         (nine-match (set-put four-set top-seg))
         (nine-set (find-matching-with-size digits-list
                                            nine-match 6))
         ;;; We can deduce the bottom segment by subtracting the nine-match
         ;;; segments from the nine-set
         (bottom-seg (head (set->list (set-difference nine-set nine-match))))
         ;;; We can find three by adding the bottom segment to 7 and looking
         ;;; for a 5-segment digit that has those segments
         (three-match (set-put seven-set bottom-seg))
         (three-set (find-matching-with-size digits-list three-match 5))
         ;;; We can deduce the middle segment by subtracting three-match from three-set
         (middle-seg (head (set->list (set-difference three-set three-match))))
         ;;; We can deduce the left bottom by subtracting nine-set from eight-set
         (left-bottom-seg (head (set->list (set-difference eight-set nine-set))))
         ;;; We can deduce the left top by subtracting three-set+left-bottom-seg from eight-set
         (left-top-seg (head (set->list (set-difference eight-set (set-put three-set left-bottom-seg)))))
         ;;; Look for six using a specialized function
         (six-set (find-six digits-list middle-seg left-bottom-seg))
         ;;; We can deduce the right-top-seg by subtracting six-set from eight-set
         (right-top-seg (head (set->list (set-difference eight-set six-set))))
         ;;; Finally we can deduce the right-bottom-seg by subtracting right-top-seg from one-set
         (right-bottom-seg (head (set->list (set-remove one-set right-top-seg))))
         ;;; We can get 0, 2, and 5 by adding/substracting segments to existing sets
         (zero-set (set-remove eight-set middle-seg))
         (two-set (set-put (set-remove three-set right-bottom-seg) left-bottom-seg))
         (five-set (set-put (set-remove three-set right-top-seg) left-top-seg)))
    ;;; Return the segment sets as an array where the index matches the digit the set represents
    (make-array zero-set one-set two-set three-set four-set five-set six-set seven-set eight-set nine-set)))

;;; Find the digit set in the digit map (which is an array)
(define (digit-set-to-digits digit-set digit-map i)
    (if (equals? digit-set (@ digit-map i)) i
       (digit-set-to-digits digit-set digit-map (+ i 1))))

;;; Compute the output value by finding each output digit
(define (get-output-value output-digits digit-map)
  (fold (lambda (sum digit-set) (+ (* 10 sum) (digit-set-to-digits digit-set digit-map 0)))
        0
        output-digits))

;;; For each entry in the problem set, deduce the digit map, then compute the output
;;; digit value
(define (process-entry entry)
  (let* (((Pair digit-sets output-digit-list) entry)
         (digit-map (deduce-digits (map (lambda (s) (list->set (string->list s))) digit-sets)))
         (output-digits (map (lambda (s) (list->set (string->list s))) output-digit-list)))
    (get-output-value output-digits digit-map)))

(define (day8b)
  (let* ((entries (map parse-line (read-lines "day8.txt")))
         (outputs (map process-entry entries)))
    ;;; Add up all the output values
    (fold + 0 outputs)))
