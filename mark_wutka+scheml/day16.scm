(load "mwlib.scm")

(define hex-map
  (make-dict (#\0 (list 0 0 0 0))
             (#\1 (list 0 0 0 1))
             (#\2 (list 0 0 1 0))
             (#\3 (list 0 0 1 1))
             (#\4 (list 0 1 0 0))
             (#\5 (list 0 1 0 1))
             (#\6 (list 0 1 1 0))
             (#\7 (list 0 1 1 1))
             (#\8 (list 1 0 0 0))
             (#\9 (list 1 0 0 1))
             (#\a (list 1 0 1 0))
             (#\b (list 1 0 1 1))
             (#\c (list 1 1 0 0))
             (#\d (list 1 1 0 1))
             (#\e (list 1 1 1 0))
             (#\f (list 1 1 1 1))
             (#\A (list 1 0 1 0))
             (#\B (list 1 0 1 1))
             (#\C (list 1 1 0 0))
             (#\D (list 1 1 0 1))
             (#\E (list 1 1 1 0))
             (#\F (list 1 1 1 1))))

;;; Define a stateful stream of bits that consists of a list
;;; of bits from the most recent hex char, and a list of
;;; hex chars. When the bit list is empty, it is populated
;;; by pulling the next hex char off the list
(type bitstream (BitStream (ref (cons int)) (ref (cons char))))

(define (take-bit bits)
  (let (((BitStream bit-list hex-list) bits))
    ;;; See if the list of bits is empty
    (if (empty? (! bit-list))
        ;;; if it is, and the hex list is empty, return Nothing
        (if (empty? (! hex-list)) (Nothing)
            ;;; Otherwise, get the bits for the next hex char
            (let ((hex-bits (dict-lookup hex-map (head (! hex-list)))))
              ;;; Take the next hex char off the hex list
              (<- hex-list (tail (! hex-list)))
              ;;; Store the last 3 bits in the bit list
              (<- bit-list (tail hex-bits))
              ;;; Return the first bit
              (Just (head hex-bits))))

        ;;; Otherwise, there are still bits in the list
        (let ((hex-bits (! bit-list)))
          ;;; remove the first bit from the list
          (<- bit-list (tail hex-bits))
          ;;; return the first bit
          (Just (head hex-bits))))))

;;; take n bits, or less if take-bit returns nothong
(define (take-n-bits' bits n acc)
  (if (= n 0) (reverse acc)
      (let ((bit (take-bit bits)))
        (if (nothing? bit) (reverse acc)
            (take-n-bits' bits (- n 1) (cons (just bit) acc))))))

(define (take-n-bits bits n) (take-n-bits' bits n nil))

;;; Return the total number of bits left in the stream
(define (stream-len bits)
  (let (((BitStream bit-list hex-list) bits))
    (+ (length (! bit-list)) (* 4 (length (! hex-list))))))

;;; Creates a substream from a string of bits
(define (make-substream bits n)
  (BitStream (Ref (take-n-bits bits n)) (Ref nil)))

;;; Converts a string of hex digits to a stream
(define (string->stream hex)
  (BitStream (Ref nil) (Ref (string->list hex))))

(define (add-bit num b) (+ (* 2 num) b))

;;; Take n bits from the stream and convert them to a number
(define (take-n-bit-number bits n)
  (let ((bit-list (take-n-bits bits n)))
    (fold add-bit 0 bit-list)))

;;; Placeholder for packet since packet and packet-data are mutually recursive
(type packet Packet)
(type packet-data (LiteralValue int) (Operator (cons packet)))
(type packet (Packet int int packet-data))

;;; Define stub parse-packet and parse-packets since there are
;;; mutually-recursive calls
(define (parse-packet bits) (Packet 0 0 (LiteralValue 0)))
(define (parse-packets bits acc) nil)

(define (parse-literal-packet bits n)
  (let ((type-bit (just (take-bit bits)))
        (bit-num (take-n-bit-number bits 4)))
    (if (= type-bit 1)
        (parse-literal-packet bits (+ (* n 16) bit-num))
        (LiteralValue (+ (* n 16) bit-num)))))

(define (parse-operator-packet bits)
  (let ((length-type (just (take-bit bits))))
    (if (= length-type 0)
        (let ((sub-length (take-n-bit-number bits 15)))
          (Operator (parse-packets (make-substream bits sub-length) nil)))
        (let ((sub-count (take-n-bit-number bits 11)))
          (Operator
            (map (lambda (i) (parse-packet bits)) (range 1 sub-count)))))))

(define (parse-packet bits)
  (let ((version (take-n-bit-number bits 3))
        (packet-id (take-n-bit-number bits 3)))
    (if (= packet-id 4) (Packet version packet-id (parse-literal-packet bits 0))
        (Packet version packet-id (parse-operator-packet bits)))))

(define (parse-packets bits acc)
  (if (<= (stream-len bits) 6) (reverse acc)
      (parse-packets bits (cons (parse-packet bits) acc))))

(define (sum-versions pack)
  (let (((Packet ver type data) pack))
    (match data
      ((LiteralValue _) ver)
      ((Operator packets) (+ ver (fold (lambda (s p) (+ s (sum-versions p)))
                                0 packets))))))

(define (day16a)
  (let* ((bits (string->stream (head (read-lines "day16.txt"))))
         (packets (parse-packets bits nil)))
    (sum-versions (head packets))))

(define (operator-packets data)
  (match data
    ((LiteralValue _) (fail "not operator packet"))
    ((Operator packets) packets)))

(define (literal-value data)
  (match data
    ((LiteralValue val) val)
    ((Operator _) (fail "not literal packet"))))

(define (evaluate-packet pack)
  (let (((Packet ver type data) pack))
    (if (= type 4) (literal-value data)
        (let* ((packets (operator-packets data)))
          (match type
            (0 (fold + 0 (map evaluate-packet packets)))
            (1 (fold * 1 (map evaluate-packet packets)))
            (2 (list-min (map evaluate-packet packets)))
            (3 (list-max (map evaluate-packet packets)))
            (5 (if (> (evaluate-packet (head packets))
                      (evaluate-packet (head (tail packets)))) 1 0))
            (6 (if (< (evaluate-packet (head packets))
                      (evaluate-packet (head (tail packets)))) 1 0))
            (7 (if (= (evaluate-packet (head packets))
                      (evaluate-packet (head (tail packets)))) 1 0)))))))

(define (day16b)
  (let* ((bits (string->stream (head (read-lines "day16.txt"))))
         (packets (parse-packets bits nil)))
    (evaluate-packet (head packets))))
