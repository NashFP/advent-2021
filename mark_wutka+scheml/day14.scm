(load "mwlib.scm")

(define (parse-element l)
  (just
    (regex-bind l "(..) -> (.)" (target insertion)
                (Pair target (head (string->list insertion))))))

(define (make-count-list element-dict old-counts acc)
  (if (empty? old-counts) acc
      (let* ((curr-count (head old-counts))
             (element (fst curr-count))
             (count (snd curr-count))
             (first-elem (string-at element 0))
             (second-elem (string-at element 1))
             (insertion (dict-lookup element-dict element)))
      (make-count-list element-dict (tail old-counts)
          (cons (Pair (list->string (list first-elem insertion)) (snd curr-count))
                (cons (Pair (list->string (list insertion second-elem)) 
                      (snd curr-count)) acc))))))

(define (create-counts-dict counts-list new-dict)
  (if (empty? counts-list) new-dict
      (let* ((curr-count (head counts-list)))
        (create-counts-dict (tail counts-list) 
                            (dict-put new-dict (fst curr-count) 
                                      (+ (dict-lookup-with-default new-dict (fst curr-count) 0)
                                         (snd curr-count)))))))

(define (make-initial-counts polymer acc)
  (if (< (length polymer) 2) (create-counts-dict acc (make-dict))
      (make-initial-counts (tail polymer)
                           (cons (Pair (list->string (take 2 polymer)) 1) acc))))

(define (do-insertion-round element-dict counts i)
  (create-counts-dict (make-count-list element-dict (dict->list counts) nil) (make-dict)))

(define (count-element elem count counts)
  (dict-put counts elem (+ count (dict-lookup-with-default counts elem 0))))

(define (count-pairs pair-counts counts)
  (if (empty? pair-counts) counts
      (let* ((pair-count (head pair-counts))
             (first-elem (string-at (fst pair-count) 0))
             (second-elem (string-at (fst pair-count) 1)))
        (count-pairs (tail pair-counts)
                     (count-element first-elem (snd pair-count)
                       (count-element second-elem (snd pair-count) counts))))))

(define (adjust-count count-pair)
  (let (((Pair elem count) count-pair))
    (Pair elem (+ (/ count 2) (% count 2)))))

(define (min-count counts)
  (let ((count-list (dict->list counts)))
    (fold (lambda (c best)
           (if (< (snd c) (snd best)) c best))
          (head count-list) (tail count-list))))
        
(define (max-count counts)
  (let ((count-list (dict->list counts)))
    (fold (lambda (c best)
           (if (> (snd c) (snd best)) c best))
          (head count-list) (tail count-list))))

(define (day14a)
  (let* ((groups (split-groups (read-lines "day14.txt")))
         (polymer (string->list (head (head groups))))
         (element-dict (list->dict (map parse-element (head (tail groups)))))
         (initial-counts (make-initial-counts polymer nil))
         (pair-counts (fold (do-insertion-round element-dict) initial-counts (range 1 10)))
         (counts (dict-map adjust-count (count-pairs (dict->list pair-counts) (make-dict)))))
    (- (snd (max-count counts)) (snd (min-count counts)))))

(define (day14b)
  (let* ((groups (split-groups (read-lines "day14.txt")))
         (polymer (string->list (head (head groups))))
         (element-dict (list->dict (map parse-element (head (tail groups)))))
         (initial-counts (make-initial-counts polymer nil))
         (pair-counts (fold (do-insertion-round element-dict) initial-counts (range 1 40)))
         (counts (dict-map adjust-count (count-pairs (dict->list pair-counts) (make-dict)))))
    (- (snd (max-count counts)) (snd (min-count counts)))))
