(define (parse-line l)
  (let ((parts (split l "-")))
    (Pair (head parts)
          (head (tail parts)))))

(define (add-connection from to cave-map)
  (if (has-key? cave-map from)
      ;;; If there is already an entry for from, add to to the list
      (dict-put cave-map from (cons to (dict-lookup cave-map from)))
      ;;; Otherwise create a new list consisting of to
      (dict-put cave-map from (list to))))

(define (add-cave-pair cave-map p)
  (let (((Pair a b) p))
    ;;; Add each direction for a pair
    (add-connection b a
        (add-connection a b cave-map))))

(define (small? cave)
  (char-lowercase? (string-at cave 0)))

(define (count-visited cave-map visited from)
  ;;; Getting to the end means 1 good path
  (if (equals? from "end") 1
    (if (small? from)
        (if (set-contains? visited from) 0
            ;;; For a small cave, add it to the set of visited caves
            ;;; before looking for paths from this cave to the end
            (fold + 0 (map (count-visited cave-map (set-put visited from))
                       (dict-lookup cave-map from))))
        ;;; otherwise, for large caves, don't need to update the visited map
        (fold + 0 (map (count-visited cave-map visited) (dict-lookup cave-map from))))))

(define (day12a)
  (let* ((pairs (map parse-line (read-lines "day12.txt")))
         (cave-map (fold add-cave-pair (make-dict) pairs)))
    (count-visited cave-map (make-set) "start")))

(define (count-visited2 cave-map visited visit-twice from)
  ;;; Getting to the end means 1 good path
  (if (equals? from "end") 1
      (if (small? from)
          (if (set-contains? visited from)
            ;;; If this cave has been visited, and it isn't the start, and no cave
            ;;; has been visited twice, allow this one to be visited twice
            (if (and (not (equals? from "start")) (nothing? visit-twice))
              (fold + 0 (map (count-visited2 cave-map visited (Just from))
                             (dict-lookup cave-map from)))
              ;;; otherwise we are at a dead end
              0)
            ;;; If this small cave hasn't been visited, add it to the visited set
            ;;; and find all valid paths from it
              (fold + 0 (map (count-visited2 cave-map (set-put visited from) visit-twice)
                           (dict-lookup cave-map from))))
        ;;; otherwise, for large caves, don't need to update the visited map
          (fold + 0 (map (count-visited2 cave-map visited visit-twice) (dict-lookup cave-map from))))))

(define (day12b)
  (let* ((pairs (map parse-line (read-lines "day12.txt")))
         (cave-map (fold add-cave-pair (make-dict) pairs)))
    (count-visited2 cave-map (make-set) (Nothing) "start")))
