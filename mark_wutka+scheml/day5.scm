(define (parse-line line)
  (just 
    (regex-bind line
                "([0-9]*),([0-9]*) -> ([0-9]*),([0-9]*)"
                ((string->int x1) (string->int y1)
                 (string->int x2) (string->int y2))
                (Pair (Pair x1 y1) (Pair x2 y2)))))

(define (horizontal? line)
  (let (((Pair x1 y1) (fst line))
        ((Pair x2 y2) (snd line)))
    (= y1 y2)))

(define (vertical? line)
  (let (((Pair x1 y1) (fst line))
        ((Pair x2 y2) (snd line)))
    (= x1 x2)))

;;; For a vertical line, normalizing means making the lowest y come first
;;; otherwise for horizontal and diagonal, make lowest x come first
(define (normalize line)
  (let (((Pair x1 y1) (fst line))
        ((Pair x2 y2) (snd line)))
    (if (vertical? line)
        (if (<= y1 y2) line
            (Pair (Pair x2 y2) (Pair x1 y1)))
        (if (<= x1 x2) line
            (Pair (Pair x2 y2) (Pair x1 y1))))))

(define (not-diagonal? line)
  (or (horizontal? line) (vertical? line)))

(define (diagonal? line)
  (not (not-diagonal? line)))

;;; Compute the slope of a line, just return 0 for a vertical line
;;; since we don't use the slope with those
(define (slope line)
  (let (((Pair x1 y1) (fst line))
        ((Pair x2 y2) (snd line)))
    (if (vertical? line) 0
       (if (= y1 y2) 0
           (if (< y1 y2) 1 -1)))))

;;; Do lines intersect at a single point?
;;; When it was just horizontal and vertical lines the function
;;; name was correct, when a diagonal intersects a horizontal
;;; or vertical line they aren't orthogonal, but I'm leaving the
;;; function name anyway
(define (orthogonal? line1 line2)
  (if (horizontal? line1) (vertical? line2)
      (if (vertical? line1) (horizontal? line2)
        (if (diagonal? line1)
            (if (not-diagonal? line2) #t
                (!= (slope line1) (slope line2)))
            #f))))

;;; Compute the y in a line at the given x
(define (y-for-x line x)
  (let ((line-slope (slope line))
        ((Pair x1 y1) (fst line)))
    (+ y1 (* (- x x1) line-slope))))

;;; Compute the y in a line for x=0
(define (y0 line)
  (let (((Pair x11 y11) (fst line)))
      (- y11 (* (slope line) x11))))

;;; diagonal-intersection requires the first line
;;; to be diagonal
(define (diagonal-intersection line1 line2)
  (let (((Pair x11 y11) (fst line1))
        ((Pair x12 y12) (snd line1))
        ((Pair x21 y21) (fst line2))
        ((Pair x22 y22) (snd line2)))
    (if (vertical? line2)
        ;;; make sure line2's x falls within line1's range
        (if (and (>= x21 x11) (<= x21 x12))
            ;;; Find line1's y when it intersects line2
            (let ((firsty (y-for-x line1 x21)))
              ;;; Make sure the y is in line2's range
                (if (and (>= firsty y21) (<= firsty y22))
                    (->list (Pair x21 firsty))
                    nil))
            nil)
        (let* ((slope1 (slope line1))
               (slope2 (slope line2))
               (y10 (y0 line1))
               (y20 (y0 line2))
               ;;; Compute x at intersection using slope and y-intercept
               (xsect (/ (- y20 y10) (- slope1 slope2)))
               ;;; some diagonals can intersect in-between points,
               ;;; use intersect-at-point to see if that happens
               (intersect-at-point (= 0 (% (- y20 y10) (- slope1 slope2)))))
          (if (and intersect-at-point
                   ;;; make sure xsect is in x range of each line
                   (and (and (>= xsect x11) (<= xsect x12))
                        (and (>= xsect x21) (<= xsect x22))))
              (->list (Pair xsect (+ y10 (* slope1 xsect))))
              nil)))))

;;; Find intersection of orthogonal lines
(define (ortho-intersection line1 line2)
  (let (((Pair x11 y11) (fst line1))
        ((Pair x12 y12) (snd line1))
        ((Pair x21 y21) (fst line2))
        ((Pair x22 y22) (snd line2)))
    (if (horizontal? line1)
        (if (and (and (>= x21 x11) (<= x21 x12))
                 (and (>= y11 y21) (<= y11 y22)))
            (->list (Pair x21 y11))
            nil)
        (if (vertical? line1)
            (if (and (and (>= y21 y11) (<= y21 y12))
                     (and (>= x11 x21) (<= x11 x22)))
                (->list (Pair x11 y21))
                nil)
            ;;; If at least one line is diagonal, use the alternate algorithm
            (diagonal-intersection line1 line2)))))

(define (colinear? line1 line2)
  (let (((Pair x11 y11) (fst line1))
        ((Pair x12 y12) (snd line1))
        ((Pair x21 y21) (fst line2))
        ((Pair x22 y22) (snd line2)))
    (if (and (horizontal? line1) (horizontal? line2))
        (= y11 y21)
        (if (and (vertical? line1) (vertical? line2))
            (= x11 x21)
            (if (and (diagonal? line1) (diagonal? line2))
                ;;; Diagonal lines are colinear if they have the same
                ;;; slope and y intercept
                (and (= (slope line1) (slope line2))
                     (= (y0 line1) (y0 line2)))
                #f)))))

(define (overlap-range p11 p12 p21 p22)
  (let ((left (max p11 p21))
        (right (min p12 p22)))
    (range left right)))

(define (overlap line1 line2)
  (let (((Pair x11 y11) (fst line1))
        ((Pair x12 y12) (snd line1))
        ((Pair x21 y21) (fst line2))
        ((Pair x22 y22) (snd line2)))
    (if (horizontal? line1)
        (map (lambda (x) (Pair x y11)) (overlap-range x11 x12 x21 x22))
        (if (vertical? line1)
            (map (lambda (y) (Pair x11 y)) (overlap-range y11 y12 y21 y22))
            (map (lambda (x) (Pair x (y-for-x line1 x))) (overlap-range x11 x12 x21 x22))))))

(define (get-overlaps line1 line2)
  (if (orthogonal? line1 line2)
      (ortho-intersection line1 line2)
      (if (colinear? line1 line2)
          (overlap line1 line2)
          nil)))

(define (add-overlaps point-dict line1 line2)
  (let ((overlaps (get-overlaps line1 line2)))
    (map (lambda (p) (dict-put point-dict p #t)) overlaps)
    point-dict))

(define (do-overlaps point-dict lines)
  (if (empty? lines) point-dict
      (progn
        (map (add-overlaps point-dict (head lines)) (tail lines))
        (do-overlaps point-dict (tail lines)))))

(define (day5a)
  (let* ((lines (map normalize (filter not-diagonal?
                         (map parse-line (read-lines "day5.txt")))))
         (point-dict (do-overlaps (make-dict) lines)))
    (length (dict->list point-dict))))


(define (day5b)
  (let* ((file-lines (map parse-line (read-lines "day5.txt")))
         (non-diagonal-lines (map normalize (filter not-diagonal? file-lines)))
         (diagonal-lines (map normalize (filter diagonal? file-lines)))
         (lines (append diagonal-lines non-diagonal-lines))
         (point-dict (do-overlaps (make-dict) lines)))
    (length (dict->list point-dict))))
