;;; I had two bugs in my code for intersecting diagonal lines
;;; and couldn't figure out why there were more points than
;;; there should have been. I wrote this version that just
;;; draws the lines into a grid, and got the right answer
;;; with it, and then used the resulting grid to diagnose
;;; the bugs in the line intersection code.

;;; Grid has min-x min-y xsize grid-data
(type grid (Grid int int int (array (ref int))))

(define (parse-line line)
  (just 
    (regex-bind line
                "([0-9]*),([0-9]*) -> ([0-9]*),([0-9]*)"
                ((string->int x1) (string->int y1)
                 (string->int x2) (string->int y2))
                (Pair (Pair x1 y1) (Pair x2 y2)))))

;;; Compute x and y size, then create the grid with an array of refs
(define (init-grid min-x max-x min-y max-y)
  (let ((xsize (+ 1 (- max-x min-x)))
        (ysize (+ 1 (- max-y min-y))))
    (Grid min-x min-y xsize (make-array-with-function (* xsize ysize)
                                                (lambda (n) (Ref 0))))))

;;; Turn a list of line segments into a list of points
(define (point-list lines acc)
  (if (empty? lines) acc
    (let ((p1 (fst (head lines)))
          (p2 (snd (head lines))))
      (point-list (tail lines) (cons p1 (cons p2 acc))))))

;;; Compute the min value in a list
(define (list-min l)
  (fold min (head l) (tail l)))

;;; Compute the max value in a list
(define (list-max l)
  (fold max (head l) (tail l)))

;;; Get lists of x and y points, compute min and max for each
;;; and initialize the grid
(define (init-grid-from-lines lines)
  (let* ((points (point-list lines nil))
         (xlist (map fst points))
         (ylist (map snd points)))
     (init-grid (list-min xlist) (list-max xlist)
                (list-min ylist) (list-max ylist))))
    
;;; Compute how much to increment each time to go from v1 to v2
(define (get-inc v1 v2)
  (if (= v1 v2) 0
      (if (< v1 v2) 1 -1)))

;;; Store an x y point in the grid, incrementing the grid's count at that point
(define (record-point x y grid)
  (let* (((Grid min-x min-y xsize grid-data) grid)
         (offset (+ (- x min-x) (* xsize (- y min-y))))
         (curr-count (@ grid-data offset)))
    (<- curr-count (+ (! curr-count) 1))
    (Grid min-x min-y xsize grid-data)))
    
;;; Record each point in a line
(define (record-line-points point stop-point xinc yinc grid)
  (if (equals? point stop-point)
      (record-point (fst stop-point) (snd stop-point) grid)
      (let (((Pair x1 y1) point))
        (record-line-points (Pair (+ x1 xinc) (+ y1 yinc))
                            stop-point
                            xinc yinc
                            (record-point x1 y1 grid)))))
        
;;; Record a line
(define (record-line grid line)
  (let (((Pair p1 p2) line))
    (record-line-points p1 p2 (get-inc (fst p1) (fst p2))
                              (get-inc (snd p1) (snd p2))
                              grid)))

(define (horizontal? line)
  (let (((Pair x1 y1) (fst line))
        ((Pair x2 y2) (snd line)))
    (= y1 y2)))

(define (vertical? line)
  (let (((Pair x1 y1) (fst line))
        ((Pair x2 y2) (snd line)))
    (= x1 x2)))

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

(define (day5a)
  (let* ((lines (map normalize (filter not-diagonal?
                         (map parse-line (read-lines "day5.txt")))))
         (grid (init-grid-from-lines lines))
         ((Grid _ _ _ grid-data) (fold record-line grid lines)))
    ;;; Count all the points that have a count > 1
    (length (filter (lambda (r) (> (! r) 1)) (array->list grid-data)))))
         

(define (day5b)
  (let* ((lines (map normalize (map parse-line (read-lines "day5.txt"))))
         (grid (init-grid-from-lines lines))
         ((Grid _ _ _ grid-data) (fold record-line grid lines)))
    ;;; Count all the points that have a count > 1
    (length (filter (lambda (r) (> (! r) 1)) (array->list grid-data)))))
         

