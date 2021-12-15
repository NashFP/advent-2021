(load "mwlib.scm")

(define (digit->int ch)
  (- (char->int ch) 48))

(define (make-grid lines)
  (let ((width (length (string->list (head lines))))
        (height (length lines))
        (data (map digit->int 
                   (fold append nil
                         (map string->list lines)))))
    (Grid width height (list->array data))))

(define (make-path-grid g)
  (let (((Grid width height _) g))
    (Grid width height (make-array-with-function (* width height)
                                                 (lambda (i) (if (= i 0) (Ref (Just 0)) (Ref (Nothing))))))))

;;; Keep a sorted list of path locations yet to be processed
;;; This is not a very efficient data structure for this, but it works.
(define (add-to-queue path-queue elem acc)
  (let* (((Pair dist loc) elem))
    (if (empty? path-queue) (reverse (cons elem acc))
        (let (((Pair next-dist loc) (head path-queue)))
          (if (< dist next-dist) (append (reverse (cons elem acc)) path-queue)
              (add-to-queue (tail path-queue) elem (cons (head path-queue) acc)))))))

;;; Check to make sure a loc is actually in the grid, and if it hasn't been processed,
;;; add it to the processing queue
(define (add-loc-to-queue dist-grid path-grid curr-dist path-queue loc)
  (let* (((Pair x y) loc)
         (loc-dist (grid-fetch dist-grid x y)))
    (if (nothing? loc-dist) path-queue
        (let ((loc-path (just (grid-fetch path-grid x y))))
          (if (just? (! loc-path)) path-queue
            (let ((new-dist (+ curr-dist (just loc-dist))))
              (<- loc-path (Just new-dist))
              (add-to-queue path-queue (Pair new-dist loc) nil)))))))

(define (add-offset loc off)
  (let (((Pair x y) loc)
        ((Pair xoff yoff) off))
    (Pair (+ x xoff) (+ y yoff))))

(define (get-adjacents loc)
  (map (add-offset loc) (list (Pair -1 0) (Pair 1 0) (Pair 0 -1) (Pair 0 1))))

;;; Keep processing until we have computed the distance to the lower right corner
(define (get-shortest-path dist-grid path-grid path-queue)
  (let* (((Pair curr-dist curr-loc) (head path-queue))
         ((Grid width height _) dist-grid))
    (if (equals? curr-loc (Pair (- width 1) (- height 1))) curr-dist
        (get-shortest-path dist-grid path-grid
                           (fold (add-loc-to-queue dist-grid path-grid curr-dist)
                                 (tail path-queue)
                                 (get-adjacents curr-loc))))))

(define (day15a)
  (let* ((dist-grid (make-grid (read-lines "day15.txt")))
         (path-grid (make-path-grid dist-grid)))
    (get-shortest-path dist-grid path-grid (list (Pair 0 (Pair 0 0))))))

(define (offset-pos off p) (+ 1 (% (+ off (- p 1)) 9)))

(define (expand-horiz str)
  (let ((line (map digit->int (string->list str))))
    (append line (append (map (offset-pos 1) line)
                       (append (map (offset-pos 2) line)
                               (append (map (offset-pos 3) line)
                                       (map (offset-pos 4) line)))))))
(define (expand-vert amt line)
  (map (offset-pos amt) line))

(define (expand-lines lines)
  (let ((expanded-lines (map expand-horiz lines)))
    (append expanded-lines
            (append (map (expand-vert 1) expanded-lines)
                    (append (map (expand-vert 2) expanded-lines)
                            (append (map (expand-vert 3) expanded-lines)
                                    (map (expand-vert 4) expanded-lines)))))))

(define (day15b)
  (let* ((lines (expand-lines (read-lines "day15.txt")))
         (width (length (head lines)))
         (height (length lines))
         (data (fold append nil lines))
         (dist-grid (Grid width height (list->array data)))
         (path-grid (make-path-grid dist-grid)))
    (get-shortest-path dist-grid path-grid (list (Pair 0 (Pair 0 0))))))
