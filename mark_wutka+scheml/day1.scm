(load "mwlib.scm")

(define (day1a)
  (let ((nums (map string->int (read-lines "day1a.txt"))))
    (length (filter id (zip-with < nums (tail nums))))))

(define (sliding-window v1 v2 v3)
  (+ v1 (+ v2 v3)))

(define (day1b)
  (let* ((nums (map string->int (read-lines "day1a.txt")))
         (windows (zip-with3 sliding-window nums (tail nums) (tail (tail nums)))))
    (length (filter id (zip-with < windows (tail windows))))))


