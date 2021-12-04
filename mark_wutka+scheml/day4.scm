(load "mwlib.scm")

(define (parse-board board)
  (list->array 
    (map string->int
         ;;; have to trim the string because some boards
         ;;; begin with a blank
         (split (string-trim
                  (string-replace
                    (join board " ") " +" " "))
                    " "))))

;;; Return values at the specified board row
(define (board-row board row)
  (let ((row-start (* row 5)))
    (map (lambda (i) (@ board (+ row-start i))) (range 0 4))))

;;; Return values at the specified board col
(define (board-col board col)
  (map (lambda (i) (@ board (+ (* i 5) col))) (range 0 4)))

;;; Returns true if a row contains a bingo
(define (row-bingo board row)
  (all (= -1) (board-row board row)))

;;; Returns true if a col contains a bingo
(define (col-bingo board col)
  (all (= -1) (board-col board col)))

;;; Returns try if a board has a winning row or column
(define (board-wins board)
  (or (some (col-bingo board) (range 0 4))
      (some (row-bingo board) (range 0 4))))

;;; Call a number on a board (replace the called
;;; number with -1 if it occurs
(define (call-number number board)
  (array-map (lambda (n) (if (= n number) -1 n)) board))

;;; Look for a winning board, if there is one, return 
;;; an option containing the pair of the winning number
;;; and the board
(define (find-win number boards)
  (if (empty? boards) (Nothing)
      (if (board-wins (head boards)) (Just (Pair number (head boards)))
          (find-win number (tail boards)))))

;;; Keep calling numbers until there is a win
(define (call-until-win boards numbers)
  (if (empty? numbers) (Nothing)
      ;;; Call the number on all the boards
      (let* ((called-boards (map (call-number (head numbers)) boards))
             ;;; look for a win
             (maybe-win (find-win (head numbers) called-boards)))
        ;;; If there's a win, return it, otherwise call the next number
        (if (just? maybe-win) maybe-win
            (call-until-win called-boards (tail numbers))))))

(define (score-board number-and-board)
  (let (((Pair number board) number-and-board))
    ;;; multiply the winning number...
    (* number 
       ;;; by the sum of the non-called numbers in the board
       (array-fold (lambda (n s) (if (>= n 0) (+ s n) s)) 0 board))))

(define (day4a)
  (let* ((groups (split-groups (read-lines "day4.txt")))
         (numbers (map string->int (split (head (head groups)) ",")))
         (boards (map parse-board (tail groups)))
         (maybe-win (call-until-win boards numbers)))
    (if (just? maybe-win)
            (score-board (just maybe-win))
            -1)))

;;; Remove any winning boards from the list
(define (remove-winning-boards boards)
  (filter (lambda (b) (not (board-wins b))) boards))

;;; Find the last winning board in a list of boards
(define (last-winning-board number boards last-win)
  (if (empty? boards) last-win
      (if (board-wins (head boards))
          (last-winning-board number (tail boards) (Just (Pair number (head boards))))
          (last-winning-board number (tail boards) last-win))))

;;; Keep calling numbers until there are either no
;;; more numbers or no more boards
(define (call-until-last-win boards numbers last-win)
  (if (empty? numbers) last-win
      (if (empty? boards) last-win
        ;;; call the next number
        (let* ((called-boards (map (call-number (head numbers)) boards)))
          ;;; remove any winning boards, and call again, updating the
          ;;; value of the last winning board
            (call-until-last-win (remove-winning-boards called-boards) (tail numbers) 
                                 (last-winning-board (head numbers) called-boards last-win))))))

(define (day4b)
  (let* ((groups (split-groups (read-lines "day4.txt")))
         (numbers (map string->int (split (head (head groups)) ",")))
         (boards (map parse-board (tail groups)))
         (maybe-win (call-until-last-win boards numbers (Nothing))))
    (if (just? maybe-win)
            (score-board (just maybe-win))
            -1)))
