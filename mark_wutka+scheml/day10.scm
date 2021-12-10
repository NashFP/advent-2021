
(define (opener? ch)
  (or (equals? ch #\()
      (or (equals? ch #\[)
          (or (equals? ch #\<)
              (equals? ch #\{)))))

(define (closer ch)
  (if (equals? ch #\() #\)
      (if (equals? ch #\[) #\]
          (if (equals? ch #\<) #\>
              #\}))))

(define (score-illegal ch)
  (if (equals? ch #\)) 3
      (if (equals? ch #\]) 57
          (if (equals? ch #\}) 1197
              25137))))

(define (find-first-illegal closer-stack chars)
  (if (empty? chars) (Nothing)
      (let ((ch (head chars)))
        (if (opener? ch)
            (find-first-illegal (cons (closer ch) closer-stack) (tail chars))
            (if (empty? closer-stack) (Just ch)
                (if (equals? ch (head closer-stack))
                    (find-first-illegal (tail closer-stack) (tail chars))
                    (Just ch)))))))

(define (day10a)
  (let* ((lines (read-lines "day10.txt"))
         (illegals (map-optional (find-first-illegal nil) (map string->list lines))))
    (fold + 0 (map score-illegal illegals))))

(define (build-completion-stack closer-stack chars)
  (if (empty? chars) closer-stack
      (let ((ch (head chars)))
        (if (opener? ch)
            (build-completion-stack (cons (closer ch) closer-stack) (tail chars))
            (build-completion-stack (tail closer-stack) (tail chars))))))

(define (score-completion ch)
  (if (equals? ch #\)) 1
      (if (equals? ch #\]) 2
          (if (equals? ch #\}) 3
              4))))

(define (calc-completion-score completions)
  (fold (lambda (s c) (+ (* 5 s) (score-completion c))) 0 completions))

(define (int-compare a b)
  (if (< a b) -1
      (if (> a b) 1
          0)))

(define (day10b)
  (let* ((lines (map string->list (read-lines "day10.txt")))
         (completions (map (build-completion-stack nil)
                           (filter (lambda (l) (nothing? (find-first-illegal nil l))) lines)))
         (completion-scores (sort int-compare (map calc-completion-score completions))))
    (nth (/ (length completion-scores) 2) completion-scores)))
