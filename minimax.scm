(define (Connect-4)
  
  (define userPiece 'X)
  (define compPiece 'O)
  (define original '((e e e e e e)
                     (e e e e e e)
                     (e e e e e e)
                     (e e e e e e)
                     (e e e e e e)
                     (e e e e e e)
                     (e e e e e e)))
  
  (define state-cols original)
  (define state-rows 'Initial)
  (define state-diags 'Initial)
  
  (define get-col
    (lambda (list-lats)
      (cond
        ((null? list-lats) '())
        (else
         (cons (caar list-lats)
               (get-col (cdr list-lats)))))))
  
  (define cut-col
    (lambda (list-lats)
      (cond
        ((null? list-lats) '())
        ((null? (car list-lats)) '())
        (else
         (cons (cdar list-lats)
               (cut-col (cdr list-lats)))))))
  
  (define transpose
    (lambda (list-lats)
      (cond
        ((null? (car list-lats)) '())
        (else
         (cons (get-col list-lats)
               (transpose (cut-col list-lats)))))))
  
  (define drop
    (lambda (col player-piece)
      (cond
        ((or (> col 7) (< col 1)) (begin
                                    (display "Please specify a column number between 1 and 7!")(newline)
                                    #f))
        (else
         (let ((column (list-ref state-cols (sub1 col))))
           (cond
             ((eq? (car column) 'e) (begin
                                      (set! state-cols (set-col col (update column player-piece) state-cols))
                                      (set! state-rows (transpose state-cols))
                                      (set! state-diags (diagonalize state-rows))))
             (else
              (begin
                (display "Column is full. Specify another column.")(newline)
                #f))))))))
  
  (define hypdrop
    (lambda (col player-piece)
      (let ((column (list-ref state-cols (sub1 col))))
        (cond
          ((eq? (car column) 'e) (begin
                                   (set! state-cols (set-col col (update column player-piece) state-cols))
                                   (set! state-rows (transpose state-cols))
                                   (set! state-diags (diagonalize state-rows))))
          (else #f)))))
  
  (define update
    (lambda (col piece)
      (cond
        ((null? (cdr col)) (cons piece (cdr col)))
        ((not (eq? (cadr col) 'e)) (cons piece (cdr col)))
        (else 
         (cons (car col) (update (cdr col) piece))))))
  
  (define set-col
    (lambda (n col state-cols)
      (cond
        ((eq? n 1) (cons col (cdr state-cols)))
        (else
         (cons (car state-cols) (set-col (sub1 n) col (cdr state-cols)))))))
  
  (define (flip rows)
    (cond
      ((null? rows) ())
      (else
       (cons (reverse (car rows)) (flip (cdr rows))))
      ))
  
  (define (diagonalize rows)
    (append (forward-diag rows) (forward-diag (flip rows)))) ; as the backward diagonals are just the forward diagonals of left-right reversed board
  
  (define (forward-diag state-row)
    (let ((R1 (list-ref state-row 0))
          (R2 (list-ref state-row 1))
          (R3 (list-ref state-row 2))
          (R4 (list-ref state-row 3))
          (R5 (list-ref state-row 4))
          (R6 (list-ref state-row 5)))
      
      ;Starting at Column 1 Row 3 and working upward
      (cons
       ;1st diagonal
       (cons (list-ref R3 0) (cons (list-ref R4 1) (cons (list-ref R5 2) (cons (list-ref R6 3) ()))))
       (cons
        ;2nd diagonal
        (cons (list-ref R2 0) (cons (list-ref R3 1) (cons (list-ref R4 2) (cons (list-ref R5 3) (cons (list-ref R6 4) ())))))
        (cons
         ;3rd diagonal
         (cons (list-ref R1 0) (cons (list-ref R2 1) (cons (list-ref R3 2) (cons (list-ref R4 3) (cons (list-ref R5 4) (cons (list-ref R6 5) ()))))))
         (cons
          ;4th diagonal
          (cons (list-ref R1 1) (cons (list-ref R2 2) (cons (list-ref R3 3) (cons (list-ref R4 4) (cons (list-ref R5 5) (cons (list-ref R6 6) ()))))))
          (cons
           ;5th diagonal
           (cons (list-ref R1 2) (cons (list-ref R2 3) (cons (list-ref R3 4) (cons (list-ref R4 5) (cons (list-ref R5 6) ())))))
           (cons
            ;6th diagonal
            (cons (list-ref R1 3) (cons (list-ref R2 4) (cons (list-ref R3 5) (cons (list-ref R4 6) ()))))
            ()))))))
      ))
  
  (define draw
    (lambda (game-state)
      (cond
        ((null? game-state) (newline))
        (else
         (begin
           (display (car game-state))
           (newline)
           (draw (cdr game-state)))))))
  
  (define (pDraw game-state)
    (begin
      (newline) 
      (draw game-state)))
  
  (define (check-won state game-piece)
    (cond
      ((null? state) #f)
      ((< (length (car state)) 4) (check-won (cdr state) game-piece))
      (else
       (cond
         ((and (eq? (list-ref (car state) 0) game-piece)
               (eq? (list-ref (car state) 1) game-piece)
               (eq? (list-ref (car state) 2) game-piece)
               (eq? (list-ref (car state) 3) game-piece)) #t)
         (else
          (check-won (cons (cdar state) (cdr state)) game-piece))))))
  
  ; won? determines if the calling player has won the game. If
  ; yes, then the predicate will return #t, otherwise, it will
  ; return #f.
  (define (won? game-piece)
    (or (check-won state-rows game-piece)
        (check-won state-cols game-piece)
        (check-won state-diags game-piece)))
  
  ; findWinningMove returns the column that will allow the specified 
  ; player to win the game IN THE NEXT MOVE. If such a move does
  ; NOT exist, then #f will be returned.
  (define (findWinningMove player-piece)
    (define temp-cols 'Initial)
    (define temp-rows 'Initial)
    (define temp-diags 'Initial)
    
    ; tryColumns returns the column number of the move that will
    ; allow the calling player to win; otherwise, the procedure
    ; will return #f.
    (define (tryColumns col)
      (cond
        ((> col 7) #f)
        (else
         (begin
           (hypdrop col player-piece)
           (cond
             ((won? player-piece) (begin
                                    (set! state-cols temp-cols)
                                    (set! state-rows temp-rows)
                                    (set! state-diags temp-diags)
                                    col))
             (else (begin
                     (set! state-cols temp-cols)
                     (set! state-rows temp-rows)
                     (set! state-diags temp-diags)
                     (tryColumns (add1 col)))))))))
    
    (begin
      (set! temp-cols state-cols)
      (set! temp-rows state-rows)
      (set! temp-diags state-diags)
      (tryColumns 1)
      ))
  
  (define (full? col)
    (cond
      ((eq? (car (list-ref state-cols (sub1 col))) 'e) #f)
      (else #t)))
  
  (define (firstOpenColumn)
    (call/cc
     (lambda (return)
       (define (innerFOC col)
         (cond
           ((not (full? col)) (return col))
           (else
            (innerFOC (add1 col)))))
       (innerFOC 1))))
  
  (define (compMove)
    (let ((compWin (findWinningMove compPiece)))
      (cond
        ((number? compWin) (drop compWin compPiece))
        (else
         (let ((userWin (findWinningMove userPiece))) ; To try to prevent from winning.
           (cond
             ((number? userWin) (drop userWin compPiece))
             (else
              (let ((result (minimax)))
                 (if (number? result)
                     (drop result compPiece)
                     (drop (firstOpenColumn) compPiece))))))))))  
  
  (define (minimax)
    (define temp-cols 'Initial)
    (define temp-rows 'Initial)
    (define temp-diags 'Initial)
    (define computedspace 'Initial)
    
    (define atom?
      (lambda (x)
        (and (not (pair? x)) (not (null? x)))
        ))
    
    (define (enumerate-interval low high)
      (if (> low high)
          '()
          (cons low (enumerate-interval (+ low 1) high))))
    
    (define (quadruples n)
      (map (lambda (i)
             (map (lambda (j)
                    (map (lambda (k)
                           (map (lambda (l) (list i j k l)) 
                                (enumerate-interval 1 n)))
                         (enumerate-interval 1 n)))
                  (enumerate-interval 1 n)))
           (enumerate-interval 1 n))
      )
    
    (define (dropandevaluate seq)
      (call/cc
       (lambda (skip)
         (cond
           ((null? seq) (evaluate state-cols state-rows state-diags compPiece))
           (else
            (if (full? (car seq))
                (skip 'NaN)
                (if (or (won? userPiece) (won? compPiece))
                    (skip 'NaN)
                    (begin
                      (if (even?(length seq)) ;Then it is computer's move
                          (drop (car seq) compPiece)
                          (drop (car seq) userPiece))
                      (dropandevaluate (cdr seq))))))
           ))))
    
    (define (filterNaN args)
      (cond
        ((null? args) ())
        ((eq? (caar args) 'NaN) (filterNaN (cdr args)))
        (else
         (cons (car args) (filterNaN (cdr args))))))
    
    (define mod 
      (lambda (pred)
        (lambda (one two three four five six seven)        
          (let* ((args (list one two three four five six seven))
                 (filteredargs (filterNaN args))             ;Need to remove NaN. If all are NaNs, then need to return meaningful answer
                 (cargs (map car filteredargs)))
            
            (begin
              (if (null? filteredargs)
                  '(NaN (N))                           ;If filteredargs is null, will return NaN, which should get removed next filter
                  (assoc (apply pred cargs) filteredargs))))  ;else return first element in filteredargs whose car is result of predicate app.
          )))
    
    (define (minimaxhelp configspace)
      (cond
        ((null? configspace) ())
        ((number? (car configspace)) 
         (let ((eval (dropandevaluate configspace)))
           (begin
             (set! state-cols temp-cols)
             (set! state-rows temp-rows)
             (set! state-diags temp-diags)
             (list eval configspace))))
        (else
         (cons (minimaxhelp (car configspace)) (minimaxhelp (cdr configspace))))))
    
    (define (minimax4 filter otherfilter computedspace)
      (cond
        ((atom? (car computedspace)) computedspace) ;then computedspace is a list of with a score and a sequence
        (else
         ((mod filter) (minimax4 otherfilter filter (list-ref computedspace 0))
                       (minimax4 otherfilter filter (list-ref computedspace 1))
                       (minimax4 otherfilter filter (list-ref computedspace 2))
                       (minimax4 otherfilter filter (list-ref computedspace 3))
                       (minimax4 otherfilter filter (list-ref computedspace 4))
                       (minimax4 otherfilter filter (list-ref computedspace 5))
                       (minimax4 otherfilter filter (list-ref computedspace 6))))
        ))
    
    (begin
      (set! temp-cols state-cols)
      (set! temp-rows state-rows)
      (set! temp-diags state-diags)
      (set! computedspace (minimaxhelp (quadruples 7)))
      (let ((result (minimax4 max min computedspace)))
        (caadr result)))             ; result is of form (score (sequence of 4 moves)). Pick the car of that sequence
    )
  
  (define evaluate
    (lambda (cols rows diags piece)
      
      (define (otherpiece piece)
        (if (eq? piece userPiece) 
            compPiece
            userPiece))
      
      (define enemypiece (otherpiece piece))
      
      (define (findpiece rest)
        (if (null? rest)
            'N
            (car rest)))
      
      
      (define (countones state left)
        (cond
          ((null? state) 0)
          ((null? (car state)) (countones (cdr state) 'N)) ;start on the rest with (N)ull left
          (else
           (let* ((rest (list-tail (car state) 1))
                  (right (findpiece rest)))
             
             (if (eq? (caar state) piece) 
                 
                 (cond
                   ((and (eq? left 'e) (eq? right 'e)) (+ 3 (countones (cons rest (cdr state)) piece))) ; highest score. piece is left
                   ((or (eq? left 'e) (eq? right 'e)) (+ 2 (countones (cons rest (cdr state)) piece)))  ; one side has e, second highest
                   (else (+ 1 (countones (cons rest (cdr state)) piece))))                              
                 ; left or right isn't e, lowest score
                 ; doesn't matter if it was left was piece because
                 ; this counts 1's in a row as if game was Connect 1
                 
                 (countones (cons rest (cdr state)) (caar state)))           ; no score increase at all. left is whatever (caar state) is
             ))
          ))
      
      (define (counttwos state left)
        (cond
          ((null? state) 0)
          ((< (length (car state)) 2) (counttwos (cdr state) 'N))
          (else
           (let* ((rest (list-tail (car state) 2))
                  (right (findpiece rest)))
             
             (if (and (eq? (list-ref (car state) 0) piece)
                      (eq? (list-ref (car state) 1) piece))
                 
                 (cond
                   ((and (eq? left 'e) (eq? right 'e)) (+ 9 (counttwos (cons rest (cdr state)) piece))) 
                   ((or (eq? left 'e) (eq? right 'e)) (+ 3 (counttwos (cons rest (cdr state)) piece)))
                   (else (+ 1 (counttwos (cons rest (cdr state)) piece))))
                 
                 (counttwos (cons (list-tail (car state) 1) (cdr state)) (caar state)))
             ))
          ))
      (define (countthrees state left)
        (cond
          ((null? state) 0)
          ((< (length (car state)) 3) (countthrees (cdr state) 'N))
          (else
           (let* ((rest (list-tail (car state) 3))
                  (right (findpiece rest)))
             
             (if (and (eq? (list-ref (car state) 0) piece)
                      (eq? (list-ref (car state) 1) piece)
                      (eq? (list-ref (car state) 2) piece))
                 
                 (cond
                   ((and (eq? left 'e) (eq? right 'e)) (+ 27 (countthrees (cons rest (cdr state)) piece))) 
                   ((or (eq? left 'e) (eq? right 'e)) (+ 9 (countthrees (cons rest (cdr state)) piece)))
                   (else (+ 1 (countthrees (cons rest (cdr state)) piece))))
                 
                 (countthrees (cons (list-tail (car state) 1) (cdr state)) (caar state)))
             ))
          ))
      
      (cond
        ((or (check-won rows piece)
             (check-won cols piece)
             (check-won diags piece)) ' 100000) ; Largest number returned by evaluate
        
        ((or (check-won rows enemypiece)
             (check-won rows enemypiece)
             (check-won rows enemypiece)) '0) 
        ; Smallest number returned by evaluate
        ; Don't apply evaluate to states where you have no piece
        ; in rows or columns or diags, as real evaluation will return
        ; will also return 0, causing ambiguity
        
        (else
         (let* ((R1 (* 3 (countones rows 'N)))
                (C1 (* 3 (countones cols 'N )))
                (D1 (* 3 (countones diags 'N)))
                (R2 (* 9 (counttwos rows 'N)))
                (C2 (* 9 (counttwos cols 'N)))
                (D2 (* 9 (counttwos diags 'N)))
                (R3 (* 27 (countthrees rows 'N)))
                (C3 (* 27 (countthrees cols 'N)))
                (D3 (* 27 (countthrees diags 'N)))
                (A1 (/ (+ R1 C1 D1) 3))
                (A2 (/ (+ R2 C2 D2) 3))
                (A3 (/ (+ R3 C3 D3) 3)))
           (+ A1 A2 A3))) ; Return the sum of the averages 
        )))
  
  (define (dispatch m)
    (cond
      ((number? m) 
       (if (not (eq? (drop m userPiece) #f)) 
           (cond
             ((won? userPiece) (begin
                                 (pDraw state-rows)
                                 (display "Congratulations! You won!")
                                 (set! state-cols original)))
             
             ((and (full? 1) (full? 2) (full? 3) (full? 4) (full? 5) (full? 6) (full? 7))
              (begin
                (pDraw state-rows)
                (display "It's a draw!")(newline)
                (set! state-cols original)))
             
             (else
              (begin
                (compMove)
                (if (won? compPiece)
                    (begin
                      (pDraw state-rows)
                      (display "I won!")(newline)
                      (set! state-cols original))
                    (pDraw state-rows)))))))
      (else
       error "Not a valid input.")))
  
  (begin
    (display "                                 Welcome to the MiniMax version of Connect-4") (newline)
    (display "                  ===========================================================================") (newline) (newline)
    (display "I suggest you turn off the Definitions pane in DrScheme.")(newline) (newline)
    (display "You have defined your game and these are the rules.") (newline) (newline)
    (display "1.  You will be playing as X and I will be playing as O.") (newline)
    (display "2.  I'll let you go first.") (newline)
    (display "3   To play, just apply the name you gave to Connect-4 (default m) to a valid column number.") (newline)
    (display "4.  I will then move and draw the board for you. The e's represent empty slots.") (newline)
    (display "5.  I may take a little while to make moves (maybe less than 10 seconds).") (newline) (newline)
    (display "Ready? Lets go!") (newline) (newline)
    dispatch))

(define m (Connect-4))

