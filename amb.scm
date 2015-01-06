;(define (Connect-4)

;;;;;;;;;;;;;;;;;;; amb stuff ;;;;;;;;;;;;;;;;;;;
(require (lib "defmacro.ss"))

(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
          (lambda ()
            ;            error "amb tree exhausted")
            #f)
          )))


(initialize-amb-fail)

(define-macro amb
  (lambda alts...
    `(let ((+prev-amb-fail amb-fail))
       (call/cc
        (lambda (+sk) ; Success Continuation
          
          ,@(map (lambda (alt)
                   `(call/cc
                     (lambda (+fk) ; Failure Continuation
                       (set! amb-fail
                             (lambda ()
                               (set! amb-fail +prev-amb-fail)
                               (+fk 'fail)))
                       (+sk ,alt))))
                 alts...)
          
          (+prev-amb-fail))))))

(define (require p)
  (if (not p) (amb)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (full? col)
  (cond
    ((eq? (car (list-ref state-cols (sub1 col))) 'e) #f)
    (else #t)))

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

; hypotDrop returns false if the drop in the specified column is not
; possible, otherwise, it will return a list containing three elements.
; They are...
;  1. Hypothetical state of the columns if the piece was dropped
;  2. Hypothetical state of the rows if the piece was dropped
;  3. Hypothetical state of the diagonals if the piece was dropped
(define (hypotDrop col s-cols player-piece)
  (let ((column (list-ref s-cols (sub1 col))))
    (cond
      ((eq? (car column) 'e)
       (let ((newCols (set-col col (update column player-piece) s-cols)))
         (cons newCols
               (let ((newRows (transpose newCols)))
                 (cons newRows
                       (cons (diagonalize newRows) '()))))))
      (else #f))))

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

(define (hypWon? s-cols s-rows s-diags game-piece)
  (or (check-won s-cols game-piece)
      (check-won s-rows game-piece)
      (check-won s-diags game-piece)))

; findWinningMove returns the column that will allow the specified 
; player to win the game IN THE NEXT MOVE. If such a move does
; NOT exist, then #f will be returned.
(define (findWinningMove cols player-piece)
  
  ; tryColumns returns the column number of the move that will
  ; allow the calling player to win; otherwise, the procedure
  ; will return #f.
  (define (tryColumns col)
    (if (> col 7) #f
        (let ((hypGameState (hypotDrop col cols player-piece)))
          (cond
            ((eq? hypGameState #f) (tryColumns (add1 col)))
            ((hypWon? (car hypGameState)
                      (cadr hypGameState)
                      (caddr hypGameState)
                      player-piece)
             col)
            (else (tryColumns (add1 col)))))))
  
  (tryColumns 1))

; firstOpenColumn returns the first column from the left
; that has an open slot; otherwise, it'll return #f.
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
  (let ((compWin (findWinningMove state-cols compPiece)))
    (cond
      ((number? compWin) (drop compWin compPiece))
      (else
       (let ((userWin (findWinningMove state-cols userPiece))) ; To try to prevent from winning.
         (cond
           ((number? userWin) (drop userWin compPiece))
           (else
            (drop (firstOpenColumn) compPiece)))))))) ; Insert ND algorithm or Minimax here


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

;(define (userMove col)
;  (if (not (eq? (drop col userPiece) #f)) (begin
;                                            (compMove)
;                                            (pDraw state-rows))))

(define (dispatch m)
  (cond
    ((number? m) 
     (if (not (eq? (drop m userPiece) #f)) 
         (cond
           ((won? userPiece) (begin
                               (pDraw state-rows)
                               (display "Congratulations! You won!")
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

;  dispatch)

; chooseMove determines a next move that benefits the specified player
; more than his/her opponent
;  Parameters: player-piece: player whose should benefit from the move
;              opp-piece: opponent of the specified player
(define (chooseMove s-cols player-piece opp-piece)
  (let ((hypMove (amb 1 2 3 4 5 6 7)))
    (let ((hypGameState (hypotDrop hypMove s-cols player-piece)))
      ; Require that the move is a valid one
      (require (not (eq? hypGameState #f)))
      
      (let ((hypCols (car hypGameState))
            (hypRows (cadr hypGameState))
            (hypDiags (caddr hypGameState)))
        
        ; Require that the move results in a better (or at least just as good)
        ; situation for the computer in relation to the user    
        (require (>= (evaluate hypCols hypRows hypDiags player-piece)
                     (evaluate hypCols hypRows hypDiags opp-piece)))
        
        ; Should not put a require in here dictating that the opponent
        ; has NOT won in his or her next move because there might be a
        ; case in which the opponent's victory is inevitable.
        
        hypMove))))

; Extends the move selector to take into account cases where the
; opponent has an imminent victory. This should basically take
; the place of compMove.
(define (selectMove col-state player-piece opp-piece)
  (let ((uWin (findWinningMove col-state player-piece)))
    (if (number? uWin) uWin
        (let ((theyWin (findWinningMove col-state opp-piece))) ; To try to prevent opponent from winning.
          (if (number? theyWin) theyWin
              (chooseMove col-state player-piece opp-piece))))))

(define (thinkAhead4 player-piece opp-piece)
  (let ((uWin (findWinningMove state-cols player-piece)))
    (if (number? uWin) uWin
        (let ((theyWin (findWinningMove state-cols opp-piece))) ; To try to prevent opponent from winning.
          (if (number? theyWin) theyWin
              
              ; Calling player's move
              (let ((move1 (selectMove state-cols player-piece opp-piece)))
                (let ((hypState1 (hypotDrop move1 state-cols player-piece)))
                  
                  ;      ; If player is capable of winning with his or her next move, then there's
                  ;      ; no need to consider any more hypothetical moves.
                  ;      (if (hypWon? (car hypState1)
                  ;                   (cadr hypState1)
                  ;                   (caddr hypState1)
                  ;                   player-piece)
                  ;          move1
                  
                  ; Opponent's move
                  (let ((move2 (selectMove (car hypState1) opp-piece player-piece)))
                    (let ((hypState2 (hypotDrop move2 (car hypState1) opp-piece)))
                      (begin
                        ; Require that the opponent not win: If the opponent emerges victorious here,
                        ; then it might have been due to an incorrect decision in the calling player's
                        ; move.
                        (require (not (hypWon? (car hypState2)
                                               (cadr hypState2)
                                               (caddr hypState2)
                                               opp-piece)))
                        ; Could just test if evaluation function is equal to 10000
                        
                        ; Calling player's move
                        (let ((move3 (selectMove (car hypState2) player-piece opp-piece)))
                          ;                                  (let ((hypState3 (hypotDrop move3 (car hypState2) player-piece)))
                          ;                                    (let ((hypCols (car hypState3))
                          ;                                          (hypRows (cadr hypState3))
                          ;                                          (hypDiags (caddr hypState3)))
                          ;                            
                          ; Opponent's move: Require that when it's time for the opponent
                          ; to go, the situation will be in your favor.
                          ;          (require (>= (evaluate (car hypState2) (cadr hypState2) (caddr hypState2) player-piece)
                          ;                       (evaluate (car hypState2) (cadr hypState2) (caddr hypState2) opp-piece)))
                          
                          move1)))))))))))

(define (makeMove col)
  (drop col userPiece)
  (let ((chosenMove (thinkAhead4 compPiece userPiece))) ;** Change this to try different algorithms! **
    (newline)
    (display "Chosen Move for the computer: ")(display chosenMove)
    (drop chosenMove compPiece))
  (pDraw state-rows)
  (display "State-Evaluation-User: ")(display (evaluate state-cols state-rows state-diags userPiece))
  (newline)
  (display "State-Evaluation-Computer: ")(display (evaluate state-cols state-rows state-diags compPiece))
  (newline)(display "======================================================"))

;;Do the following to test if computer is playing intelligently
(makeMove 3)
(makeMove 4)
(makeMove 6)
(makeMove 5)


;(makeMove 3)
;(makeMove 4)
;(makeMove 5)
;(makeMove 4)

; No way to make a move that will be beneficial to computer without losing!
;(drop 2 userPiece)
;(drop 1 compPiece) ; CHANGE THIS FROM 1 TO 7! Only 3 will yield equivalent evaluations and that enables player to win.
;(pDraw state-rows)
;(display "State-Evaluation-User: ")(display (evaluate state-cols state-rows state-diags userPiece))
;(newline)
;(display "State-Evaluation-Computer: ")(display (evaluate state-cols state-rows state-diags compPiece))

;(makeMove 2)
;(makeMove 6)
;(makeMove 2)
;(makeMove 3)
;(makeMove 5)
;(makeMove 5)

; User Win: Using selectMove instead of thinkAhead4 (Change in makeMove above)
;Chosen Move for the computer: 4
;(e e e e e e e)
;(e e e e e e e)
;(e O e e e e e)
;(e X X O X e e)
;(e O O O X e e)
;(e O X X X O e)

; Using thinkAhead4
;Chosen Move for the computer: 5
;(e e e e e e e)
;(e e e e e e e)
;(e X e e O e e)
;(e O O e X e e)
;(e O X e X e e)
;(O O X X X O e)
;
;State-Evaluation-User: 83
;State-Evaluation-Computer: 91




;State-Evaluation-User: 87
;State-Evaluation-Computer: 96

;(hypdrop 6 userPiece)
;(hypdrop 5 userPiece)
;(hypdrop 4 userPiece)
;(pDraw state-rows)

;(findWinningMove2 userPiece)

;(define wah (hypotDrop 4 state-cols userPiece))
;(hypdrop 4 userPiece)
;(equal? (car wah) state-cols)
;(equal? (cadr wah) state-rows)
;(equal? (caddr wah) state-diags)


;(define m (Connect-4))