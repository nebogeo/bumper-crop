#lang racket

(define (dice-roll)
  (+ (random 6) 1))

(define (player name location points)
  (list name location points))

(define (player-name p) (list-ref p 0))
(define (player-location p) (list-ref p 1))
(define (player-points p) (list-ref p 2))

;; pass in whole board in case we need other players
(define (player-update p board)
  (let* ((new-location 
          (modulo ;; loop
           (+ (dice-roll) (player-location p))
           (length (board-places board))))
         (place (list-ref (board-places board) new-location))
         (action (place-action place)))
    (action
     (player 
      (player-name p)
      new-location 
      (player-points p)))))

;; action takes player and returns a new one
(define (place action)
  (list action))

(define (place-action p) (list-ref p 0))

(define (board places players)
  (list places players))

(define (board-places b) (list-ref b 0))
(define (board-players b) (list-ref b 1))

(define (board-update b)
  (board
   (board-places b)
   (map 
    (lambda (player)
      (player-update player b))
    (board-players b))))

(define (board-pretty-print b)
  (let ((index 0))
    (for-each
     (lambda (place)
       (display "[ ")
       (for-each 
        (lambda (player)
          (when (eq? index (player-location player))
            (display (player-name player))
            (display (player-points player))
            (display " ")))
        (board-players b))
       (display " ]")(newline)
       (set! index (+ index 1)))
     (board-places b)))
  (newline))

;;;;;;;;;;;;;;;;;;
;; test

(define (null-action p) p)

(define (plus-action p)
  (player
   (player-name p)
   (player-location p)
   (+ (player-points p) 1)))

(define (neg-action p)
  (player
   (player-name p)
   (player-location p)
   (max (- (player-points p) 1) 0)))

(define (rwd-action p)
  (player
   (player-name p)
   (- (player-location p) 3)
   (player-points p)))


(define test
  (board
   (list
    (place null-action)
    (place neg-action)
    (place null-action)
    (place plus-action)
    (place null-action)
    (place neg-action)
    (place null-action)
    (place plus-action)
    (place null-action)
    (place plus-action)
    (place plus-action)
    (place null-action)
    (place null-action)
    (place neg-action)
    (place neg-action)
    (place rwd-action)
    (place null-action)
    (place null-action)
    (place neg-action)
    (place rwd-action)
    (place plus-action)
    (place null-action)
    
    )
   (list
    (player "aa" 0 0)
    (player "ab" 0 0)
    (player "ba" 0 0)
    (player "bb" 0 0)
    (player "ac" 0 0)
    (player "ca" 0 0))))

(for ((i (in-range 0 10)))
  (board-pretty-print test)
  (set! test (board-update test)))