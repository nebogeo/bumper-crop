#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; game core
;; (list-replace '(1 2 3 4) 2 100) => '(1 2 100 4)

;; notes

;; player inventory
;; * ox 
;; * field 
;; * water bucket
;; * money
;; * crop state(s)

;; [ fields tractor lottery 

;; functionality
;; * lottery = 1 crop cycle at any time
;; * miss turn
;; * extra turn
;; * money to/from all players

;; crop state (required)
;; * buy/treat (money) ->
;; * plough (money/ox/tractor) -> 
;; * sow (water) ->
;; * irrigate (electricity, money, water)
;; * weed (money)
;; * fertilise (ox)
;; * pest control (free,
;; * disease control


(define (list-replace l i v)
  (cond
    ((null? l) l)
    ((zero? i) (cons v (list-replace (cdr l) (- i 1) v)))
    (else (cons (car l) (list-replace (cdr l) (- i 1) v)))))

(define (abs n)
  (if (< n 0) (- n) n))

(define (dice-roll)
  (+ 1 (random 6)))

(define (player name location points view)
  (list name location points view))

(define (player-name p) (list-ref p 0))
(define (player-location p) (list-ref p 1))
(define (player-points p) (list-ref p 2))
(define (player-view p) (list-ref p 3))
(define (player-modify-view p v) (list-replace p 3 v))

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
      (player-points p)
      0 ;(player-view-move (player-view p) new-location)
      ))))

;; action takes player and returns a new one
(define (place action)
  (list action))

(define (place-action p) (list-ref p 0))

(define (game-board places players)
  (list places players))

(define (board-places b) (list-ref b 0))
(define (board-players b) (list-ref b 1))
(define (board-modify-players b v) (list-replace b 1 v))

(define (board-update b player-index)
  (game-board
   (board-places b)
   (list-replace
    (board-players b)
    player-index
    (player-update (list-ref (board-players b) player-index) b))))


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

;;;;



(define (null-action p) p)

(define (plus-action p)
  (player
   (player-name p)
   (player-location p)
   (+ (player-points p) 1)
   0))

(define (neg-action p)
  (player
   (player-name p)
   (player-location p)
   (max (- (player-points p) 1) 0) 
   0))

(define (rwd-action p)
  (player
   (player-name p)
   (- (player-location p) 3)
   (player-points p)
   0))


(define test
  (game-board
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
    (player "aa" 0 0 0)
    (player "ab" 0 0 0)
    (player "ba" 0 0 0)
    (player "bb" 0 0 0)
    (player "ac" 0 0 0)
    (player "ca" 0 0 0))))

(for ((i (in-range 0 10)))
  (board-pretty-print test)
  (set! test (board-update test 0)))
