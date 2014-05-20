;; Starwisp Copyright (C) 2013 Dave Griffiths
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(display "starwisp.scm")(newline)

(alog "hello from starwisp.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; game core

(define (abs n)
  (if (< n 0) (- n) n))

(define (dice-roll)
  1)
;;  (+ 1 (inexact->exact (abs (floor (* (rndf) 6))))))

(define crop-cycle (list 'buy-treat 'plough 'sow))
(define crop-tasks (list 'irrigate 'weed 'fertilise
                         'pest-control 'disease-control))

(define (crop type)
  (list type 0 '()))

(define (crop-type c) (list-ref c 0))
(define (crop-stage c) (list-ref c 1))
(define (crop-modify-stage c v) (list-replace c 1 v))
(define (crop-tasks c) (list-ref c 2))
(define (crop-modify-tasks c v) (list-replace c 2 v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (player name location points money items crops view)
  (list name location points money items crops view))

(define (new-player name location view)
  (player name location 0 500 '() (list (crop "Cocoa")) view))

(define (player-name p) (list-ref p 0))
(define (player-location p) (list-ref p 1))
(define (player-modify-location p v) (list-replace p 1 v))
(define (player-points p) (list-ref p 2))
(define (player-money p) (list-ref p 3))
(define (player-modify-money p v) (list-replace p 3 v))
(define (player-items p) (list-ref p 4))
(define (player-modify-items p v) (list-replace p 4 v))
(define (player-crops p) (list-ref p 5))
(define (player-modify-crops p v) (list-replace p 5 v))
(define (player-view p) (list-ref p 6))
(define (player-modify-view p v) (list-replace p 6 v))

(define (player-add-crop p c)
  (player-modify-crops p (cons c (player-crops p))))

;; pass in whole board in case we need other players
(define (player-update p choice board)
  (let* ((new-location
          (modulo ;; loop
           (+ (dice-roll) (player-location p))
           (length (board-places board))))
         (place (list-ref (board-places board) new-location))
         (action (place-action place)))
    (msg "updating" (player-name p))
    (msg "location: " new-location)
    (action
     (player-modify-location
      (player-modify-view
       p (player-view-move (player-view p) new-location))
      new-location) choice)))

(define (player-random-choice p board)
  (let ((place (list-ref (board-places board) (player-location p))))
    (choose (place-choices place))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; action takes player and returns a new one
(define (place number code choices action)
  (list number code choices action))

(define (place-number p) (list-ref p 0))
(define (place-code p) (list-ref p 1))
(define (place-choices p) (list-ref p 2))
(define (place-action p) (list-ref p 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (game-board places players)
  (list places players))

(define (board-places b) (list-ref b 0))
(define (board-players b) (list-ref b 1))
(define (board-modify-players b v) (list-replace b 1 v))

(define (random-choice b player-index)
  (player-random-choice (list-ref (board-players b) player-index) b))

(define (board-update b player-index choice)
  (game-board
   (board-places b)
   (list-replace
    (board-players b)
    player-index
    (player-update (list-ref (board-players b) player-index) choice b))))


;;;;;;;;;;;;;;;;;;
;; test

(define (null-action p c) p)

(define (shop-action player choice)
  (msg "shop-action:" choice)
  (cond
   ((eq? choice 'buy-potatoes)
    (player-add-crop player (crop "potatoes")))
   ((eq? choice 'buy-wheat)
    (player-add-crop player (crop "wheat")))
   ((eq? choice 'buy-barley)
    (player-add-crop player (crop "barley")))
   (else player)))

(define (build-board)
  (build-list
   (lambda (i) (place i 'shop '(buy-wheat buy-barley buy-potatoes) shop-action))
   (length board-pos-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; board 3D stuff

(define rot (vector 0 0 0))

(define board-ori (vector -8.5 0 -6))
(define board-size 1.6)

(define south (vector 0 0 1))
(define north (vector 0 0 -1))
(define west (vector -1 0 0))
(define east (vector 1 0 0))

(define board-pos-list
  (foldl
   (lambda (p r)
     (append r (list (vadd (vmul
                            (cond
                             ((eqv? p #\n) north)
                             ((eqv? p #\s) south)
                             ((eqv? p #\e) east)
                             ((eqv? p #\w) west))
                            board-size)
                           (list-ref r (- (length r) 1))))))
   (list board-ori)
   (string->list "eeeeeeeeeeswwwwwwwwseeeeeeesswwwwwwwwseeeeeeeeeesswwwwwwwnnnnnn")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; game

(define (make-player-view col)
  (let ((p (with-state
            (scale (vector 0.2 0.2 0.2))
            (colour col)
            (raw-obj peice)
            )))
    (with-primitive p (apply-transform))
    (list p 0 (vector 0 0 0) (vector 0 0 0))))

(define (player-view-root p) (list-ref p 0))
(define (player-view-t p) (list-ref p 1))
(define (player-view-modify-t p v) (list-replace p 1 v))
(define (player-view-from p) (list-ref p 2))
(define (player-view-modify-from p v) (list-replace p 2 v))
(define (player-view-to p) (list-ref p 3))
(define (player-view-modify-to p v) (list-replace p 3 v))

(define (player-view-update p)
  (cond
   ((< (player-view-t p) 1)
    (with-primitive
     (player-view-root p)
     (identity)
     (translate (vadd (vmul (player-view-from p) (- 1 (player-view-t p)))
                      (vmul (player-view-to p) (player-view-t p)))))
    (player-view-modify-t p (+ (player-view-t p) 0.1)))
   (else p)))

(define (player-view-move p dest)
  (player-view-modify-to
   (player-view-modify-from
    (player-view-modify-t p 0)
    (player-view-to p))
   (list-ref board-pos-list dest)))

(define (game-update g)
  (board-modify-players
   g
   (map
    (lambda (p)
      (player-modify-view
       p (player-view-update (player-view p))))
    (board-players g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interface

(define-fragment-list '())

(define game '())
(define trigger #f)
(define display-location 1)
(define user-player 0)
(define player-choice 'null)

(define (build-seeds type cost)
  (horiz
   (text-view 0 type 20 (layout 'fill-parent 'wrap-content 1 'left 10))
   (text-view 0 (number->string cost) 20 (layout 'fill-parent 'wrap-content 1 'left 10))
   (button (make-id (string-append "buy-" type)) "Buy!"
           20 (layout 'fill-parent 'wrap-content 1 'left 10)
           (lambda ()
             (set! player-choice (string->symbol (string-append "buy-" type)))
             (msg "set player choice to " type)
             '()))))

(define (build-shop)
  (update-widget 'linear-layout (get-id "display") 'contents
                 (list
                  (build-seeds "Wheat" 200)
                  (build-seeds "Potatoes" 100)
                  (build-seeds "Barley" 150)
                  (button
                   (make-id "shop-done")
                   "Finished" 30 (layout 'fill-parent 'wrap-content -1 'left 10)
                   (lambda ()
                     (set! trigger #t)
                     (list (build-display 0)))))))

(define (build-display location)
  (update-widget 'linear-layout (get-id "display") 'contents
                 (cond
                  ((eqv? location 0)
                   (let ((player (list-ref (board-players game) user-player)))
                     (msg player)
                     (list
                      (text-view 0 (player-name player) 20 (layout 'fill-parent 'wrap-content 1 'left 10))
                      (text-view 0 (string-append "Money: " (number->string (player-money player)))
                                 20 (layout 'fill-parent 'wrap-content 1 'left 10))

                      (apply vert
                             (map
                              (lambda (crop)
                                (text-view 0 (crop-type crop)
                                           20 (layout 'fill-parent 'wrap-content 1 'left 10)))
                              (player-crops player)))

                      (mbutton 'ready (lambda ()
                                        (list (build-display display-location))
                                        )))))
                  (else
                   (list
                    (text-view
                     0 (string-append "Location: " (number->string location))
                     20 (layout 'fill-parent 'wrap-content -1 'left 10))
                    (image-view 0 (string-append "card" (number->string location))
                                (layout 'wrap-content 'fill-parent 0.2 'left 10))

                    (horiz
                     (button (make-id (string-append "ready-yes" (number->string location)))
                             "Yes" 30 (layout 'fill-parent 'wrap-content 1 'left 10)
                             (lambda ()
                               (set! trigger #t)
                               (if (eqv? location 1)
                                   (list (build-shop))
                                   (list (build-display 0)))))
                     (button (make-id (string-append "ready-no" (number->string location)))
                             "No" 30 (layout 'fill-parent 'wrap-content 1 'left 10)
                             (lambda ()
                               (set! trigger #t)
                               (list (build-display 0)))))))
                  )))


(define-activity-list
  (activity
   "main"
   (linear-layout
    0 'horizontal
    (layout 'wrap-content 'fill-parent 1 'centre 0)
    (list 0 0 0 0)
    (list

     (nomadic
      (make-id "b2x") (layout 'fill-parent 'fill-parent 0.5 'left 0)
      (lambda ()
        (display "hello from nomadic callback")(newline)
        (clear)
        (texture (load-texture "board.png"))
        (define c (raw-obj board))
        (with-camera
         (translate (vector 0 0 5))
         (rotate (vector 45 45 0)))

        (set! game
          (game-board
           (build-board)
           (list
            (new-player "Player 1" 0 (make-player-view (vector 1 0 0)))
            (new-player "Player 2" 0 (make-player-view (vector 0 1 0)))
            (new-player "Player 3" 0 (make-player-view (vector 0 0 1)))
            (new-player "Player 4" 0 (make-player-view (vector 1 0 1)))
            )))

        (lock-camera (player-view-root (player-view (car (board-players game)))))
        (define next-player 0)

        (define next-move-time (+ (time-now) 1))

        (every-frame
         (begin
           (if (eqv? next-player user-player)
               (when trigger
                     (msg "player update with choice" player-choice)
                     (set! game (board-update game next-player player-choice))
                     (set! player-choice 'null)
                     (set! display-location (player-location (list-ref (board-players game) user-player)))
                     (set! next-move-time (+ (time-now) 0.5))
                     (set! next-player (modulo (+ next-player 1) 4))
                     (set! trigger #f)
                     )
               (when (< next-move-time (time-now))
                     (set! game (board-update game next-player (random-choice game next-player)))
                     (set! next-move-time (+ next-move-time 0.5))
                     (set! next-player (modulo (+ next-player 1) 4))))

           (set! game (game-update game))
           ))

        '()))

     (linear-layout
      (make-id "display") 'vertical
      (layout 300 'fill-parent -1 'left 0)
      (list 155 255 155 255)
      (list
       (button (make-id "start") "Start" 30 (layout 'fill-parent 'fill-parent 0.8 'left 10)
               (lambda ()
                 (set! trigger #t)
                 (list (build-display 0))))
       )
      )



     ))
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))


  )


(msg "done....")
