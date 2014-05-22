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

(define max-players 4)
(define move-time 0.5)

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

(define (player name type location points money items crops view)
  (list name type location points money items crops view))

(define (new-player name type location view)
  (player name type location 0 500 '() (list (crop "Cocoa")) view))

(define (player-name p) (list-ref p 0))
(define (player-type p) (list-ref p 1))
(define (player-location p) (list-ref p 2))
(define (player-modify-location p v) (list-replace p 2 v))
(define (player-points p) (list-ref p 3))
(define (player-money p) (list-ref p 4))
(define (player-modify-money p v) (list-replace p 4 v))
(define (player-items p) (list-ref p 5))
(define (player-modify-items p v) (list-replace p 5 v))
(define (player-crops p) (list-ref p 6))
(define (player-modify-crops p v) (list-replace p 6 v))
(define (player-view p) (list-ref p 7))
(define (player-modify-view p v) (list-replace p 7 v))

(define (player-add-crop p c)
  (player-modify-crops p (cons c (player-crops p))))

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

(define (new-game-board places players)
  (list places players))

(define (board-places b) (list-ref b 0))
(define (board-players b) (list-ref b 1))
(define (board-modify-players b v) (list-replace b 1 v))

(define (board-modify-player fn b index)
  (board-modify-players
   b (list-replace
      (board-players b)
      index
      (fn (list-ref (board-players b) index)))))

(define (random-choice b player-index)
  (player-random-choice (list-ref (board-players b) player-index) b))

(define (board-move-player b player-index dice-roll)
  (let* ((new-location
          (modulo ;; loop
           (+ dice-roll (player-location (list-ref (board-players b) player-index)))
           (length (board-places b)))))

    (msg "moving" player-index)
    (msg "to location: " new-location)

    ;; replace player with one in new location
    (board-modify-player
     (lambda (p)
       (player-modify-location
        (player-modify-view
         p (player-view-move (player-view p) new-location))
        new-location))
     b player-index)))

(define (board-player-choice b player-index choice)
  (board-modify-player
   (lambda (p)
     (let* ((action (place-action (list-ref (board-places b) (player-location p)))))
       (action p choice)))
   b player-index))

(define (board-player-location b player-index)
  (player-location (list-ref (board-players b) player-index)))

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

;; helpers

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

(define (game-board-update g)
  (board-modify-players
   g
   (map
    (lambda (p)
      (player-modify-view
       p (player-view-update (player-view p))))
    (board-players g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interface


;; states are dice, move, play
(define (game-view) (list 'dice 0))

(define (game-view-state s) (list-ref s 0))
(define (game-view-modify-state s v) (list-replace s 0 v))
(define (game-view-current-player s) (list-ref s 1))
(define (game-view-modify-current-player s v) (list-replace s 1 v))

(define (game-view-current-location game-view game)
  (board-player-location game (game-view-current-player game-view)))

;; hmm just stored for callbacks here
(define player-choice 'none)

(define (build-seeds name type cost)
  (horiz
   (text-view 0 name 20 (layout 'fill-parent 'wrap-content 1 'left 10))
   (text-view 0 (number->string cost) 20 (layout 'fill-parent 'wrap-content 1 'left 10))
   (button (make-id (string-append "buy-" type)) "Buy!"
           20 (layout 'fill-parent 'wrap-content 1 'left 10)
           (lambda ()
             (set! player-choice (string->symbol (string-append "buy-" type)))
             (msg "set player choice to " type)
             '()))))

(define (build-shop game-view game-board)
  (update-widget 'linear-layout (get-id "display") 'contents
                 (list
                  (build-seeds "Wheat" "wheat" 200)
                  (build-seeds "Potatoes" "potatoes" 100)
                  (build-seeds "Barley" "barley" 150)
                  (button
                   (make-id "shop-done")
                   "Finished" 30 (layout 'fill-parent 'wrap-content -1 'left 10)
                   (lambda ()
                     (game-player-choice! player-choice)
                     (render-interface)
                     )))))

(define (game-view-build-interface game-view game-board)
  (let ((player (list-ref (board-players game-board)
                          (game-view-current-player game-view))))
    (cond
     ((eq? (game-view-state game-view) 'dice)
      (if (eq? (player-type player) 'ai)
          (ai-turn!)
          (list
           (update-widget
            'linear-layout (get-id "display") 'contents
            (list
             (mtext 'dice)
             (mbutton 'dice-ready
                      (lambda ()
                        (game-dice-result! 1)
                        (game-change-state! 'move)
                        (render-interface))))))))


     ((eq? (game-view-state game-view) 'move)
      (list
       (update-widget
        'linear-layout (get-id "display") 'contents
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
                           (game-change-state! 'play)
                           (render-interface)))))))


     ((eq? (game-view-state game-view) 'play)
      ;; display card
      (let ((location (player-location player)))
        (list
         (update-widget
          'linear-layout (get-id "display") 'contents
          (list
           (text-view
            0 (string-append "Player: " (number->string (+ (game-view-current-player game-view) 1)))
            20 (layout 'fill-parent 'wrap-content -1 'left 10))
           (text-view
            0 (string-append "Location: " (number->string location))
            20 (layout 'fill-parent 'wrap-content -1 'left 10))
           (image-view 0 (string-append "card" (number->string location))
                       (layout 'wrap-content 'fill-parent 0.2 'left 10))

           (horiz
            ;; dispatch to ui for this card
            (button (make-id (string-append "ready-yes" (number->string location)))
                    "Yes" 30 (layout 'fill-parent 'wrap-content 1 'left 10)
                    (lambda ()
                      ;; build-ui for this card...
                      ;; all shops (all human)
                      (list (build-shop game-view game-board))
                      ))

            ;; maybe not for all cards?
            (button (make-id (string-append "ready-no" (number->string location)))
                    "No" 30 (layout 'fill-parent 'wrap-content 1 'left 10)
                    (lambda ()
                      (game-next-player!)
                      (render-interface)
                      )))))))
      ))))

;;;

(define game-board '()) ;; has to be built later in gl callback...
(define game-view (game-view))

;; realm of side effects

(define (render-interface)
  (game-view-build-interface game-view game-board))

(define (game-change-state! s)
  (set! game-view (game-view-modify-state game-view s)))

(define (game-dice-result! v)
  (set! game-board
        (board-move-player game-board (game-view-current-player game-view) v)))

(define (game-next-player!)
  (set! game-view
        (game-view-modify-current-player
         game-view
         (modulo (+ (game-view-current-player game-view) 1) max-players)))
  (msg "next player now" (game-view-current-player game-view))
  (game-change-state! 'dice))

(define (game-player-choice! choice)
  (set! game-board (board-player-choice
                    game-board
                    (game-view-current-player game-view) choice)))

(define (ai-turn!)
  (game-dice-result! 1)
  (let* ((player
          (list-ref (board-players game-board)
                    (game-view-current-player game-view)))
         (choice
          (player-random-choice
           player
           game-board)))
    (game-player-choice! choice)
    (msg "ai-turn" (player-name player))
    (list
     (update-widget 'linear-layout (get-id "display") 'contents
                    (list
                     (mtext 'AI-move)))
     (toast (string-append (player-name player) " moved"))
     (delayed (string-append "skip-" (player-name player)) (* 1000 move-time)
              (lambda ()
                (msg "hello from " (player-name player))
                (game-next-player!)
                (render-interface)
                )))))



;;;

(define-fragment-list '())

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

        (set! game-board
              (new-game-board
               (build-board)
               (list
                (new-player "Player 1" 'human 0 (make-player-view (vector 1 0 0)))
                (new-player "Player 2" 'ai 0 (make-player-view (vector 0 1 0)))
                (new-player "Player 3" 'ai 0 (make-player-view (vector 0 0 1)))
                (new-player "Player 4" 'ai 0 (make-player-view (vector 1 0 1)))
                )))

        (lock-camera (player-view-root (player-view (car (board-players game-board)))))

        (define next-move-time -1)

        (every-frame (set! game-board (game-board-update game-board)))

        '()))

     (linear-layout
      (make-id "display") 'vertical
      (layout 300 'fill-parent -1 'left 0)
      (list 155 255 155 255)
      (list
       (button (make-id "start") "Start" 30 (layout 'fill-parent 'fill-parent 0.8 'left 10)
               (lambda ()
                 (game-change-state! 'dice)
                 (render-interface)))
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
