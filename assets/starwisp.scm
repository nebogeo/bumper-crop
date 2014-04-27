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
  (+ 1 (inexact->exact (abs (floor (* (rndf) 6))))))

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
      (player-view-move (player-view p) new-location)))))

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

;;;;;;;;;;;;;;;;;;
;; test

(define (null-action p) p)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; board 3D stuff

(define rot (vector 0 0 0))

(define board-ori (vector -5 -0.3 2.96))
(define board-size 0.666666)

(define south (vector 0 0 1))
(define north (vector 0 0 -1))
(define west (vector -1 0 0))
(define east (vector 1 0 0))

(define board-pos-list
  (foldl
   (lambda (p r)
     (cons (vadd (vmul
                  (cond
                   ((eqv? p #\n) north)
                   ((eqv? p #\s) south)
                   ((eqv? p #\e) east)
                   ((eqv? p #\w) west))
                  board-size)
                 (car r)) r))
   (list board-ori)
   (string->list "nnneeennnwwwnnneeennneeessseeennneeessseeessswwwssseeessswwwssswwwnnnwwwssswwwnnnwww")))

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


(define-activity-list
  (activity
   "main"
   (linear-layout
    0 'horizontal
    (layout 'wrap-content 'fill-parent 1 'centre 0)
    (list 0 0 0 0)
    (list

     (linear-layout
      0 'vertical
      (layout 'fill-parent 'wrap-content 1 'top 0)
      (list 255 255 255 255)
      (list
       (text-view 0 "Soil Test" 40 (layout 'fill-parent 'wrap-content 1 'left 10))
       (text-view 0 "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industry's standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. " 20 (layout 'fill-parent 'wrap-content 1 'left 10))
       (button 0 "One" 40 (layout 'fill-parent 'wrap-content 1 'left 10)
               (lambda () '()))

       )
      )


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

        (define game
          (game-board
           (build-list
            (lambda (i) (place null-action))
            (length board-pos-list))
           (list
            (player "aa" 0 0 (make-player-view (vector 1 0 0)))
            (player "bb" 0 0 (make-player-view (vector 0 1 0)))
            (player "cc" 0 0 (make-player-view (vector 0 0 1)))
            (player "aa" 0 0 (make-player-view (vector 1 0 1)))
            (player "aa" 0 0 (make-player-view (vector 1 0 0)))
            (player "bb" 0 0 (make-player-view (vector 0 1 0)))
            (player "cc" 0 0 (make-player-view (vector 0 0 1)))
            (player "aa" 0 0 (make-player-view (vector 1 0 1)))
            )))

        (lock-camera (player-view-root (player-view (car (board-players game)))))
        (define frame 0)

        (define next-player 0)

        (every-frame
         (begin
           (set! frame (+ frame 1))
           (when (zero? (modulo frame 20) )
                 (set! next-player (modulo (+ next-player 1) 8))
                 (set! game (board-update game next-player)))

           (set! game (game-update game))
           ))

        '()))

     (linear-layout
      0 'vertical
      (layout 'fill-parent 'fill-parent 0.1 'top 20)
      (list 0 0 0 0)
      (list
       (button 0 "Roll dice" 40 (layout 'fill-parent 'wrap-content 1 'left 10)
               (lambda () '()))
       (button 0 "One" 40 (layout 'fill-parent 'wrap-content 1 'left 10)
               (lambda () '()))
       (button 0 "Two" 40 (layout 'fill-parent 'wrap-content 1 'left 10)
               (lambda () '()))
       (button 0 "Three" 40 (layout 'fill-parent 'wrap-content 1 'left 10)
               (lambda () '()))
       )
      )))
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
