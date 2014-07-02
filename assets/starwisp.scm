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

(define max-players 4)
(define move-time 0.5)

(define db "/sdcard/bumpercrop/settings.db")
(db-open db)(setup db "local")

(define settings-entity-id-version 1)

(insert-entity-if-not-exists
 db "local" "app-settings" "null" settings-entity-id-version
 (list
  (ktv "user-id" "varchar" "not set")
  (ktv "language" "int" 0)
  (ktv "house-id" "int" 0)
  (ktv "photo-id" "int" 0)
  (ktv "current-village" "varchar" "none")))

(define (get-setting-value name)
  (ktv-get (get-entity db "local" settings-entity-id-version) name))

(define (set-setting! key type value)
  (update-entity
   db "local" settings-entity-id-version (list (ktv key type value))))

(define (get/inc-setting key)
  (let ((r (get-setting-value key)))
    (set-setting! key "int" (+ r 1))
    r))

(set! i18n-lang (get-setting-value "language"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; game core

(define (abs n)
  (if (< n 0) (- n) n))

(define (dice-roll)
  (+ 1 (random 5)))

(define crop-prepare-buy-treat 1)
(define crop-prepare-plough 2)
(define crop-prepare-sow 3)

(define crop-tasks (list 'irrigate 'weed 'fertilise 'pests 'disease))

(define (crop type) (list type 0 '()))
(define (crop-ready type) (list type 1 '()))

(define (crop-type c) (list-ref c 0))
(define (crop-stage c) (list-ref c 1))
(define (crop-modify-stage c v) (list-replace c 1 v))
(define (crop-tasks c) (list-ref c 2))
(define (crop-modify-tasks c v) (list-replace c 2 v))

(define (crop-bought? c) (> (crop-stage c) 0))
(define (crop-ploughed? c) (> (crop-stage c) 1))
(define (crop-sown? c) (> (crop-stage c) 2))

(define (crop-check-stage c s)
  (cond
   ((eqv? (crop-stage c) (- s 1)) 'ok)
   ((>= (crop-stage c) s) 'already-done-that)
   (else 'crop-not-ready)))

(define (crop-task-complete? c t)
  (contains? (crop-tasks c) t))

(define (crop-check-task c v)
  (cond
   ((< (crop-stage c) crop-prepare-sow) 'crop-not-ready)
   ((crop-task-complete? c v) 'already-done-that)
   (else 'ok)))

(define (crop-add-task c v)
  (crop-modify-tasks c (cons v (crop-tasks c))))

(define (crop-delete c)
  (crop-modify-tasks (crop-modify-stage c 0) '()))

(define (crop-harvest-payout c multiplier)
  (let ((cycles (+ 3 (length (crop-tasks c)))))
    (* multiplier
       (cond
        ((eq? (crop-type c) 'wheat)
         (cond
          ((and (>= cycles 5) (< cycles 7)) 3000)
          ((>= cycles 7) 6000)
          (else 0)))
        ((eq? (crop-type c) 'onion)
         (cond
          ((and (>= cycles 5) (< cycles 7)) 2000)
          ((>= cycles 7) 4000)
          (else 0)))
        ((eq? (crop-type c) 'potato)
         (cond
          ((and (>= cycles 5) (< cycles 7)) 500)
          ((>= cycles 7) 1000)
          (else 0)))
        (else 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (player name type location points money items crops skip view)
  (list name type location points money items crops skip view))

(define (new-player name type location view)
  (player name type location 0 3000
          (list 'ox 'ox 'field 'bucket 'bucket)
          (list (crop-ready 'onion) (crop 'potato) (crop 'wheat)) 'none view))

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
(define (player-skip p) (list-ref p 7))
(define (player-modify-skip p v) (list-replace p 7 v))
(define (player-view p) (list-ref p 8))
(define (player-modify-view p v) (list-replace p 8 v))

(define (player-add-crop p c)
  (player-modify-crops p (cons c (player-crops p))))

(define (player-find-crop p t)
  (let ((r (foldl
            (lambda (c r)
              (if (and (not r) (eq? (crop-type c) t)) c r))
            #f
            (player-crops p))))
    (when (not r) (msg "couldn't find crop" t "in" p))
    r))

(define (player-add-item p . args)
  (for-each
   (lambda (item)
     (action-toast! p 'received-item item))
   args)
  (player-modify-items
   p (append (player-items p) args)))

(define (player-has-item? p . args)
  (let ((r (contains-all-list (player-items p) args)))
    (when (not r) (action-toast! p 'lack-items 'null))
    r))

(define (player-count-items p n)
  (define (_ l)
    (cond
     ((null? l) 0)
     ((eq? (car l) n) (+ 1 (_ (cdr l))))
     (else (_ (cdr l)))))
  (_ (player-items p)))

(define (player-remove-item p . args)
  (player-modify-items
   p (remove-list (player-items p) args)))

(define (player-remove-all-items p n)
  (player-modify-items
   p (filter (lambda (i) (not (eq? i n))) (player-items p))))

(define (player-add-money p v)
  (if (> v 0)
      (action-toast! p 'received-money 'null)
      (action-toast! p 'lost-money 'null))
  (player-modify-money p (+ (player-money p) v)))

(define (player-random-choice p board)
  (let ((place (list-ref (board-places board) (player-location p))))
    (if (null? (place-choices place))
        'null
        (choose (place-choices place)))))

(define (player-modify-crop fn p cropname)
  (player-modify-crops
   p (map
      (lambda (crop)
        (if (eq? (crop-type crop) cropname)
            (fn crop)
            crop))
      (player-crops p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; action takes player and returns a new one
(define (place number code choices action interface)
  (list number code choices action interface))

(define (place-number p) (list-ref p 0))
(define (place-code p) (list-ref p 1))
(define (place-choices p) (list-ref p 2))
(define (place-action p) (list-ref p 3))
(define (place-interface p) (list-ref p 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-game-board places players)
  (list places players))

(define (board-places b) (list-ref b 0))
(define (board-players b) (list-ref b 1))
(define (board-modify-players b v) (list-replace b 1 v))

(define (board-player-place b p)
  (list-ref (board-places b) (player-location p)))

(define (board-modify-player fn b index)
  (board-modify-players
   b (list-replace
      (board-players b)
      index
      (fn (list-ref (board-players b) index)))))

(define (random-choice b player-index)
  (player-random-choice (list-ref (board-players b) player-index) b))

(define (board-move-player b player-index dice-roll)
  (msg "board-move-player" player-index dice-roll)
  (let* ((new-location
          (modulo ;; loop
           (+ dice-roll (player-location (list-ref (board-players b) player-index)))
           (length (board-places b)))))

    ;; replace player with one in new location
    (board-modify-player
     (lambda (p)
       (player-modify-location
        (player-modify-view
         p (player-view-move (player-view p) new-location))
        new-location))
     b player-index)))

(define (board-player-choice b player-index choice)
  (msg "bpc")
  (msg player-index)
  (board-modify-player
   (lambda (p)
     (msg p)
     (let ((action (place-action (board-player-place b p))))
       (msg action)
       (action p choice)))
   b player-index))

(define (board-player-location b player-index)
  (player-location (list-ref (board-players b) player-index)))

;;;;;;;;;;;;;;;;;;
;; game code

(define (null-action p c) p)

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
            (rotate (vector 0 90 0))
            (scale (vector 0.07 0.07 0.07))
            (translate (vector (* (vx col) 10) (* 0.1 (rndf)) (* (vy col) 10)))
            (colour col)
            (texture (load-texture "peice.png"))
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

(msg "-- one")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; game code

(define action-toasts '())
(define (action-toast! player code crop)
  (set! action-toasts (cons (list 'action (player-name player) code crop) action-toasts)))

(define (harvest-toast! player payout crop)
  (set! action-toasts (cons (list 'harvest (player-name player) payout crop) action-toasts)))

;; build toasts and icon anims
(define (render-action-toasts!)
  (let ((r (map
            (lambda (c)
              (let ((type (list-ref c 0))
                    (player (list-ref c 1))
                    (code (list-ref c 2))
                    (crop (list-ref c 3)))
              (if (eq? type 'action)
                  (toast (string-append
                          player ": "
                          (mtext-lookup
                           (if (eq? crop 'null)
                               code
                               (string->symbol (string-append (symbol->string code) "-"
                                                              (symbol->string crop)))))))
                  (toast (string-append player ": " (mtext-lookup 'harvest " " (number->string code)))))))
            action-toasts))
        (anim ;; search for events to animate
         (foldl
          (lambda (c r)
            (let* ((type (list-ref c 0))
                   (player (list-ref c 1))
                   (code (list-ref c 2))
                   (crop (list-ref c 3))
                   (icon-id (make-id (string-append
                                      player "anim-icon"
                                      (symbol->string code)
                                      (symbol->string crop)))))

              (cond
               ((not (null? r)) r) ;; only one per update...
               ((eq? type 'action)
                (cond
                 ((or
                   ;; crop actions
                   (eq? code 'buy)
                   (eq? code 'disease)
                   (eq? code 'fertilise)
                   (eq? code 'irrigate)
                   (eq? code 'pest)
                   (eq? code 'plough)
                   (eq? code 'sow)
                   (eq? code 'weed))
                  (append
                   (list
                    (update-widget 'relative-layout (get-id "game-parent") 'contents-add
                                   (list (image-view icon-id (string-append (symbol->string code) "_icon")
                                                     (rlayout 'wrap-content 'wrap-content (list 800 150 0 0) '()))))
                    (update-widget 'image-view icon-id 'animate
                                   (list 0 0 400 0))) r)
                  )
                 ((eq? code 'received-item)
                  (append
                   (list
                    (update-widget 'relative-layout (get-id "game-parent") 'contents-add
                                   (list (image-view icon-id (string-append (symbol->string crop) "_item")
                                                     (rlayout 'wrap-content 'wrap-content (list 800 30 0 0) '()))))
                    (update-widget 'image-view icon-id 'animate
                                   (list 0 0 400 0))) r))
;                 ((or
;                   ;; cash actions
;                   (eq? code 'lost-money)
;                   (eq? code 'received-money))
;                  (append
;                   (list
;                    (update-widget 'relative-layout (get-id "game-parent") 'contents-add
;                                   (list (image-view icon-id "buy_icon"
;                                                     (rlayout 'wrap-content 'wrap-content (list 800 20 0 0) '()))))
;                    (update-widget 'image-view icon-id 'animate
;                                   (list 0 0 400 0))) r))
                 (else
                  (msg "no anim for" type code crop)
                  r)))
               (else
                (msg "no anim for" type code crop)
                r))))
          '()
          action-toasts)))
    (set! action-toasts '())
    (append r anim)))

(define (player-check-money p v)
  (cond
   ((>= (player-money p) v) #t)
   (else (action-toast! p 'not-enough-cash 'null) #f)))

(define (player-check-crop-stage player crop-name value money)
  (let ((crop (player-find-crop player crop-name)))
    (if (or (not crop) (not (player-check-money player money)))
        #f
        (let ((r (crop-check-stage crop value)))
          (cond
           ((eq? r 'ok)
             ;; send success message now
            (cond
             ((eqv? value 1) (action-toast! player 'buy crop-name))
             ((eqv? value 2) (action-toast! player 'plough crop-name))
             ((eqv? value 3) (action-toast! player 'sow crop-name)))
            #t)
           ;; fail messages
           ((eq? r 'crop-not-ready)
            (action-toast! player 'not-ready crop-name)
            #f)
           ((eq? r 'already-done-that)
            (action-toast! player 'already-done crop-name)
            #f)
           (else (msg "error not handled" r) #f))))))

;; unchecked version
(define (player-update-crop player crop-name value)
  (player-modify-crop
   (lambda (crop)
     (crop-modify-stage crop value))
   player crop-name))

(define (player-delete-crop player crop-name)
  (player-modify-crop
   (lambda (crop)
     (crop-delete crop))
   player crop-name))


(define (player-check-crop-task player crop-name task money)
  (let ((crop (player-find-crop player crop-name)))
    (if (or (not crop) (not (player-check-money player money)))
        #f
        (let ((r (crop-check-task crop task)))
          ;; todo - do something with error
          (cond
           ((eq? r 'ok)
            (cond
             ((eqv? task 'irrigate) (action-toast! player 'irrigate crop-name))
             ((eqv? task 'weed) (action-toast! player 'weed crop-name))
             ((eqv? task 'fertilise) (action-toast! player 'fertilise crop-name))
             ((eqv? task 'pests) (action-toast! player 'pest crop-name))
             ((eqv? task 'disease) (action-toast! player 'disease crop-name)))
            #t)
           ((eq? r 'crop-not-ready)
            (action-toast! player 'not-ready crop-name)
            #f)
           ((eq? r 'already-done-that)
            (action-toast! player 'already-done crop-name)
            #f)
           (else (msg "error not handled" r) #f))))))

;; todo seed distribution???
(define (player-harvest player crop-name)
  (msg "player-harvest" crop-name player)
  ;; check 5 cycles (two tasks)
  (let ((crop (player-find-crop player crop-name)))
    (if (and crop (>= (length (crop-tasks crop)) 2))
        (let ((payout (crop-harvest-payout
                       crop (player-count-items player 'field))))
          (harvest-toast! player payout crop-name)
          (player-delete-crop (player-add-money player payout) crop-name))
        player)))

(define (player-update-crop-task player crop-name task)
  (player-modify-crop
   (lambda (crop)
     (crop-add-task crop task))
   player crop-name))

(define (place-interface-yn build-next)
  (lambda (game-view game-board player)
    (horiz
     ;; dispatch to ui for this card
     (mbutton-scale 'card-yes
                    (lambda ()
                      (build-next game-view game-board)))

     (mbutton-scale 'card-no
              (lambda ()
                (game-change-state! 'end)
                (cons
                 (clear-left-display)
                 (render-interface)))))))

(define (next-yes player-choice)
  (lambda (game-view game-board)
    (game-player-choice! player-choice)
    (game-change-state! 'end)
    (cons
     (clear-left-display)
     (render-interface))))

(msg "-- two")

(define (place-interface-crop)
  (lambda (game-view game-board player)
    (horiz
     (button (symbol->id 'onion)
             (mtext-lookup 'onion)
             20 (layout 'fill-parent 'wrap-content 1 'centre 5)
             (lambda ()
               (game-player-choice! 'onion)
               (render-interface)))

     (button (symbol->id 'potato)
             (mtext-lookup 'potato)
             20 (layout 'fill-parent 'wrap-content 1 'centre 5)
             (lambda ()
               (game-player-choice! 'potato)
               (render-interface)))

     (button (symbol->id 'wheat)
             (mtext-lookup 'wheat)
             20 (layout 'fill-parent 'wrap-content 1 'centre 5)
             (lambda ()
               (game-player-choice! 'wheat)
               (render-interface))))))





(define (place-interface-ok game-view game-board player)
  (set-current! 'waiting-for-ok #t)
  ;; cards with no choice - just ok...
  (horiz
   ;; dispatch to ui for this card
   (mbutton 'card-ok
            (lambda ()
              (set-current! 'waiting-for-ok #f)
              (game-player-choice! 'no-choice)
              (game-change-state! 'end)
              (cons
               (clear-left-display)
               (render-interface))))))



(define (build-board)
  (list
   ;; needs a freebie
   (place 0 'shop '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-stage player choice crop-prepare-buy-treat 100)
                (player-add-money
                 (player-update-crop player choice crop-prepare-buy-treat)
                 -100)
                player))
          (place-interface-crop))

   (place 1 'another-go '()
          (lambda (player choice)
            (player-modify-skip player 'another-go))
          place-interface-ok)

   (place 2 'shop '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-stage player choice crop-prepare-buy-treat 100)
                (player-add-money
                 (player-update-crop player choice crop-prepare-buy-treat)
                 -100)
                player))
          (place-interface-crop))

   (place 2 'inherit-field '()
          (lambda (player choice) (player-add-item player 'field))
          place-interface-ok)

   (place 4 'tiger-attack '()
          (lambda (player action)
            (player-modify-skip
             (player-add-money player 100)
             'miss-turn))
          place-interface-ok)

   (place 5 'shop '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-stage player choice crop-prepare-buy-treat 300)
                (player-add-money
                 (player-update-crop player choice crop-prepare-buy-treat)
                 -300)
                player))
          (place-interface-crop))

   (place 6 'buy-tractor '(yes no)
          (lambda (player choice)
            (if (and (eq? choice 'yes) (player-check-money player 600))
                (player-add-money (player-add-item player 'tractor) -600)
                player))
          (place-interface-yn (next-yes 'yes)))

   (place 7 'win-lottery '()
          (lambda (player choice) (player-add-item player 'lottery))
          place-interface-ok)

   ;; miss turn
   (place 8 'miss-turn '()
          (lambda (player choice)
            (player-modify-skip player 'miss-turn))
          place-interface-ok) ; 9

   (place 9 'money-order '()
          (lambda (player choice)
            (player-add-money player 500))
          place-interface-ok) ; 10

   (place 10 'plough '(wheat onion potato)
          (lambda (player choice)
            (cond
             ;; if we have a tractor, free and another go
             ((player-has-item? player 'tractor)
              (if (player-check-crop-stage player choice crop-prepare-plough 0)
                  (player-update-crop
                   (player-modify-skip player 'another-go)
                   choice crop-prepare-plough)
                  player))
             ;; if we have two oxen, it's free
             ((player-has-item? player 'ox 'ox)
              (if (player-check-crop-stage player choice crop-prepare-plough 0)
                  (player-update-crop player choice crop-prepare-plough)
                  player))
             (else
              ;; otherwise it costs 100 each
              (if (player-check-crop-stage player choice crop-prepare-plough 100)
                  (player-update-crop
                   (player-add-money player -100)
                   choice crop-prepare-plough)
                  player))))
          (place-interface-crop))

   (place 11 'plough '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-stage player choice crop-prepare-plough 100)
                (player-update-crop
                 (player-add-money player -100)
                 choice crop-prepare-plough)
                player))
          (place-interface-crop))

   (place 12 'political-camp '()
          (lambda (player choice)
            (player-modify-skip player 'another-go))
          place-interface-ok) ; 13

   (place 13 'shop '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-stage player choice crop-prepare-buy-treat 400)
                (player-add-money
                 (player-update-crop player choice crop-prepare-buy-treat)
                 -400)
                player))
          (place-interface-crop))

   (place 14 'sow '(wheat onion potato)
          (lambda (player choice)
            (if (and
                 (player-check-crop-stage player choice crop-prepare-sow 100)
                 (player-has-item? player 'bucket))
                (player-update-crop
                 (player-remove-item player 'bucket)
                 choice crop-prepare-sow)
                player))
          (place-interface-crop))

   (place 15 'buy-rainwater '(yes no)
          (lambda (player choice)
            (if (and (eq? choice 'yes) (player-check-money player 500))
                (player-add-money (player-add-item player 'rainwater) -500)
                player))
          (place-interface-yn (next-yes 'yes)))

   (place 16 'win-lottery '()
          (lambda (player choice) (player-add-item player 'lottery))
          place-interface-ok)

   (place 17 'plough '(wheat onion potato)
          (lambda (player choice)
            (cond
             ;; if we have a tractor, free and another go
             ((player-has-item? player 'tractor)
              (if (player-check-crop-stage player choice crop-prepare-plough 0)
                  (player-update-crop
                   (player-modify-skip player 'another-go)
                   choice crop-prepare-plough)
                  player))
             ;; if we have two oxen, it's free
             ((player-has-item? player 'ox 'ox)
              (if (player-check-crop-stage player choice crop-prepare-plough 0)
                  (player-update-crop player choice crop-prepare-plough)
                  player))
             (else
              ;; otherwise it costs 100 each
              (if (player-check-crop-stage player choice crop-prepare-plough 100)
                  (player-update-crop
                   (player-add-money player -100)
                   choice crop-prepare-plough)
                  player))))
          (place-interface-crop))


   ;; microcredit loan
   ;; todo - remove from other players
   (place 18 'empty '()
          (lambda (player choice)
            (player-add-money player 100))
          place-interface-ok) ; 19

   (place 19 'sow '(wheat onion potato)
          (lambda (player choice)
            (if (and (player-check-crop-stage player choice crop-prepare-sow 0)
                     (player-has-item? player 'ox 'ox 'bucket))
                (player-update-crop
                 (player-remove-item player 'bucket)
                 choice crop-prepare-sow)
                player))
          (place-interface-crop))

   (place 20 'sow '(wheat onion potato)
          (lambda (player choice)
            (if (and (player-check-crop-stage player choice crop-prepare-sow 0)
                     (player-has-item? player 'bucket))
                (player-update-crop
                 (player-remove-item player 'bucket)
                 choice crop-prepare-sow)
                player))
          (place-interface-crop))

   (place 21 'weed '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-task player choice 'weed 100)
                (player-update-crop-task
                 (player-add-money
                  player -100) choice 'weed)
                player))
          (place-interface-crop))

   (place 22 'plough '(wheat onion potato)
          (lambda (player choice)
            (cond
             ;; if we have a tractor, free and another go
             ((player-has-item? player 'tractor)
              (if (player-check-crop-stage player choice crop-prepare-plough 0)
                  (player-update-crop
                   (player-modify-skip player 'another-go)
                   choice crop-prepare-plough)
                  player))
             ;; if we have two oxen, it's free
             ((player-has-item? player 'ox 'ox)
              (if (player-check-crop-stage player choice crop-prepare-plough 0)
                  (player-update-crop player choice crop-prepare-plough)
                  player))
             (else
              ;; otherwise it costs 100 each
              (if (player-check-crop-stage player choice crop-prepare-plough 100)
                  (player-update-crop
                   (player-add-money player -100)
                   choice crop-prepare-plough)
                  player))))
          (place-interface-crop))

   (place 23 'irrigate '(wheat onion potato)
          (lambda (player choice)
            (if (and
                 (player-check-crop-task player choice 'irrigate 100)
                 (player-has-item? player 'bucket 'bucket))
                (player-update-crop-task
                 (player-add-money
                  (player-remove-item player 'bucket 'bucket)
                  -100) choice 'irrigate)
                player))
          (place-interface-crop))

   ;; tractor breaks down
   (place 24 'empty '()
          (lambda (player choice)
            (if (player-has-item? player 'tractor)
                (player-modify-skip player 'miss-turn)
                player))
          place-interface-ok) ; 20

   (place 25 'pest '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-task player choice 'pests 0)
                (player-update-crop-task player choice 'pests)
                player))
          (place-interface-crop))

   (place 26 'fertilise '(wheat onion potato)
          (lambda (player choice)
            (if (player-has-item? player 'ox)
                (if (player-check-crop-task player choice 'fertilise 0)
                    (player-update-crop-task player choice 'fertilise)
                    player)
                (if (player-check-crop-task player choice 'fertilise 100)
                    (player-update-crop-task
                     (player-add-money player -100)
                     player choice 'fertilise)
                    player)))
          (place-interface-crop))

   (place 27 'sow '(wheat onion potato)
          (lambda (player choice)
            (if (and (player-has-item? player 'bucket 'bucket)
                     (player-check-crop-stage player choice crop-prepare-sow 0))
                (player-update-crop
                 (player-remove-item player 'bucket 'bucket)
                 choice crop-prepare-sow)
                player))
          (place-interface-crop))

   (place 28 'irrigate '(wheat onion potato)
          (lambda (player choice)
            (if (and
                 (player-check-crop-task player choice 'irrigate 0)
                 (player-has-item? player 'bucket 'bucket))
                (player-update-crop-task
                 (player-remove-item player 'bucket 'bucket)
                 choice 'irrigate)
                player))
          (place-interface-crop))

   (place 29 'weed '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-task player choice 'weed 200)
                (player-update-crop-task
                 (player-add-money
                  player -200) choice 'weed)
                player))
          (place-interface-crop))

   (place 30 'lose-water '()
          (lambda (player choice)
            (player-remove-all-items
             (player-remove-all-items player 'rainwater)
             'bucket))
          place-interface-ok)

   (place 31 'disease '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-task player choice 'disease 0)
                (player-update-crop-task player choice 'disease)
                player))
          (place-interface-crop))

   (place 32 'buy-solar '(yes no)
          (lambda (player choice)
            (if (and (eq? choice 'yes) (player-check-money player 500))
                (player-add-money (player-add-item player 'solar) -500)
                player))
          (place-interface-yn (next-yes 'yes)))

   (place 33 'add-water '()
          (lambda (player choice)
            (player-add-item player 'bucket 'bucket 'bucket 'bucket))
          place-interface-ok)

   (place 34 'irrigate '(wheat onion potato)
          (lambda (player choice)
            (if (and
                 (player-check-crop-task player choice 'irrigate 100)
                 (player-has-item? player 'bucket 'bucket))
                (player-update-crop-task
                 (player-remove-item
                  (player-add-money player -100) 'bucket 'bucket)
                 choice 'irrigate)
                player))
          (place-interface-crop))

   (place 35 'pest '(wheat onion potato)
          (lambda (player choice)
            (if (player-has-item? player 'ox)
                (if (player-check-crop-task player choice 'pests 0)
                    (player-update-crop-task player choice 'pests)
                    player))
                (if (player-check-crop-task player choice 'pests 200)
                    (player-update-crop-task
                     (player-add-money player -200) choice 'pests)
                    player))
          (place-interface-crop))

   (place 36 'tax '()
          (lambda (player choice)
            (if (player-has-item? player 'tractor)
                (player-add-money player -100)
                player))
          place-interface-ok)

   (place 37 'federal-planning '()
          (lambda (player choice)
            (player-add-money
             (player-remove-item
              (if (player-has-item? player 'field 'field)
                  player
                  (player-delete-crop
                   (player-delete-crop
                    (player-delete-crop player 'onion) 'potato) 'wheat))
              'field)
             2500))
          place-interface-ok)

   (place 38 'disease '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-task player choice 'disease 200)
                (player-update-crop-task
                 (player-add-money player -200) choice 'disease)
                player))
          (place-interface-crop))

   (place 39 'disease '()
          (lambda (player choice)
            (if (player-has-item? player 'solar)
                (player-modify-skip player 'another-go)
                (player-modify-skip player 'miss-turn)))
          place-interface-ok)

   ;; 200 total - not per crop...?
   (place 40 'fertilise '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-task player choice 'fertilise 0)
                (player-update-crop-task
                 (player-add-money player 200)
                 choice 'fertilise)
                player))
          (place-interface-crop))

   (place 41 'bricks '()
          (lambda (player choice)
            (if (player-has-item? player 'ox)
                (player-add-money player (* (player-count-items player 'ox) 200))
                player))
          place-interface-ok)

   (place 42 'pest '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-task player choice 'pests 100)
                (player-update-crop-task
                 (player-add-money player -100) choice 'pests)
                player))
          (place-interface-crop))

   (place 43 'weed '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-task player choice 'weed 200)
                (player-update-crop-task
                 (player-add-money
                  player -200) choice 'weed)
                player))
          (place-interface-crop))

   (place 44 'ox-die '()
          (lambda (player choice)
            (player-remove-all-items player 'ox))
          place-interface-ok)

   (place 45 'fertilise '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-task player choice 'fertilise 100)
                (player-update-crop-task
                 (player-add-money player -100)
                 choice 'fertilise)
                player))
          (place-interface-crop))


   ;; others pay 100 each
   (place 46 'electricity-bill '() null-action place-interface-ok)

   (place 47 'win-lottery '()
          (lambda (player choice) (player-add-item player 'lottery))
          place-interface-ok)

   (place 48 'harvest '(wheat onion potato)
          (lambda (player choice)
            (cond
             ((player-has-item? player 'tractor) (player-harvest player choice))
             ((player-has-item? player 'ox 'ox) (player-harvest player choice))
             (else
              (if (player-check-money player 100)
                  (player-harvest (player-add-money player -100) choice)
                  player))))
          (place-interface-crop))

   ;; pay 500???
   (place 49 'rats '()
          (lambda (player choice)
            (foldl
             (lambda (crop-name r)
               (if (not (crop-task-complete? (player-find-crop player crop-name) 'pests))
                   (player-delete-crop player crop-name)
                   player))
             player
             (list 'onion 'potato 'wheat)))
          place-interface-ok)

   (place 50 'wedding '()
          (lambda (player choice)
            (player-add-money player -3000))
          place-interface-ok)

   (place 51 'harvest '(wheat onion potato)
          (lambda (player choice)
            (cond
             ((player-has-item? player 'tractor) (player-harvest player choice))
             ((player-has-item? player 'ox 'ox) (player-harvest player choice))
             (else
              (if (player-check-money player 100)
                  (player-harvest (player-add-money player -100) choice)
                  player))))
          (place-interface-crop))

   ;; todo: from all players
   (place 52 'bridge-broken '()
          (lambda (player choice)
            (player-add-money player -100))
          place-interface-ok)

   ;; move ahead 2 or another turn
   (place 53 'empty '()
          (lambda (player choice)
            (player-modify-skip player 'another-go))
          place-interface-ok)

   (place 54 'gurt '()
          (lambda (player choice)
            (player-add-money player -200))
          place-interface-ok)

   (place 55 'harvest '(wheat onion potato)
          (lambda (player choice)
            (cond
             ((player-has-item? player 'tractor) (player-harvest player choice))
             ((player-has-item? player 'ox 'ox) (player-harvest player choice))
             (else
              (if (player-check-money player 100)
                  (player-harvest (player-add-money player -100) choice)
                  player))))
          (place-interface-crop))

   ;; 200 for any harvested crop???
   (place 56 'empty '() null-action place-interface-ok)

   ;; -100 for any harvested crop
   (place 57 'empty '() null-action place-interface-ok)

   (place 58 'tractor-transport '()
          (lambda (player choice)
            (if (player-has-item? player 'tractor)
                (player-add-money player 100)
                player))
          place-interface-ok)

   (place 59 'remove-debts '()
          (lambda (player choice)
            (if (< (player-money player) 0)
                (player-modify-money player 0)
                player))
          place-interface-ok)))


(define (build-dice-screen game-view game-board player location place)
  (if (eq? (player-type player) 'ai)
      (ai-turn!)
      (list
       (update-widget
        'linear-layout (get-id "display") 'contents
        (list
         (text-view 0 (player-name player) 40 (layout 'fill-parent 'wrap-content -1 'centre 10))
         (mtext 'your-turn)
         (mbutton-scale 'dice-ready
                        (lambda ()
                          (let ((d (dice-roll)))
                            (game-dice-result! d)
                            (game-change-state! 'play)
                            (render-interface)))))))))

(define (clear-left-display)
  (update-widget 'linear-layout (get-id "left-display") 'contents '()))

(define (build-play-screen game-view game-board player location place)
  ;; display card
  (list
   (update-widget
    'linear-layout (get-id "left-display") 'contents
    (list
     (image-view 0 (string-append "card" (number->string (+ location 1)))
                 (layout 'wrap-content 'wrap-content 3 'centre 10))

     ((place-interface place) game-view game-board player)
     ))))

(define (render-crop crop)
  (append
   (list
    (image-view 0 "strip" fillwrap)
    (text-view
     0 (string-append
        (cond
         ((crop-sown? crop) (mtext-lookup 'growing))
         ((crop-bought? crop) (mtext-lookup 'preparing))
         (else (mtext-lookup 'no-crop)))
        " "
        (mtext-lookup (crop-type crop)))
     25 (layout 'wrap-content 'wrap-content 1 'centre 0)))

   (if (zero? (crop-stage crop))
       '()
       (list
        (if (not (crop-sown? crop))
            (horiz
             (vert
              (image-view 0 (if (crop-bought? crop) "buy_icon" "buy_icon_grey")
                          (layout 'wrap-content 'fill-parent 1 'centre 2))
              (text-view 0 (mtext-lookup 'buy) 15 (layout 'wrap-content 'wrap-content 1 'centre 2)))
             (vert
              (image-view 0 (if (crop-ploughed? crop) "plough_icon" "plough_icon_grey")
                          (layout 'wrap-content 'fill-parent 1 'centre 2))
              (text-view 0 (mtext-lookup 'plough) 15 (layout 'wrap-content 'wrap-content 1 'centre 2)))
             (vert
              (image-view 0 "sow_icon_grey"
                          (layout 'wrap-content 'fill-parent 1 'centre 2))
              (text-view 0 (mtext-lookup 'sow) 15 (layout 'wrap-content 'wrap-content 1 'centre 2))))
            (horiz
             (vert
              (image-view 0 (if (crop-task-complete? crop 'irrigate) "irrigate_icon" "irrigate_icon_grey")
                          (layout 'wrap-content 'wrap-content 1 'centre 2))
              (text-view 0 (mtext-lookup 'irrigate) 15 (layout 'wrap-content 'wrap-content 1 'centre 2)))
             (vert
              (image-view 0 (if (crop-task-complete? crop 'weed) "weed_icon" "weed_icon_grey")
                          (layout 'wrap-content 'wrap-content 1 'centre 2))
              (text-view 0 (mtext-lookup 'weed) 15 (layout 'wrap-content 'wrap-content 1 'centre 2)))
             (vert
              (image-view 0 (if (crop-task-complete? crop 'fertilise) "fertilise_icon" "fertilise_icon_grey")
                          (layout 'wrap-content 'wrap-content 1 'centre 2))
              (text-view 0 (mtext-lookup 'fertilise) 15 (layout 'wrap-content 'wrap-content 1 'centre 2)))
             (vert
              (image-view 0 (if (crop-task-complete? crop 'pests) "pest_icon" "pest_icon_grey")
                          (layout 'wrap-content 'wrap-content 1 'centre 2))
              (text-view 0 (mtext-lookup 'pests) 15 (layout 'wrap-content 'wrap-content 1 'centre 2)))
             (vert
              (image-view 0 (if (crop-task-complete? crop 'disease) "disease_icon" "disease_icon_grey")
                          (layout 'wrap-content 'wrap-content 1 'centre 2))
              (text-view 0 (mtext-lookup 'disease) 15 (layout 'wrap-content 'wrap-content 1 'centre 2)))))))))

(define (item-type-to-image type)
  (cond
   ((eq? type 'ox) "ox_item")
   ((eq? type 'bucket) "bucket_item")
   ((eq? type 'bird) "bird_item")
   ((eq? type 'lottery) "lottery_item")
   ((eq? type 'mortgage) "mortgage_item")
   ((eq? type 'rainwater) "rainwater_item")
   ((eq? type 'solar) "solar_item")
   ((eq? type 'field) "field_item")
   ((eq? type 'tractor) "tractor_item")))

(define (render-items items)
  (apply
   horiz
   (map
    (lambda (item)
      (msg item)
      (dbg (image-view
            0 (item-type-to-image item)
            (layout 'fill-parent 'wrap-content 1 'centre 2))))
    items)))


(define (build-end-screen game-view game-board player location place)
  (list
   (update-widget
    'linear-layout (get-id "display") 'contents
    (list
     (image-view 0 "strip" fillwrap)
     (horiz
      (text-view 0 (player-name player) 20 (layout 'fill-parent 'wrap-content 1 'centre 10))
      (text-view 0 (string-append (mtext-lookup 'money) ": Rs" (number->string (player-money player)))
                 20 (layout 'fill-parent 'wrap-content 1 'centre 10)))

     (image-view 0 "strip" fillwrap)
     (scroll-view-vert
      0 (layout 'fill-parent 'wrap-content 1 'centre 0)
      (list
       ;;(mtext 'inventory)
       (apply vert
              (append
               (render-items (player-items player))
               (list (image-view 0 "strip" fillwrap)))
              (apply append (map render-crop (player-crops player))))))
      (mbutton 'finished
               (lambda ()
                 (cond
                  ((not (get-current 'waiting-for-ok #f))
                   (game-next-player!)
                   (cons
                    (clear-left-display)
                    (render-interface)))
                  (else ;; auto ok if it's waiting
                   (set-current! 'waiting-for-ok #f)
                   (game-player-choice! 'no-choice)
                   (game-change-state! 'end)
                   (game-next-player!)
                   (cons
                    (clear-left-display)
                    (render-interface))))))))))

(define (game-view-build-interface game-view game-board)
  (msg "game-view-build-interface")
  (let* ((player (dbg (list-ref (board-players game-board)
                           (game-view-current-player game-view))))
         (location (dbg (player-location player)))
         (place (if (< location 0) '()
                    (list-ref (board-places game-board) location))))
    (cond
     ((eq? (game-view-state game-view) 'dice)
      (build-dice-screen game-view game-board player location place))
     ((eq? (game-view-state game-view) 'play)
      (append
       (build-play-screen game-view game-board player location place)
       (build-end-screen game-view game-board player location place)))
     ((eq? (game-view-state game-view) 'end)
      (build-end-screen game-view game-board player location place))     )))

;;;

(define game-board '()) ;; has to be built later in gl callback...
(define game-view (game-view))

;; realm of side effects

(define (render-interface)
  (append
   (game-view-build-interface game-view game-board)
   (render-action-toasts!)))

(define (game-change-state! s)
  (set! game-view (game-view-modify-state game-view s)))

(define (game-dice-result! v)
  ;(action-toast! (list-ref (board-players game-board)
  ;                         (game-view-current-player game-view))
  ;               (list-ref (list 'rolled-one 'rolled-two 'rolled-three
  ;                               'rolled-four 'rolled-five 'rolled-six) (- v 1)))
  (set! game-board
        (board-move-player game-board (game-view-current-player game-view) v)))

(define (find-next-player!)
  ;; increment the player index
  (set! game-view
        (game-view-modify-current-player
         game-view
         (modulo (+ (game-view-current-player game-view) 1) max-players)))

  (let ((player (list-ref (board-players game-board)
                          (game-view-current-player game-view))))

    (cond
     ((not (eq? (player-skip player) 'miss-turn)) 0)
     (else
      (action-toast! player 'misses-turn 'null)
      ;; clear the skip flag
      (set! game-board (board-modify-player
                        (lambda (p)
                          (player-modify-skip p 'none))
                        game-board (game-view-current-player game-view)))
      (find-next-player!)))))


(define (game-next-player!)
  (let ((player (list-ref (board-players game-board)
                          (game-view-current-player game-view))))
    (if (eq? (player-skip player) 'another-go)
        ;; reset skip value
        (set! game-board (board-modify-player
                          (lambda (p)
                            (player-modify-skip p 'none))
                          game-board (game-view-current-player game-view)))
        (find-next-player!))

    ;; reload player
    (let ((player (list-ref (board-players game-board)
                            (game-view-current-player game-view))))

      (when (eq? (player-type player) 'human)
            (lock-camera (player-view-root (player-view (list-ref (board-players game-board)
                                                                  (game-view-current-player game-view))))))
      (game-change-state! 'dice))))

(define (game-player-choice! choice)
  (msg "gpc")
  (set! game-board (board-player-choice
                    game-board
                    (dbg (game-view-current-player game-view)) choice)))

(define (ai-turn!)
  (game-dice-result! (dice-roll))
  (let* ((player
          (list-ref (board-players game-board)
                    (game-view-current-player game-view)))
         (choice
          (player-random-choice
           player
           game-board)))
    (msg "here->" choice)
    (game-player-choice! choice)
    (msg "ai-turn" (player-name player))
    (game-change-state! 'end)

    (append
     (render-interface)
     (list
      (delayed (string-append "skip-" (player-name player)) (* 1000 move-time 2)
               (lambda ()
                 (game-next-player!)
                 (render-interface)))))))



;;;

(define-fragment-list '())

(set-current! 'player-1 'human)
(set-current! 'player-2 'ai)
(set-current! 'player-3 'ai)
(set-current! 'player-4 'ai)

(define-activity-list

  (activity
   "main"
   (linear-layout
    0 'horizontal
    (layout 'wrap-content 'fill-parent 1 'centre 0)
    (list 0 0 0 0)
    (list
     (image-view 0 "stripv" (layout 'wrap-content 'fill-parent -1 'centre 0))
     (vert
      (mtitle 'bumpercrop)

      (horiz
       (vert
        (mspinner 'language (list 'english 'hindi)
                  (lambda (c)
                    (cond
                     ((not (eqv? c i18n-lang))
                      (msg "lang setting" c)
                      (set-setting! "language" "int" c)
                      (set! i18n-lang c)
                      (list (start-activity-goto "main" 0 "")))
                     (else '()))))
        (mbutton 'about (lambda (v) '()))
        (mbutton-scale 'start-game-from-main
                       (lambda ()
                         (list (start-activity "game" 0 "")))))

       (vert
        (mspinner
         'player-1 (list 'human 'ai)
         (lambda (v) (set-current! 'player-1 (list-ref (list 'human 'ai) v)) '()))
        (mspinner
         'player-2 (list 'ai 'human)
         (lambda (v) (set-current! 'player-2 (list-ref (list 'ai 'human) v)) '()))
        (mspinner
         'player-3 (list 'ai 'human)
         (lambda (v) (set-current! 'player-3 (list-ref (list 'ai 'human) v)) '()))
        (mspinner
         'player-4 (list 'ai 'human)
         (lambda (v) (set-current! 'player-4 (list-ref (list 'ai 'human) v)) '())))))


     (image-view 0 "stripv" (layout 'wrap-content 'fill-parent -1 'centre 0))
     )
    )
   (lambda (activity arg)
     (activity-layout activity))
   (lambda (activity arg)
     (msg "hello")
     (msg (get-setting-value "language"))
     (list
      (update-widget 'spinner (get-id "language-spinner") 'selection
                     (get-setting-value "language"))))
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity) '())
   (lambda (activity requestcode resultcode) '()))

  (activity
   "game"
   (relative-layout
    (make-id "game-parent")
    (rlayout 'wrap-content 'fill-parent (list 0 0 0 0) '())
    (list 0 0 0 0)
    (list
     (linear-layout
      0 'horizontal
      (layout 'wrap-content 'fill-parent 1 'centre 0)
      (list 0 0 0 0)
      (list
       (linear-layout
        (make-id "left-display") 'vertical
        (layout 'wrap-content 'fill-parent -1 'centre 0)
        (list 155 255 155 0)
        (list))

       (image-view 0 "stripv" (layout 'wrap-content 'fill-parent -1 'centre 0))

       (nomadic
        (make-id "b2x") (layout 'fill-parent 'fill-parent 0.5 'centre 0)
        (lambda ()
          (display "hello from nomadic callback")(newline)
          (clear)

          (set! game-board
                (new-game-board
                 (build-board)
                 (list
                  (new-player (mtext-lookup 'player-1) (get-current 'player-1 'human) -1 (make-player-view (vector 1 0.5 0.5)))
                  (new-player (mtext-lookup 'player-2) (get-current 'player-2 'ai) -1 (make-player-view (vector 0.5 1 0.5)))
                  (new-player (mtext-lookup 'player-3) (get-current 'player-3 'ai) -1 (make-player-view (vector 0.5 0.5 1)))
                  (new-player (mtext-lookup 'player-4) (get-current 'player-4 'ai) -1 (make-player-view (vector 1 0.5 1)))
                  )))

          (define c (raw-obj board))
          (with-primitive
           c
           (texture (load-texture "board.png"))
         (hint-unlit))
        (with-camera
         (translate (vector 0 0 5))
         (rotate (vector 45 45 0)))


        (lock-camera (player-view-root (player-view (car (board-players game-board)))))

        (define next-move-time -1)

        (every-frame (set! game-board (game-board-update game-board)))

        '()))

     (image-view 0 "stripv" (layout 'wrap-content 'fill-parent -1 'centre 0))

     (linear-layout
      (make-id "display") 'vertical
      (layout 300 'fill-parent -1 'centre 0)
      (list 155 255 155 0)
      (list
        (mbutton-scale 'start-game
                       (lambda ()
                         (game-change-state! 'dice)
                         (render-interface)))))

     ))))
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
