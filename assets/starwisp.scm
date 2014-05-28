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
(msg i18n-lang)
(msg "heeee")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; game core

(define (abs n)
  (if (< n 0) (- n) n))

(define (dice-roll)
  1)
;;  (+ 1 (inexact->exact (abs (floor (* (rndf) 6))))))

(define crop-prepare-buy-treat 1)
(define crop-prepare-plough 2)
(define crop-prepare-sow 3)

(define crop-tasks (list 'irrigate 'weed 'fertilise
                         'pest-control 'disease-control))

(define (crop type) (list type 0 '()))

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
   ((>= (crop-stage c) s)'already-done-that)
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

(define (item type) (list type))
(define (item-type i) (list-ref i 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (player name type location points money items crops view)
  (list name type location points money items crops view))

(define (new-player name type location view)
  (player name type location 0 500
          (list (item 'ox) (item 'ox) (item 'field) (item 'bucket) (item 'bucket))
          (list (crop 'onion) (crop 'potato) (crop 'wheat)) view))

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

(define (player-find-crop p t)
  (let ((r (foldl
            (lambda (c r)
              (if (and (not r) (eq? (crop-type c) t)) c r))
            #f
            (player-crops p))))
    (when (not r) (msg "couldn't find crop" t "in" p))
    r))

(define (player-add-item p c)
  (player-modify-items p (cons c (player-items p))))

(define (player-has-item? p c)
  (foldl
   (lambda (i r)
     (if (and (not r) (eq? (item-type i) c)) #t r))
   #f
   (player-items p)))

(define (player-remove-item p c)
  (player-modify-items
   p (filter
      (lambda (i) (not (eq? (item-type i) c)))
      (player-items p))))

(define (player-add-money p v)
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
  (list-ref (board-places b) (- (player-location p) 1)))

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
     (let ((action (place-action (board-player-place b p))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; game code

(define action-toasts '())
(define (action-toast! player code)
  (set! action-toasts (cons (list (player-name player) code) action-toasts)))

(define (render-action-toasts!)
  (let ((r (map
            (lambda (c)
              (toast (string-append (car c) ": " (mtext-lookup (cadr c)))))
            action-toasts)))
    (set! action-toasts '())
    r))

(define (player-check-money p v)
  (cond
   ((> (player-money p) v) #t)
   (else (action-toast! p 'not-enough-cash) #f)))

(define (player-check-crop-stage player crop-name value money)
  (let ((crop (player-find-crop player crop-name)))
    (msg "ccs" crop value)
    (if (or (not crop) (not (player-check-money player money)))
        #f
        (let ((r (crop-check-stage crop value)))
          (msg "result" r)
          (cond
           ((eq? r 'ok)
            (cond
             ;; send success message now
             ((eq? crop-name 'onion)
              (cond
               ((eqv? value 1) (action-toast! player 'bought-onion))
               ((eqv? value 2) (action-toast! player 'ploughed-onion))
               ((eqv? value 3) (action-toast! player 'sowed-onion))))
             ((eq? crop-name 'potato)
              (cond
               ((eqv? value 1) (action-toast! player 'bought-potato))
               ((eqv? value 2) (action-toast! player 'ploughed-potato))
               ((eqv? value 3) (action-toast! player 'sowed-potato))))
             ((eq? crop-name 'wheat)
              (cond
               ((eqv? value 1) (action-toast! player 'bought-wheat))
               ((eqv? value 2) (action-toast! player 'ploughed-wheat))
               ((eqv? value 3) (action-toast! player 'sowed-wheat)))))
            #t)
           ;; fail messages
           ((eq? r 'crop-not-ready)
            (cond
             ((eq? crop-name 'onion) (action-toast! player 'onion-not-ready))
             ((eq? crop-name 'potato) (action-toast! player 'potato-not-ready))
             ((eq? crop-name 'wheat) (action-toast! player 'wheat-not-ready)))
            #f)
           ((eq? r 'already-done-that)
            (cond
             ((eq? crop-name 'onion) (action-toast! player 'onion-already-done))
             ((eq? crop-name 'potato) (action-toast! player 'potato-already-done))
             ((eq? crop-name 'wheat) (action-toast! player 'wheat-already-done)))
            #f)
           (else (msg "error not handled" r) #f))))))

;; unchecked version
(define (player-update-crop player crop-name value)
  (player-modify-crop
   (lambda (crop)
     (crop-modify-stage crop value))
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
             ((eq? crop-name 'onion)
              (cond
               ((eqv? task 'irrigated) (action-toast! player 'irrigated-onion))
               ((eqv? task 'weed) (action-toast! player 'weed-onion))
               ((eqv? task 'fertilise) (action-toast! player 'fertilise-onion))
               ((eqv? task 'pests) (action-toast! player 'pests-onion))
               ((eqv? task 'disease) (action-toast! player 'disease-onion))))
             ((eq? crop-name 'potato)
              (cond
               ((eqv? task 'irrigated) (action-toast! player 'irrigated-potato))
               ((eqv? task 'weed) (action-toast! player 'weed-potato))
               ((eqv? task 'fertilise) (action-toast! player 'fertilise-potato))
               ((eqv? task 'pests) (action-toast! player 'pests-potato))
               ((eqv? task 'disease) (action-toast! player 'disease-potato))))
             ((eq? crop-name 'wheat)
              (cond
               ((eqv? task 'irrigated) (action-toast! player 'irrigated-wheat))
               ((eqv? task 'weed) (action-toast! player 'weed-wheat))
               ((eqv? task 'fertilise) (action-toast! player 'fertilise-wheat))
               ((eqv? task 'pests) (action-toast! player 'pests-wheat))
               ((eqv? task 'disease) (action-toast! player 'disease-wheat)))))
            #t)
           ((eq? r 'crop-not-ready)
            (cond
             ((eq? crop-name 'onion) (action-toast! player 'onion-not-ready))
             ((eq? crop-name 'potato) (action-toast! player 'potato-not-ready))
             ((eq? crop-name 'wheat) (action-toast! player 'wheat-not-ready)))
            #f)
           ((eq? r 'already-done-that)
            (cond
             ((eq? crop-name 'onion) (action-toast! player 'onion-already-done))
             ((eq? crop-name 'potato) (action-toast! player 'potato-already-done))
             ((eq? crop-name 'wheat) (action-toast! player 'wheat-already-done)))
            #f)
           (else (msg "error not handled" r) #f))))))

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

(define (place-interface-crop)
  (lambda (game-view game-board player)
    (vert
     (horiz
      ;; dispatch to ui for this card
      (mbutton-scale 'onion
                     (lambda ()
                       (game-player-choice! 'onion)
                       (render-interface)))

      (mbutton-scale 'wheat
                     (lambda ()
                       (game-player-choice! 'wheat)
                       (render-interface))))
     (horiz
      (mbutton-scale 'potato
                     (lambda ()
                       (game-player-choice! 'potato)
                       (render-interface)))

      (mbutton-scale 'finished-crop
                     (lambda ()
                       (game-change-state! 'end)
                       (cons
                        (clear-left-display)
                        (render-interface))))))))



(define (place-interface-ok game-view game-board player)
  ;; cards with no choice - just ok...
  (horiz
   ;; dispatch to ui for this card
   (mbutton 'card-ok
            (lambda ()
              ;(cond
              ; ((eq? (place-code place) 'another-go)
              ;  (game-change-state! 'dice)
              ;  (render-interface))
              (game-player-choice! 'no-choice)
              (game-change-state! 'end)
              (cons
               (clear-left-display)
               (render-interface))))))



(define (build-board)
  (list
   (place 0 'shop '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-stage player choice crop-prepare-buy-treat 100)
                (player-add-money
                 (player-update-crop player choice crop-prepare-buy-treat)
                 -100)
                player))
          (place-interface-crop))
   (place 1 'another-go '() null-action place-interface-ok)
   (place 2 'shop '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-stage player choice crop-prepare-buy-treat 100)
                (player-add-money
                 (player-update-crop player choice crop-prepare-buy-treat)
                 -100)
                player))
          (place-interface-crop))
   (place 2 'inherit-field '()
          (lambda (player choice) (player-add-item player (item 'field)))
          place-interface-ok)
   (place 4 'tiger-attack '()
          (lambda (player action) (player-add-money player 100))
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
                (player-add-money (player-add-item player (item 'tractor)) -600)
                player))
          (place-interface-yn (next-yes 'yes)))
   (place 7 'win-lottery '()
          (lambda (player choice) (player-add-item player (item 'lottery)))
          place-interface-ok)
   ;; miss turn
   (place 8 'miss-turn '() null-action place-interface-ok) ; 9

   (place 9 'money-order '() (lambda (player choice) (player-add-money player 500)) place-interface-ok) ; 10
   (place 10 'plough '(wheat onion potato)
          (lambda (player choice)
            (if (and (player-check-crop-stage player choice crop-prepare-plough 0)
                     (or (player-has-item? player 'ox)
                         (player-has-item? player 'tractor)))
                (player-update-crop player choice crop-prepare-plough)
                player))
          (place-interface-crop))
   (place 11 'plough '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-stage player choice crop-prepare-plough 100)
                (player-update-crop
                 (player-add-money player -100)
                 choice crop-prepare-plough)
                player))
          (place-interface-crop))
   ;; free turn
   (place 12 'political-camp '() null-action place-interface-ok) ; 13

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
            (if (player-check-crop-stage player choice crop-prepare-sow 100)
                (player-update-crop
                 (player-add-money player -100)
                 choice crop-prepare-sow)
                player))
          (place-interface-crop))

   (place 15 'buy-rainwater '(yes no)
          (lambda (player choice)
            (if (and (eq? choice 'yes) (player-check-money player 500))
                (player-add-money (player-add-item player (item 'rainwater)) -500)
                player))
          (place-interface-yn (next-yes 'yes)))

   (place 16 'win-lottery '()
          (lambda (player choice) (player-add-item player (item 'lottery)))
          place-interface-ok)

   (place 17 'plough '(wheat onion potato)
          (lambda (player choice)
            (if (and (player-check-crop-stage player choice crop-prepare-plough 0)
                     (or (player-has-item? player 'ox)
                         (player-has-item? player 'tractor)))
                (player-update-crop player choice crop-prepare-plough)
                player))
          (place-interface-crop))

   ;; microcredit loan
   (place 18 'empty '() null-action place-interface-ok) ; 19

   (place 19 'sow '(wheat onion potato)
          (lambda (player choice)
            (if (and (player-check-crop-stage player choice crop-prepare-sow 100)
                     (player-has-item? player 'ox)
                     (player-has-item? player 'bucket))
                (player-update-crop
                 (player-add-money
                  (player-remove-item player 'bucket)
                  -100)
                 choice crop-prepare-sow)
                player))
          (place-interface-crop))

   (place 20 'sow '(wheat onion potato)
          (lambda (player choice)
            (if (and (player-check-crop-stage player choice crop-prepare-sow 100)
                     (player-has-item? player 'bucket))
                (player-update-crop
                 (player-add-money
                  (player-remove-item player 'bucket)
                  -100)
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
            (if (and (player-check-crop-stage player choice crop-prepare-plough 0)
                     (or (player-has-item? player 'ox)
                         (player-has-item? player 'tractor)))
                (player-update-crop player choice crop-prepare-plough)
                player))
          (place-interface-crop))

   (place 23 'irrigate '(wheat onion potato)
          (lambda (player choice)
            (if (and
                 (player-check-crop-task player choice 'irrigate 100)
                 (player-has-item? player 'bucket))
                (player-update-crop-task
                 (player-add-money
                  (player-remove-item player 'bucket)
                  -100) choice 'irrigate)
                player))
          (place-interface-crop))

   ;; tractor breaks down
   (place 24 'empty '() null-action place-interface-ok) ; 20

   (place 25 'pest '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-task player choice 'pest 0)
                (player-update-crop-task player choice 'bird)
                player))
          (place-interface-crop))

   (place 26 'fertilise '(wheat onion potato)
          (lambda (player choice)
            (if (player-check-crop-task player choice 'fertilise 100)
                (player-update-crop-task
                 (if (player-has-item? player 'ox)
                     player (player-add-money player -100))
                 choice fertilise)
                player))
          (place-interface-crop))

   (place 27 'sow '(wheat onion potato)
          (lambda (player choice)
            (if (and (player-has-item? player 'bucket)
                     (player-check-crop-task player choice 'fertilise 0))
                (player-update-crop
                 (player-remove-item player 'bucket)
                 choice crop-prepare-sow)
                player))
          (place-interface-crop))

   (place 23 'irrigate '(wheat onion potato)
          (lambda (player choice)
            (if (and
                 (player-check-crop-task player choice 'fertilise 0)
                 (player-has-item? player 'bucket))
                (player-update-crop-task
                 (player-remove-item player 'bucket)
                 choice 'irrigate)
                player))
          (place-interface-crop))

   (place 24 'weed '(wheat onion potato)
          (lambda (player choice)
            (player-update-crop-task
             (player-add-money
              player -200) choice 'weed))
          (place-interface-crop))

   (place 25 'lose-water '(wheat onion potato)
          (lambda (player choice)
            (player-remove-item
             (player-remove-item player 'rainwater) 'bucket))
          (place-interface-crop))

   (place 26 'disease '(wheat onion potato)
          (lambda (player choice)
            (player-update-crop-task player choice 'disease))
          (place-interface-crop))

   (place 27 'empty '() null-action place-interface-ok) ; 28
   (place 28 'empty '() null-action place-interface-ok) ; 29
   (place 29 'empty '() null-action place-interface-ok) ; 30
   (place 30 'empty '() null-action place-interface-ok) ; 31
   (place 31 'empty '() null-action place-interface-ok) ; 32
   (place 32 'empty '() null-action place-interface-ok) ; 33
   (place 33 'empty '() null-action place-interface-ok) ; 34
   (place 34 'empty '() null-action place-interface-ok) ; 35
   (place 35 'empty '() null-action place-interface-ok) ; 36
   (place 36 'empty '() null-action place-interface-ok) ; 37
   (place 37 'empty '() null-action place-interface-ok) ; 38
   (place 38 'empty '() null-action place-interface-ok) ; 39
   (place 39 'empty '() null-action place-interface-ok) ; 40
   (place 40 'empty '() null-action place-interface-ok) ; 41
   (place 41 'empty '() null-action place-interface-ok) ; 42
   (place 42 'empty '() null-action place-interface-ok) ; 43
   (place 43 'empty '() null-action place-interface-ok) ; 44
   (place 44 'empty '() null-action place-interface-ok) ; 45
   (place 45 'empty '() null-action place-interface-ok) ; 46
   (place 46 'empty '() null-action place-interface-ok) ; 47
   (place 47 'empty '() null-action place-interface-ok) ; 48
   (place 48 'empty '() null-action place-interface-ok) ; 49
   (place 49 'empty '() null-action place-interface-ok) ; 50
   (place 50 'empty '() null-action place-interface-ok) ; 51
   (place 51 'empty '() null-action place-interface-ok) ; 52
   (place 52 'empty '() null-action place-interface-ok) ; 53
   (place 53 'empty '() null-action place-interface-ok) ; 54
   (place 54 'empty '() null-action place-interface-ok) ; 55
   (place 55 'empty '() null-action place-interface-ok) ; 56
   (place 56 'empty '() null-action place-interface-ok) ; 57
   (place 57 'empty '() null-action place-interface-ok) ; 58
   (place 58 'empty '() null-action place-interface-ok) ; 59
   (place 59 'empty '() null-action place-interface-ok))) ; 60

(define (build-dice-screen game-view game-board player location place)
  (if (eq? (player-type player) 'ai)
      (ai-turn!)
      (list
       (update-widget
        'linear-layout (get-id "display") 'contents
        (list
         (mbutton-scale 'dice-ready
                        (lambda ()
                          (game-dice-result! 1)
                          (game-change-state! 'play)
                          (render-interface))))))))

(define (clear-left-display)
  (update-widget 'linear-layout (get-id "left-display") 'contents '()))

(define (build-play-screen game-view game-board player location place)
  ;; display card
  (list
   (update-widget
    'linear-layout (get-id "left-display") 'contents
    (list
     (image-view 0 (string-append "card" (number->string location))
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
              (image-view 0 (if (crop-task-complete? crop 'pest) "pest_icon" "pest_icon_grey")
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
            0 (item-type-to-image (item-type item))
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
                 (game-next-player!)
                 (cons
                  (clear-left-display)
                  (render-interface))))))))

(define (game-view-build-interface game-view game-board)
  (let* ((player (dbg (list-ref (board-players game-board)
                           (game-view-current-player game-view))))
         (location (dbg (player-location player)))
         (place (if (zero? location) '()
                    (list-ref (board-places game-board) (- location 1)))))
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
  (set! game-board
        (board-move-player game-board (game-view-current-player game-view) v)))

(define (game-next-player!)
  (set! game-view
        (game-view-modify-current-player
         game-view
         (modulo (+ (game-view-current-player game-view) 1) max-players)))

  (msg "next player now" (game-view-current-player game-view))
  (lock-camera (player-view-root
                (player-view (list-ref (board-players game-board)
                                       (game-view-current-player game-view)))))

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
     (delayed (string-append "skip-" (player-name player)) (* 1000 move-time 2)
              (lambda ()
                (game-next-player!)
                (render-interface))))))



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
     (image-view 0 "stripv" (layout 'wrap-content 'fill-parent -1 'centre 0))
     (vert
      (mtitle 'bumpercrop)

      (horiz
       (vert
        (mspinner 'language (list 'english 'hindi)
                  (lambda (c)
                    (msg "lang setting" c)
                    (set-setting! "language" "int" c)
                    (set! i18n-lang c)
                    '()))
        (mbutton 'about (lambda (v) '()))
        (mbutton-scale 'start-game
                     (lambda ()
                       (list (start-activity "game" 0 "")))))

       (vert
        (mspinner 'player-1 (list 'human 'robot) (lambda (v) '()))
        (mspinner 'player-2 (list 'human 'robot) (lambda (v) '()))
        (mspinner 'player-3 (list 'human 'robot) (lambda (v) '()))
        (mspinner 'player-4 (list 'human 'robot) (lambda (v) '())))))


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
        (texture (load-texture "board.png"))
        (define c (raw-obj board))
        (with-primitive c (hint-unlit))
        (with-camera
         (translate (vector 0 0 5))
         (rotate (vector 45 45 0)))

        (set! game-board
              (new-game-board
               (build-board)
               (list
                (new-player (mtext-lookup 'player-1) 'human 5 (make-player-view (vector 1 0 0)))
                (new-player (mtext-lookup 'player-2) 'ai 0 (make-player-view (vector 0 1 0)))
                (new-player (mtext-lookup 'player-3) 'ai 0 (make-player-view (vector 0 0 1)))
                (new-player (mtext-lookup 'player-4) 'ai 0 (make-player-view (vector 1 0 1)))
                )))

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
