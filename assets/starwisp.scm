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

(define (crop-set-stage c s)
  (cond
   ((eqv? (crop-stage c) (- s 1))
    (list #t (crop-modify-stage c s)))
   ((>= (crop-stage c) s)
    (list #f 'already-done-that))
   (else
    (list #f 'crop-not-ready))))

(define (crop-task-complete? c t)
  (contains? (crop-tasks c) t))

(define (crop-add-task c v)
  (cond
   ((< (crop-stage c) crop-prepare-sow)
    (list #f 'crop-not-ready))
   (else
    (list #t (crop-modify-tasks c (cons v (crop-tasks c)))))))

(define (item type) (list type))
(define (item-type i) (list-ref i 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (player name type location points money items crops view)
  (list name type location points money items crops view))

(define (new-player name type location view)
  (player name type location 0 500
          (list (item 'ox) (item 'ox) (item 'field))
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

(define (player-add-item p c)
  (player-modify-items p (cons c (player-items p))))

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

(define action-success 0)


(define (player-update-crop player crop-name value)
  (player-modify-crop
   (lambda (crop)
     (let ((r (crop-set-stage crop crop-prepare-buy-treat)))
       ;; todo - do something with error
       (if (car r)
           (cadr r)
           (begin
             (msg "crop error" (cadr r))
             crop))))
   player crop-name))

(define (shop-action player choice)
  (cond
   ((eq? choice 'buy-potato)
    (player-update-crop player 'potato crop-prepare-buy-treat))
   ((eq? choice 'buy-wheat)
    (player-update-crop player 'wheat crop-prepare-buy-treat))
   ((eq? choice 'buy-onion)
    (player-update-crop player 'onion crop-prepare-buy-treat))
   (else player)))

(define (inherit-field-action player choice)
  (msg "inherit-field-action")
  (player-add-item player (item "field")))

(define (tiger-attack-action player choice)
  (player-add-money player 100))


(define (build-board)
  (list
   (place 0 'shop '(buy-wheat buy-onion buy-potato) shop-action) ; 1
   (place 1 'another-go '() null-action) ; 2
   (place 2 'shop '(buy-wheat buy-onion buy-potato) shop-action) ; 3
   (place 2 'inherit-field '() inherit-field-action) ; 4
   (place 4 'tiger-attack '() tiger-attack-action) ; 5
   (place 5 'empty '() null-action) ; 6
   (place 6 'empty '() null-action) ; 7
   (place 7 'empty '() null-action) ; 8
   (place 8 'empty '() null-action) ; 9
   (place 9 'empty '() null-action) ; 10
   (place 10 'empty '() null-action) ; 11
   (place 11 'empty '() null-action) ; 12
   (place 12 'empty '() null-action) ; 13
   (place 13 'empty '() null-action) ; 14
   (place 14 'empty '() null-action) ; 15
   (place 15 'empty '() null-action) ; 16
   (place 16 'empty '() null-action) ; 17
   (place 17 'empty '() null-action) ; 18
   (place 18 'empty '() null-action) ; 19
   (place 19 'empty '() null-action) ; 20
   (place 20 'empty '() null-action) ; 21
   (place 21 'empty '() null-action) ; 22
   (place 22 'empty '() null-action) ; 23
   (place 23 'empty '() null-action) ; 24
   (place 24 'empty '() null-action) ; 25
   (place 25 'empty '() null-action) ; 26
   (place 26 'empty '() null-action) ; 27
   (place 27 'empty '() null-action) ; 28
   (place 28 'empty '() null-action) ; 29
   (place 29 'empty '() null-action) ; 30
   (place 30 'empty '() null-action) ; 31
   (place 31 'empty '() null-action) ; 32
   (place 32 'empty '() null-action) ; 33
   (place 33 'empty '() null-action) ; 34
   (place 34 'empty '() null-action) ; 35
   (place 35 'empty '() null-action) ; 36
   (place 36 'empty '() null-action) ; 37
   (place 37 'empty '() null-action) ; 38
   (place 38 'empty '() null-action) ; 39
   (place 39 'empty '() null-action) ; 40
   (place 40 'empty '() null-action) ; 41
   (place 41 'empty '() null-action) ; 42
   (place 42 'empty '() null-action) ; 43
   (place 43 'empty '() null-action) ; 44
   (place 44 'empty '() null-action) ; 45
   (place 45 'empty '() null-action) ; 46
   (place 46 'empty '() null-action) ; 47
   (place 47 'empty '() null-action) ; 48
   (place 48 'empty '() null-action) ; 49
   (place 49 'empty '() null-action) ; 50
   (place 50 'empty '() null-action) ; 51
   (place 51 'empty '() null-action) ; 52
   (place 52 'empty '() null-action) ; 53
   (place 53 'empty '() null-action) ; 54
   (place 54 'empty '() null-action) ; 55
   (place 55 'empty '() null-action) ; 56
   (place 56 'empty '() null-action) ; 57
   (place 57 'empty '() null-action) ; 58
   (place 58 'empty '() null-action) ; 59
   (place 59 'empty '() null-action))) ; 60

;; hmm just stored for callbacks here
(define player-choice 'none)

(define (build-seeds type cost)
  (let ((name (mtext-lookup type)))
    (horiz
     (text-view 0 name 20 (layout 'fill-parent 'wrap-content 1 'centre 10))
     (text-view 0 (number->string cost) 20 (layout 'fill-parent 'wrap-content 1 'centre 10))
     (button (make-id (string-append "buy-" name)) (mtext-lookup 'buy)
             20 (layout 'fill-parent 'wrap-content 1 'centre 10)
             (lambda ()
               (set! player-choice (string->symbol (string-append "buy-" (symbol->string type))))
               (msg "set player choice to " type)
               '())))))

(define (build-shop game-view game-board)
  (update-widget 'linear-layout (get-id "left-display") 'contents
                 (list
                  (build-seeds 'wheat 200)
                  (build-seeds 'potato 100)
                  (build-seeds 'onion 150)
                  (mbutton
                   'shop-done
                   (lambda ()
                     (game-player-choice! player-choice)
                     (game-change-state! 'end)
                     (cons
                      (clear-left-display)
                      (render-interface))
                     )))))

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
                 (layout 'wrap-content 'fill-parent 0.2 'centre 10))

     (if (eq? (place-code place) 'shop)
         (horiz
          ;; dispatch to ui for this card
          (mbutton 'card-yes (lambda () (list (build-shop game-view game-board))))

          ;; maybe not for all cards?
          (mbutton 'card-no
                  (lambda ()
                    (game-change-state! 'end)
                    (cons
                     (clear-left-display)
                     (render-interface)))))

         (horiz
          ;; dispatch to ui for this card
          (mbutton 'card-ok
                  (lambda ()
                    (cond
                     ((eq? (place-code place) 'another-go)
                      (game-change-state! 'dice)
                      (render-interface))
                     (else
                      (game-player-choice! 'no-choice)
                      (game-change-state! 'end)
                      (cons
                       (clear-left-display)
                       (render-interface)))))
                  )))



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
              (image-view 0 (if (crop-task-complete 'irrigate) "irrigate_icon" "irrigate_icon_grey")
                          (layout 'wrap-content 'wrap-content 1 'centre 2))
              (text-view 0 (mtext-lookup 'irrigate) 15 (layout 'wrap-content 'wrap-content 1 'centre 2)))
             (vert
              (image-view 0 (if (crop-task-complete 'weed) "weed_icon" "weed_icon_grey")
                          (layout 'wrap-content 'wrap-content 1 'centre 2))
              (text-view 0 (mtext-lookup 'weed) 15 (layout 'wrap-content 'wrap-content 1 'centre 2)))
             (vert
              (image-view 0 (if (crop-task-complete 'fertilise) "fertilise_icon" "fertilise_icon_grey")
                          (layout 'wrap-content 'wrap-content 1 'centre 2))
              (text-view 0 (mtext-lookup 'fertilise) 15 (layout 'wrap-content 'wrap-content 1 'centre 2)))
             (vert
              (image-view 0 (if (crop-task-complete 'pest) "pest_icon" "pest_icon_grey")
                          (layout 'wrap-content 'wrap-content 1 'centre 2))
              (text-view 0 (mtext-lookup 'pests) 15 (layout 'wrap-content 'wrap-content 1 'centre 2)))
             (vert
              (image-view 0 (if (crop-task-complete 'disease) "disease_icon" "disease_icon_grey")
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
                 (render-interface)))))))

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
                (append
                 (render-interface)
                 (list
                  (toast (string-append (player-name player) " did..." (symbol->string choice)))))
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
                (new-player (mtext-lookup 'player-1) 'human 0 (make-player-view (vector 1 0 0)))
                (new-player (mtext-lookup 'player-2) 'ai 0 (make-player-view (vector 0 1 0)))
                (new-player (mtext-lookup 'player-3) 'ai 0 (make-player-view (vector 0 0 1)))
                (new-player (mtext-lookup 'player-4) 'human 0 (make-player-view (vector 1 0 1)))
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
