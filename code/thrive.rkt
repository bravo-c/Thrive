#lang racket

(require 2htdp/universe
         2htdp/image)
(require rackunit)

;; a Posn is a x,y coordinate pair (posn Rational Rational)
(struct posn ([x #:mutable]
              [y #:mutable]))

;; a World is a (world WholeNum Player (Listof Enemy) (Listof Food))
(struct world ([time   #:mutable]
               [player #:mutable]
               [loe    #:mutable]
               [lof    #:mutable]))

;; a Player is a (player Posn Number Symbol Posn Boolean Number)
;;    where the Posn is the player's position
;;    and the Number represents the player's health points
;;    and the Symbol represents the color of the player
;;    and the second Posn represents the movement vector of the Player
;;    and the Boolean represents if the Player is invulnerable 
;;    and the second Number represents the invulnerability timer
(struct player ([pos     #:mutable]
                [hp      #:mutable]
                [color   #:mutable]
                [vector  #:mutable]
                [inv     #:mutable]
                [counter #:mutable]))

;; an Enemy is a (enemy Posn Number)
;;    where the Posn is the enemy's position
;;    and the Number represents the damage the enemy causes on impact
(struct enemy ([pos #:mutable]
               [dmg #:mutable]))

;; a Food is a (food Posn Number)
;;    where the Posn is the Food's position
;;    and the Number represents the amount of hp the food heals for
(struct food ([pos #:mutable]
              [hp  #:mutable]))

;; CONSTANTS
(define MAX_SPEED 5) ; maximum move-speed of the player
(define COLOR_LIST (list 'red 'green 'blue 'yellow 'purple)) 
(define BOARD_WIDTH 500) 
(define BOARD_HEIGHT BOARD_WIDTH)
(define CENTER (posn (/ BOARD_WIDTH 2) (/ BOARD_HEIGHT 2)))
(define BACKGROUND (empty-scene BOARD_WIDTH BOARD_HEIGHT))
(define GAME_OVER
  (place-image (text "Game Over :(" 36 "indigo")
               (posn-x CENTER)
               (posn-y CENTER)
               BACKGROUND))
(define ENEMY_SPEED .5) 
(define MOMENTUM_VAL .01) 
(define INV_COST 500) 
(define ENEMY_IMG (rectangle 15 15 "solid" "black"))
(define FOOD_IMG (circle 5 "solid" "orange"))
(define NEW_E_TICKS 15)
(define NEW_F_TICKS 18)
(define NEW_C_TICKS 3)

;; INITAL STATE OF THE GAME:
;; use "(main INIT_STATE)" to start the game~
(define INIT_PLAYER (player CENTER
                            100
                            'red
                            (posn 0 0)
                            #false
                            0))
(define INIT_STATE (world 0 INIT_PLAYER '() '()))

;; the game is run from here
(define (main x)
  (big-bang x
    [on-tick   tock                 ]
    [to-draw   render               ]
    [on-key    move                 ]
    [stop-when game-over render-end ]))

;; tock: World -> World
;; updates the world on tick
(define (tock x)
  (let ([p (world-player x)])
    (set-world-time! x (add1 (world-time x))) 
    (set-world-player! x (update-player x)) 
    (set-world-loe! x (move-enemies (add-enemy x (remove-overlapped-e x)) (player-pos p))) 
    (set-world-lof! x (add-food x (remove-overlapped-f x))) 
    x))

;; update-player : World -> Player
;; helper function for on-tick, mutates then returns player
(define (update-player w)
  (let ([p (world-player w)])
    (set-player-pos! p (update-pos p))
    (set-player-hp! p (update-player-hp p (player-hp p) (list-overlapped-e w) (list-overlapped-f w)))
    (set-player-color! p (update-color w))
    (set-player-vector! p (momentum-update (player-vector p)))
    (set-player-inv! p (inv-check (player-inv p) (player-counter p)))
    (set-player-counter! p (update-counter p (player-counter p)))
    p))

;; inv-check: Boolean Number -> Boolean
;; checks to see if the invulnerability has run out and updates it as such
(define (inv-check b-in count)
  (if (not b-in)
      #false
      (< count 100)))

;; update-counter: Player Number -> Number
;; updates the invulnerability counter if it is active
(define (update-counter player count)
  (if (player-inv player)
      (+ count 1)
      0))

;; TODO (1) figure out how to do scrolling in render?

;; render: World -> Image 
;; renders the world
(define (render x)
  (overlay
   (text (number->string (player-hp (world-player x))) 24 "olive")
   (place-image (player->image (world-player x))
                (posn-x (player-pos (world-player x)))
                (posn-y (player-pos (world-player x)))
                (place-foods x (place-enemies x BACKGROUND)))))

;; render-end: World -> Image
;; produces the game-over screen
(define (render-end w)
  GAME_OVER)

;; player-move: Symbol Player -> Player
;; updates the velocity of player
(define (player-move sym player)
  (let ([new-left  (- (posn-x (player-vector player)) 1)]
        [new-right (+ (posn-x (player-vector player)) 1)]
        [new-up    (- (posn-y (player-vector player)) 1)]
        [new-down  (+ (posn-y (player-vector player)) 1)])
    (cond
      ; TODO (1) there should probably be a function handling the logic
      ; for whether or not the update to speed is valid per MAX_SPEED
      [(symbol=? sym 'l)
       (when (< (abs new-left) MAX_SPEED)
         (set-posn-x! (player-vector player) new-left))
       player]
      [(symbol=? sym 'r)
       (when (< (abs new-right) MAX_SPEED)
         (set-posn-x! (player-vector player) new-right))
       player]
      [(symbol=? sym 'u)
       (when (< (abs new-up) MAX_SPEED)
         (set-posn-y! (player-vector player) new-up))
       player]
      [(symbol=? sym 'd)
       (when (< (abs new-down) MAX_SPEED)
         (set-posn-y! (player-vector player) new-down))
       player]
      [else player])))

;; move: World KeyEvent -> World
;; manipulates the player's velocity
(define (move x ke)
  (cond
    [(and (> (player-hp (world-player x)) INV_COST) (key=? ke "i"))
     (set-player-inv! (world-player x) #t)
     (set-player-hp! (world-player x) (- (player-hp (world-player x)) INV_COST))
     x]
    [(or (key=? ke "right") (key=? ke "d"))
     (set-world-player! x (player-move 'r (world-player x))) x]
    [(or (key=? ke "left") (key=? ke "a"))
     (set-world-player! x (player-move 'l (world-player x))) x]
    [(or (key=? ke "up") (key=? ke "w"))
     (set-world-player! x (player-move 'u (world-player x))) x]
    [(or (key=? ke "down") (key=? ke "s"))
     (set-world-player! x (player-move 'd (world-player x))) x]
    [else x]))

;; momentum-update: Posn -> Posn
;; update movement vector per momentum spec
(define (momentum-update pos)
  (posn (cond
          [(and (< (posn-x pos) 0) (>= (posn-x pos) (- MAX_SPEED)))
           (+ (posn-x pos) MOMENTUM_VAL)]
          [(and (> (posn-x pos) 0) (<= (posn-x pos) MAX_SPEED))
           (- (posn-x pos) MOMENTUM_VAL)]
          [else (posn-x pos)])
        (cond
          [(and (< (posn-y pos) 0) (>= (posn-y pos) (- MAX_SPEED)))
           (+ (posn-y pos) MOMENTUM_VAL)]
          [(and (> (posn-y pos) 0) (<= (posn-y pos) MAX_SPEED))
           (- (posn-y pos) MOMENTUM_VAL)]
          [else (posn-y pos)])))

; update-pos: Player -> Posn
; updates the Player's speed using it's movement vector
(define (update-pos player)
  (posn (+ (posn-x (player-vector player)) (posn-x (player-pos player)))
        (+ (posn-y (player-vector player)) (posn-y (player-pos player)))))

; game-over: World -> Boolean
; determines if the game is over
(define (game-over w)
  (<= (player-hp (world-player w)) 0))

; update-color: World -> Symbol
; choose a new, random color from color-list every NEW_C_TICKS ticks
(define (update-color w)
  (if (= (modulo (world-time w) NEW_C_TICKS) 0)
      (list-ref COLOR_LIST (random (length COLOR_LIST)))
      (player-color (world-player w))))

;; posn-close?: Posn Posn -> Boolean
;; compares two Posns for pseudo-equality
(define (posn-close? p1 p2)
  (and (<= (abs (- (posn-x p1) (posn-x p2))) 15)
       (<= (abs (- (posn-y p1) (posn-y p2))) 15)))

; remove-overlapped-e: World -> [List-of Enemy]
; updates the enemies based on the state of the world
(define (remove-overlapped-e w)
  (local [(define p (player-pos (world-player w)))]
    (filter (lambda (x) (not (posn-close? (enemy-pos x) p))) (world-loe w))))

; list-overlapped-e: World -> [List-of Enemy]
; creates a list of the overlapped Enemies for updating player hp
(define (list-overlapped-e w)
  (local [(define p (player-pos (world-player w)))]
    (filter (lambda (x) (posn-close? (enemy-pos x) p)) (world-loe w))))

; remove-overlapped: World -> [List-of Food]
; updates the enemies based on the state of the world
(define (remove-overlapped-f w)
  (local [(define p (player-pos (world-player w)))]
    (filter (lambda (x) (not (posn-close? (food-pos x) p))) (world-lof w))))

; list-overlapped-f: World -> [List-of Food]
; creates a list of the overlapped Foods for updating player hp
(define (list-overlapped-f w)
  (local [(define p (player-pos (world-player w)))]
    (filter (lambda (x) (posn-close? (food-pos x) p)) (world-lof w))))

; hp-list [List-of Food] -> [List-of Number]
; gets the list of heal values from a list of Foods
(define (hp-list lof)
  (map (lambda (a-food) (food-hp a-food)) lof))

; for tests
(define e1 (enemy (posn 0 0) 10))
(define f1 (food (posn 0 0) 10))
; tests
(check-equal? (hp-list '()) '())
(check-equal? (hp-list (list f1)) '(10))
(check-equal? (hp-list (list f1 f1)) '(10 10))
(check-equal? (hp-list (list f1 f1 f1)) '(10 10 10))

; dmg-list: [List-of Enemy] -> [List-of Number]
; gets the list of damage values from a list of Enemies
(define (dmg-list loe)
  (map (lambda (an-enemy) (enemy-dmg an-enemy)) loe))

; tests
(check-equal? (dmg-list '()) '())
(check-equal? (dmg-list (list e1)) '(10))
(check-equal? (dmg-list (list e1 e1)) '(10 10))
(check-equal? (dmg-list (list e1 e1 e1)) '(10 10 10))

; update-player-hp: Player Number [List-of Enemy] [List-of Food] -> Number
; updates the player based on the lists of overlapping Enemies and Foods and invuln state
(define (update-player-hp player cur-hp loe lof)
  (let ([hp-gain (foldr + 0 (hp-list lof))]
        [hp-loss (* -1 (foldr + 0 (dmg-list loe)))])
    (if (player-inv player)
        (+ hp-gain cur-hp)
        (+ hp-loss hp-gain cur-hp))))

; tests
(check-equal? (update-player-hp INIT_PLAYER 100 '() '()) 100)
(check-equal? (update-player-hp INIT_PLAYER 100 (list e1 e1 e1) '()) 70)
(check-equal? (update-player-hp INIT_PLAYER 100 (list e1 e1) '()) 80)
(check-equal? (update-player-hp INIT_PLAYER 100 (list e1) (list f1)) 100)

; place-enemies: World Image -> Image
; renders all the enemies onto the BACKGROUND
(define (place-enemies x img)
  (foldr place-enemy BACKGROUND (world-loe x)))

; place-enemy: Enemy Image -> Image
; renders an enemy onto img
(define (place-enemy e img)
  (place-image ENEMY_IMG
               (posn-x (enemy-pos e))
               (posn-y (enemy-pos e))
               img))

; place-foods: World Image -> Image
; renders all the Foods onto the img
(define (place-foods w img)
  (foldr place-food img (world-lof w)))

; place-foods: Food Image -> Image
; renders a Food onto the img
(define (place-food f img)
  (place-image FOOD_IMG
               (posn-x (food-pos f))
               (posn-y (food-pos f))
               img))

; player-image: Player -> Image
; creates the image of the player based on the color from the world
(define (player->image p)
  (rectangle 15
             15
             "solid"
             (symbol->string (player-color p))))

;; enemy-move: Enemy Posn -> Enemy
;; have the enemy move towards a given position
(define (enemy-move e p)
  (cond
    [(and (< (posn-x (enemy-pos e)) (posn-x p))
          (< (posn-y (enemy-pos e)) (posn-y p)))
     (enemy (posn (+ (posn-x (enemy-pos e)) ENEMY_SPEED)
                  (+ (posn-y (enemy-pos e)) ENEMY_SPEED))
            (enemy-dmg e))]
    [(and (> (posn-x (enemy-pos e)) (posn-x p))
          (> (posn-y (enemy-pos e)) (posn-y p)))
     (enemy (posn (- (posn-x (enemy-pos e)) ENEMY_SPEED)
                  (- (posn-y (enemy-pos e)) ENEMY_SPEED))
            (enemy-dmg e))]
    [(and (> (posn-x (enemy-pos e)) (posn-x p))
          (< (posn-y (enemy-pos e)) (posn-y p)))
     (enemy (posn (- (posn-x (enemy-pos e)) ENEMY_SPEED)
                  (+ (posn-y (enemy-pos e)) ENEMY_SPEED))
            (enemy-dmg e))]
    [(and (< (posn-x (enemy-pos e)) (posn-x p))
          (> (posn-y (enemy-pos e)) (posn-y p)))
     (enemy (posn (+ (posn-x (enemy-pos e)) ENEMY_SPEED)
                  (- (posn-y (enemy-pos e)) ENEMY_SPEED))
            (enemy-dmg e))]
    [(and (= (posn-x (enemy-pos e)) (posn-x p))
          (< (posn-y (enemy-pos e)) (posn-y p)))
     (enemy (posn (posn-x (enemy-pos e))
                  (+ (posn-y (enemy-pos e)) ENEMY_SPEED))
            (enemy-dmg e))]
    [(and (= (posn-x (enemy-pos e)) (posn-x p))
          (> (posn-y (enemy-pos e)) (posn-y p)))
     (enemy (posn (posn-x (enemy-pos e))
                  (- (posn-y (enemy-pos e)) ENEMY_SPEED))
            (enemy-dmg e))]
    [(and (> (posn-x (enemy-pos e)) (posn-x p))
          (= (posn-y (enemy-pos e)) (posn-y p)))
     (enemy (posn (- (posn-x (enemy-pos e)) ENEMY_SPEED)
                  (posn-y (enemy-pos e)))
            (enemy-dmg e))]
    [(and (< (posn-x (enemy-pos e)) (posn-x p))
          (= (posn-y (enemy-pos e)) (posn-y p)))
     (enemy (posn (+ (posn-x (enemy-pos e)) ENEMY_SPEED)
                  (posn-y (enemy-pos e)))
            (enemy-dmg e))]
    [else e]))

;; move-enemies: (Listof Enemy) Posn -> (Listof Enemy)
;; move all the enemies in a list
(define (move-enemies loe p)
  (map (lambda (x) (enemy-move x p)) loe)) 

;; === TODO abstract add-enemy, add-food to add-generic with extra arg? === ;;

;; add-enemy: World (Listof Enemy) -> (Listof Enemy)
;; constructs a new Enemy at a random location every NEW_E_TICKS ticks 
(define (add-enemy w l)
  (if (= (modulo (world-time w) NEW_E_TICKS) 0)
      (cons (enemy (posn (random BOARD_WIDTH)
                         (random BOARD_HEIGHT))
                   (random 10))
            l)
      l))

;; add-food: World (Listof Food) -> (Listof Food)
;; constructs a new Food at a random location every NEW_F_TICKS ticks 
(define (add-food w l)
  (if (= (modulo (world-time w) NEW_F_TICKS) 0)
      (cons (food (posn (random BOARD_WIDTH)
                        (random BOARD_HEIGHT))
                  (random 10))
            l)
      l))
