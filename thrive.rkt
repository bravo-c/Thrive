#| author: Christopher Bravo             |#
#| title: Thrive                         |#
#|                                       |#
#| Thrive is an innovative take          |#
#| on a graphical game in ISL+lambda.    |#
#|                                       |#
#|            Have fun!                  |#
#|                                       |#
#|---------------------------------------|#

; a World is a (make-world Number Player [List-of Enemy] [List-of Food])
(define-struct world [time player loe lof])

; a Player is a (make-Player Posn Number Posn Boolean)
;    where the Posn is the player's position
;    and the Number represents the player's health points
;    and the Symbol represents the color of the player
;    and the second Posn represents the movement vector of the Player
;    and the Boolean represents if the Player is invulnerable 
;    and the second Number represents the invulnerability timer
(define-struct player [pos hp color vector inv counter])

; an Enemy is a (make-enemy Posn Number)
;    where the Posn is the enemy's position
;    and the Number represents the damage the enemy causes on impact
(define-struct enemy [pos dmg])

; a Food is a (make-food Posn Number)
;    where the Posn is the Food's position
;    and the Number represents the amount of hp the food heals for
(define-struct food [pos hp])

;; DEFINITION OF CONSTANTS BELOW~

(define move-speed 5) ; maximum move-speed of the player
(define colorz (list 'red 'green 'blue 'yellow 'purple)) ; potential player colors
(define BOARD_WIDTH 500) ; width of the board
(define BOARD_HEIGHT BOARD_WIDTH) ; height of the board
(define BACKGROUND (empty-scene BOARD_WIDTH BOARD_HEIGHT)) ; the background board
(define ENEMY_SPEED .5) ; speed of the enemy
(define FRICTION_VAL .01) ; constant friction applied to the movement of the Player
(define INV_COST 500) ; cost to use invulnerability

;; INITAL STATE OF THE GAME: use "(main INIT_STATE)" to start the game~
(define INIT_PLAYER (make-player (make-posn (/ BOARD_WIDTH 2) (/ BOARD_HEIGHT 2))
                                 100
                                 'red
                                 (make-posn 0 0)
                                 #false
                                 0))
(define INIT_STATE (make-world 0 INIT_PLAYER '() '()))

; the game is run from here
(define (main x)
  (big-bang x
            [on-tick   tock]
            [to-draw   render]
            [on-key    move]
            [stop-when game-over]))

; tock: World -> World
; updates the world on tick
(define (tock x)
  (make-world (+ (world-time x) 1)
              (make-player (update-pos (world-player x))
                           (update-player-hp (world-player x)
                                             (player-hp (world-player x))
                                             (list-overlapped-e x)
                                             (list-overlapped-f x))
                           (update-color x)
                           (friction-update (player-vector (world-player x)))
                           (inv-check (player-inv (world-player x))
                                      (player-counter (world-player x)))
                           (update-counter (world-player x)
                                           (player-counter (world-player x))))
              (move-enemies (add-enemy x (remove-overlapped-e x)) (player-pos (world-player x)))
              (add-food x (remove-overlapped-f x))))

; inv-check: Boolean Number -> Boolean
; checks to see if the invulnerability has run out and updates it as such
(define (inv-check b-in count)
  (if (not b-in)
      #false
      (< count 100)))

; update-counter: Player Number -> Number
; updates the invulnerability counter if it is active
(define (update-counter player count)
  (if (player-inv player)
      (+ count 1)
      0))

; render: World -> Image 
; renders the world 
(define (render x)
  (overlay
   ;(text (bool->string (player-inv (world-player x))) 24 "red")
   (text (number->string (player-hp (world-player x))) 24 "olive")
   (place-image (player->image x (world-player x))
               (posn-x (player-pos (world-player x)))
               (posn-y (player-pos (world-player x)))
               (place-foods x (place-enemies x BACKGROUND)))))

; bool->string: Boolean -> String
; represents a boolean as a String
(define (bool->string b)
  (if b
      "on"
      "off"))

; move: World KeyEvent -> World
; manipulates the player's velocity
(define (move x ke)
  (cond
    [(and (> (player-hp (world-player x)) INV_COST) (key=? ke "i"))
     (make-world (world-time x)
                 (make-player (player-pos (world-player x))
                              (- (player-hp (world-player x)) INV_COST)
                              (player-color (world-player x))
                              (player-vector (world-player x))
                              #true
                              (player-counter (world-player x)))
                 (world-loe x)
                 (world-lof x))]
    [(key=? ke "right") (make-world (world-time x)
                                    (player-move 'r (world-player x))
                                    (world-loe x)
                                    (world-lof x))]
    [(key=? ke "left") (make-world (world-time x)
                                   (player-move 'l (world-player x))
                                   (world-loe x)
                                   (world-lof x))]
    [(key=? ke "up") (make-world (world-time x)
                                 (player-move 'u (world-player x))
                                 (world-loe x)
                                 (world-lof x))]
    [(key=? ke "down") (make-world (world-time x)
                                   (player-move 'd (world-player x))
                                   (world-loe x)
                                   (world-lof x))]
    [else x]))

; friction-update: Posn -> Posn
; apply friction to the movement vector
(define (friction-update pos)
  (make-posn (cond
               [(and (< (posn-x pos) 0) (>= (posn-x pos) -5))
                (+ (posn-x pos) FRICTION_VAL)]
               [(and (> (posn-x pos) 0) (<= (posn-x pos) 5))
                (- (posn-x pos) FRICTION_VAL)]
               [else (posn-x pos)])
             (cond
               [(and (< (posn-y pos) 0) (>= (posn-y pos) -5))
                (+ (posn-y pos) FRICTION_VAL)]
               [(and (> (posn-y pos) 0) (<= (posn-y pos) 5))
                (- (posn-y pos) FRICTION_VAL)]
               [else (posn-y pos)])))

; update-pos: Player -> Posn
; updates the Player's speed using it's movement vector
(define (update-pos player)
  (make-posn (+ (posn-x (player-vector player)) (posn-x (player-pos player)))
             (+ (posn-y (player-vector player)) (posn-y (player-pos player)))))

; game-over: World -> Boolean
; determines if the game is over
(define (game-over w)
  (<= (player-hp (world-player w)) 0))

; update-color: World -> Symbol
; based on the time, chooses a random color from the list of colors
(define (update-color w)
  (if (= (modulo (world-time w) 3) 0)
      (list-get colorz (random (length colorz)))
      (player-color (world-player w))))

; remove-overlapped-e: World -> [List-of Enemy]
; updates the enemies based on the state of the world
(define (remove-overlapped-e w)
  (local [(define p (player-pos (world-player w)))]
    (filter (lambda (x) (not (posn=? (enemy-pos x) p))) (world-loe w))))

; list-overlapped-e: World -> [List-of Enemy]
; creates a list of the overlapped Enemies for updating player hp
(define (list-overlapped-e w)
  (local [(define p (player-pos (world-player w)))]
    (filter (lambda (x) (posn=? (enemy-pos x) p)) (world-loe w))))

; remove-overlapped: World -> [List-of Food]
; updates the enemies based on the state of the world
(define (remove-overlapped-f w)
  (local [(define p (player-pos (world-player w)))]
    (filter (lambda (x) (not (posn=? (food-pos x) p))) (world-lof w))))

; list-overlapped-f: World -> [List-of Food]
; creates a list of the overlapped Foods for updating player hp
(define (list-overlapped-f w)
  (local [(define p (player-pos (world-player w)))]
    (filter (lambda (x) (posn=? (food-pos x) p)) (world-lof w))))

; update-player-hp: Player Number [List-of Enemy] [List-of Food] -> Number
; updates the player based on the lists of overlapping Enemies and Foods and invuln state
(define (update-player-hp player n loe lof)
  (if (player-inv player)
      (+ (foldr + 0 (hp-list lof)) n)
      (+ (* -1 (foldr + 0 (dmg-list loe)))
         (foldr + 0 (hp-list lof))
         n)))

;(define e1 (make-enemy (make-posn 0 0) 10))
;(define f1 (make-food (make-posn 0 0) 10))
;(check-expect (update-player-hp 100 '() '()) 100)
;(check-expect (update-player-hp 100 (list e1 e1 e1) '()) 70)
;(check-expect (update-player-hp 100 (list e1 e1) '()) 80)

; dmg-list: [List-of Enemy] -> [List-of Number]
; gets the list of damage values from a list of Enemies
(define (dmg-list loe)
  (cond
    [(empty? loe) '()]
    [else (cons (enemy-dmg (first loe)) (dmg-list (rest loe)))]))

;(check-expect (dmg-list '()) '())
;(check-expect (dmg-list (list e1)) '(10))
;(check-expect (dmg-list (list e1 e1)) '(10 10))
;(check-expect (dmg-list (list e1 e1 e1)) '(10 10 10))

; hp-list [List-of Food] -> [List-of Number]
; gets the list of heal values from a list of Foods
(define (hp-list lof)
  (cond
    [(empty? lof) '()]
    [else (cons (food-hp (first lof)) (hp-list (rest lof)))]))

;(check-expect (hp-list '()) '())
;(check-expect (hp-list (list f1)) '(10))
;(check-expect (hp-list (list f1 f1)) '(10 10))
;(check-expect (hp-list (list f1 f1 f1)) '(10 10 10))

; place-enemies: World Image -> Image
; renders all the enemies onto the BACKGROUND
(define (place-enemies x img)
  (foldr place-enemy BACKGROUND (world-loe x)))

; place-enemy: Enemy Image -> Image
; renders an enemy onto img
(define (place-enemy e img)
  (place-image (rectangle 15
                          15
                          "solid"
                          "black")
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
  (place-image (circle 5
                       "solid"
                       "orange")
               (posn-x (food-pos f))
               (posn-y (food-pos f))
               img))

; player-image: World Player -> Image
; creates the image of the player based on the color from the world
(define (player->image world p)
  (rectangle 15
             15
             "solid"
             (symbol->string (player-color p))))

; list-get: [List-of X] Nat -> X
; a utility function to get an item from a list
(define (list-get li idx)
  (if (= idx 0)
      (first li)
      (list-get (rest li) (- idx 1))))

; player-move: Symbol Player -> Player
; updates the velocity of player
; TODO consider doing an instant flip?
(define (player-move sym player)
  (cond
    [(symbol=? sym 'l) (make-player (player-pos player)
                                    (player-hp player)
                                    (player-color player)
                                    (if (< (abs (- (posn-x (player-vector player)) 1)) move-speed)
                                        (make-posn (- (posn-x (player-vector player)) 1) 
                                                   (posn-y (player-vector player)))
                                        (player-vector player))
                                    (player-inv player)
                                    (player-counter player))]
    [(symbol=? sym 'r) (make-player (player-pos player)
                                    (player-hp player)
                                    (player-color player)
                                    (if (< (abs (+ (posn-x (player-vector player)) 1)) move-speed)
                                        (make-posn (+ (posn-x (player-vector player)) 1) 
                                                   (posn-y (player-vector player)))
                                        (player-vector player))
                                    (player-inv player)
                                    (player-counter player))]
    [(symbol=? sym 'u) (make-player (player-pos player)
                                    (player-hp player)
                                    (player-color player)
                                    (if (< (abs (- (posn-y (player-vector player)) 1)) move-speed)
                                        (make-posn (posn-x (player-vector player))
                                                   (- (posn-y (player-vector player)) 1))
                                        (player-vector player))
                                    (player-inv player)
                                    (player-counter player))]
    [(symbol=? sym 'd) (make-player (player-pos player)
                                    (player-hp player)
                                    (player-color player)
                                    (if (< (abs (+ (posn-y (player-vector player)) 1)) move-speed)
                                        (make-posn (posn-x (player-vector player)) 
                                                   (+ (posn-y (player-vector player)) 1))
                                        (player-vector player))
                                    (player-inv player)
                                    (player-counter player))]
    [else player]))

; enemy-move: Enemy Posn -> Enemy
; have the enemy move towards a given position
(define (enemy-move e p)
  (cond
    [(and (< (posn-x (enemy-pos e)) (posn-x p))
          (< (posn-y (enemy-pos e)) (posn-y p)))
     (make-enemy (make-posn (+ (posn-x (enemy-pos e)) ENEMY_SPEED)
                            (+ (posn-y (enemy-pos e)) ENEMY_SPEED))
                 (enemy-dmg e))]
    [(and (> (posn-x (enemy-pos e)) (posn-x p))
          (> (posn-y (enemy-pos e)) (posn-y p)))
     (make-enemy (make-posn (- (posn-x (enemy-pos e)) ENEMY_SPEED)
                            (- (posn-y (enemy-pos e)) ENEMY_SPEED))
                 (enemy-dmg e))]
    [(and (> (posn-x (enemy-pos e)) (posn-x p))
          (< (posn-y (enemy-pos e)) (posn-y p)))
     (make-enemy (make-posn (- (posn-x (enemy-pos e)) ENEMY_SPEED)
                            (+ (posn-y (enemy-pos e)) ENEMY_SPEED))
                 (enemy-dmg e))]
    [(and (< (posn-x (enemy-pos e)) (posn-x p))
          (> (posn-y (enemy-pos e)) (posn-y p)))
     (make-enemy (make-posn (+ (posn-x (enemy-pos e)) ENEMY_SPEED)
                            (- (posn-y (enemy-pos e)) ENEMY_SPEED))
                 (enemy-dmg e))]
    [(and (= (posn-x (enemy-pos e)) (posn-x p))
          (< (posn-y (enemy-pos e)) (posn-y p)))
     (make-enemy (make-posn (posn-x (enemy-pos e))
                            (+ (posn-y (enemy-pos e)) ENEMY_SPEED))
                 (enemy-dmg e))]
    [(and (= (posn-x (enemy-pos e)) (posn-x p))
          (> (posn-y (enemy-pos e)) (posn-y p)))
     (make-enemy (make-posn (posn-x (enemy-pos e))
                            (- (posn-y (enemy-pos e)) ENEMY_SPEED))
                 (enemy-dmg e))]
    [(and (> (posn-x (enemy-pos e)) (posn-x p))
          (= (posn-y (enemy-pos e)) (posn-y p)))
     (make-enemy (make-posn (- (posn-x (enemy-pos e)) ENEMY_SPEED)
                            (posn-y (enemy-pos e)))
                 (enemy-dmg e))]
    [(and (< (posn-x (enemy-pos e)) (posn-x p))
          (= (posn-y (enemy-pos e)) (posn-y p)))
     (make-enemy (make-posn (+ (posn-x (enemy-pos e)) ENEMY_SPEED)
                            (posn-y (enemy-pos e)))
                 (enemy-dmg e))]
    [else e]))

; posn=?: Posn Posn -> Boolean
; compares two Posns for pseudo-equality
(define (posn=? p1 p2)
  (and (<= (abs (- (posn-x p1) (posn-x p2))) 15)
       (<= (abs (- (posn-y p1) (posn-y p2))) 15)))

; move-enemies: [List-of Enemy] Posn -> [List-of Enemy]
; move all the enemies in a list
(define (move-enemies loe p)
  (map (lambda (x) (enemy-move x p)) loe)) 

; add-enemy: World [List-of Enemy] -> [List-of Enemy]
; constructs a new Enemy at a random location
(define (add-enemy w l)
  (if (= (modulo (world-time w) 15) 0)
      (cons (make-enemy (make-posn (random BOARD_WIDTH) (random BOARD_HEIGHT)) (random 10)) l)
      l))

; add-food: World [List-of Food] -> [List-of Food]
; constructs a new Food at a random location
(define (add-food w l)
  (if (= (modulo (world-time w) 18) 0)
      (cons (make-food (make-posn (random BOARD_WIDTH) (random BOARD_HEIGHT)) (random 10)) l)
      l))
