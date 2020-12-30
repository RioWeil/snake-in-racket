;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/image)
(require 2htdp/universe)
(require spd/tags)
(require racket/list)

;; My Snake program
;; Written by Rio Weil, 2020/12/29

(@htdw GameState)
;; =================
;; Constants:

(define WIDTH 400)  ;; Must be multiples of (* 2 CELL-SIZE)
(define HEIGHT 400) ;; Game board will have (/ WIDTH CELL-SIZE) * (/ HEIGHT CELL-SIZE) cells.

(define TICK-SPEED 0.1) ;; Advances game every TICK-SPEED seconds.

(define CELL-SIZE 20)

(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))

(define TOTAL-CELLS (* (/ WIDTH CELL-SIZE) (/ HEIGHT CELL-SIZE)))

(define SNAKE-BODY (square (* CELL-SIZE (/ 9 10)) "solid" "black"))
(define FOOD (square CELL-SIZE "solid" "red"))

(define MTS (empty-scene WIDTH HEIGHT))

(define START-TEXT (above (text "Snake in Racket" 50 "black")
                          (text "By Rio Weil" 32 "black")
                          (square 10 "solid" "white")
                          (text "Click to start!" 32 "black")
                          (square 10 "solid" "white")
                          (text "Rules:" 32 "blue")
                          (text "1. Control the snake (black squares) with the arrow keys." 14 "blue")
                          (text "2. Collect food (red squares) to make your snake grow." 14 "blue")
                          (text "3. You win if your snake covers the whole screen." 14  "blue")
                          (text "4. You lose if the head of the snake collides with a wall or itself." 14 "blue")))
(define GAME-OVER-TEXT (above (text "GAME OVER" 50 "black")
                              (text "Click to play again!" 32 "black")))
(define WIN-TEXT (above (text "You won!" 50 "black")
                        (text "Click to play again!" 32 "black")))

(define START-SCREEN (overlay START-TEXT MTS))
(define GAME-OVER-SCREEN (overlay GAME-OVER-TEXT MTS))
(define WIN-SCREEN (overlay WIN-TEXT MTS))

;; =================
;; Data definitions:
(@htdd Direction)
;; Direction is one-of:
;;           - "left"
;;           - "right"
;;           - "up"
;;           - "down"
;; Examples redundant for enumerations

(define (fn-for-dir d)
  (cond [(string=? "left" d) (...)]
        [(string=? "right" d) (...)]
        [(string=? "up" d) (...)]
        [else (...)]))
                
(@htdd Position)
(define-struct pos (x y))
;; Position is (make-pos Natural Natural)
;; interp. is a position of some cell on the board with x, y coords
;; Coordinates can be multiplied by CELL-SIZE to give screen coords
;; CONSTRAINT: x is between 0 and (/ WIDTH CELL-SIZE) - 1
;;             y is between 0 and (/ HEIGHT CELL-SIZE) - 1
(define POS0 (make-pos 0 0))
(define POS1 (make-pos CTR-X CTR-Y))
(define POS2 (make-pos 0 CTR-Y))

(define (fn-for-pos p)
  (... (pos-x p)
       (pos-y p)))


(@htdd GameRun)
;; GameRun is one of:
;;         - "start"
;;         - "running"
;;         - "game over"
;;         - "win"
;; Examples redundant for enumerations

(define (fn-for-gr g)
  (cond [(string=? "start" g) (...)]
        [(string=? "running" g) (...)]
        [(string=? "game over" g) (...)]
        [else (...)]))


(@htdf create-food)
(@signature (listof Position) -> Position)
;; Produces a random valid position not included in the provided list.
;; For game, places food piece in position not occupied currently by snake.

(define (create-food lop)
  (local [(define try (make-pos (random (/ WIDTH CELL-SIZE))
                                (random (/ HEIGHT CELL-SIZE))))]
    (if (not (member? try lop))
        try
        (create-food lop))))


(@htdd GameState)
(define-struct gs (snake dir food run))
;; GameState is (make-gs (listof Position) Direction Position GameRun)
;; interp. is the state of the Snake Game.
;;         snake is the list of positions the Snake body cells (front to back)
;;         dir is the current direction of travel of the snake
;;         food is the position of the food piece on the board
;;         run is the current gamestate; running, start, game over, or win.
(define START-GS (make-gs empty
                          "up"
                          (make-pos 0 0)
                          "start"))                        
(define LOSE-GS (make-gs empty
                         "up"
                         (make-pos 0 0)
                         "game over"))
(define WIN-GS (make-gs empty
                        "up"
                        (make-pos 0 0)
                        "win"))
(define INITIAL-GS (make-gs (list (make-pos (/ CTR-X CELL-SIZE) (- (/ CTR-Y CELL-SIZE) 1))
                                  (make-pos (/ CTR-X CELL-SIZE) (/ CTR-Y CELL-SIZE))
                                  (make-pos (/ CTR-X CELL-SIZE) (+ (/ CTR-Y CELL-SIZE) 1)))
                            "up"
                            (create-food (list (make-pos (/ CTR-X CELL-SIZE) (+ (/ CTR-Y CELL-SIZE) 1))
                                              (make-pos (/ CTR-X CELL-SIZE) (/ CTR-Y CELL-SIZE))
                                              (make-pos (/ CTR-X CELL-SIZE) (- (/ CTR-Y CELL-SIZE) 1))))
                            "running"))
(define EXAMPLE-GS (make-gs (list (make-pos (+ (/ CTR-X CELL-SIZE) 2) (- (/ CTR-Y CELL-SIZE) 1))
                                  (make-pos (+ (/ CTR-X CELL-SIZE) 1) (- (/ CTR-Y CELL-SIZE) 1))
                                  (make-pos (/ CTR-X CELL-SIZE) (- (/ CTR-Y CELL-SIZE) 1))
                                  (make-pos (/ CTR-X CELL-SIZE) (/ CTR-Y CELL-SIZE))
                                  (make-pos (/ CTR-X CELL-SIZE) (+ (/ CTR-Y CELL-SIZE) 1)))
                            "right"
                            (make-pos (/ CTR-X CELL-SIZE) (+ (/ CTR-Y CELL-SIZE) 2))
                            "running"))

(define (fn-for-gs g)
  (... (fn-for-lop (gs-snake g))
       (fn-for-dir (gs-dir g))
       (fn-for-pos (gs-food g))
       (fn-for-gr (gs-run g))))


;; =================
;; Functions:

(@htdf main)
(@signature GameState -> GameState)
;; start the world with (main START-GS)

(@template htdw-main)

(define (main g)
  (big-bang g                               ; GameState
            (on-tick   tock TICK-SPEED)     ; GameState -> GameState
            (to-draw   render)              ; GameState -> Image
            (on-mouse  click-reset)         ; GameState Integer Integer MouseEvent -> GameState
            (on-key    control-direction))) ; GameState KeyEvent -> GameState


(@htdf tock)
(@signature GameState -> GameState)
;; Produce next GameState.
;; If game is at start/game over/win screen, do nothing.
;; If game is running, move snake head one space in dir direction,
;;                     and move each body cell forwards by 1.
;; If head overlaps with food square, add body cell to back of snake
;;                                    create food at new random location.
;; If snake head overlaps with body or hits wall, sets state to game over
;; If snake fills entire screen, sets state to win.
(check-expect (tock WIN-GS) WIN-GS)
(check-expect (tock LOSE-GS) LOSE-GS)
(check-expect (tock START-GS) START-GS)
(check-expect (tock (make-gs (list (make-pos 0 0)
                                   (make-pos 0 1)
                                   (make-pos 0 2))
                             "up"
                             (make-pos 5 5)
                             "running"))
              LOSE-GS)
(check-expect (tock (make-gs (list (make-pos 1 1)
                               (make-pos 0 1)
                               (make-pos 0 0)
                               (make-pos 1 0)
                               (make-pos 2 0)
                               (make-pos 2 1)
                               (make-pos 2 2))
                             "right"
                             (make-pos 10 10)
                             "running"))
              LOSE-GS)
;; Test for the win condition was tedious to write out, but was playtested.
(check-random (tock (make-gs (list (make-pos 2 2)
                                   (make-pos 2 1)
                                   (make-pos 2 0))
                             "down"
                             (make-pos 2 3)
                             "running"))
              (make-gs (list (make-pos 2 3)
                             (make-pos 2 2)
                             (make-pos 2 1)
                             (make-pos 2 0))
                       "down"
                       (create-food (list (make-pos 2 3)
                                          (make-pos 2 2)
                                          (make-pos 2 1)
                                          (make-pos 2 0)))
                       "running"))
(check-expect (tock (make-gs (list (make-pos 2 2)
                                   (make-pos 1 2)
                                   (make-pos 1 1))
                             "down"
                             (make-pos 10 10)
                             "running"))
              (make-gs (list (make-pos 2 3)
                             (make-pos 2 2)
                             (make-pos 1 2))
                       "down"
                       (make-pos 10 10)
                       "running"))

(define (tock g)
  (cond [(string=? (gs-run g) "running")
         (cond [(going-to-hit-wall? (first (gs-snake g)) (gs-dir g))
                LOSE-GS]
               [(going-to-hit-body? (gs-snake g) (gs-dir g))
                LOSE-GS]
               [(going-to-eat-food? (gs-snake g) (gs-dir g) (gs-food g))
                (if (= TOTAL-CELLS (add1 (length (gs-snake g))))
                    WIN-GS
                    (make-gs (append (list (gs-food g)) (gs-snake g))
                             (gs-dir g)
                             (create-food (append (list (gs-food g)) (gs-snake g)))
                             "running"))]
               [else
                (make-gs (update-snake (gs-snake g) (gs-dir g))
                         (gs-dir g)
                         (gs-food g)
                         "running")])]
        [else g]))

(@htdf going-to-hit-wall?)
(@signature Position Direction -> Boolean)
;; Produces true if position incremented in direction will hit wall.
(check-expect (going-to-hit-wall? (make-pos 1 (/ CTR-Y CELL-SIZE)) "left") false)
(check-expect (going-to-hit-wall? (make-pos 0 (/ CTR-Y CELL-SIZE)) "left") true)
(check-expect (going-to-hit-wall? (make-pos 1 (/ CTR-Y CELL-SIZE)) "right") false)
(check-expect (going-to-hit-wall? (make-pos 0 (/ CTR-Y CELL-SIZE)) "right") false)
(check-expect (going-to-hit-wall? (make-pos (- (/ WIDTH CELL-SIZE) 1) (/ CTR-Y CELL-SIZE)) "left") false)
(check-expect (going-to-hit-wall? (make-pos (- (/ WIDTH CELL-SIZE) 2) (/ CTR-Y CELL-SIZE)) "left") false)
(check-expect (going-to-hit-wall? (make-pos (- (/ WIDTH CELL-SIZE) 1) (/ CTR-Y CELL-SIZE)) "right") true)
(check-expect (going-to-hit-wall? (make-pos (- (/ WIDTH CELL-SIZE) 2) (/ CTR-Y CELL-SIZE)) "right") false)
(check-expect (going-to-hit-wall? (make-pos (/ CTR-X CELL-SIZE) 1) "up") false)
(check-expect (going-to-hit-wall? (make-pos (/ CTR-X CELL-SIZE) 0) "up") true)
(check-expect (going-to-hit-wall? (make-pos (/ CTR-X CELL-SIZE) 1) "down") false)
(check-expect (going-to-hit-wall? (make-pos (/ CTR-X CELL-SIZE) 0) "down") false)
(check-expect (going-to-hit-wall? (make-pos (/ CTR-X CELL-SIZE) (- (/ HEIGHT CELL-SIZE) 1)) "up") false)
(check-expect (going-to-hit-wall? (make-pos (/ CTR-X CELL-SIZE) (- (/ HEIGHT CELL-SIZE) 2)) "up") false)
(check-expect (going-to-hit-wall? (make-pos (/ CTR-X CELL-SIZE) (- (/ HEIGHT CELL-SIZE) 1)) "down") true)
(check-expect (going-to-hit-wall? (make-pos (/ CTR-X CELL-SIZE) (- (/ HEIGHT CELL-SIZE) 2)) "down") false)

(define (going-to-hit-wall? p d)
  (hit-wall? (next-pos p d)))

(@htdf hit-wall?)
(@signature Position -> Boolean)
;; Produces true if provided position (snake head) has hit a wall/out of bounds.
(check-expect (hit-wall? (make-pos -1 (/ CTR-Y CELL-SIZE))) true)
(check-expect (hit-wall? (make-pos 0 (/ CTR-Y CELL-SIZE))) false)
(check-expect (hit-wall? (make-pos 1 (/ CTR-Y CELL-SIZE))) false)
(check-expect (hit-wall? (make-pos (- (/ WIDTH CELL-SIZE) 1) (/ CTR-Y CELL-SIZE))) false)
(check-expect (hit-wall? (make-pos (/ WIDTH CELL-SIZE) (/ CTR-Y CELL-SIZE))) true)
(check-expect (hit-wall? (make-pos (+ (/ WIDTH CELL-SIZE) 1) (/ CTR-Y CELL-SIZE))) true)
(check-expect (hit-wall? (make-pos (/ CTR-X CELL-SIZE) -1)) true)
(check-expect (hit-wall? (make-pos (/ CTR-X CELL-SIZE) 0)) false)
(check-expect (hit-wall? (make-pos (/ CTR-X CELL-SIZE) 1)) false)
(check-expect (hit-wall? (make-pos (/ CTR-X CELL-SIZE) (- (/ HEIGHT CELL-SIZE) 1))) false)
(check-expect (hit-wall? (make-pos (/ CTR-X CELL-SIZE) (/ HEIGHT CELL-SIZE))) true)
(check-expect (hit-wall? (make-pos (/ CTR-X CELL-SIZE) (+ (/ HEIGHT CELL-SIZE) 1))) true)

(define (hit-wall? p)
  (not (and (<= 0 (pos-x p) (- (/ WIDTH CELL-SIZE) 1))
            (<= 0 (pos-y p) (- (/ HEIGHT CELL-SIZE) 1)))))

(@htdf going-to-hit-body?)
(@signature (listof Position) Direction -> Boolean)
;; Produces true if snake moving forwards will hit itself.
(check-expect (going-to-hit-body? (list (make-pos 0 0)
                                        (make-pos 0 1)
                                        (make-pos 0 2))
                                  "right")
              false)
(check-expect (going-to-hit-body? (list (make-pos 0 0)
                                        (make-pos 0 1)
                                        (make-pos 1 1)
                                        (make-pos 1 0))
                                  "right")
              false)
(check-expect (going-to-hit-body? (list (make-pos 0 0)
                                        (make-pos 0 1)
                                        (make-pos 1 1)
                                        (make-pos 1 0)
                                        (make-pos 2 0))
                                  "right")
              true)

(define (going-to-hit-body? lop d)
  (hit-body? (update-snake lop d)))

(@htdf hit-body?)
(@signature (listof Position) -> Boolean)
;; Produces true if snake head has hit the body
(check-expect (hit-body? (list (make-pos 2 1)
                               (make-pos 1 1)
                               (make-pos 0 1))) false)
(check-expect (hit-body? (list (make-pos 2 1)
                               (make-pos 1 1)
                               (make-pos 0 1)
                               (make-pos 0 0)
                               (make-pos 1 0)
                               (make-pos 2 0)
                               (make-pos 2 1))) true)

(define (hit-body? s)
  (not (false? (check-duplicates s))))


(@htdf going-to-eat-food?)
(@signature (listof Position) Direction Position -> Boolean)
;; Produces true if snake head is about to move to a square with food
;; i.e. if first element of lop moves in direction of position, does it
;;      equal the provided position
(check-expect (going-to-eat-food? (list (make-pos 1 0)
                                  (make-pos 0 0))
                            "down"
                            (make-pos 1 1))
              true)

(check-expect (going-to-eat-food? (list (make-pos 1 0)
                                  (make-pos 0 0))
                            "right"
                            (make-pos 1 1))
              false)

(define (going-to-eat-food? lop d p)
  (local [(define next-head (next-pos (first lop) d))]
    (and (= (pos-x next-head) (pos-x p))
         (= (pos-y next-head) (pos-y p)))))


(@htdf next-pos)
(@signature Position Direction -> Position)
;; Produces the position incremeneted by 1 in the provided direction
(check-expect (next-pos (make-pos 2 6) "up") (make-pos 2 5))
(check-expect (next-pos (make-pos 2 6) "down") (make-pos 2 7))
(check-expect (next-pos (make-pos 2 6) "right") (make-pos 3 6))
(check-expect (next-pos (make-pos 2 6) "left") (make-pos 1 6))

(@template Direction)

(define (next-pos p d)
  (cond [(string=? "left" d) (make-pos (- (pos-x p) 1) (pos-y p))]
        [(string=? "right" d) (make-pos (+ (pos-x p) 1) (pos-y p))]
        [(string=? "up" d) (make-pos (pos-x p) (- (pos-y p) 1))]
        [else (make-pos (pos-x p) (+ (pos-y p) 1))]))



(@htdf update-snake)
(@signature (listof Position) Direction -> (listof Position))
;; Updates the lop by appending one position moved in one unit towards direction
;; and then removing the last element of the list.
(check-expect (update-snake (list (make-pos 2 0)
                                  (make-pos 1 0)
                                  (make-pos 0 0))
                            "down")
              (list (make-pos 2 1)
                    (make-pos 2 0)
                    (make-pos 1 0)))
(check-expect (update-snake (list (make-pos 1 1)
                                  (make-pos 1 0)
                                  (make-pos 0 0)
                                  (make-pos 0 1))
                            "right")
              (list (make-pos 2 1)
                    (make-pos 1 1)
                    (make-pos 1 0)
                    (make-pos 0 0)))

(@template Direction)

(define (update-snake lop d)
  (cons (next-pos (first lop) d)
        (reverse (rest (reverse lop)))))






(@htdf render)
(@signature GameState -> Image)
;; Draws the current GameState
(check-expect (render START-GS) START-SCREEN)
(check-expect (render LOSE-GS) GAME-OVER-SCREEN)
(check-expect (render WIN-GS) WIN-SCREEN)
(check-expect (render EXAMPLE-GS)
              (place-image
               SNAKE-BODY
               (get-coordinate (+ (/ CTR-X CELL-SIZE) 2))
               (get-coordinate (- (/ CTR-Y CELL-SIZE) 1))
               (place-image
                SNAKE-BODY
                (get-coordinate (+ (/ CTR-X CELL-SIZE) 1))
                (get-coordinate (- (/ CTR-Y CELL-SIZE) 1))
                (place-image
                 SNAKE-BODY
                 (get-coordinate (/ CTR-X CELL-SIZE))
                 (get-coordinate (- (/ CTR-Y CELL-SIZE) 1))
                 (place-image
                  SNAKE-BODY
                  (get-coordinate (/ CTR-X CELL-SIZE))
                  (get-coordinate (/ CTR-Y CELL-SIZE))
                  (place-image
                   SNAKE-BODY
                   (get-coordinate (/ CTR-X CELL-SIZE))
                   (get-coordinate (+ (/ CTR-Y CELL-SIZE) 1))
                   (place-image
                    FOOD
                    (get-coordinate (/ CTR-X CELL-SIZE))
                    (get-coordinate (+ (/ CTR-Y CELL-SIZE) 2))
                    MTS)))))))
                                                
(@template GameState)

(define (render g)
  (cond [(string=? "start" (gs-run g)) START-SCREEN]
        [(string=? "game over" (gs-run g)) GAME-OVER-SCREEN]
        [(string=? "win" (gs-run g)) WIN-SCREEN]
        [else
         (draw-snake (gs-snake g)
                     (draw-food (gs-food g)
                                MTS))]))


(@htdf draw-food)
(@signature Position Image -> Image)
;; Draws food on the screen/scene
(check-expect (draw-food (make-pos (/ CTR-X CELL-SIZE) (/ CTR-Y CELL-SIZE)) MTS)
              (place-image FOOD
                           (get-coordinate (/ CTR-X CELL-SIZE))
                           (get-coordinate (/ CTR-Y CELL-SIZE))
                           MTS))

(define (draw-food p img)
  (place-image FOOD
               (get-coordinate (pos-x p))
               (get-coordinate (pos-y p))
               MTS))


(@htdf draw-snake)
(@signature (listof Position) Image -> Image)
;; Draws the snake on the screen
(check-expect (draw-snake (list (make-pos (/ CTR-X CELL-SIZE) (- (/ CTR-Y CELL-SIZE) 1))
                                (make-pos (/ CTR-X CELL-SIZE) (/ CTR-Y CELL-SIZE))
                                (make-pos (/ CTR-X CELL-SIZE) (+ (/ CTR-Y CELL-SIZE) 1)))
                          (place-image FOOD 5 5 MTS))
              (place-image SNAKE-BODY
                           (get-coordinate (/ CTR-X CELL-SIZE))
                           (get-coordinate (- (/ CTR-Y CELL-SIZE) 1))
                           (place-image
                            SNAKE-BODY
                            (get-coordinate (/ CTR-X CELL-SIZE))
                            (get-coordinate (/ CTR-Y CELL-SIZE))
                            (place-image
                             SNAKE-BODY
                             (get-coordinate (/ CTR-X CELL-SIZE))
                             (get-coordinate (+ (/ CTR-Y CELL-SIZE) 1))
                             (place-image FOOD 5 5 MTS)))))

(@template (listof Position))

(define (draw-snake lop img)
  (cond [(empty? lop) img]
        [else
         (place-image SNAKE-BODY
                      (get-coordinate (pos-x (first lop)))
                      (get-coordinate (pos-y (first lop)))
                      (draw-snake (rest lop) img))]))


(@htdf get-coordinate)
(@signature Integer -> Integer)
;; Produce the screen coordinates to draw objects from coords stored in position.
(check-expect (get-coordinate 0) (/ CELL-SIZE 2))
(check-expect (get-coordinate 5) (+ (* 5 CELL-SIZE) (/ CELL-SIZE 2)))

(define (get-coordinate i)
  (+ (* i CELL-SIZE) (/ CELL-SIZE 2)))


(@htdf click-reset)
(@signature GameState Integer Integer MouseEvent -> GameState)
;; Resets GameState to initial state when at start, game over, or win screen.
(check-expect (click-reset EXAMPLE-GS 0 0 "move") EXAMPLE-GS)
(check-expect (click-reset EXAMPLE-GS 0 0 "button-down") EXAMPLE-GS)
(check-expect (click-reset WIN-GS 0 0 "move") WIN-GS)
(check-expect (click-reset WIN-GS 0 0 "button-down") INITIAL-GS)
(check-expect (click-reset LOSE-GS 0 0 "button-down") INITIAL-GS)
(check-expect (click-reset START-GS 0 0 "button-down") INITIAL-GS)
(check-expect (click-reset START-GS 10 50 "button-down") INITIAL-GS)

(@template MouseEvent)

(define (click-reset g x y mevt)
  (cond [(mouse=? "button-down" mevt)
         (if (or (string=? "game over" (gs-run g))
                 (string=? "win" (gs-run g))
                 (string=? "start" (gs-run g)))
             INITIAL-GS
             g)]
        [else g]))

(@htdf control-direction)
(@signature GameState KeyEvent -> GameState)
(check-expect (control-direction (make-gs (list (make-pos 10 10))
                                          "left"
                                          (make-pos 0 0)
                                          "running")
                                 "left")
              (make-gs (list (make-pos 10 10)) "left" (make-pos 0 0) "running"))
(check-expect (control-direction (make-gs (list (make-pos 10 10))
                                          "left"
                                          (make-pos 0 0)
                                          "running")
                                 "right")
              (make-gs (list (make-pos 10 10)) "left" (make-pos 0 0) "running"))
(check-expect (control-direction (make-gs (list (make-pos 10 10))
                                          "left"
                                          (make-pos 0 0)
                                          "running")
                                 "up")
              (make-gs (list (make-pos 10 10)) "up" (make-pos 0 0) "running"))
(check-expect (control-direction (make-gs (list (make-pos 10 10))
                                          "left"
                                          (make-pos 0 0)
                                          "running")
                                 "down")
              (make-gs (list (make-pos 10 10)) "down" (make-pos 0 0) "running"))
(check-expect (control-direction (make-gs (list (make-pos 10 10))
                                          "up"
                                          (make-pos 0 0)
                                          "running")
                                 "left")
              (make-gs (list (make-pos 10 10)) "left" (make-pos 0 0) "running"))
(check-expect (control-direction (make-gs (list (make-pos 10 10))
                                          "up"
                                          (make-pos 0 0)
                                          "running")
                                 "right")
              (make-gs (list (make-pos 10 10)) "right" (make-pos 0 0) "running"))
(check-expect (control-direction (make-gs (list (make-pos 10 10))
                                          "up"
                                          (make-pos 0 0)
                                          "running")
                                 "up")
              (make-gs (list (make-pos 10 10)) "up" (make-pos 0 0) "running"))
(check-expect (control-direction (make-gs (list (make-pos 10 10))
                                          "up"
                                          (make-pos 0 0)
                                          "running")
                                 "down")
              (make-gs (list (make-pos 10 10)) "up" (make-pos 0 0) "running"))
(check-expect (control-direction EXAMPLE-GS " ") EXAMPLE-GS)
(check-expect (control-direction WIN-GS "left") WIN-GS)
(check-expect (control-direction LOSE-GS "left") LOSE-GS)
(check-expect (control-direction START-GS "left") START-GS)

(@template KeyEvent)

(define (control-direction g kevt)
  (cond [(string=? (gs-run g) "running")
                   (cond [(key=? "left" kevt)
                          (if (or (string=? (gs-dir g) "up") (string=? (gs-dir g) "down"))
                              (make-gs (gs-snake g) "left" (gs-food g) "running")
                              g)]
                         [(key=? "right" kevt)
                          (if (or (string=? (gs-dir g) "up") (string=? (gs-dir g) "down"))
                              (make-gs (gs-snake g) "right" (gs-food g) "running")
                              g)]
                         [(key=? "up" kevt)
                          (if (or (string=? (gs-dir g) "left") (string=? (gs-dir g) "right"))
                              (make-gs (gs-snake g) "up" (gs-food g) "running")
                              g)]
                         [(key=? "down" kevt)
                          (if (or (string=? (gs-dir g) "left") (string=? (gs-dir g) "right"))
                              (make-gs (gs-snake g) "down" (gs-food g) "running")
                              g)]
                         [else g])]
        [else g]))

(main START-GS)