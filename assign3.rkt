;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assign3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; CMPU-101 
; Fall 2018
; Assign 3
; Gabriel Kardos 
;
; Description: Uses a list of bouncing balls to animate many balls
; of different sizes and colors, all moving in the same scene at 
; different speeds.

(require 2htdp/image) 
(require 2htdp/universe)

(define RADIUS 25)

; Scene dimensions
(define WIDTH 500)
(define HEIGHT 300)

; Create the background scene image
(define BACKGROUND
  (place-image (rectangle WIDTH HEIGHT "solid" "lightgray")
               (/ WIDTH 2) (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT)))

; Data Definitions 
(define-struct ball (im x y dx dy))
; A ball is a (make-ball im p dx dy) where
; im is an image (of the ball), 
; x and y are numbers representing the ball's position, and
; dx and dy are numbers representing the ball's horizontal and 
;   vertical velocity

; Data Definition for a list-of-balls:
; A list-of-balls is either:
; 1. '(), or
; 2. (cons b lob), where b is a ball
;    and lob is a list-of-balls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define four (4) example ball CONSTANTS:
;   one touching each edge of the scene (top, bottom, left, right)
;   These will help you test bounce conditions.

; here's one of my ball CONSTANTS, which you may use or modify
; if you like to define the rest.
(define BALL-AT-LEFT 
  (make-ball (circle (+ RADIUS 4) "solid" "teal")
             (+ RADIUS 4) (/ HEIGHT 2) 4 -4))

(define BALL-AT-TOP
  (make-ball (circle (+ RADIUS 10) "solid" "blue")
             (/ WIDTH 2.5) (+ RADIUS 10) -2 -3))

(define BALL-AT-BOTTOM
  (make-ball (circle (- RADIUS 5) "solid" "orange")
             (/ WIDTH 2) (- HEIGHT (- RADIUS 5)) -1 3))

(define BALL-AT-RIGHT
  (make-ball (circle (- RADIUS 12) "solid" "red")
             (- WIDTH (- RADIUS 12)) (/ HEIGHT 2) -3 6))


; Define INIT-LOB to be a list-of-balls:
; You will use this to be the initial state of the world.
; I've defined it to be the empty list, but you should define it
; to contain the four example ball CONSTANTS you just defined. 
(define INIT-LOB (list (make-ball (circle (+ RADIUS 4) "solid" "teal") (+ RADIUS 4) (/ HEIGHT 2) 4 -4)
                       (make-ball (circle (+ RADIUS 10) "solid" "blue") (/ WIDTH 2.5) (+ RADIUS 10) -2 -3)
                       (make-ball (circle (- RADIUS 5) "solid" "orange") (/ WIDTH 2) (- HEIGHT (- RADIUS 5)) 1 -3)
                       (make-ball (circle (- RADIUS 12) "solid" "red") (- WIDTH (- RADIUS 12)) (/ HEIGHT 2) -3 6)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Templates for a ball and a list-of-balls.
; Use these to help you get started with the functions below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> ???
; Template for a function that consumes a ball
(define (fun-for-ball b) 
  (...(ball-im b)...
   ...(ball-x b)...(ball-y b)...
   ...(ball-dx b)...(ball-dy b)...))

; list-of-balls -> ???
; Template for a function that consumes a list-of-balls
(define (fun-for-list-of-balls lob) 
  (cond
    [(empty? lob)...] 
    [else (...(fun-for-ball (first lob))...
           ...(fun-for-lob (rest lob))...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Design the functions below, in order. I've supplied the
; signature, purpose statement, and header for each function.
;
; You provide the check-expect examples, and using the appropriate
; template, complete the function bodies.
;
; I recommend you proceed in order, and complete each function,
; with passing tests, before going on to the next.
;
; The reason for completing the functions in the order they appear
; is earlier functions can be used as helper functions for the
; later functions.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> number
; computes the radius of given ball
(define (ball-radius b)
  (/ (image-width (ball-im b)) 2))
(check-expect (ball-radius (make-ball (circle 40 "solid" "blue") 2 4 6 8)) 40)
(check-expect (ball-radius (make-ball (circle 1 "outline" "yellow") 1 3 5 7)) 1)

; ball -> boolean
; determines whether the ball reached the top edge of scene
(define (top-edge? b)
  (= (ball-y b) (ball-radius b)))
(check-expect (top-edge? (make-ball (circle 10 "solid" "green") 20 10 4 -6)) #t)
(check-expect (top-edge? (make-ball (circle 11.5 "outline" "orange") 5 11.55 14 -506)) #f)

; ball -> boolean
; determines whether the ball reached the bottom edge of scene
(define (bottom-edge? b)
  (= (ball-y b) (- HEIGHT (/ (ball-radius b) 2))))
(check-expect (bottom-edge? (make-ball (circle 12 "solid" "purple") 14 294 -1 -2)) #t)
(check-expect (bottom-edge? (make-ball (circle 18 "solid" "black") 18 291 -40 0)) #t)

; ball -> boolean
; determines whether the ball reached the left edge of scene
(define (left-edge? b)
  (= (ball-x b) (/ (ball-radius b) 2)))
(check-expect (left-edge? (make-ball (circle 2 "solid" "lightgray") 1 11 111 1111)) #t)
(check-expect (left-edge? (make-ball (circle 14 "solid" "teal") 8 9 11 13)) #f)

; ball -> boolean
; determines whether the ball reached the right edge of scene
(define (right-edge? b)
  (= (ball-x b) (- WIDTH (/ (ball-radius b) 2))))
(check-expect (right-edge? (make-ball (circle 12 "solid" "brown") 494 16 -2 -2)) #t)
(check-expect (right-edge? (make-ball (circle 36 "solid" "gray") 483 12 -106 3.14159)) #f)

; ball -> ball
; reverse ball's up-down direction   
(define (reverse-up-down b)
  (make-ball (ball-im b) (ball-x b) (ball-y b) (ball-dx b) (* -1 (ball-dy b))))
(check-expect (reverse-up-down (make-ball (circle 14 "solid" "yellow") 11 19 27 35)) (make-ball (circle 14 "solid" "yellow") 11 19 27 -35))
(check-expect (reverse-up-down (make-ball (circle 11 "outline" "darkgray") 99 99 98 97)) (make-ball (circle 11 "outline" "darkgray") 99 99 98 -97))

; ball -> ball
; reverse ball's left-right direction   
(define (reverse-left-right b)
  (make-ball (ball-im b) (ball-x b) (ball-y b) (* -1 (ball-dx b)) (ball-dy b)))
(check-expect (reverse-left-right (make-ball (circle 40 "solid" "red") 16 32 64 128)) (make-ball (circle 40 "solid" "red") 16 32 -64 128))
(check-expect (reverse-left-right (make-ball (circle 20 "outline" "blue") 18 18 188 1808)) (make-ball (circle 20 "outline" "blue") 18 18 -188 1808))

; ball -> ball
; changes direction of given ball if it hit the top or bottom edge
(define (bounce-up-down b)
  (cond
    [(or (and (top-edge? b) (< (ball-dy b) 0)) (and (bottom-edge? b) (> (ball-dy b) 0))) (reverse-up-down b)]
    [else b]))
(check-expect (bounce-up-down (make-ball (circle 50 "outline" "beige") 199 25 1909 -9)) (make-ball (circle 50 "outline" "beige") 199 25 1909 -9))
(check-expect (bounce-up-down (make-ball (circle 209 "solid" "green") 29 209 1909 -8)) (make-ball (circle 209 "solid" "green") 29 209 1909 8))

; ball -> ball
; changes direction of given ball if it hit the left or right edge
(define (bounce-left-right b)
  (cond
    [(or (and (left-edge? b) (< (ball-dx b) 0)) (and (right-edge? b) (> (ball-dx b) 0))) (reverse-left-right b)]
    [else b]))

; It looks like I am missing some functions.

; needs-horizontal-reverse? : ball -> boolean
; determines whether ball is at left or right edge of background and is in danger of escaping from view
(define (needs-horizontal-reverse? b)
  (or (and (left-edge? b) (< (ball-dx b) 0)) (and (right-edge? b) (> (ball-dx b) 0))))
(define (needs-horizontal-reverse2? b)
  (or (left-edge? b) (right-edge? b)))

; needs-vertical-reverse? : ball -> boolean
; determines whether ball is at top or bottom edge of background and is in danger of escaping from view
(define (needs-vertical-reverse? b)
  (or (and (top-edge? b) (< (ball-dy b) 0)) (and (bottom-edge? b) (> (ball-dy b) 0))))
(define (needs-vertical-reverse2? b)
  (or (top-edge? b) (bottom-edge? b)))

; maintain-heading? : ball -> bool
; determines whether ball is not colliding with a wall
(define (maintain-heading? b)
  (and (not (needs-horizontal-reverse? b)) (not (needs-vertical-reverse? b))))
(define (maintain-heading2? b)
  (and (not (needs-horizontal-reverse2? b)) (not (needs-vertical-reverse2? b))))

; ball -> ball
; moves the given ball by its dx and dy amounts
(define (move-ball b)
  (cond
    [(needs-horizontal-reverse? b) (make-ball (ball-im b) (+ (* -1 (ball-dx b)) (ball-x b)) (+ (ball-dy b) (ball-y b)) (* 1 (ball-dx b)) (ball-dy b))]
    [(needs-vertical-reverse? b) (make-ball (ball-im b) (+ (ball-dx b) (ball-x b)) (+ (* -1 (ball-dy b)) (ball-y b)) (ball-dx b) (* 1 (ball-dy b)))]
    [(maintain-heading? b) (make-ball (ball-im b) (+ (ball-x b) (ball-dx b)) (+ (ball-y b) (ball-dy b)) (ball-dx b) (ball-dy b))]))
(define (move-ball2 b)
  (cond
    [(needs-horizontal-reverse2? b) (make-ball (ball-im b) (+ (* -1 (ball-dx b)) (ball-x b)) (+ (ball-dy b) (ball-y b)) (* 1 (ball-dx b)) (ball-dy b))]
    [(needs-vertical-reverse2? b) (make-ball (ball-im b) (+ (ball-dx b) (ball-x b)) (+ (* -1 (ball-dy b)) (ball-y b)) (ball-dx b) (* 1 (ball-dy b)))]
    [(maintain-heading2? b) (make-ball (ball-im b) (+ (ball-x b) (ball-dx b)) (+ (ball-y b) (ball-dy b)) (ball-dx b) (ball-dy b))]))

; list-of-balls -> list-of-balls
; moves (and possibly bounces) each ball in given list
(define (move-list-of-balls lob)
  (cond
    [(empty? lob) '()]
    [(cons? lob)
     (cons (move-ball (first lob))
           (move-list-of-balls (rest lob)))]))
(define (move-list-of-balls2 lob)
  (cond
    [(empty? lob) '()]
    [(cons? lob)
     (cons (move-ball2 (first lob))
           (move-list-of-balls (rest lob)))]))   

; ball image -> image
; renders given ball b on given background bg
(define (render-ball b bg)
  (place-image (ball-im b) (ball-x b) (ball-y b) bg))
  
; list-of-balls -> image 
; produces image of each ball at each given current position on
; background.
; (Yes, I provided this function for you! You shouldn't have to
;  touch it if you've correctly implemented the functions above.)
(define (render-balls lob) 
  (cond [(empty? lob) BACKGROUND]
        [else (render-ball (first lob)
                           (render-balls (rest lob)))]))

; Here's the main function with the big-bang expression!
; Once you've implemented move-list-of-balls, uncomment on-tick below.
;(define (main w)
 ; (big-bang w
            ;(on-tick move-list-of-balls2 1/28) 
            ;(to-draw render-balls)))

; Run program automatically, or type this in Interactions Pane:
; Use INIT-LOB as the initial state of the world...
;(main INIT-LOB)