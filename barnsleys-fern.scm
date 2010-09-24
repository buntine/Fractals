;;; Fractals
;;; Barnsley's Fern
;;; Andrew Buntine, 2010 (http://www.andrewbuntine.com)
;;;
;;; The Barnsley Fern is a fractal named after the British mathematician
;;; Michael Barnsley. The fern is a basic example of a mathematically
;;; generated pattern that can be reproducible at any magnification or reduction.
;;;
;;; Hopefully I can impress my Maths tutor with this.
;;;
;;; Usage:
;;;   (make-fern)

(require (lib "graphics.ss" "graphics"))

(define *ITERATIONS* 600000)
(define *WIDTH* 550)
(define *HEIGHT* 550)

; Valid ranges for the plottable points.
(define *RANGE_X* '(-2.1818 2.6556))
(define *RANGE_Y* '(0 9.95851))

; Matrix constants for each function as found at:
;   http://en.wikipedia.org/wiki/Barnsley_fern.
;
; Format (where 'p' is the probability factor and 'g' the colour):
;   (a b c d e f p g)
(define matrix
  '(#(0 0 0 0.16 0 0 0.01 "green")
    #(0.85 0.04 -0.04 0.85 0 1.6 0.85 "green")
    #(0.2 -0.26 0.23 0.22 0 1.6 0.08 "green")
    #(-0.15 0.28 0.26 0.24 0 0.44 0.06 "green")))

; Or try the following matrix to generate the "Fishbone fern" as
; described here: http://www.home.aone.net.au/~byzantium/ferns/fractal.html
;(define matrix
;  '(#(0 0 0 0.25 0 -0.4 0.02 "green")
;    #(0.95 0.002 -0.002 0.93 -0.002 0.5 0.84 "green")
;    #(0.035 -0.11 0.27 0.01 -0.05 0.005 0.07 "green")
;    #(-0.04 0.11 0.27 0.01 0.047 0.06 0.07 "green")))

; Returns a given value from a row of the matrix
; as fed in through 'input'.
(define (mval chr input)
  (let ((ref '((#\a 0) (#\b 1) (#\c 2)
              (#\d 3) (#\e 4) (#\f 5)
              (#\p 6) (#\g 7))))
    (vector-ref input
                (cadr (assoc chr ref)))))

; Randomly selects the function input with
; non-uniform probability (given by p in the matrix).
(define (choose-function row rnd)
  (let* ((input (list-ref matrix row))
         (remaining (- rnd (mval #\p input))))
    (if (<= remaining 0)
      input
      (choose-function (+ row 1) remaining))))

; Finds a point to draw the next pixel given the correct
; matrix value references.
; In common notation: (x * va) + (y * vb) + vc
(define (find-point x y va vb vc input)
  (+ (+ (* x (mval va input))
        (* y (mval vb input)))
     (mval vc input)))

(define (find-x x y input)
  (find-point x y #\a #\b #\e input))

(define (find-y x y input)
  (find-point x y #\c #\d #\f input))

; Just a few abstractions to give better names.
(define range-w (- (cadr *RANGE_X*) (car *RANGE_X*)))
(define range-h (- (cadr *RANGE_Y*) (car *RANGE_Y*)))
(define range-min-w (car *RANGE_X*))
(define range-min-h (car *RANGE_Y*))

; Calculates a pixel position and draws a point on it.
; We are only interested in this functions side-effects.
; NOTE: I got some help from http://vb-helper.com for
;       the pixel-x/pixel-y formulas.
(define (paint-pixel vp x y color)
  (let* ((pixel-x (* (/ (- x range-min-w)
                        range-w)
                     *WIDTH*))
         (pixel-y (- (- *HEIGHT* 1)
                     (* (/ (- y range-min-h) range-h)
                        *HEIGHT*)))
         (posn (make-posn (round pixel-x) (round pixel-y))))
    ; Draw the pixel if it's in range.
    (if (and (>= pixel-x 0) (>= pixel-y 0)
             (< pixel-x *WIDTH*) (< pixel-y *HEIGHT*))
      ((draw-pixel vp) posn color))))

; Plots the next point on the canvas using x,y as the
; seed and i as the iteration.
(define (plot-points vp i x y)
  (let* ((input (choose-function 0 (random)))
         (next-x (find-x x y input))
         (next-y (find-y x y input)))
    (paint-pixel vp next-x next-y (mval #\g input))
    (if (< i *ITERATIONS*)
      (plot-points vp (+ i 1) next-x next-y))))

; Initialising function. This is the one you should invoke!
(define (draw-fern)
  (open-graphics)
  (let ((vp (open-viewport "Fractals - Bernley's Fern" *WIDTH* *HEIGHT*)))
    (plot-points vp 0 1 1)))
