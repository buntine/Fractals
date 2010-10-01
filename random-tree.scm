;;; Fractals
;;; Random tree.
;;; Andrew Buntine, 2010 (http://www.andrewbuntine.com)
;;;
;;; A tree in nature is a great example of a fractal. By use of tree recursion, we
;;; can easily generate a semi-realistic looking tree. This program implements some
;;; basic randomisation so we can more accurately emulate the natural tree form. It also
;;; draws a few trees with differing levels of depth to emulate (poorly) a forest.
;;;
;;; See fractals.scm for execution instructions.

(module random-tree mzscheme
  (require (lib "graphics.ss" "graphics"))

  (define *DEPTH* 8)
  (define *LENGTH* 100)
  (define *ANGLE* 35)
  (define *WIDTH* 500)
  (define *HEIGHT* 300)
  (define *PI* 3.14159265358979)

  ; Plots a new position given a current position, a length
  ; and a theta value.
  (define (posn-for-theta posn len theta)
    (let ((plot (lambda (c fn)
                  (round (+ c (* len (fn theta)))))))
      (make-posn (plot (posn-x posn) cos)
                 (plot (posn-y posn) sin))))

  ; Draws a line starting from the given position, going
  ; len pixels at an angle of d degrees. Returns the
  ; ending position.
  (define (plot-line vp posn d len)
    (let* ((theta (* d (/ *PI* 180)))
           (end-posn (posn-for-theta posn len theta)))
      ((draw-line vp) posn end-posn "black")
      end-posn))

  ; Generates a random number between 6 and *ANGLE*.
  (define (random-angle)
    (+ 6 (random (- *ANGLE* 6))))

  ; Draws a new branch of the tree.
  (define (draw-branch vp posn depth angle)
     (let* ((len (/ *LENGTH* depth))
            (end-posn (plot-line vp posn angle len)))
       (if (< depth *DEPTH*)
         (begin
           (sleep 0.02)
           (draw-branch vp end-posn (+ depth 1) (- angle (random-angle)))
           (draw-branch vp end-posn (+ depth 1) (+ angle (random-angle)))))))

  (define (draw-random-tree)
    (open-graphics)
    (let ((vp (open-viewport "Fractals - Random trees" *WIDTH* *HEIGHT*)))
      (draw-branch vp (make-posn (/ *WIDTH* 6) *HEIGHT*) 2 270)
      (draw-branch vp (make-posn (/ *WIDTH* 3) *HEIGHT*) 1 270)
      (draw-branch vp (make-posn (/ *WIDTH* 2) *HEIGHT*) 3 280)
      (draw-branch vp (make-posn (/ *WIDTH* 1.3) *HEIGHT*) 2 270)))

  (provide draw-random-tree))
