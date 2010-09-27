;;; Fractals
;;; Uniform tree.
;;; Andrew Buntine, 2010 (http://www.andrewbuntine.com)
;;;
;;; A tree in nature is a great example of a fractal. By use of tree recursion, we
;;; can easily generate a semi-realistic looking tree. This particular example does not
;;; imply any type of randomisation like we see in nature (branches of differing
;;; angles and lengths, etc).
;;;
;;; See fractals.scm for execution instructions.

(module uniform-tree mzscheme
  (require (lib "graphics.ss" "graphics"))

  (define *DEPTH* 8)
  (define *LENGTH* 70)
  (define *WIDTH* 300)
  (define *HEIGHT* 300)
  (define *PI* 3.14159265358979)

  (define (posn-for-theta posn len theta)
    (let ((plot (lambda (c fn)
                  (round (+ c (* len (fn theta)))))))
      (make-posn (plot (posn-x posn) cos)
                 (plot (posn-y posn) sin))))

  (define (plot-line vp posn d len)
    (let* ((theta (* d (/ *PI* 180)))
           (end-posn (posn-for-theta posn len theta)))
      ((draw-line vp) posn end-posn "black")
      end-posn))

  (define (draw-branch vp posn depth angle)
     (let* ((len (/ *LENGTH* depth))
            (end-posn (plot-line vp posn angle len)))
       (if (< depth *DEPTH*)
         (begin
           (draw-branch vp end-posn (+ depth 1) (- angle 30))
           (draw-branch vp end-posn (+ depth 1) (+ angle 30))))))

  (define (draw-uniform-tree)
    (open-graphics)
    (let ((vp (open-viewport "Fractals - Uniform tree" *WIDTH* *HEIGHT*)))
      (draw-branch vp (make-posn (/ *WIDTH* 2) *HEIGHT*) 1 270)))

  (provide draw-uniform-tree))
