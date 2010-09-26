;;; Fractals
;;; Uniform tree.
;;; Andrew Buntine, 2010 (http://www.andrewbuntine.com)
;;;
;;; A tree in nature is a great example of a fractal. By use of mutual recursion, we
;;; easily generate a semi-realistic looking tree. This particular example does not
;;; imply any type of randomisation like we see in nature (branches of differing
;;; angles and lengths, etc).
;;;
;;; See fractals.scm for execution instructions.

(module uniform-tree mzscheme
  (require (lib "graphics.ss" "graphics"))

  (define *DEPTH* 7)
  (define *LENGTH* 40)
  (define *WIDTH* (* *DEPTH* *LENGTH*))
  (define *HEIGHT* (* *DEPTH* *LENGTH*))

  (define (draw-uniform-tree)
    (open-graphics)
    (let ((vp (open-viewport "Fractals - Uniform tree" *WIDTH* *HEIGHT*)))
      '()))

  (provide draw-uniform-tree))
