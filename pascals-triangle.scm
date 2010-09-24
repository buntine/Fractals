;;; Fractals
;;; Pascal's Triangle - Approximation of the Sierpinski Triangle
;;; Andrew Buntine, 2010 (http://www.andrewbuntine.com)
;;;
;;; The Sierpinski triangle (or gasket) is a fractal named after Waclaw Sierpinski, who
;;; first described it in 1915. This fractal, like Barnsley's Fern, is a basic example
;;; of a self-similar set.
;;;
;;; There are many ways to generate the Sierpinski triangle, but I am creating an
;;; approximation based on the numbers found in a far more ancient pattern - Pascal's Triangle.
;;; If you colour the odd numbers black and leave the even ones white, a familiar pattern
;;; begins to emerge!
;;;
;;; See fractals.scm for execution instructions.

(module pascals-triangle mzscheme
  (require (lib "graphics.ss" "graphics"))

  ; Initialising function. This is the one you should invoke!
  (define (draw-pascals-triangle)
    '())

  (provide draw-pascals-triangle))
