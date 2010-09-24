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

  (define *ROWS* 64)
  (define *CELL* '(4 4))
  (define *WIDTH* (* (car *CELL*) *ROWS*))
  (define *HEIGHT* (* (cadr *CELL*) *ROWS*))

  ; Generates the elements of Pascal's triangle to
  ; the given width (rows). The triangle is represented
  ; as a list of expanding sublists - one for each row.
  (define (pascals-triangle width)  
    (reverse (pascal-helper '((1)) width)))  

  (define (pascal-helper rows width)  
    (if (= (length rows) width)  
      rows  
      (pascal-helper (cons (next-row (car rows))  
                           rows)  
                     width)))  

  ; Generates the next row, given the current one.
  (define (next-row previous)  
    (let ((body (row-body '() previous)))  
      (if (null? body)  
        '(1 1)  
        (cons 1 (append body '(1))))))  

  ; Returns a list of the next rows body (leaving off the 1's).
  (define (row-body body previous)  
    (if (null? (cdr previous))  
      body  
      (row-body (cons (+ (car previous) (cadr previous))  
                      body)  
                (cdr previous))))  

  ; Initialising function. This is the one you should invoke!
  (define (draw-pascals-triangle)
    (pascals-triangle *ROWS*))

  (provide draw-pascals-triangle))
