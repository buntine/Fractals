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
  (define *CELL* '(6 6))
  (define *WIDTH* (* 2 (* (car *CELL*) *ROWS*)))
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

  ; Draws the given row to the viewport, at the given y.
  (define (draw-row vp row x y)
    (let ((color (if (odd? (car row)) "black" "white")))
      ((draw-solid-ellipse vp) (make-posn x y) (car *CELL*) (cadr *CELL*) color)
      (if (> (length row) 1)
        (draw-row vp (cdr row) (+ x (car *CELL*)) y))))

  ; Draws each row of the triangle to the viewport.
  (define (draw-rows vp rows)
    (let* ((row (car rows))
           (y (* (- (length row) 1) (car *CELL*))))
      (draw-row vp row (- (/ *WIDTH* 2)
                          (+ (* (length row)
                             (car *CELL*)) 3)) y)
      (if (> (length rows) 1)
        (draw-rows vp (cdr rows)))))

  ; Initialising function. This is the one you should invoke!
  (define (draw-pascals-triangle)
    (open-graphics)
    (let ((vp (open-viewport "Fractals - Sierpinski Triangle (approx. from Pascal's Triangle)" *WIDTH* *HEIGHT*)))
      (draw-rows vp (pascals-triangle *ROWS*))))

  (provide draw-pascals-triangle))
