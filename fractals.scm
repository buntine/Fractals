;;; Fractals
;;; Andrew Buntine, 2010 (http://www.andrewbuntine.com)
;;;
;;; This project consists of my efforts in trying to do some interesting
;;; things with Fractals in PLT Scheme/Racket.
;;;
;;; Usage:
;;;   Load this file and then execute the "draw" function, passing the
;;;   filename (without extension) as a symbol.
;;;
;;;   For example:
;;;     (require "/path/to/this/repo/fractals.scm")
;;;     (draw 'barnsleys-fern)
;;;     (draw 'pascals-triangle)
;;;
;;;   Or (after requiring) call the programs directly:
;;;     (draw-pascals-triangle)

(module fractals mzscheme
  (require "barnsleys-fern.scm")
  (require "pascals-triangle.scm")
  (require "uniform-tree.scm")

  ; Executes a program given by prg.
  (define (draw prg)
    ((eval (string->symbol
             (string-append "draw-"
                            (symbol->string prg))))))

  (provide draw
           (all-from "barnsleys-fern.scm")
           (all-from "pascals-triangle.scm")
           (all-from "uniform-tree.scm")))
