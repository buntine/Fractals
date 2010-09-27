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

  (define *DEPTH* 6)
  (define *LENGTH* 40)
  (define *WIDTH* (* *DEPTH* *LENGTH* 2))
  (define *HEIGHT* (* *DEPTH* *LENGTH*))

  (define (draw-branch posn depth offset vp)
    (let* ((len (/ *LENGTH* depth))
           (end-posn (make-posn (+ (posn-x posn) offset) (- (posn-y posn) len))))
      ((draw-line vp) posn end-posn "blue")
      (if (< depth *DEPTH*)
        (begin
          (draw-branch end-posn (+ depth 1) (- offset len) vp)
          (draw-branch end-posn (+ depth 1) (+ offset len) vp)))))

  (define (draw-uniform-tree)
    (open-graphics)
    (let ((vp (open-viewport "Fractals - Uniform tree" *WIDTH* *HEIGHT*)))
      (draw-branch (make-posn (/ *WIDTH* 2) *HEIGHT*)
                   1 0 vp)))

  (provide draw-uniform-tree))
