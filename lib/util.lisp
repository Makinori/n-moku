;;(in-package :n-moku)
;;;; utility
(in-package #:n-moku)

;;; Matrix calculation

(defun slant-lines (board board-size)
  "
ARGMENTS : two-dimensional-list, board width or height
EVAL-TO  : rotated-two-dimensional-list

this FUNCTION make two-dimensional-list(matrix) -45degree rotated from center of matrix.

;;;;; example ;;;;;;;;;;;;;;;;;;;;;;;;
CL-USER> (slant-lines '((0 1) (2 3)) 2)
 ((0) (2 1) (3))

  |input|return
yx| 0 1 | 0  1   
===============
 0| 0 1 | 0  #
---------------
 1| 2 3 | 1  2
---------------
 2| # # | 3  #
---------------
"
  (mapcar
   #'(lambda (c)
       (let ((time (car c))  (x (cadr c))  (y (cddr c)))
         (loop for i  to time
            collect (nth (+ i x) (nth (- y i) board)))))
   (append (loop for x below board-size
              collect (cons x (cons 0 x)))
           (loop for x downfrom (- board-size 2) to 0
              collect (cons x (cons (- board-size x 1)
                                    (1- board-size)))))))

(defun get-lines (f board-size)
  "
example

CL-USER> (get-lines #'(lambda (x y) (+ (* y 3) (* x 1))) 3)
 ((0 1 2) (3 4 5) (6 7 8))
"
  (loop for y below board-size
     collect (loop for x below board-size
                collect (funcall f x y))))


(defun get-n-length-lines (line length)
  "
example

CL-USER> (get-n-length-lines '(1 2 3 4 5) 3)
 ((1 2 3) (2 3 4) (3 4 5))
"
  (loop for x to (length line)
     if (> (length line) (+ x -1 length))
     collect (subseq line x (+ x length))))


;;;; Lazy-function

