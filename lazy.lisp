(in-package #:n-moku)

;;;; lazy functions


(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nli))
      (lambda ()
        (unless ,forced
          (setf ,value (progn ,@body))
          (setf ,forced t))
        ,value))))

(defun force (lazy-value)
  (funcall lazy-value))

;;; lazy-list
(defmacro lazy-cons (a b)
  `(lazy (cons ,a ,b)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))



