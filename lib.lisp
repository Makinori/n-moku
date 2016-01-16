
;;;; list-function ;;;;;;;;;;;;;;;;;

(defun transpose (matrix)
	(cond ((every #'null matrix) nil)
				(t (cons (mapcar #'car matrix)
								 (transpose (mapcar #'cdr matrix ))))))

(defun flatten (list)
	(if (null list)
			nil
			(let ((elem (car list))
						(last-list (cdr list)))
				(if (listp elem)
						(append (flatten elem) (flatten last-list))
						(append (cons elem nil) (flatten last-list))))))

(defun diagonal-list (matrix)
	(cond ((null matrix) nil)
				((every #'null matrix) nil)
				(t (cons (caar matrix)
								 (diagonal-list
									(mapcar #'cdr (cdr matrix)))))))

(defun rewrite-element (n element list)
	(cond ((null list) '())
				((= n 0) (cons element (cdr list)))
				(t (cons (car list) (rewrite-element (- n 1) element (cdr list))))))

(defun stair-list (max &key (start 0))
	(if (= start max) nil
			(cons start (stair-list max :start (1+ start)))))

;; loading

(load "core.lisp")
(load "ai.lisp")
(load "main.lisp")
