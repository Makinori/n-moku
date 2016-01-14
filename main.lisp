
(defparameter *side* 3)
(defparameter *player-a* 'A)
(defparameter *player-b* 'B)
(defparameter *blank* nil)

(defparameter *default-board* (make-list *side* :initial-element
																				 (make-list *side* :initial-element *blank*)))

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


;;;; board-function ;;;;;;;;;;;;;;;;;;;;;
;;; player-action
(defun change-board (player coordinate board)
	(let* ((x (car coordinate))
				 (y (cadr coordinate))
				 (determinted-col (rewrite-element x player (nth y board))))
		(rewrite-element y determinted-col board)))

(defun verificate-act (coordinate board) ;; verificatinon action
	(let ((x (car coordinate))
				(y (cadr coordinate)))
		(eq *blank* (nth x (nth y board)))))

(defun player-act (player coordinate board)
	(if (verificate-act coordinate board)
			(change-board player coordinate board)))

;;;;;;
(defun win-lose-judge (board player)
	(let ((transposed-list (transpose board))
				(diagonal-list-a (diagonal-list board))
				(diagonal-list-b (diagonal-list (mapcar #'reverse board))))
		(some #'(lambda (list)
							(every #'(lambda (x) (eq x player)) list))
					(append board transposed-list (list diagonal-list-a diagonal-list-b)))))

(defun eval-function (board self-player enemy-player)
	(cond ((win-lose-judge board self-player) 100)
				((win-lose-judge board enemy-player) -100)
				(t 0)))

(defun wood-search ())


;;;; IO ;;;;;;;;;;;;;;;;;;;;;;
(defun show-board (board)
	(labels ((show-each-piece (x)
						 (format t " ~a" (cond ((eq x *player-a*) 'A)
																	 ((eq x *player-b*) 'B)
																	 ((eq x *blank*) '*)))))
		(mapcar #'(lambda (list)(mapcar #'show-each-piece list)
											(format t "~%"))
						board)
		(format t "=====================~%")))


(defun get-coordinate-by-input ()
	(let* ((x (read))
				 (y (read)))
		(cond ((and	(<= 0 x) (< x *side*)
								(<= 0 y) (< y *side*))
					 (list x y))
					(t (get-coordinate-by-input)))))

(defun repl (board player-list)
	(let* ((next-player-list (append (cdr player-list) (list (car player-list))))
				 (chosen-coordinate (get-coordinate-by-input))
				 (acted-board (player-act (car player-list) chosen-coordinate board)))
		(if (not acted-board)
				(repl board player-list)
				(progn (show-board acted-board)
							 (if (win-lose-judge acted-board (car player-list) )
									 (format t "~% PLAYER-~S win this game" (car player-list))
									 (repl acted-board next-player-list))))))

(defun game ()
	(repl *default-board* (list *player-a* *player-b*)))
