
;;;; parameters
(defparameter *num-players* 2)
(defparameter *board-size* 3)
(defparameter *line-up-num* 3)
(defparameter *board-hex-num* (* *board-size* *board-size*))

(defparameter *player-a* 1)
(defparameter *player-b* 2)
(defparameter *buffer* 0)

(defparameter *default-board*
	(make-array *board-hex-num* :initial-element *buffer*))

(defparameter *board-hex-array*
	(loop for x to (1- *board-hex-num*)
			 collect x))

(defparameter *win-lines*
	(labels ((get-win-line (line)
						 (loop for x to (length line)
								if (> (length line) (+ x -1 *line-up-num*))
								collect (subseq line x (+ x *line-up-num*))))
					 (get-lines (f)
						 (loop for y below *board-size*
								collect (loop for x below *board-size*
													 collect (funcall f x y))))
					 (slant-lines (board)
						 (mapcar
							#'(lambda (c)
									(let ((time (car c))
												(x (cadr c))
												(y (cddr c)))
										(loop for i from 0 to time
											 collect (nth (+ i x) (nth (- y i) board)))))
							(append (loop for x from 0 to (1- *board-size*)
												 collect (cons x (cons 0 x)))
											(loop for x downfrom (- *board-size* 2)
												 to 0
												 collect (cons x (cons (- *board-size* x 1)
																							 (1- *board-size*))))))))
		(let* ((add-b (get-lines #'(lambda (x y) (+ x (* *board-size* y))))) ;;add:list-address
					 (t-add-b (get-lines #'(lambda (x y) (+ y (* *board-size* x))))) ;;t:transposed
					 (y-r-add-b (get-lines #'(lambda (x y) ;y-r:y-axis-rotated
																		 (+ (* y *board-size*) *board-size* -1 (- x))))))
			(mapcan #'get-win-line
							(append add-b
											t-add-b
											(slant-lines add-b)
											(slant-lines y-r-add-b))))))


;;;; core
(defun board-array (lst)
	(make-array *board-hex-num* :initial-contents lst))

(defun player-latter (n)
	(cond ((eql n *player-a*) #\A)
				((eql n *player-b*) #\B)
				((eql n *buffer*) #\*)
				(t #\N)))

(defun draw-board (board)
	(loop for y below *board-size*
		 do (progn (fresh-line)
							 (loop for x below *board-size*
									do (format t "~a " (player-latter
																			(aref board (+ x (* *board-size* y)))))))))


;;;; rure-base
(defun putable-address (board)
	(remove-if-not (lambda (n)
									 (eq *buffer* (aref board n)))
					*board-hex-array*))

(defun put-to-board (player moveing board)
	(coerce (loop for x to (1- *board-hex-num*)
						 collect (if (eql x moveing)
												 player
												 (aref board x)))
					'vector))

(defun change-player (player)
	(1+ (mod player *num-players*)))

(defun win-judge (player board)
	(some #'(lambda (line)
						(every #'(lambda (n)
											 (eql (aref board n) player))
									 line))
				*win-lines*))

;;;; tree
(defun game-tree (player moveing board)
	(list player
				moveing
				board
				(if (win-judge player board)
						T
						(passing-moves board player))))


(defun passing-moves (board player)
	(let* ((next-player (change-player player))
				 (able-moves (putable-address board)))
		(mapcar
		#'(lambda (moveing)
				(game-tree next-player moveing (put-to-board next-player moveing board)))
		able-moves)))

;;;; game
(defun human-vs-human (tree)
	(print-info tree)
	(let ((next-moves (cadddr tree)))
		(cond ((null next-moves) (announce-draw))
					((listp next-moves) (human-vs-human (handle-human tree)))				 
					((eq t next-moves) (announce-winner (car tree))))))

(defun handle-human (tree)
	(fresh-line)
	(princ "chose your move:")
	(let ((moves (cadddr tree)))
		(loop for move in moves
				 for n from 1
				 do (let ((action (cadr move)))
							(fresh-line)
							(format t "~a. -> put to ~a" n action)))
		(fresh-line)
		(nth (1- (read)) moves)))

(defun announce-draw ()
	(format t "~%this game is draw ~%~%"))

(defun announce-winner (player)
	(format t "~%player ~a won this game~%~%" (player-latter player))
	)


(defun print-info (tree)
	(format t "~%")
	(format t "cullent-player = ~a" (player-latter (change-player (car tree))))
	(draw-board (caddr tree)))

;;;; test
(defun test ()
	(time (progn
					(game-tree *player-a* nil *default-board*)
					nil)))

(defun game-tree-last-lenght (tree)
	(let ((next-tree (car (last tree))))
		(if (not next-tree)
				1
				(reduce #'+
								(mapcar #'(lambda (tr)
														(game-tree-last-lenght tr))
												next-tree)))))

(defun show-tree (tree &optional (stage 0))
	(let ((player (car tree))
				(moveing (cadr tree))
				(board (caddr tree))
				(next-tree-list (cdddr tree))
				(indent (concatenate 'string (loop for x below stage collect #\_))))
		(format t "~d player:~d   moveing:~d   board:~d~%"
						indent player moveing board)
		
		(if (not (listp next-tree-list))
				(format t "game is settled~%")
				(mapcar #'(lambda (tr)
										(show-tree tr (+ stage 4)))
								(car next-tree-list)))
		nil))
