;;;; IO ;;;;;;;;;;;;;;;;;;;;;;
(defun show-all-ways-d (board this-player)
	(tree-search (make-game-tree :board board
																 :player this-player
																 :player-list (list this-player))))

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
		(cond (t (list x y))
					(t (get-coordinate-by-input)))))

(defun repl (board player-list)
	(let* ((next-player-list (append (cdr player-list) (list (car player-list))))
				 (chosen-coordinate (get-coordinate-by-input))
				 (acted-board (player-act (car player-list) chosen-coordinate board)))
		(if (not acted-board)
				(repl board player-list)
				(progn ;;(format t "~s ~%" (blank-coordinate-list acted-board))
							 (show-board acted-board)
							 (if (win-lose-judge acted-board (car player-list) )
									 (format t "~% PLAYER-~S win this game" (car player-list))
									 (repl acted-board next-player-list))))))

(defun game ()
	(repl *default-board* (list *player-a* *player-b*)))
