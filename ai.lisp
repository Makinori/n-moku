
(defstruct game-tree
	(action (list -1 -1))
	(eval 0)
	(board *default-board*)
	(player *blank*)
	(player-list (list *blank*))
	(tree nil))

(defparameter *default-game-tree*
	(make-game-tree
	 :action '(-1 -1)
	 :eval 0
	 :board *default-board*
	 :player *player-b*
	 :player-list (list *player-a* *player-b*)
	 :tree nil))


(defun eval-function (board this-player self-player)
	(let ((judge (win-lose-judge board this-player)))
		(cond ((and (eq self-player this-player) judge)
					 100)
					((and (not (eq self-player this-player)) judge)
					 -100)
					(t 0))))


(defun blank-coordinate-list (board)
	(remove nil
					(reduce #'append
									(mapcar									 
									 #'(lambda (y)
											 (mapcar #'(lambda (x)
																	 (if (eq *blank* (nth x (nth y board)))
																			 (list x y) nil))
															 (stair-list (length (nth y board)))))
									 (stair-list (length board)))
									:initial-value nil :from-end t)))

;; tree search ;;;;;;;;

(defun print-game-tree (game-tree &key (indent 0))
	(mapcar (lambda (n) n (format t " ")) (stair-list indent))
	(format t "coord:~S  player:~S eval:~S~%"
					(game-tree-action game-tree)
					(car (game-tree-player-list game-tree))
					(game-tree-eval game-tree))
	(mapcar #'(lambda (tree)
							(print-game-tree tree :indent (+ 2 indent)))
					(game-tree-tree game-tree)))


(defun tree-search (game-tree)
	(make-game-tree
	 :board (game-tree-board game-tree)
	 :player (game-tree-player game-tree)
	 :player-list (game-tree-player-list game-tree)
	 :tree (tree-search-dash game-tree)))

(defun tree-search-dash (game-tree)
	(let* ((blank-coordinate-list (blank-coordinate-list (game-tree-board game-tree)))
				 (this-board (game-tree-board game-tree))
				 (this-player (car (game-tree-player-list game-tree)))
				 (next-player-list (append (cdr (game-tree-player-list game-tree))
																	 (list (car (game-tree-player-list game-tree))))))
		(mapcar 
		 #'(lambda (coordinate)
				 (let* ((changed-board (player-act this-player coordinate this-board)))
					 (make-game-tree
						:action coordinate
						:eval (eval-function changed-board
																 this-player
																 (game-tree-player game-tree))
						:board changed-board
						:player (game-tree-player game-tree)
						:player-list next-player-list
						:tree (tree-search-dash (make-game-tree :board changed-board
																										:player (game-tree-player game-tree)
																										:player-list next-player-list))
						)))
		 blank-coordinate-list
		 )))



;; test

(defun ignore-var (var)
	var
	nil)

(defun search-test (&key (time 10000))
	(time (ignore-var (mapcar #'(lambda (n) n (tree-search *default-game-tree*))
																	 (stair-list time)))))

