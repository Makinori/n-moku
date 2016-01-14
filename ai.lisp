(defun eval-function (board self-player enemy-player)
	(cond ((win-lose-judge board self-player) 100)
				((win-lose-judge board enemy-player) -100)
				(t 0)))

(defun wood-search ())


 
