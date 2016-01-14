
;;;; data-base  ;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *side* 3)
(defparameter *player-a* 'A)
(defparameter *player-b* 'B)
(defparameter *blank* nil)

(defparameter *default-board* (make-list *side* :initial-element
																				 (make-list *side* :initial-element *blank*)))


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

