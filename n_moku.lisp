(load "util.lisp")


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
#|(defparameter *default-board*	#(2 0 0 1
																0 2 1 0
																0 2 0 0
                                1 0 1 0))
|#
(defparameter *default-board* #(2 1 2
                                0 1 0
                                0 2 1))


(defparameter *board-hex-array*
	(loop for x to (1- *board-hex-num*)
		 collect x))

(defparameter *win-lines*
		(let* (;;b:board, add:list-address, t:transposed, y-r:y-axis-rotated
           (add-b (get-lines #'(lambda (x y) (+ x (* *board-size* y))) *board-size*)) 
					 (t-add-b (get-lines #'(lambda (x y) (+ y (* *board-size* x))) *board-size*)) 
					 (y-r-add-b (get-lines
                       #'(lambda (x y) (+ (* y *board-size*) *board-size* -1 (- x)))
                       *board-size*)))
			(mapcan #'(lambda (lis) (get-n-length-lines lis *line-up-num*))
							(append add-b	  t-add-b
											(slant-lines add-b *board-size*)
                      (slant-lines y-r-add-b *board-size*)))))


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


;;;; rule-base
(defun putable-address (board)
	(remove-if-not (lambda (n)
									 (eq *buffer* (aref board n)))
								 *board-hex-array*))

(defun put-to-board (player moving board)
	(coerce (loop for x to (1- *board-hex-num*)
						 collect (if (eql x moving)
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
(defstruct game-tree
	(player 0)
  (stair 0)
	(moving nil)
	(board *default-board*)
	(evaluation 0)
	(tree nil))

(defun game-tree (player stair moving board)
  (make-game-tree
   :player player
   :moving moving
   :board board
   :stair stair
   :evaluation (eval-board player stair board)
   :tree (if (win-judge player board)
             nil
             (passing-moves player (1+ stair) board))))

(defun passing-moves (player stair board)
	(let* ((next-player (change-player player))
				 (able-moves (putable-address board)))
		(mapcar
		 #'(lambda (moving)
				 (game-tree next-player stair moving (put-to-board next-player moving board)))
		 able-moves)))


;;;; ai
(defparameter *win-point* 100)
(defparameter *draw-point* 0)

(defun eval-board (player stair board)
	(cond ((win-judge player board) (- *win-point* stair))
				(t *draw-point*)))


(defun rate-position (player tree)
	(let ((players-turn? (equal player (game-tree-player tree)) )
				(moves (game-tree-tree tree))
				(eval  (game-tree-evaluation tree)))
		(cond
			((null moves) (cons eval (game-tree-moving tree)))
      (t (reduce #'(lambda (x y)
                     (if (> (car x) (car y))
                         x y))
                 (get-ratings player tree))))))

(defun get-ratings (player tree)
	(mapcar #'(lambda (move)
						(rate-position player move ))
					(game-tree-tree tree)))

(defun rate-test ()
  (let ((tree (game-tree *player-a* 0 '() *default-board*)))
    (show-tree tree)
    (format t "A: ~d~%B: ~d" 
            (rate-position *player-a* tree)
            (rate-position *player-b* tree))))

;;;; game
(defun human-vs-human (tree)
	(print-info tree)
	(let ((next-moves (game-tree-tree tree)))
		(cond ((null next-moves)
					 (if (win-judge (game-tree-player tree) (game-tree-board tree))
							 (announce-draw)
							 (announce-winner (game-tree-player tree))))
					(t (human-vs-human (handle-human tree))))))

(defun human-vs-computer (tree players-turn?)
  (print-info tree)
  (cond ((null (game-tree-tree tree))
         (if (win-judge (game-tree-player tree) (game-tree-board tree))
             (announce-winner (game-tree-player tree))
             (announce-draw)))
        (t (human-vs-computer
            (funcall (if players-turn? #'handle-human #'handle-computer) tree)
            (not players-turn?)))))

(defun computer-vs-computer (tree)
  (print-info tree)
  (cond ((null (game-tree-tree tree))
         (if (win-judge (game-tree-player tree) (game-tree-board tree))
             (announce-winner (game-tree-player tree))
             (announce-draw)))
        (t (computer-vs-computer
            (handle-computer tree)))))



(defun handle-human (tree)
	(fresh-line)
	(princ "chose your move:")
	(let ((moves (game-tree-tree tree)))
		(loop for move in moves
			 for n from 1
			 do (let ((action (game-tree-moving move)))
						(fresh-line)
						(format t "~a. -> put to ~a" n action)))
		(fresh-line)
		(nth (1- (read)) moves)))


(defun handle-computer (tree)
  (let* ((rate-position (cdr (rate-position (game-tree-player tree) tree)))
         (game-tree-list (game-tree-tree tree)))
    (do ((i 0 (1+ i)))
        ((= rate-position (game-tree-moving (nth i game-tree-list)))
           (nth i game-tree-list)))))


(defun announce-draw ()
	(format t "~%this game is draw ~%~%"))

(defun announce-winner (player)
	(format t "~%player ~a won this game~%~%" (player-latter player)))

(defun print-info (tree)
	(format t "~%")
  (format t "~%cullent-player = ~a"
          (player-latter (change-player (game-tree-player tree))))
	(draw-board (game-tree-board tree)))


;;;; test

(defun show-tree (tree &optional (stage 0))
	(let ((player (game-tree-player tree))
				(moving (game-tree-moving tree))
				(board (game-tree-board tree))
				(evaluation (game-tree-evaluation tree))
				(next-tree-list (game-tree-tree tree))
				(indent (concatenate 'string (loop for x below stage collect #\_))))
		(format t "~d player:~d   moving:~d   board:~d  eval:~d~%"
						indent player moving board evaluation)		
		(mapcar #'(lambda (tr)
								(show-tree tr (+ stage 4)))
						next-tree-list)
		nil))


(defun ai-test (tree)
  (labels 
      ((com-vs-com (tr)
         (format t "~% Player:~r, moving:~d, board:~a, eval:~d"
                 (game-tree-player tr)
                 (game-tree-moving tr)
                 (game-tree-board tr)
                 (game-tree-evaluation tr))
       (cond ((null (game-tree-tree tr))
              (if (win-judge (game-tree-player tr) (game-tree-board tr))
                  (announce-winner (game-tree-player tr))
                  (announce-draw)))
             (t (com-vs-com
                 (handle-computer tr))))))
    (time (com-vs-com tree))))

