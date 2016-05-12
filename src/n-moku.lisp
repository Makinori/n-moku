(in-package #:n-moku)

;;;; parameters
(defparameter *num-players* 2)
(defparameter *board-size* 16)
(defparameter *line-up-num* 5)
(defparameter *board-hex-num* (* *board-size* *board-size*))

(defparameter *player-a* 1)
(defparameter *player-b* 2)
(defparameter *buffer* 0)

(defparameter *default-board*
	(make-array *board-hex-num* :initial-element *buffer*))
#|(defparameter *default-board*	#(0 0 0 0
                                0 2 1 0
                                0 1 2 0
                                0 0 0 0))
|#

(defparameter *board-hex-array*
	(loop for x to (1- *board-hex-num*)
		 collect x))

(let* (;;b:board, add:list-address, t:transposed, y-r:y-axis-rotated
       (add-b (get-lines #'(lambda (x y) (+ x (* *board-size* y))) *board-size*)) 
       (t-add-b (get-lines #'(lambda (x y) (+ y (* *board-size* x))) *board-size*)) 
       (y-r-add-b (get-lines
                   #'(lambda (x y) (+ (* y *board-size*) *board-size* -1 (- x)))
                   *board-size*))
       (b-tb-yrb-lines (append  add-b t-add-b
                                (slant-lines add-b *board-size*)
                                (slant-lines y-r-add-b *board-size*))))
  (defparameter *eval-lines* b-tb-yrb-lines)
  (defparameter *win-lines*
        (mapcan #'(lambda (lis) (get-n-length-lines lis *line-up-num*))
                b-tb-yrb-lines)))

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

(let ((copied-board))
  (defun put-to-board (player moving board)
    (setf copied-board (copy-seq board))
    (if moving
        (setf (aref copied-board moving) player)
        moving)
    copied-board))

(defun put-test ()
  (let ((tree (force (game-tree *player-a* 0 '() *default-board*))))
    (put-to-board (game-tree-player tree) 10
                         (game-tree-board tree))))


(defun change-player (player)
	(1+ (mod player *num-players*)))



#|(defun win-judge (player board)
	(some #'(lambda (line)
						(every #'(lambda (n)
											 (eql (aref board n) player))
									 line))
				*win-lines*))|#

(defun random-board ()
  (coerce
   (loop for x in (coerce *default-board* 'list)
      collect (random 3))
   'vector))

(defun win-judge (player board)
  (let ((player-line 0))
    (loop for line in *eval-lines*
       until (= player-line *line-up-num*)
       do (setq player-line 0)
         (loop for p in line
            until (= player-line *line-up-num*)
            do (if (= (aref board p) player)
                   (incf player-line)
                   (setq player-line 0))))         
    (= player-line *line-up-num*)))


;;;; tree, AI
(defstruct game-tree
	(player 0)
  (stair 0)
	(moving nil)
	(board *default-board*)
	(evaluation 0)
	(tree nil))

(defun game-tree (player stair moving board &optional lazy-tree)
  (lazy (search-game-tree player stair moving board lazy-tree)))

(defun search-game-tree (player stair moving board &optional lazy-tree)
  (make-game-tree
   :player player
   :moving moving
   :board board
   :stair stair
   :evaluation (eval-board player stair board)
   :tree (cond (lazy-tree lazy-tree)
               ((win-judge player board) nil)
               (t (passing-moves player (1+ stair) board)))))



(defun passing-moves (player stair board)
	(let ((next-player (change-player player)))
		(mapcar
		 #'(lambda (moving)
				 (lazy (search-game-tree next-player
                                 stair moving (put-to-board next-player moving board))))
		 (putable-address board))))

(defparameter *memod-time* 0)
(defparameter *memo-time* 0)

#|
(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp))) ;;tree-list :: game-tree-tree
  (defun game-tree (player stair moving board)
    (or (if (gethash board previous)
            (funcall old-game-tree player stair moving board (gethash board previous)))
        (let ((game-tree (funcall old-game-tree player stair moving board)))
          (setf (gethash board previous) (game-tree-tree (force game-tree)))
          game-tree))))
|#

(defparameter *win-point* 100)
(defparameter *draw-point* 0)


(defun eval-board (player stair board)
	(cond ((win-judge player board) (- *win-point* stair))
        (t *draw-point*)))

(let ((old-eval-board (symbol-function 'eval-board))
      (previous (make-hash-table :test #'equalp)))
  (defun eval-board (player stair board)
    (or (gethash board previous)
        (setf (gethash board previous) (funcall old-eval-board player stair board)))))

;;; min-max-way
(defun rate-position (player lazy-tree)
	(let ((tree (force lazy-tree)))
    (let ((moves (game-tree-tree tree))
          (eval  (game-tree-evaluation tree))
          (reduce-function
           #'(lambda (x y)
               (if (> (car x) (car y)) x y))))
      (cond
        ((null moves) (cons eval (game-tree-moving tree)))
        (t (reduce reduce-function
                   (get-ratings player tree)))))))

(defun get-ratings (player tree)
	(mapcar #'(lambda (move)
                   (rate-position player move ))
       (game-tree-tree tree)))

(defparameter *memo-used-time* 0)
(defparameter *memoed-time* 0)

(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table :test #'equalp)))
  (defun rate-position (player lazy-tree)
    (let ((tab (gethash player previous)))
      (unless tab
        (setf tab (setf (gethash player previous) (make-hash-table :test #'equalp))))
      (or (gethash lazy-tree previous)
          (setf (gethash lazy-tree previous)
                (funcall old-rate-position player lazy-tree))))))


(defun rate-test ()
  (let ((tree (game-tree *player-a* 0 '() *default-board*)))
    (format t "A: ~d~%B: ~d" 
            (rate-position *player-a* tree)
            (rate-position *player-b* tree))))


;;;; game

(defun game (handle-func1 handle-func2 lazy-tree &key (info-function #'print-info))
  (let ((tree (force lazy-tree)))
    (funcall info-function tree)
    (cond ((null (game-tree-tree tree))
           (if (win-judge (game-tree-player tree) (game-tree-board tree))
               (announce-winner (game-tree-player tree))
               (announce-draw)))
          (t (game
              handle-func2 handle-func1
              (funcall handle-func1 lazy-tree)
              :info-function info-function)))))

(defun human-vs-human (lazy-tree)
  (game #'handle-human #'handle-human lazy-tree))

(defun human-vs-computer (lazy-tree &optional (player-initiative? t))
  (multiple-value-bind (func1 func2)
        (if player-initiative?
            (values #'handle-human #'handle-computer)
            (values #'handle-computer #'handle-human))
    (game func1 func2 lazy-tree)))

(defun computer-vs-computer (lazy-tree)
  (game #'handle-computer #'handle-computer lazy-tree) )


(defun handle-human (lazy-tree)
  (let ((tree (force lazy-tree)))
    (fresh-line)
    (princ "chose your move:")
    (let ((moves (mapcar #'force (game-tree-tree tree))))
      (loop for move in moves
         for n from 1
         do (let ((action (game-tree-moving move)))
              (fresh-line)
              (format t "~a. -> put to ~a" n action)))
      (fresh-line)
      (nth (1- (read)) (game-tree-tree tree)))))

(defun handle-computer (lazy-tree)
  (let ((tree (force lazy-tree)))
    (let* ((rate-position (cdr (rate-position (game-tree-player tree) lazy-tree)))
           (game-tree-list (game-tree-tree tree)))
      (do ((i 0 (1+ i)))
          ((= rate-position (game-tree-moving (force (nth i game-tree-list))))
           (nth i game-tree-list))))))


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

(defun ai-test (tree)
  (com-vs-com tree))

(defun profile-n-moku ()
  (sb-profile:unprofile)
  (sb-profile:profile "N-MOKU" "CL-USER")
  (time 
   ;;(com-vs-com (game-tree 1 0 '() *default-board*)))
   (monte-carlo *player-a* (game-tree *player-a* 0 '() *default-board*)))
   ;;(human-vs-human (game-tree *player-b* 0 '() *default-board*)))
   ;;(dotimes (x 50) (playout *player-a* (game-tree *player-a* 0 '() *default-board*)) )
   ;;(dotimes (x 100) (call/playout *player-a* *default-board* 0)))
  (sb-profile:report)
  (sb-profile:unprofile))

(defun show-line-info (tree)
  (format t "~% Player:~r, moving:~d, board:~a, eval:~d"
          (game-tree-player tree)
          (game-tree-moving tree)
          (game-tree-board tree)
          (game-tree-evaluation tree)))


(defun com-vs-com (lazy-tree)
  (game #'handle-computer #'handle-computer lazy-tree :info-function #'show-line-info))
