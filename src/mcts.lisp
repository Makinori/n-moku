(in-package #:n-moku)

;;; monte-carlo tree-search
(defparameter *address-win-lines*
  (coerce (loop for x in *board-hex-array*
             collect (remove-if-not #'(lambda (l) (find x l))
                                    *win-lines*))
          'vector))


(defparameter *address-eval-lines*
  (let ((exist-address nil)
        (address-eval-lines (make-array *board-hex-num* :initial-element nil)))
    (dolist (x *board-hex-array*)
      (setq exist-address (apply #'append (aref *address-win-lines* x)))
      (setf (aref address-eval-lines x)
            (mapcar
             #'(lambda (list) (remove-if-not #'(lambda (x) (find x exist-address)) list))
             (remove-if-not
              #'(lambda (list) (and (find x list)))
              *eval-lines*))))
    address-eval-lines))


(defun win-judge/mcts (player moving puted-board)
  (let ((player-line 0))
    (loop for line in (aref *address-eval-lines* moving)
       until (= player-line *line-up-num*)
       do (setq player-line 0)
         (loop for p in line
            until (= player-line *line-up-num*)
            do (if (= (aref puted-board p) player)
                   (incf player-line)
                   (setq player-line 0))))
    (= player-line *line-up-num*)))

(defparameter *board-hex-array/mcts* (coerce *board-hex-array* 'vector))

(defun rand-num-address (num board)
  (do ((x 0 (1+ x))
       (i 0 i))
      ((and (= i num) (= *buffer* (aref board x))) x)
    (if (= *buffer* (aref board x)) (incf i))))
       
  
(let ((l-stair 0) (l-board #()) (first-move nil) (start-player 0))
  (defun put-to-l-board (player moving)
    (setf (aref l-board moving) player) )
 
  (defun call/playout (player  board stair)
    (setq l-stair stair l-board (copy-seq board)
          start-player player
          first-move (rand-num-address (random (- *board-hex-num* l-stair)) l-board))
    (playout player first-move ))
  
  (defun playout (player moving)
    (put-to-l-board player moving)
    (cond ((or (win-judge/mcts player moving l-board) (= *board-hex-num* (1+ l-stair)))
           (cons (= player start-player) first-move))
          (t (incf l-stair)
             (playout (change-player player)
                      (rand-num-address (random (- *board-hex-num* l-stair)) l-board))))))

(defun monte-carlo (player lazy-tree)
  (let* ((tree  (force lazy-tree))
         (board (game-tree-board tree))
         (stair (game-tree-stair tree))
         (move-result-list (make-array (length (game-tree-tree tree))
                                       :initial-element (cons 0 0)))
         (result nil)
         (result-address nil))
    (labels
        ((reflect-playout (remaining-time)
           (when (> remaining-time 0)
             (setq result (call/playout player board stair))
             (setq result-address (aref move-result-list (cdr result)))
             (setf (aref move-result-list (cdr result))
                   (cons (+ (car result-address) (if (car result) 1 0))
                         (+ (cdr result-address) 1)))
             (reflect-playout (1- remaining-time)))))
      (reflect-playout 10000)
      move-result-list)))

