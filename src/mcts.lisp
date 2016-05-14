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

;;(declaim (inline win-judge/mcts))
(defun win-judge/mcts (player moving puted-board)
  (declare (type integer player moving)
           (type simple-vector puted-board))
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

;;(declaim (inline rand-num-address))
(defun rand-num-address (num board)
  (declare (type integer num)
           (type simple-vector board))
  (do ((x 0 (1+ x))
       (i 0 i))
      ((and (= i num) (= *buffer* (aref board x))) x)
    (if (= *buffer* (aref board x)) (incf i))))
       
  
(let ((l-board #()) (first-move nil) (start-player 0) (filled-length 0))
  (declare (dynamic-extent l-board first-move start-player filled-length))
  (defun put-to-l-board (player moving)
    (setf (aref l-board moving) player) )
 
  (defun call/playout (player board)
    (setq l-board (copy-seq board))
    (setq start-player player)
    (setq filled-length (reduce #'(lambda (x y)
                                    (+ x (if (= *buffer* y) 0 1)))
                                l-board :initial-value 0))
    (setq first-move (rand-num-address (random (- *board-hex-num* filled-length)) l-board))
    (playout player first-move )
    )
  
  (defun playout (player moving)
    (put-to-l-board player moving)
    (incf filled-length)    
    (cond ((win-judge/mcts player moving l-board)
           (cons (if (= player start-player) 1 -1) first-move))
          ((= (1- *board-hex-num*) filled-length)
           (cons 0 first-move))
          (t (playout (change-player player)
                      (rand-num-address (random (- *board-hex-num* filled-length))
                                        l-board))))))

(defun monte-carlo (player lazy-tree)
  (let* ((tree  (force lazy-tree))
         (board (game-tree-board tree))
         (stair (game-tree-stair tree))
         (move-result-list (map 'vector #'(lambda (x) x
                                                  (cons 0 0))
                                (copy-seq *board-hex-array/mcts*)))
         (result nil)
         (result-address nil))
    (labels
        ((reflect-playout (remaining-time)
           (when (> remaining-time 0)
             (setq result (call/playout player board))
             (setq result-address (aref move-result-list (cdr result)))
             (setf (aref move-result-list (cdr result))
                   (cons (+ (car result-address)  (car result))
                         (+ (cdr result-address) 1)))
             (reflect-playout (1- remaining-time)))))
      (reflect-playout 20000)
      move-result-list)))

(defun handle-monte-carlo (lazy-tree)
  (let* ((playout (mapcar
                   #'(lambda (res hex)
                       (cons res hex))
                   (coerce (monte-carlo (game-tree-player (force lazy-tree)) lazy-tree)
                           'list)
                   *board-hex-array*))
         (good-address (cdar (remove-if
                              #'(lambda (x)
                                  (= 0 (cdar x)))
                              (sort playout #'(lambda (x y)
                                                (> (handler-case (/ (caar x) (cdar x))
                                                     (division-by-zero () 0))
                                                   (handler-case (/ (caar y) (cdar x))
                                                     (division-by-zero () 0)))))))))
    (address-to-tree good-address (game-tree-tree (force lazy-tree)))))
