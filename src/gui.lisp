
(in-package #:n-moku)


;;; parameters
(defparameter *gui.board* (copy-seq *default-board*))
(defparameter *gui.now-moving* nil)

(defparameter *gui.player-a-function* #'(lambda ()))
(defparameter *gui.player-b-function* #'(lambda ()))

(defparameter *game-tree* '())


;;; board-frame

(defparameter *cell-size* 24)
(defparameter *margin-size* (/ *cell-size* 1))


(defmacro +margin-coordinate (margin &body body)
  `(let ((m ,margin))
     (mapcar #'(lambda (b) (+ b m)) (list ,@body))))


(defun draw-board-diff (canvas address player cell-size margin)
  (if address
      (let ((y (floor (/ address *board-size*)))
            (x (mod   address *board-size*)))
        (itemconfigure canvas
                       (multiple-value-bind (x0 y0 x1 y1)
                           (values-list
                            (+margin-coordinate margin
                              (* (+ 0 x) cell-size) (* (+ 0 y) cell-size)
                              (* (+ 1 x) cell-size) (* (+ 1 y) cell-size)))
                         (create-oval canvas x0 y0 x1 y1))
                       "fill" (if (= *player-b* player) "#ffffff" "#000000")))))

(defun draw-board-first (canvas board cell-size margin)
  (itemconfigure canvas
                 (multiple-value-bind (x0 y0 x1 y1)
                     (values-list
                      (+margin-coordinate
                          margin
                        0 0
                        (* *board-size* cell-size) (* *board-size* cell-size)))
                   (create-rectangle canvas x0 y0 x1 y1))
                 "fill" "#ff9900")
  (loop for y below *board-size*
     do (loop for x below *board-size*
           do (itemconfigure
               canvas
               (create-line canvas
                            (+margin-coordinate
                                (* margin 1.5)
                              0
                              (* y cell-size)
                              (* (1- *board-size*) cell-size) (* y cell-size)))
               "fill" "#000000")
             (itemconfigure
              canvas
              (create-line canvas
                           (+margin-coordinate
                               (* margin 1.5)
                             (* x cell-size) 0
                             (* x cell-size) (* (1- *board-size*) cell-size)))
              "fill" "#000000")))
  (loop for y below *board-size*
     do (loop for x below *board-size*
           do (if (not (equal (aref board (+ x (* y *board-size*))) *buffer*))
                  (draw-board-diff canvas (+ x (* y *board-size* ))
                                   (aref board (+ x (* y *board-size*)))
                                   cell-size margin)))))



(defun handle-gui-address (canvas cell-size margin)
  (let ((address nil))
    (bind
     canvas "<ButtonPress-1>"
     (lambda (evt)
       (let* ((y-crd (truncate (- (event-y evt) margin) cell-size))
              (x-crd (truncate (- (event-x evt) margin) cell-size)))
         (setf address (+ (* y-crd *board-size*) x-crd)))))
    (print address)
    address))


(defun display-frame ()
  (let* ((moving-list nil)
         (this-turn-tree (force (game-tree *player-b* 0 '() (copy-seq *default-board*))))

         (handle-func1 'handle-human)
         (handle-func2 'handle-monte-carlo)
         (handle-func handle-func1)
         (handled-tree nil))
    (with-ltk ()
      (let* (;; widgets ;;;;;;;;;;;;;;
             (board-frame (make-instance 'frame))
             (canvas (make-canvas board-frame
                                  :width (* (+ *board-size* 3) *cell-size*)
                                  :height (* (+ *board-size* 3) *cell-size*)))
             (button-frame (make-instance 'frame))
             (tb (make-text button-frame :width nil :height 5)))
        
        (labels
            ((show&change-info ()
               (setq this-turn-tree (force handled-tree))
               (setq handled-tree nil)
               (setq moving-list (cons (game-tree-moving this-turn-tree) moving-list))
               
               (draw-board-diff canvas
                                (game-tree-moving this-turn-tree)
                                (game-tree-player this-turn-tree)
                                *cell-size* *margin-size*)
               (append-text tb (format nil "player:~d moved:~d ~%"
                                       (game-tree-player this-turn-tree)
                                       (game-tree-moving this-turn-tree)))
               (cond ((null (game-tree-tree this-turn-tree))
                      (if (win-judge (game-tree-player this-turn-tree)
                                     (game-tree-board this-turn-tree))
                          (append-text tb (format nil "player ~d won this game~%"
                                                  (game-tree-player this-turn-tree)))
                          (append-text tb (format nil "this game is draw~%"))))
                     (t
                      (setq handle-func
                            (if (equal handle-func1 handle-func)
                                handle-func2 handle-func1))))))
          moving-list
          this-turn-tree
          ;; pack ;;;;;;;;;;;;;;;;
          (pack board-frame)
          (pack canvas :side :left)
          (pack button-frame)
          (pack tb :side :left)
          
          (draw-board-first canvas (game-tree-board this-turn-tree)
                            *cell-size* *margin-size*)

          ;; agame-loop ;;;;;;;;;;;;;
          (bind
           canvas "<ButtonPress-1>"
           (lambda (evt)
             evt
             (if
              ;; handle-gui-human
              (equal handle-func 'handle-human)
              (let* ((y-crd (truncate (- (event-y evt) *margin-size*) *cell-size*))
                     (x-crd (truncate (- (event-x evt) *margin-size*) *cell-size*))
                     (address (+ (* y-crd *board-size*) x-crd)))
                (setq handled-tree
                      (address-to-tree address (game-tree-tree this-turn-tree ))
                      )))
             
             (when handled-tree
               
               (show&change-info)
               
               
               (cond
                 ((equal handle-func 'handle-human) nil)
                 ((equal handle-func 'handle-monte-carlo)
                  (setq handled-tree (handle-monte-carlo (lazy this-turn-tree)))
                  (show&change-info)
                  
                  ))
               
               
               ))
           
           ))
        ))))


