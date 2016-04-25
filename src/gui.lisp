
(in-package #:n-moku)


;;; parameters
(defparameter *gui.board* (copy-seq *default-board*))
(defparameter *gui.now-moving* nil)

(defparameter *gui.player-a-function* #'(lambda ()))
(defparameter *gui.player-b-function* #'(lambda ()))

(defparameter *game-tree* '())


;;; board-frame

(defparameter *cell-size* 24)
(defparameter *margin-size (/ *cell-size* 1))

(defmacro +margin-coordinate (margin &body body)
  `(mapcar #'(lambda (b) (+ b ,margin)) (list ,@body)))


(defun draw-board (canvas board cell-size margin)
  (itemconfigure canvas
                 (multiple-value-bind (x0 y0 x1 y1)
                     (values-list
                      (+margin-coordinate margin
                        0 0
                        (* *board-size* cell-size) (* *board-size* cell-size)))
                   (create-rectangle canvas x0 y0 x1 y1))
                 "fill" "#ff9900")
  (loop for y below *board-size*
     do 
       (loop for x below *board-size*
          do
            (itemconfigure
             canvas
             (create-line canvas
                          (+margin-coordinate (* margin 1.5)
                            0                               (* y cell-size)
                            (* (1- *board-size*) cell-size) (* y cell-size)))
             "fill" "#000000")
            (itemconfigure
             canvas
             (create-line canvas
                          (+margin-coordinate (* margin 1.5)
                            (* x cell-size) 0
                            (* x cell-size) (* (1- *board-size*) cell-size)))
             "fill" "#000000")))
  (loop for y below *board-size* 
     with address-player = nil
     do
       (loop for x below *board-size*
          do
            (setq address-player (aref board (+ x (* y *board-size*))))
            (unless (= *buffer* address-player)
              (itemconfigure
               canvas
               (multiple-value-bind (x0 y0 x1 y1)
                   (values-list
                    (+margin-coordinate margin
                      (* (+ 0 x) cell-size) (* (+ 0 y) cell-size)
                      (* (+ 1 x) cell-size) (* (+ 1 y) cell-size)))
                 (create-oval canvas x0 y0 x1 y1))
               "fill" (if (= *player-a* address-player) "#ffffff" "#000000"))
              ))))

(defun display-frame ()
  (with-ltk ()
    (let* ((board-frame (make-instance 'frame))
           (canvas (make-canvas board-frame
                                :width (* (+ *board-size* 3) *cell-size*)
                                :height (* (+ *board-size* 3) *cell-size*))))
      (pack board-frame)
      (pack canvas :side :left)
      (draw-board canvas *gui.board* *cell-size* *margin-size)
      
      )))
