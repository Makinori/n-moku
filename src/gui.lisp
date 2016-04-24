
(in-package #:n-moku)



(defvar *gui.board* *default-board*)
(defvar *gui.now-moving* nil)

(defvar *gui.player-a-function* #'(lambda ()))
(defvar *gui.player-b-function* #'(lambda ()))

(defvar *game-tree*)

(defun hello ()
  (with-ltk ()
    (let*
        ((f (make-instance 'frame))
         (b1 (make-instance 'button
                           :master f
                           :text "press Me"
                           :command (lambda ()
                                      (format t "hello world~&"))))
         (b2 (make-instance 'button
                            :master f
                            :text "press Me 2"
                            :command (lambda ()
                                       (format t "button2 ~%")))))
      (pack f)
      (pack b1 :side :left)
      (pack b2 :side :left)
      (configure f :borderwidth 5 :relief :sunken )
  )))

(defun buttons ()
  (with-ltk ()
    (let*
        ((f (make-instance 'frame))
         (button-col-f-s (loop for x from 0 to *board-size*
                            collect (make-instance 'frame
                                                   :master f)))
         (buttons (mapcar
                   #'(lambda (num)
                       (make-instance
                        'button
                        :master (nth (floor (/ num *board-size*))
                                     button-col-f-s)
                        :text num
                        :command (lambda ()
                                   (format t "presssed: ~d~%" num))))
                   *board-hex-array*)))
      (mapcar #'(lambda (b) (print (slot-value b 'master))) buttons)
      (pack f)
      (dolist (button-col-f button-col-f-s)
        (pack button-col-f :side :top))
      (dolist (button buttons)
        (pack button :side :left))
      (configure f :borderwidth 5 :relief :sunken )
  )))

(defun main-frame ()
  (with-ltk ()
    (let* ((board-frame (make-instance 'frame))
           ))))

