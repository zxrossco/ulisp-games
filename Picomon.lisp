;
; PICOMON
;
; Guessing game, follow what you're shown
;

;
; rgb : Convert r,g,b, values into 16 bit int
(defun rgb (r g b)
  (logior (ash (logand r #xf8) 8) (ash (logand g #xfc) 3) (ash b -3)))

; screen positions
(defvar positions (list '(160 60) '(240 120) '(160 180) '(80 120)))

; button pins
(defvar *button-up* 3)
(defvar *button-down* 4)
(defvar *button-left* 15)
(defvar *button-right* 5)
(defvar *button-center* 14)
(defvar *button-select* 1)
(defvar *button-back* 0)

; game buttons
(defvar *buttons* (list *button-up* *button-right* *button-down* *button-left* *button-center* *button-select* *button-back*))

; game colors
(defvar *white* (rgb 255 255 255))
(defvar *black* (rgb 0 0 0))
(defvar *red* (rgb 255 0 0))
(defvar *green* (rgb 0 255 0))
(defvar *blue* (rgb 0 0 255))
(defvar *yellow* (rgb 255 254 0))
(defvar *radius* 40)

(defvar color (list *red* *green* *blue* *yellow*))

(defun rnd ()
  (logior #b01111111 (ash (random 2) 7)))

(defun rndrgb ()
  (rgb (rnd) (rnd) (rnd)))

; Pretty title
(defun title () 
  (list 
    (list "P" (rndrgb)) 
    (list "I" (rndrgb)) 
    (list "C" (rndrgb)) 
    (list "O" (rndrgb)) 
    (list "M" (rndrgb)) 
    (list "O" (rndrgb)) 
    (list "N" (rndrgb))))




(defun circ (x)
  (let* ((posX (car (nth x positions)))
        (posY (cadr( nth x positions))))
  (fill-circle posX posY *radius* (nth x color))
  (draw-circle posX posY *radius* *white*)))


(defun erase-circ (x)
(let* ((posX (car (nth x positions)))
      (posY (cadr( nth x positions))))
  (fill-circle posX posY *radius* *black*)
  (draw-circle posX posY *radius* *white*)))

; Colored circle, beep a tone, earse the circle
(defun game-beep (x)
    (circ x)
    (note 27 (nth x '(0 3 5 7)) 4)
    (del)
    (erase-circ x)
    (note)
    (del))

; Simple Pimon game loop
(defun loop-pimon () 
  (let* ((seq (list (random 4))))
    (loop
     (mapc game-beep (reverse seq))
     (mapc player-check (reverse seq))
     (push (mod (+ (car seq) (random 3) 1) 4) seq)
     (del))))

; Pimon Game Entry     
(defun play-pimon ()
  (fill-screen)
  (mapc erase-circ '(0 1 2 3))
  (delay 500)
  (mapc game-beep '(0 1 2 3))
  (mapc erase-circ '(0 1 2 3))
  (delay 500)
  (draw-ready)
  (delay 1000)
  (fill-screen)
  (mapc erase-circ '(0 1 2 3))
  (loop-pimon))

; wait a while
(defun del () (delay 350))



; make the button a pullup
(defun init-button (button-pin)
  (pinmode button-pin :input-pullup))

; make all the buttons a pullup
(defun init-buttons ()
  (mapcar init-button *buttons*))


; read the *buttons* 0-6 in turn, return the index if the button is pressed low/nil
(defun check-button ()
  (init-buttons)
  (dolist (i '(0 1 2 3 4 5 6))
        (when (and (not (digitalread (nth i *buttons*)))
                   (not (digitalread (nth i *buttons*)))) 
          (return i))))

(defun wait-for-button ()
  (let ((start-time (millis)))
    (loop
        (let ((button-index (check-button)))
          (when button-index
            (return button-index)))
      (when (> (- (millis) start-time) 1000)
        (return nil)))))

; Play a little jingle
(defun endgame ()
  (mapc 
   (lambda (n) (note 27 n 4) (delay 100))
   '(0 2 4 5 7 9 11 12)) 
  (note)
  (return nil))

; Game check a button 0-3 is pressed
(defun player-check (btn) 
  (if (equal btn (wait-for-button)) (game-beep btn) (endgame)))



(defun draw-ready ()
  (with-gfx (strm)
      (set-cursor 100 100)
      (set-text-size 5)
      (set-text-color *yellow*)
      (princ "READY" strm)))


(defun draw-title ()
  (with-gfx (strm)
    (set-cursor 60 50)
    (set-text-size 5)
    (mapc (lambda (ltr) (set-text-color (cadr ltr)) (princ (car ltr) strm)) (title)))
)

(defun draw-presskey ()
     (with-gfx (strm)
       (set-cursor 70 150)
       (set-text-size 2)
       (set-text-color *yellow*)
       (princ "Press a key..." strm))
)

(defun draw-score (score)
      (with-gfx (strm)
        (fill-screen)
        (fill-round-rect 30 100 240 35 5 *yellow*)
        (set-cursor 40 105)
        (set-text-size 3)
        (set-text-color *black* *yellow*)
        (princ "Your Score " strm)
        (princ score strm)))

(defun game-welcome ()
  (fill-screen)
  (set-rotation 1)
  
    (loop
        (draw-presskey)
        (loop
            (draw-title)
            (delay 100)
              (when (check-button)
                (return)))
        (let ((result (play-pimon)))
          (draw-title)
          (draw-presskey)
          (draw-score (- (length result) 1)))))


