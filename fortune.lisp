(defvar fortunes
  #("Fortune favours the brave"
    "No gods, No masters"
    "If you gaze for long into an abyss, the abyss gazes also into you"))

(defun get-fortune ()
  (aref fortunes (random (length fortunes))))
