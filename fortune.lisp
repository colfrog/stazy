(defvar fortunes
  #("Fortune favours the brave"
    "No gods, No masters"
    "Who do you think you are?"
    "Quelle chance!"
    "A fake plastic watering can..."
    "This page was made possible by Mark C. Lisp"
    "Sponsored by your local pizza place"
    "What's new pussycat?"
    "(&#955;&#119909;.&#119909;&#x1D465;)(&#955;&#119909;.&#119909;&#x1D465;)"
    "If you gaze for long into an abyss, the abyss gazes also into you"))

(defun get-fortune ()
  (aref fortunes (random (length fortunes))))
