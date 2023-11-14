(defvar fortunes
  #("Fortune favours the brave"
    "No gods, No masters"
    "Quelle chance!"
    "The One True God is &#9675;"
    "Larry Wall was right"
    "Dennis Ritchie is coding for God now"
    "No man ever steps in the same river twice,<br />for it's not the same river and he's not the same man"
    "This page was made possible by Mark C. Lisp"
    "Sponsored by your local pizza place"
    "What's new pussycat?"
    "(&#955;&#119909;.&#119909;&#x1D465;)(&#955;&#119909;.&#119909;&#x1D465;)"
    "If you gaze for long into an abyss, the abyss gazes also into you"))

(defun get-fortune ()
  (aref fortunes (random (length fortunes))))
