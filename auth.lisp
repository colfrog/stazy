(in-package :stazy)

(defun get-md5 (s)
  "Returns the md5 sum of s as a string"
  (let ((sum (md5sum-string s))
	(out ""))
    (dotimes (i (length sum))
      (setf out (concatenate 'string out (format nil "~2,'0x" (aref sum i)))))
    out))

(defun credentials-valid (username password)
  (let ((pass-md5 (sqlite:execute-single *db* "SELECT password FROM users WHERE username = ?" username)))
    (when pass-md5 (equal (get-md5 password) pass-md5))))

(define-easy-handler (login-page :uri "/login")
    ((username :request-type :POST)
     (password :request-type :POST))
  (if (is-logged-in) (redirect "/")
      (let ((error-message nil))
	(when (and username password)
	  (if (credentials-valid username password)
	      (progn
	        (setf (session-value :username) username)
		(redirect "/admin/"))
	      (setf error-message "Invalid username or password")))
	(with-layout ("Login")
	  (:div
	   (when error-message
	     (htm (:h5 :style "color:red;" (str error-message))))
	   (:form :method "POST" :action "/login"
		  (:label :for "username" "Username: ")
		  (:input :type "text" :name "username")
		  (:br)
		  (:label :for "password" "Password: ")
		  (:input :type "password" :name "password")
		  (:br)
		  (:input :type "submit")))))))

(define-easy-handler (logout-page :uri "/logout") ()
  (remove-session (start-session))
  (redirect "/"))
