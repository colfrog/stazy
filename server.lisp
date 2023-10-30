(ql:quickload :hunchentoot)
(ql:quickload :sqlite)
(ql:quickload :cmark)
(ql:quickload :cl-who)

(defvar *db-path* (or (last (uiop:command-line-arguments)) "/media/laurent/storage/tulip/db.sqlite"))
(defvar *db* (sqlite:connect *db-path*))

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))

(hunchentoot:define-easy-handler (home-page :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (let ((markdown
	  (sqlite:execute-single
	   *db*
	   "select markdown from home where username = 'laurent'")))
    (libcmark:markdown-to-html markdown (length markdown) 0)))

(hunchentoot:define-easy-handler (post-page :uri "/post") (title)
  (setf (hunchentoot:content-type*) "text/html")
  (let* ((results
	   (sqlite:execute-to-list
	    *db*
	    "select markdown, submitted from posts where username = 'laurent' and title = ?"
	    title))
	 (markdown (caar results))
	 (submitted (cadar results)))
    (cl-who:with-html-output-to-string (*standard-output*)
      (:main (:h1 (cl-who:esc title))
	     (:h4 (cl-who:esc submitted))
	     (:article
	      (cl-who:str
	       (libcmark:markdown-to-html markdown (length markdown) 0)))))))

(hunchentoot:define-easy-handler
    (image
     :uri (lambda (request)
	    (string= (string (hunchentoot:request-uri request)) "/i")))
    ()
  (setf (hunchentoot:content-type*) "text/html")
  (cl-who:with-html-output-to-string (*standard-output*)
    (:h1 "test")))
