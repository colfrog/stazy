(ql:quickload :hunchentoot)
(ql:quickload :sqlite)
(ql:quickload :cmark)
(ql:quickload :cl-who)
(ql:quickload :quri)

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
	    (let ((uri (hunchentoot:request-uri request)))
	      (string= (and (> (length uri) 3) (subseq uri 0 2)) "/i"))))
    ()
  (setf (hunchentoot:content-type*) "image/png")
  (let ((image-name
	 (quri:url-decode
	  (subseq (hunchentoot:request-uri hunchentoot:*request*) 3))))
    (sqlite:execute-single
     *db*
     "select image from images where username = 'laurent' and id = ?" image-name)))
