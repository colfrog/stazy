(ql:quickload :hunchentoot)
(ql:quickload :sqlite)
(ql:quickload :cmark)
(ql:quickload :cl-who)
(ql:quickload :quri)

(defvar *db-path* (or (last (uiop:command-line-arguments)) "/media/laurent/storage/tulip/db.sqlite"))
(defvar *db* (sqlite:connect *db-path*))

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4242))

(defmacro with-layout ((title) &body body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head
       (:title
	(cl-who:str
	 (if ,title
	     (concatenate 'string "nilio - " ,title)
	     "nilio"))))
      (:body
       (:header
	(:center
	 (:a :href "/" (:h1 "nilio"))))
       (:nav
	(let ((title-list
		(sqlite:execute-to-list
		 *db*
		 "select title from posts where username = 'laurent'")))
	  (dolist (item title-list)
	    (cl-who:htm
	     (:a :href (cl-who:str
			(concatenate
			 'string
			 "/post?title="
			 (quri:url-encode (car item))))
	      (:h5 (cl-who:str (car item))))))))
       (:main
	,@body)
       (:footer (:small "Created by Laurent Cimon"))))))
      

(hunchentoot:define-easy-handler (home-page :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (let ((markdown
	  (sqlite:execute-single
	   *db*
	   "select markdown from home where username = 'laurent'")))
    (with-layout (nil)
      (cl-who:str
       (libcmark:markdown-to-html markdown (length markdown) 0)))))

(hunchentoot:define-easy-handler (post-page :uri "/post") (title)
  (setf (hunchentoot:content-type*) "text/html")
  (let* ((results
	   (sqlite:execute-to-list
	    *db*
	    "select markdown, submitted from posts where username = 'laurent' and title = ?"
	    title))
	 (markdown (caar results))
	 (submitted (cadar results)))
    (when results
      (with-layout (title)
	(:h1 (cl-who:esc title))
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
