(in-package :stazy)

(defparameter *username* "laurent")
(defvar *db-path* "db.sqlite")
(defvar *db* (sqlite:connect *db-path*))
(defvar *server* nil)
(defvar *port* 4243)

(defun start-stazy ()
  (when (null *server*)
    (setf *server* (make-instance 'hunchentoot:easy-acceptor :port *port*)))
  (start *server*))

(defun stop-stazy ()
  (stop *server*))

(defun is-logged-in ()
  (session-value :username))

(push (create-static-file-dispatcher-and-handler
       "/style.css" "public/style.css")
      *dispatch-table*)
(push (create-static-file-dispatcher-and-handler
       "/favicon.ico" "public/favicon.ico")
      *dispatch-table*)

(defmacro with-layout ((title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head
       (:meta :name "viewport" :content "width=device-width,initial-scale=1")
       (:link :href "https://mastodon.bsd.cafe/@clf" :rel "me")
       (:link :href "https://mas.to/@clf" :rel "me")
       (:link :href "https://bsd.network/@xi" :rel "me")
       (:link :rel "icon" :type "image/x-icon" :href "/favicon.ico")
       (:link :rel "stylesheet" :href "/style.css")
       (:title
	(str
	 (if ,title
	     (concatenate 'string "nilio - " ,title)
	     "nilio"))))
      (:body
       (:header
	(:center
	 (:div :id "header-text"
	       (:a :href (if (is-logged-in) "/admin" "/") (:h1 "nilio"))
	       (:h4 (str
		     (concatenate
		      'string
		      "&laquo; "
		      (get-fortune)
		      " &raquo;"))))))
       (:main
	,@body)
       (:nav
	(let ((post-list
		(execute-to-list
		 *db*
		 "select title, date(submitted) from posts where username = ?"
		 *username*)))
	  (dolist (item (reverse post-list))
	    (htm
	     (:div :class "nav-entry"
		   (:center
		    (:a :href
			(concatenate
			 'string
			 "post?title="
			 (url-encode (car item)))
			(:h5 (str (car item))))
		    (:small (str (cadr item)))))))))
       (:footer (:small "Created by Laurent Cimon"))))))


(define-easy-handler (home-page :uri "/") ()
  (let ((markdown
	  (execute-single
	   *db*
	   "select markdown from home where username = ?" *username*)))
    (when markdown
      (with-layout (nil)
	(:article
	 (str
	  (markdown-to-html markdown (length markdown) 0)))))))

(define-easy-handler (post-page :uri "/post") (title)
  (when title
    (let* ((results
	    (execute-to-list
	     *db*
	     "select markdown, submitted || ' (UTC)' from posts where username = ? and title = ?"
	     *username* title))
	   (markdown (caar results))
	   (submitted (cadar results)))
      (when results
	(with-layout (title)
	  (:h1 (esc title))
	  (:h4 (esc submitted))
	  (:article
	   (str
	    (markdown-to-html markdown (length markdown) 0))))))))

(define-easy-handler
    (image
     :uri (lambda (request)
	    (let ((uri (request-uri request)))
	      (string= (and (> (length uri) 3) (subseq uri 0 2)) "/i"))))
    ()
  (setf (content-type*) "image/png")
  (let ((image-name
	 (url-decode
	  (subseq (request-uri *request*) 3))))
    (execute-single
     *db*
     "select image from images where username = ? and id = ?" *username* image-name)))

(define-easy-handler (admin-redirect :uri "/admin") ()
  (redirect "/admin/"))

(define-easy-handler (admin-home :uri "/admin/") ()
  (if (is-logged-in)
      (with-layout ("Admin Home")
	(:article
	 (:center
	  (:a :href "new-post" "New post")
	  (:br)
	  (:a :href "post" "Edit home")
	  (:br)
	  (:a :href "images" "Edit images")
	  (:br)
	  (:a :href "/logout" "Logout"))))
      (redirect "/login")))

(define-easy-handler (admin-new-post-page :uri "/admin/new-post") ()
  (when (is-logged-in)
    (with-layout ("New Post")
      (:form :id "new-post-form" :method "post" :action "/admin/new"
	     (:label "Title: ")
	     (:input :type "text" :id "title-input" :name "title" :required)
	     (:br)
	     (:textarea :id "text-input" :rows "30" :cols "80" :name "text" :required)
	     (:br)
	     (:input :type "submit")))))

(define-easy-handler (new-post-function :uri "/admin/new")
    ((title :request-type :POST)
     (text :request-type :POST))
  (when (and (is-logged-in) title text)
    (execute-non-query *db* "insert into posts (username, title, markdown) values (?, ?, ?)"
			      *username* title text)
    (redirect (concatenate 'string "/post?title=" title))))

(define-easy-handler (admin-edit-post-page :uri "/admin/post") (title)
  (when (is-logged-in)
    (let ((text
	   (if title
	       (execute-single
		*db*
		"select markdown from posts where title = ?" title)
	       (execute-single
		*db*
		"select markdown from home where username = ?" *username*))))
      (with-layout ("Edit Post")
	(:form :id "edit-post-form" :method "post" :action "/admin/edit"
	       (:input :type "hidden" :id "title-input" :name "title" :readonly "readonly" :value title)
	       (:textarea :id "text-input" :rows "30" :cols "80" :name "text"
			  (str text))
	       (:br)
	       (:input :type "submit"))))))

(define-easy-handler (admin-edit :uri "/admin/edit")
    ((title :request-type :POST)
     (text :request-type :POST))
  (when (and (is-logged-in) title text)
    (if (not (equal title ""))
	(progn
	  (execute-non-query *db* "update posts set markdown = ? where username = ? and title = ?"
				    text *username* title)
	  (redirect (concatenate 'string "/post?title=" (url-encode title))))
	(progn
	  (if (= 0 (execute-single *db* "select count(*) from home where username = ?" *username*))
	      (execute-non-query *db* "insert into home (username, markdown) values (?, ?)" *username* text)
	      (execute-non-query *db* "update home set markdown = ? where username = ?" text *username*))
	  (redirect "/")))))

(define-easy-handler (admin-image-page :uri "/admin/images") ()
  (when (is-logged-in)
    (let ((image-ids (execute-to-list *db* "select id from images where username = ?" *username*)))
      (with-layout ("Images")
	(:div
	 (:center
	  (:form
	   :id "new-image-form" :method "post" :action "/admin/i" :enctype "multipart/form-data"
	   (:label "ID:")
	   (:input :id "image-id" :name "id" :type "text")
	   (:br)
	   (:input :id "image-file" :name "file" :type "file" :accept "image/png,image/jpeg")
	   (:br)
	   (:input :type "submit")))
	 (:article
	  :style "display:flex; flex-direction:row; flex-wrap:wrap;"
	  (dolist (id image-ids)
	    (let ((link (concatenate 'string "/i/" (car id))))
	      (htm
	       (:span
		:style "padding:24px"
		(:p (:b (str (car id))))
		(:a :href link (:img :src link :style "max-width: 280px; max-height: 280px;"))
		(:form
		 :id (concatenate 'string "delete-" (car id) "-form")
		 :method "post"
		 :action "/admin/i/delete"
		 (:input :name "id" :value (car id) :type "hidden")
		 (:input :type "submit" :value "x"))))))))))))

(define-easy-handler (admin-image-upload :uri "/admin/i")
    ((id :request-type :POST)
     (file :request-type :POST))
  (when (and (is-logged-in) id file)
    (with-open-file (stream (car file) :element-type 'unsigned-byte)
      (let ((buffer (make-array (file-length stream) :initial-element nil)))
	(read-sequence buffer stream)
	(execute-non-query *db* "insert into images (username, id, image) values (?, ?, ?)" *username* id buffer)))
    (redirect "/admin/images")))

(define-easy-handler (admin-image-delete :uri "/admin/i/delete")
    ((id :request-type :POST))
  (when (and (is-logged-in) id)
    (execute-non-query *db* "delete from images where username = ? and id = ?" *username* id)
    (redirect "/admin/images")))
