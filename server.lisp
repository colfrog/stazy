(ql:quickload :hunchentoot)
(ql:quickload :sqlite)
(ql:quickload :cmark)
(ql:quickload :cl-who)
(ql:quickload :quri)

(load "fortune.lisp")

(defparameter *username* "laurent")
(defvar *db-path* "/home/laurent/src/stazy/db.sqlite")
(defvar *db* (sqlite:connect *db-path*))

(hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 4243))

(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/style.css" "public/style.css")
      hunchentoot:*dispatch-table*)
(push (hunchentoot:create-static-file-dispatcher-and-handler
       "/favicon.ico" "public/favicon.ico")
      hunchentoot:*dispatch-table*)

(defmacro with-layout ((title) &body body)
  `(cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
     (:html
      (:head
       (:meta :name "viewport" :content "width=device-width,initial-scale=1")
       (:link :href "https://mastodon.bsd.cafe/@clf" :rel "me")
       (:link :href "https://mas.to/@clf" :rel "me")
       (:link :href "https://bsd.network/@xi" :rel "me")
       (:link :rel "icon" :type "image/x-icon" :href "/favicon.ico")
       (:link :rel "stylesheet" :href "/style.css")
       (:title
	(cl-who:str
	 (if ,title
	     (concatenate 'string "nilio - " ,title)
	     "nilio"))))
      (:body
       (:header
	(:center
	 (:div :id "header-text"
	       (:a :href "/" (:h1 "nilio"))
	       (:h4 (cl-who:str
		     (concatenate
		      'string
		      "&laquo; "
		      (get-fortune)
		      " &raquo;"))))))
       (:main
	,@body)
       (:nav
	(let ((post-list
		(sqlite:execute-to-list
		 *db*
		 "select title, date(submitted) from posts where username = ?"
		 *username*)))
	  (dolist (item (reverse post-list))
	    (cl-who:htm
	     (:div :class "nav-entry"
		   (:center
		    (:a :href
			(concatenate
			 'string
			 "post?title="
			 (quri:url-encode (car item)))
			(:h5 (cl-who:str (car item))))
		    (:small (cl-who:str (cadr item)))))))))
       (:footer (:small "Created by Laurent Cimon"))))))


(hunchentoot:define-easy-handler (home-page :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html")
  (let ((markdown
	  (sqlite:execute-single
	   *db*
	   "select markdown from home where username = ?" *username*)))
    (when markdown
      (with-layout (nil)
	(:article
	 (cl-who:str
	  (libcmark:markdown-to-html markdown (length markdown) 0)))))))

(hunchentoot:define-easy-handler (post-page :uri "/post") (title)
  (setf (hunchentoot:content-type*) "text/html")
  (when title
    (let* ((results
	    (sqlite:execute-to-list
	     *db*
	     "select markdown, submitted || ' (UTC)' from posts where username = ? and title = ?"
	     *username* title))
	   (markdown (caar results))
	   (submitted (cadar results)))
      (when results
	(with-layout (title)
	  (:h1 (cl-who:esc title))
	  (:h4 (cl-who:esc submitted))
	  (:article
	   (cl-who:str
	    (libcmark:markdown-to-html markdown (length markdown) 0))))))))

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
     "select image from images where username = ? and id = ?" *username* image-name)))

(hunchentoot:define-easy-handler (admin-redirect :uri "/admin") ()
  (hunchentoot:redirect "/admin/"))

(hunchentoot:define-easy-handler (admin-home :uri "/admin/") ()
  (with-layout ("Admin Home")
    (:article
     (:center
      (:a :href "post" "Edit home")
      (:br)
      (:a :href "images" "Edit images")))))

(hunchentoot:define-easy-handler (admin-new-post-page :uri "/admin/new-post") ()
  (setf (hunchentoot:content-type*) "text/html")
  (with-layout ("New Post")
    (:form :id "new-post-form" :method "post" :action "/admin/new"
	   (:label "Title: ")
	   (:input :type "text" :id "title-input" :name "title" :required)
	   (:br)
	   (:textarea :id "text-input" :rows "30" :cols "80" :name "text" :required)
	   (:br)
	   (:input :type "submit"))))

(hunchentoot:define-easy-handler (new-post-function :uri "/admin/new")
    ((title :request-type :POST)
     (text :request-type :POST))
  (when (and title text)
    (sqlite:execute-non-query *db* "insert into posts (username, title, markdown) values (?, ?, ?)"
			      *username* title text)
    (hunchentoot:redirect (concatenate 'string "/post?title=" title))))

(hunchentoot:define-easy-handler (admin-edit-post-page :uri "/admin/post") (title)
  (let ((text
	  (if title
	      (sqlite:execute-single
	       *db*
	       "select markdown from posts where title = ?" title)
	      (sqlite:execute-single
	       *db*
	       "select markdown from home where username = ?" *username*))))
    (with-layout ("Edit Post")
      (:form :id "edit-post-form" :method "post" :action "/admin/edit"
	     (:input :type "hidden" :id "title-input" :name "title" :readonly "readonly" :value title)
	     (:textarea :id "text-input" :rows "30" :cols "80" :name "text" (cl-who:str text))
	     (:br)
	     (:input :type "submit")))))

(hunchentoot:define-easy-handler (admin-edit :uri "/admin/edit")
    ((title :request-type :POST)
     (text :request-type :POST))
  (when (and title text)
    (if (not (equal title ""))
	(progn
	  (sqlite:execute-non-query *db* "update posts set markdown = ? where username = ? and title = ?"
				    text *username* title)
	  (hunchentoot:redirect (concatenate 'string "/post?title=" (quri:url-encode title))))
	(progn
	  (if (= 0 (sqlite:execute-single *db* "select count(*) from home where username = ?" *username*))
	      (sqlite:execute-non-query *db* "insert into home (username, markdown) values (?, ?)" *username* text)
	      (sqlite:execute-non-query *db* "update home set markdown = ? where username = ?" text *username*))
	  (hunchentoot:redirect "/")))))

(hunchentoot:define-easy-handler (admin-image-page :uri "/admin/images") ()
  (let ((image-ids (sqlite:execute-to-list *db* "select id from images where username = ?" *username*)))
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
	    (cl-who:htm
	     (:span
	      :style "padding:24px"
	      (:p (:b (cl-who:str (car id))))
	      (:a :href link (:img :src link :style "max-width: 280px; max-height: 280px;"))
	      (:form
	       :id (concatenate 'string "delete-" (car id) "-form")
	       :method "post"
	       :action "/admin/i/delete"
	       (:input :name "id" :value (car id) :type "hidden")
	       (:input :type "submit" :value "x")))))))))))

(hunchentoot:define-easy-handler (admin-image-upload :uri "/admin/i")
    ((id :request-type :POST)
     (file :request-type :POST))
  (when (and id file)
    (with-open-file (stream (car file) :element-type 'unsigned-byte)
      (let ((buffer (make-array (file-length stream) :initial-element nil)))
	(read-sequence buffer stream)
	(sqlite:execute-non-query *db* "insert into images (username, id, image) values (?, ?, ?)" *username* id buffer)))
    (hunchentoot:redirect "/admin/images")))

(hunchentoot:define-easy-handler (admin-image-delete :uri "/admin/i/delete")
    ((id :request-type :POST))
  (when id
    (sqlite:execute-non-query *db* "delete from images where username = ? and id = ?" *username* id)
    (hunchentoot:redirect "/admin/images")))
