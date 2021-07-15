(ql:quickload '(clack websocket-driver alexandria cl-json cl-who))

(defvar *connections* (make-hash-table))

(defun handle-new-connection (con)
  (setf (gethash con *connections*)
        (format nil "user-~a" (random 100000))))


(defun broadcast-to-room (connection message)
    (format t "~a~%" (cdr (car (cl-json:decode-json-from-string message))))
  (let ((message (format nil "<div hx-swap-oob='afterbegin:#chat-echo-area'><li>~a: ~a</li></div>"
                         (gethash connection *connections*)
                         (cdr (car (cl-json:decode-json-from-string message))))))
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))

(defun handle-close-connection (connection)
  (let ((message (format nil " .... ~a has left."
                         (gethash connection *connections*))))
    (remhash connection *connections*)
    (loop :for con :being :the :hash-key :of *connections* :do
          (websocket-driver:send con message))))

(defun chat-server (env)
  (let ((ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
                         (lambda () (handle-new-connection ws)))

    (websocket-driver:on :message ws
                         (lambda (msg)
                           (format t "Message: ~a~%" msg)
                           (broadcast-to-room ws msg)))

    (websocket-driver:on :close ws
                         (lambda (&key code reason)
                           (declare (ignore code reason))
                           (handle-close-connection ws)))
    (lambda (responder)
      (declare (ignore responder))
      (websocket-driver:start-connection ws))))


(defun client-server (env)
  (declare (ignore env))
  `(200 (:content-type "text/html") (,(cl-who:with-html-output (*standard-output* nil :indent t)
	   (:html
              (:head
	       (:meta :charset "UTF-8")
                 (:title "LISP chat"))
              (:body
	       (:div :hx-ws "connect:ws://localhost:12345" :style "position: fixed; bottom:0;"
		     (:ul :id "chat-echo-area")
		     (:form :hx-ws "send:submit"
			    (:input :name "chat-input" :placeholder "say something")))
	       (:script :src "https://unpkg.com/htmx.org@1.4.1")))))))
	      

(defvar *chat-handler* (clack:clackup #'chat-server :port 12345))
(defvar *client-handler* (clack:clackup #'client-server :port 8080))
