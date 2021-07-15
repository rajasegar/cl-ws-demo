(defsystem "cl-ws-demo"
  :version "0.1.0"
  :author "Rajasegar Chandran"
  :license ""
  :depends-on ("clack"
               "websocket-driver"
               "alexandria"
	       "cl-json"
	       "cl-who")
  :components ((:file "app"))
  :description "Common Lisp Web sockets demo")
