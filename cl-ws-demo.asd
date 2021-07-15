(defsystem "cl-ws-demo"
  :version "0.1.0"
  :author "Rajasegar Chandran"
  :license ""
  :depends-on ("clack"
               "websocket-driver"
               "alexandria")
  :components ((:file "app"))
  :description ""
  :in-order-to ((test-op (test-op "cl-tabular-test"))))
