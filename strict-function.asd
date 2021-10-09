(defsystem "strict-function"
  :license "MIT"
  :author "cxxxr"
  :description "Utility of function definition"
  :depends-on ("alexandria"
               "trivia")
  :components ((:file "strict-function"))
  :in-order-to ((test-op (test-op "strict-function/test"))))

(defsystem "strict-function/test"
  :depends-on ("rove"
               "alexandria"
               "strict-function")
  :components ((:file "test"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
