(defpackage #:strict-function/test
  (:use #:cl
        #:rove
        #:alexandria
        #:strict-function))
(in-package #:strict-function/test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "sb-introspect"))

(defmacro signals* (form condition error-message)
  (with-unique-names (outer c)
    `(block ,outer
       (handler-bind ((condition (lambda (,c)
                                   (when (typep ,c ,condition)
                                     (return-from ,outer
                                       (equal ,error-message
                                              (princ-to-string ,c)))))))
         ,form
         nil))))

(defmacro define-test (name &body body)
  `(deftest ,name
     (let ((*package* (find-package :strict-function/test)))
       ,@body)))

;;;
(define-strict-function func-ordinary-1 (:inputs ((a fixnum) (b fixnum)))
  (+ a b))

(define-test func-ordinary
  (ok (equal '(a b) (sb-introspect:function-lambda-list 'func-ordinary-1)))
  (ok (= 30 (func-ordinary-1 10 20)))
  (ok (signals* (func-ordinary-1 1.0 10)
                'strict-invalid-input
                "input value 1.0 is not of type FIXNUM: A"))
  (ok (signals* (func-ordinary-1 100 2.0)
                'strict-invalid-input
                "input value 2.0 is not of type FIXNUM: B")))

;;;
(define-test warn-unused-variable
  (labels ((collect-warnings (form)
             (let ((warnings '()))
               (handler-bind ((warning
                                (lambda (c)
                                  (push c warnings))))
                 (let ((*error-output* (make-string-output-stream)))
                   (eval form)))
               warnings))
           (test (form &rest messages)
             (loop :with warnings := (collect-warnings form)
                   :for message :in messages
                   :do (ok (find message warnings :test #'string= :key #'princ-to-string)))))
    (test '(define-strict-function func-style-warning (:inputs ((foo integer))))
          "The variable FOO is defined but never used.")
    (test '(define-strict-function func-style-warning (:inputs ((a integer) &key (b integer))))
          "The variable A is defined but never used."
          "The variable B is defined but never used.")
    (test '(define-strict-function func-style-warning (:inputs ((a integer)
                                                                &optional (b integer))))
          "The variable A is defined but never used."
          "The variable B is defined but never used.")
    (test '(define-strict-function func-style-warning (:inputs ((a integer) &rest (b list))))
          "The variable A is defined but never used."
          "The variable B is defined but never used.")
    (test '(define-strict-function func-style-warning (:inputs ((a integer)
                                                                &optional (b character)
                                                                &rest (c list)
                                                                &key (d fixnum)
                                                                &aux (e string) (f string))))
          "The variable A is defined but never used."
          "The variable B is defined but never used."
          "The variable C is defined but never used."
          "The variable D is defined but never used."
          "The variable E is defined but never used."
          "The variable F is defined but never used.")))

;;;
(define-strict-function func-rest-1 (:inputs ((a fixnum "a is fixnum")
                                              (b fixnum "b is fixnum")
                                              &rest (numbers list
                                                             "proper-list of fixnum")))
  (+ a b (apply #'+ numbers)))

(define-test func-rest
  (ok (equal '(a b &rest numbers) (sb-introspect:function-lambda-list 'func-rest-1)))
  (ok (= 3 (func-rest-1 1 2)))
  (ok (= 6 (func-rest-1 1 2 3)))
  (ok (= 10 (func-rest-1 1 2 3 4)))
  (ok (signals* (func-rest-1 1.0 10)
                'strict-invalid-input
                "input value 1.0 is not of type FIXNUM: A"))
  (ok (signals* (func-rest-1 100 2.0)
                'strict-invalid-input
                "input value 2.0 is not of type FIXNUM: B")))

;;;
(define-strict-function func-outputs-1 (:inputs ((arg integer))
                                        :outputs (integer 0 *))
  arg)

(define-strict-function func-outputs-2 (:inputs ((values list))
                                        :outputs (values (integer 0 *)
                                                         boolean))
  (apply #'values values))

(define-test func-outputs
  (testing "basic"
    (ok (= 1 (func-outputs-1 1)))
    (ok (= 0 (func-outputs-1 0)))
    (ok (signals* (func-outputs-1 -1)
                  'strict-invalid-output
                  "output value -1 is not of type (INTEGER 0 *)")))
  (testing "multiple values"
    (ok (signals* (func-outputs-2 '(100))
                  'strict-invalid-multivalued-number
                  "wrong number of multiple values: expected number = 2, actual values = (100)"))
    (ok (equal (multiple-value-list (func-outputs-2 '(100 t))) '(100 t)))
    (ok (equal (multiple-value-list (func-outputs-2 '(100 nil))) '(100 nil)))
    (ok (signals*
         (func-outputs-2 '(1 2 3))
         'strict-invalid-multivalued-number
         "wrong number of multiple values: expected number = 2, actual values = (1 2 3)"))))

;;;
(define-strict-function func-optional-1 (:inputs (&optional (a integer)))
  a)

(define-strict-function func-optional-2 (:inputs (&optional ((a 100) integer)))
  a)

(define-strict-function func-optional-3 (:inputs (&optional ((a "test") integer)))
  a)

(define-strict-function func-optional-4 (:inputs (&optional ((a #\a) character))
                                         :use-supplied-vars t)
  (list a a-supplied-p))

(define-strict-function func-optional-5 (:inputs (&optional ((a 1 a-p) integer)))
  (list a a-p))

(define-test func-optional
  (testing "basic"
    (ok (null (func-optional-1)))
    (ok (= 1 (func-optional-1 1)))
    (ok (signals* (func-optional-1 "test")
                  'strict-invalid-input
                  "input value \"test\" is not of type INTEGER: A")))
  (testing "default value"
    (ok (= 100 (func-optional-2)))
    (ok (signals* (func-optional-2 "test")
                  'strict-invalid-input
                  "input value \"test\" is not of type INTEGER: A")))
  (ok (equal "test" (func-optional-3)))
  (testing "use-supplied-vars"
    (ok (equal (list #\a nil)
               (func-optional-4)))
    (ok (equal (list #\a t)
               (func-optional-4 #\a)))
    (ok (equal (list #\b t)
               (func-optional-4 #\b))))
  (testing "specify supplied-p"
    (ok (equal (list 1 nil) (func-optional-5)))
    (ok (equal (list 2 t) (func-optional-5 2)))))

;;;
(define-strict-function func-key-1 (:inputs (&key (a string)))
  a)

(define-strict-function func-key-2 (:inputs (&key ((a "default") string)))
  a)

(define-strict-function func-key-3 (:inputs (&key ((a 1 a-p) integer)))
  (list a a-p))

(define-strict-function func-key-4 (:inputs (&key ((a "default") integer)))
  a)

(define-strict-function func-key-5 (:inputs (&key (x integer) (y integer))
                                    :use-supplied-vars t)
  (list x x-supplied-p
        y y-supplied-p))

(define-strict-function func-key-6 (:inputs (&key (x integer) &allow-other-keys))
  x)

(define-test func-key
  (testing "basic"
    (ok (trivia:match (sb-introspect:function-lambda-list 'func-key-1)
          ((list '&key (list (list :a 'a)
                             nil
                             (satisfies (lambda (x)
                                          (and (null (symbol-package x))
                                               (string= x "A-SUPPLIED-P"))))))
           t)))
    (ok (signals* (func-key-1 :a 2)
                  'strict-invalid-input
                  "input value 2 is not of type STRING: A"))
    (ok (null (func-key-1)))
    (ok (equal "test" (func-key-1 :a "test"))))
  (testing "default value"
    (ok (equal "default" (func-key-2))))
  (testing "specify supplied-p"
    (ok (equal (list 1 nil) (func-key-3)))
    (ok (equal (list 2 t) (func-key-3 :a 2))))
  (ok (equal "default" (func-key-4)))
  (testing "use-supplied-vars"
    (ok (equal (list nil nil nil nil)
               (func-key-5)))
    (ok (equal (list 10 t nil nil)
               (func-key-5 :x 10)))
    (ok (equal (list nil nil 20 t)
               (func-key-5 :y 20)))
    (ok (equal (list 10 t 20 t)
               (func-key-5 :x 10 :y 20)))
    (signals* (func-key-5 :x "test" :y 20)
              'strict-invalid-input
              "input value x is not of type INTEGER")
    (signals* (func-key-5 :x 10 :y "test")
              'strict-invalid-input
              "input value y is not of type INTEGER"))
  (testing "&allow-other-keys"
    (ok (eql 10 (func-key-6 :x 10)))
    (ok (eql 10 (func-key-6 :x 10 :other-key 20 :other-key-2 "foo")))))

;;;

;; TODO: compile time error
(define-strict-function func-aux-1 (:inputs (&aux (x string)))
  x)

(define-strict-function func-aux-2 (:inputs (&aux ((x "foo") string)))
  x)

(define-strict-function func-aux-3 (:inputs (&aux ((x "foo") string)
                                                  ((y 12) number)))
  (list x y))

(define-test func-aux
  (ok (equal "foo" (func-aux-2)))
  (ok (equal (list "foo" 12) (func-aux-3))))

;; TODO:
;; - condition test
