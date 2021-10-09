(defpackage #:strict-function
  (:use #:cl
        #:alexandria)
  (:export #:strict-function-error
           #:strict-invalid-input
           #:strict-invalid-output
           #:strict-invalid-multivalued-number
           #:define-strict-function

           #:strict-unexpected-condition-error-expected-function-name
           #:strict-unexpected-condition-error-expected-conditions
           #:strict-unexpected-condition-error-actual-condition

           #:clear-unexpected-conditions
           #:collect-unexpected-conditions))
(in-package #:strict-function)

(define-condition strict-function-error (simple-error) ())

(define-condition strict-type-error (strict-function-error)
  ((expected-type :initarg :expected-type
                  :reader strict-type-error-expected-type)
   (value :initarg :value
          :reader strict-type-error-value)))

(define-condition strict-invalid-input (strict-type-error)
  ((var :initarg :var
        :reader strict-invalid-input-var
        :type symbol))
  (:report (lambda (c s)
             (with-slots (expected-type value var) c
               (format s "input value ~S is not of type ~S: ~S" value expected-type var)))))

(define-condition strict-invalid-output (strict-type-error)
  ()
  (:report (lambda (c s)
             (with-slots (expected-type value) c
               (format s "output value ~S is not of type ~S" value expected-type)))))

(define-condition strict-invalid-multivalued-number (strict-function-error)
  ((expected-nth-values :initarg :expected-nth-values
                        :reader strict-invalid-multivalued-number-expected-nth-values)
   (actual-values :initarg :actual-values
                  :reader strict-invalid-multivalued-number-actual-values))
  (:report
   (lambda (c s)
     (with-slots (expected-nth-values actual-values) c
       (format s "wrong number of multiple values: expected number = ~D, actual values = ~S"
               expected-nth-values actual-values)))))

(define-condition strict-unexpected-condition-error (strict-function-error)
  ((function-name
    :initarg :function-name
    :reader strict-unexpected-condition-error-expected-function-name)
   (expected-conditions
    :initarg :expected-conditions
    :reader strict-unexpected-condition-error-expected-conditions)
   (actual-condition
    :initarg :actual-condition
    :reader strict-unexpected-condition-error-actual-condition)
   (backtrace
    :initarg :backtrace))
  (:report (lambda (c s)
             (format s "unexpected conditions in ~S, expected = ~S, actual = ~S"
                     (strict-unexpected-condition-error-expected-function-name c)
                     `(or ,@(strict-unexpected-condition-error-expected-conditions c))
                     (type-of (strict-unexpected-condition-error-actual-condition c))))))

(defun validate-input (var value type)
  (unless (typep value type)
    (error 'strict-invalid-input
           :var var
           :expected-type type
           :value value)))

(defun validate-outputs (result outputs)
  (unless (typep result outputs)
    (error 'strict-invalid-output
           :expected-type outputs
           :value result)))

(defun validate-number-of-outputs (values n)
  (unless (length= values n)
    (error 'strict-invalid-multivalued-number
           :expected-nth-values n
           :actual-values values)))

(defvar *unexpected-condition-error-occurred-functions* '())

(defun validate-conditions (actual-condition expected-conditions function-name)
  (unless (some (curry #'typep actual-condition) expected-conditions)
    (pushnew function-name *unexpected-condition-error-occurred-functions*)
    (push (make-condition 'strict-unexpected-condition-error
                          :function-name function-name
                          :expected-conditions expected-conditions
                          :actual-condition actual-condition
                          :backtrace (with-output-to-string (out)
                                       (uiop:print-backtrace :stream out)))
          (get function-name 'unexpected-conditions))))

(defun clear-unexpected-conditions ()
  (dolist (fn-name *unexpected-condition-error-occurred-functions*)
    (setf (get fn-name 'unexpected-conditions) '()))
  (setf *unexpected-condition-error-occurred-functions* '()))

(defun collect-unexpected-conditions (&optional (function-name nil function-name-p))
  (let ((fn-names
          (if function-name-p
              (progn
                (list function-name))
              *unexpected-condition-error-occurred-functions*)))
    (loop :for fn-name :in fn-names
          :append (get fn-name 'unexpected-conditions))))

(defun lambda-list-keyword-p (x)
  (member x lambda-list-keywords))

(defun parse-param (param)
  (trivia:match param
    ((trivia:guard param (lambda-list-keyword-p param))
     (list param param nil))
    ((list name type)
     (list name type nil))
    ((list name type doc)
     (list name type doc))
    (otherwise
     (error "invalid parameter: ~S" param))))

(defun parse-inputs (inputs)
  (loop :for param :in inputs
        :for (name type doc) := (parse-param param)
        :do (assert (or (null doc) (stringp doc))
                    (doc)
                    (format nil "wrong form in :inputs ((~S ~S ==> ~S <==) ...)" name type doc))
        :collect name :into lambda-list
        :collect type :into param-types
        :collect doc :into docstrings
        :finally (return (values lambda-list param-types docstrings))))

(defun concreate-check-types (param-type-pairs supplied-pairs)
  (loop :for (param type) :in param-type-pairs
        :for supplied-var := (assoc-value supplied-pairs param)
        :if supplied-var
        :collect `(when ,supplied-var
                    (validate-input ',param ,param ',type))
        :else
        :collect `(validate-input ',param ,param ',type)))

(defun construct-rebind-parameters (param-type-pairs body-decls body)
  `(let ,(loop :for (param) :in param-type-pairs
               :collect `(,param ,param))
     ,@body-decls
     ,body))

(defun construct-check-conditions-form (conditions body function-name)
  (with-unique-names (c)
    (if (null conditions)
        body
        `(handler-bind ((error (lambda (,c)
                                 (validate-conditions ,c
                                                      ',conditions
                                                      ',function-name))))
           ,body))))

(defun make-nth-functions (n)
  (loop :for i :from 1 :repeat n
        :for nth-fn := (intern (string-upcase (format nil "~:R" i)))
        :do (assert (fdefinition nth-fn))
        :collect nth-fn))

(defmacro validate-multiple-outputs (body types)
  (with-unique-names (results)
    (let ((n-values (length types)))
      `(let ((,results (multiple-value-list ,body)))
         (validate-number-of-outputs ,results ,n-values)
         ,@(loop :for nth-fn :in (make-nth-functions n-values)
                 :for type :in types
                 :collect `(validate-outputs (,nth-fn ,results) ',type))
         (apply #'values ,results)))))

(defun construct-check-outputs (body outputs outputs-specified-p)
  (cond ((not outputs-specified-p)
         body)
        (t
         (trivia:match outputs
           ((list* 'values types)
            `(validate-multiple-outputs ,body ,types))
           (otherwise
            (with-unique-names (result)
              `(let ((,result ,body))
                 (validate-outputs ,result ',outputs)
                 (the ,outputs ,result))))))))

(defun construct-body (function-name param-type-pairs body-decls body outputs outputs-specified-p
                       conditions)
  (let* ((body `(progn ,@body))
         (body (construct-rebind-parameters param-type-pairs body-decls body))
         (body (construct-check-conditions-form conditions body function-name)))
    (construct-check-outputs body outputs outputs-specified-p)))

(defun mapping-lambda-list-parameters-to-types (lambda-list param-types)
  (multiple-value-bind (required-parameters
                        optional-parameters
                        rest-parameter
                        keyword-parameters
                        allow-other-keys-p
                        aux-parameters
                        existance-of-key-p)
      (parse-ordinary-lambda-list lambda-list)
    (declare (ignore allow-other-keys-p existance-of-key-p))
    (labels ((corr-type (name &optional (take-var #'take-var))
               (let ((pos (position name lambda-list
                                    :key take-var)))
                 (list name (elt param-types pos))))
             (take-var (ll)
               (if (consp ll)
                   (first ll)
                   ll))
             (take-var-keyword-param (ll)
               (cond ((not (consp ll)) ll)
                     ((consp (first ll)) (second (first ll)))
                     (t (first ll)))))
      (append (loop :for name :in required-parameters
                    :collect (corr-type name))
              (loop :for (name) :in optional-parameters
                    :collect (corr-type name))
              (if rest-parameter
                  (list (corr-type rest-parameter)))
              (loop :for ((* name)) :in keyword-parameters
                    :collect (corr-type name #'take-var-keyword-param))
              (loop :for (name) :in aux-parameters
                    :collect (corr-type name))))))

(defun reform-lambda-list (lambda-list use-supplied-vars)
  (let ((supplied-pairs '()))
    (labels ((make-supplied-var (key)
               (if use-supplied-vars
                   (intern (format nil "~A-SUPPLIED-P" key))
                   (make-symbol (format nil "~A-SUPPLIED-P" key))))
             (reform-optional-parameters (params)
               (loop :for (param-var value-form supplied-var) :in params
                     :for supplied-var-1 := (or supplied-var (make-supplied-var param-var))
                     :do (push (cons param-var supplied-var-1) supplied-pairs)
                     :collect `(,param-var ,value-form ,supplied-var-1)))
             (reform-keyword-parameters (params)
               (loop :for ((key param-var) value-form supplied-var) :in params
                     :for supplied-var-1 := (or supplied-var (make-supplied-var key))
                     :do (push (cons param-var supplied-var-1) supplied-pairs)
                     :collect `((,key ,param-var) ,value-form ,supplied-var-1))))
      (multiple-value-bind (required-parameters
                            optional-parameters
                            rest-parameter
                            keyword-parameters
                            allow-other-keys-p
                            aux-parameters
                            existance-of-key-p)
          (parse-ordinary-lambda-list lambda-list)
        (values (append required-parameters
                        (when optional-parameters
                          (cons '&optional (reform-optional-parameters optional-parameters)))
                        (when rest-parameter (list '&rest rest-parameter))
                        (when existance-of-key-p
                          (cons '&key (reform-keyword-parameters keyword-parameters)))
                        (when allow-other-keys-p
                          (list '&allow-other-keys))
                        (when aux-parameters
                          (cons '&aux aux-parameters)))
                supplied-pairs)))))

(defmacro define-strict-function (function-name
                                  (&key inputs
                                        (outputs nil outputs-specified-p)
                                        conditions
                                        use-supplied-vars)
                                  &body body)
  (check-type conditions list)
  (multiple-value-bind (lambda-list param-types param-docs)
      (parse-inputs inputs)
    (declare (ignore param-docs))
    (multiple-value-bind (lambda-list supplied-pairs)
        (reform-lambda-list lambda-list use-supplied-vars)
      (multiple-value-bind (body decls docstring)
          (parse-body body :documentation t)
        (let* ((param-type-pairs (mapping-lambda-list-parameters-to-types lambda-list
                                                                          param-types)))
          `(defun ,function-name ,lambda-list
             ,@(when docstring (list docstring))
             ,@(concreate-check-types param-type-pairs supplied-pairs)
             ,(construct-body function-name
                              param-type-pairs
                              decls
                              body
                              outputs
                              outputs-specified-p
                              conditions)))))))
