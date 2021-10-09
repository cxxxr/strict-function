# strict-function

Utility of function definition

The original implementation can be found at http://common-lisp.net/project/qitab/archives/quux-2013-09-24.tar.bz2 (quux/lisp/quux/strict-functions.lisp)

## Example

```common-lisp
(define-strict-function func-ordinary-1 (:inputs ((a fixnum) (b fixnum)))
  (+ a b))

(define-strict-function func-rest-1 (:inputs ((a fixnum "a is fixnum")
                                              (b fixnum "b is fixnum")
                                              &rest (numbers list
                                                             "proper-list of fixnum")))
  (+ a b (apply #'+ numbers)))

(define-strict-function func-outputs-1 (:inputs ((arg integer))
                                        :outputs (integer 0 *))
  arg)

(define-strict-function func-outputs-2 (:inputs ((values list))
                                        :outputs (values (integer 0 *)
                                                         boolean))
  (apply #'values values))

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

(define-strict-function func-aux-1 (:inputs (&aux (x string)))
  x)

(define-strict-function func-aux-2 (:inputs (&aux ((x "foo") string)))
  x)

(define-strict-function func-aux-3 (:inputs (&aux ((x "foo") string)
                                                  ((y 12) number)))
  (list x y))

```

## LICENSE
MIT
