;;; simplistic compile-time pattern-match macro that only works on a cons tree.
;;; https://github.com/guicho271828/trivia/wiki/Level-0-Patterns
;;; 165 lines

(defpackage :ros.matcher
  (:use :cl)
  (:export #:match0
           #:ematch0
           #:lambda-match0
           #:lambda-ematch0))

(in-package :ros.matcher)

;;; following 3 definitions are minimally modified from alexandria

(deftype string-designator ()
  "A string designator type. A string designator is either a string, a symbol,
or a character."
  `(or symbol string character))

(defmacro with-gensyms (names &body forms)
  "Binds each variable named by a symbol in NAMES to a unique symbol around
FORMS. Each of NAMES must either be either a symbol, or of the form:

 (symbol string-designator)

Bare symbols appearing in NAMES are equivalent to:

 (symbol symbol)

The string-designator is used as the argument to GENSYM when constructing the
unique symbol the named variable will be bound to."
  `(let ,(mapcar (lambda (name)
                   (multiple-value-bind (symbol string)
                       (etypecase name
                         (symbol
                          (values name (symbol-name name)))
                         ((cons symbol (cons string-designator null))
                          (values (first name) (string (second name)))))
                     `(,symbol (gensym ,string))))
                 names)
     ,@forms))

(defmacro once-only (specs &body forms)
  "Evaluates FORMS with symbols specified in SPECS rebound to temporary
variables, ensuring that each initform is evaluated only once.

Each of SPECS must either be a symbol naming the variable to be rebound, or of
the form:

  (symbol initform)

Bare symbols in SPECS are equivalent to

  (symbol symbol)

Example:

  (defmacro cons1 (x) (once-only (x) `(cons ,x ,x)))
  (let ((y 0)) (cons1 (incf y))) => (1 . 1)
"
  (let ((gensyms (loop repeat (length specs)
                       collect (gensym "ONCE-ONLY")))
        (names-and-forms (mapcar (lambda (spec)
                                   (etypecase spec
                                     (list
                                      (destructuring-bind (name form) spec
                                        (cons name form)))
                                     (symbol
                                      (cons spec spec))))
                                 specs)))
    ;; bind in user-macro
    `(let ,(mapcar (lambda (g n) (list g `(gensym ,(string (car n)))))
                   gensyms names-and-forms)
       ;; bind in final expansion
       `(let (,,@(mapcar (lambda (g n)
                           ``(,,g ,,(cdr n)))
                   gensyms names-and-forms))
          (declare (ignorable ,,@gensyms))
          ;; bind in user-macro
          ,(let ,(mapcar (lambda (n g) (list (car n) g))
                         names-and-forms gensyms)
             ,@forms)))))

;;; following definitions are copied from trivia.level0

(defvar *what*)
(defvar *bindings*)
(defvar *env*)
(defmacro match0 (*what* &body clauses &environment *env*)
  "Level0 pattern matchers provide a very limited set of patterns. These
are used for implementing level 1 / 2 patterns. As this is a part of a
compiler, the implementation of level 0 matcher is aimed at cleanness and
not speed.

Available Patterns are restricted to: list, list*, cons, constant,
variable, wildcard (_).

Any symbols are considered as variables, except for constant
symbols (namely, t, nil and keywords) and those named as _ (checked by the
symbol's name, they don't have to be exported). These are identical to
the corresponding patterns in Optima.

Example:

  (match0 tree
    ((list* x (list :hi! y _) _ z)
     (vector x y z)))

Checks if tree is a cons tree of length more than 3,
and if the second element is a list of length 3,
and if its first element is a constant `:hi!'.
If they hold true, bind x, y and z and evaluate (vector x y z).
"
  (once-only (*what*)
    (parse-patterns clauses)))


(defmacro ematch0 (what &body clauses)
  `(match0 ,what
     ,@clauses
     (_ (error "level0 match error!"))))

(defmacro lambda-match0 (&body clauses)
  (with-gensyms (arg)
    `(lambda (,arg)
       (match0 ,arg
         ,@clauses))))

(defmacro lambda-ematch0 (&body clauses)
  (with-gensyms (arg)
    `(lambda (,arg)
       (ematch0 ,arg
         ,@clauses))))

(defun parse-patterns (clauses)
  (if (null clauses)
      nil
      (destructuring-bind ((pattern &rest body) . rest) clauses
        (multiple-value-bind (condition bindings)
            (let ((*bindings* nil))
              (values (make-pattern-predicate pattern)
                      *bindings*))
          `(if ,condition
               (let* ,(reverse bindings)
                 (declare (ignorable ,@(mapcar #'first bindings)))
                 ,@body)
               ,(parse-patterns rest))))))

(defun make-pattern-predicate (pattern)
  (if (atom pattern)
      (cond
        ((constantp pattern *env*) `(equal ,*what* ,pattern))
        ((symbolp pattern)
         (unless (string= "_" (symbol-name pattern))
           (push `(,pattern ,*what*) *bindings*))
         t)
        (t (error "what is this? ~a" pattern)))
      (destructuring-bind (name . args) pattern
        (ecase name
          (quote `(equal ,*what* ',@args))
          (cons
           (destructuring-bind (car cdr) args
             `(and (consp ,*what*)
                   ,(let* ((what `(car ,*what*))
                           (*what* what))
                      (once-only (*what*)
                        (push `(,*what* ,what) *bindings*)
                        (make-pattern-predicate car)))
                   ,(let* ((what `(cdr ,*what*))
                           (*what* what))
                      (once-only (*what*)
                        (push `(,*what* ,what) *bindings*)
                        (make-pattern-predicate cdr))))))
          (list
           (if args
               (destructuring-bind (car . cdr) args
                 (make-pattern-predicate
                  (if cdr
                      `(cons ,car (list ,@cdr))
                      `(cons ,car nil))))
               `(null ,*what*)))
          (list*
           (destructuring-bind (car . cdr) args
             (make-pattern-predicate
              (if cdr
                  `(cons ,car (list* ,@cdr))
                  car))))))))

