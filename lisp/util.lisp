(cl:in-package :cl-user)

(defpackage :ros.util
  (:use :cl)
  (:export :uname :uname-m :homedir :config :use :impl :which :list%
           :parse-version-spec :download :expand :sh :version :chdir
           :core-extention))

(in-package :ros.util)

(defun uname ()
  (ros:roswell '("roswell-internal-use" "uname") :string t))

(defun uname-m ()
  (ros:roswell '("roswell-internal-use" "uname" "-m") :string t))

(defun homedir ()
  (ros:opt "homedir"))

(defun impl (imp)
  (ros:roswell `("roswell-internal-use" "impl" ,(or imp "")) :string t))

(defun which (cmd)
  (let ((result (ros:roswell `("roswell-internal-use" "which" ,cmd) :string t)))
    (unless (zerop (length result))
      result)))

(defun download (uri file &key proxy)
  (declare (ignorable proxy))
  (ensure-directories-exist file)
  (ros:roswell `("roswell-internal-use" "download" ,uri ,file) :interactive nil))

(defun expand (archive dest &key verbose)
  (ros:roswell `(,(if verbose "-v" "")"roswell-internal-use tar" "-xf" ,archive "-C" ,dest)
               (or #-win32 :interactive nil) nil))

(defun core-extention (&optional (impl (ros:opt "impl")))
  (ros:roswell `("roswell-internal-use" "core-extention" ,impl) :string t))

(defun config (c)
  (ros:roswell `("config" "show" ,c) :string t))

(defun (setf config) (val item)
  (ros:roswell `("config" "set" ,item ,val) :string t)
  val)

(defun list% (&rest params)
  (string-right-trim #.(format nil "~A" #\Newline)
                     (ros:roswell `("list" ,@params) :string nil)))

(defun chdir (dir &optional (verbose t))
  (funcall (intern (string :chdir) :uiop/os) dir)
  (when verbose
    (format t "~&chdir ~A~%" dir)))

(defun sh ()
  (or #+win32
      (unless (ros:getenv "MSYSCON")
	(format nil "~A" (#+sbcl sb-ext:native-namestring #-sbcl namestring
				 (merge-pathnames (format nil "impls/~A/~A/msys~A/usr/bin/bash" (uname-m) (uname)
							  #+x86-64 "64" #-x86-64 "32") (homedir)))))
      (which "bash")
      "sh"))

(defun version (&optional (opt ""))
  (ros:roswell `("roswell-internal-use" "version"
					,(string-downcase opt)) :string t))

(defvar *version*
  `(
    :roswell ,(version)
	     :lisp ,(lisp-implementation-type)
	     :version ,(lisp-implementation-version)
	     :date ,(get-universal-time)))

(defun parse-version-spec (string)
  "Parse the given version specification string and returns a list of strings (LISP VERSION).
If it does not contain a version substring, VERSION becomes a null.
If it is a version string only (detected when it starts from digit-char), LISP becomes NIL.
Examples:
ccl-bin/1.11 -> (\"ccl-bin\" \"1.11\")
ccl-bin      -> (\"ccl-bin\" nil)
1.11         -> (nil \"1.11\")
"
  (let ((pos (position #\/ string)))
    (if pos
        `(,(subseq string 0 pos) ,(subseq string (1+ pos)))
        (if (digit-char-p (aref string 0))
            `(nil ,string)
            `(,string nil)))))

(defun use (arg)
  "Parse the lisp version string (such as ccl-bin/1.11) and set it to the correct config slot(s)"
  (when (and arg
             (ignore-errors
               (ros:roswell `("-L" ,arg "version=t" "run"))))
    (destructuring-bind (lisp version) (parse-version-spec arg)
      (cond ((and lisp version)
             (setf (config "default.lisp") lisp
                   (config (format nil "~A.version" lisp)) version))
            (lisp
             (setf (config "default.lisp")
                   lisp))
            (version
             (setf (config (format nil "~A.version" (config "default.lisp")))
                   version))))
    t))
