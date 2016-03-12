(cl:in-package :cl-user)

#-asdf
(require :asdf)
#+sbcl
(require :sb-posix)

(ros:quicklisp :environment nil)

(unless (find-package :uiop)
  #+quicklisp(ql:quickload :uiop :silent t))

(ql:quickload '(:cl-html-parse :simple-date-time :split-sequence) :silent t)

(ros:util)

(defpackage :ros.install
  (:use :cl :ros.util)
  (:export :*build-hook*))

(in-package :ros.install)

(defvar *opts* nil)
(defvar *ros-path* nil)
(defvar *build-hook* nil)

#+sbcl
(defclass count-line-stream (sb-gray:fundamental-character-output-stream)
  ((base :initarg :base
         :initform *standard-output*
         :reader count-line-stream-base)
   (print-char :initarg :print-char
               :initform `((700 . line-number)(10 . #\.))
               :accessor count-line-stream-print-char)
   (count-char :initarg :count-char
	       :initform #\NewLine
	       :reader count-line-stream-count-char)
   (count :initform -1
          :accessor count-line-stream-count)))
#+sbcl
(defmethod sb-gray:stream-write-char ((stream count-line-stream) character)
  (when (char= character (count-line-stream-count-char stream))
    (loop
       :with count := (incf (count-line-stream-count stream))
       :with stream- := (count-line-stream-base stream)
       :for (mod . char) :in (count-line-stream-print-char stream)
       :when (zerop (mod count mod))
       :do (if (characterp char)
               (write-char char stream-)
               (funcall char stream))
       (force-output stream-))))

#+sbcl
(defun line-number (stream)
  (format (count-line-stream-base stream) "~&~6d " (count-line-stream-count stream)))

(defun date (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (second minute hour date month year day daylight time-zone)
      (decode-universal-time universal-time)
    (format nil "~A ~A ~2A ~2,,,'0A:~2,,,'0A:~2,,,'0A ~3A~A ~A"
            (nth day '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
            (nth month '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
            date hour minute second time-zone (if daylight "S" " ") year)))

(defun get-opt (item)
  (second (assoc item *opts* :test #'equal)))

(defun set-opt (item val)
  (let ((found (assoc item *opts* :test #'equal)))
    (if found
        (setf (second found) val)
        (push (list item val) *opts*))))

;;end here from util/opts.c

(defvar *install-cmds* nil)
(defvar *help-cmds* nil)
(defvar *list-cmd* nil)

(defun installedp (argv)
  (and (probe-file (merge-pathnames (format nil "impls/~A/~A/~A/~A/" (uname-m) (uname) (getf argv :target) (get-opt "as")) (homedir))) t))

(defun install-running-p (argv)
  ;;TBD
  (declare (ignore argv))
  nil)

(defun setup-signal-handler (path)
  ;;TBD
  (declare (ignore path)))

#+win32
(defun mingw-namestring (path)
  (string-right-trim (format nil "~%")
                     (uiop:run-program `(,(sh) "-lc" ,(format nil "cd ~S;pwd" (uiop:native-namestring path)))
                                       :output :string)))

(defun start (argv)
  (ensure-directories-exist (homedir))
  #+win32
  (let* ((w (ros:opt "wargv0"))
         (a (ros:opt "argv0"))
         (path (uiop:native-namestring
                (make-pathname :type nil :name nil :defaults (if (zerop (length w)) a w)))))
    (ros:setenv "MSYSTEM" #+x86-64 "MINGW64" #-x86-64 "MINGW32")
    (ros:setenv "PATH" (format nil "~A;~A"(subseq path 0 (1- (length path))) (ros:getenv "PATH"))))
  (let ((target (getf argv :target))
        (version (getf argv :version)))
    (when (and (installedp argv) (not (get-opt "install.force")))
      (format t "~A/~A is already installed. Try (TBD) for the forced re-installation.~%"
              target version)
      (return-from start (cons nil argv)))
    (when (install-running-p argv)
      (format t "It seems there are another ongoing installation process for ~A/~A somewhere in the system.~%"
              target version)
      (return-from start (cons nil argv)))
    (ensure-directories-exist (merge-pathnames (format nil "tmp/~A-~A/" target version) (homedir)))
    (let ((p (merge-pathnames (format nil "tmp/~A-~A.lock" target version) (homedir))))
      (setup-signal-handler p)
      (with-open-file (o p :direction :probe :if-does-not-exist :create))))
  (cons t argv))

(defun setup (argv)
  (setf (config "default.lisp") (getf argv :target)
        (config (format nil "~A.version" (getf argv :target))) (get-opt "as"))
  (cons t argv))

(defun install-ros (from)
  (let ((to (ensure-directories-exist
             (make-pathname
              :defaults (merge-pathnames "bin/" (homedir))
              :name (pathname-name from)
              :type (unless (or #+unix (equalp (pathname-type from) "ros"))
                      (pathname-type from))))))
    (format *error-output* "~&~A~%" to)
    (uiop/stream:copy-file from to)
    ;; Experimented on 0.0.3.38 but it has some problem. see https://github.com/roswell/roswell/issues/53
    #+nil(if (equalp (pathname-type from) "ros")
             (ros:roswell `("build" ,from "-o" ,to) :interactive nil)
             (uiop/stream:copy-file from to))
    #+sbcl(sb-posix:chmod to #o700)))

(defun install-script (path body)
  (declare (ignorable path body))
  #-win32
  (progn
    (with-open-file (o path
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (format o "#!/bin/sh~%~A" body))
    (sb-posix:chmod path #o755))
  #+win32
  (progn))

(defun parse-date (str)
  (setq str (string-trim " " str)
        str (substitute #\: #\- str)
        str (substitute #\: #\space str))
  (setq str (split-sequence:split-sequence #\: str))
  (make-instance 'simple-date-time:date-time
                 :day (parse-integer (first str))
                 :month (simple-date-time::from-short-month-name (second str))
                 :year (parse-integer (third str))
                 :hour (parse-integer (fourth str))
                 :minute (parse-integer (fifth str))
                 :second 0))
;;(parse-date "22-Aug-2015 18:19  ")
