(uiop:define-package :translator/common/common
    (:use :cl :alexandria)
  (:nicknames :common)
  (:export #:*mc-delimiters*
           #:*keywords*
           #:*identifiers*
           #:*constants*
           #:*symbol-categories*
           #:lexem
           #:line
           #:column
           #:code
           #:value
           #:get-lexem-type
           #:dump-lexem
           ;; utils
           #:awhen
           #:aif
           #:it))

(in-package :translator/common/common)

(defclass lexem ()
  ((line
    :initarg :line
    :accessor line)
   (column
    :initarg :column
    :accessor column)
   (code
    :initarg :code
    :accessor code)
   (value
    :initarg :value
    :accessor value)))

(defparameter *symbol-categories*
  (make-array '(128)
              :initial-contents
              '(6 6 6 6 6 6 6 6 0 0 0 0 0 0 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6
                0 6 6 6 6 6 6 6 51 6 6 6 6 6 3 6 1 1 1 1 1 1 1 1 1 1 3 3 41 3 42 6
                6 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 6 6 6 6 6
                6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6)))

(defparameter *keywords* nil)
(defparameter *identifiers* nil)
(defparameter *constants* nil)
(defparameter *mc-delimiters* nil)

(defmethod get-lexem-type ((l lexem))
  (let ((cd (code l)))
    (cond ((<= 0 cd 255) :ascii)
          ((<= 301 cd 400) :multichar-delimiter)
          ((<= 401 cd 500) :keyword)
          ((<= 501 cd 1000) :constant)
          (t ':identifier))))

(defun dump-lexem (lexem)
  (format nil "Lexem: code - ~5A |value - ~10A |type - ~10A |line - ~2A |column - ~2A"
          (code lexem) (value lexem)
          (get-lexem-type lexem)
          (line lexem) (column lexem)))

;; =================== UTILS ===================

(defmacro awhen (expr &body body)
  `(let ((it ,expr))
     (when it ,@body)))

(defmacro aif (expr true-branch false-branch)
  `(let ((it ,expr))
     (if it ,true-branch ,false-branch)))
