;;;; package.lisp

(defpackage #:testing-runtime
  (:documentation "Creates the code testing runtime")
    (:use #:cl)
    (:export #:is-safe))

(defpackage #:sxm-compiler
  (:nicknames #:sxm)
  (:use #:cl #:fiveam)
  (:export #:gen-exam-files))

(defpackage #:5ig
  (:use #:cl #:sxm-compiler)
  (:export #:gen-exam-files))
