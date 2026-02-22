;;;; package.lisp

(defpackage #:testing-runtime
  (:documentation "Creates the code testing runtime")
    (:use #:cl)
    (:export #:is-safe))

(defpackage #:utils
  (:use #:cl)
  (:export #:read-form-and-intern))

(defpackage #:sxm-compiler
  (:nicknames #:sxm)
  (:use #:cl #:fiveam #:testing-runtime #:utils)
  (:export #:gen-exam-files))

(defpackage #:5ig
  (:use #:cl #:sxm-compiler)
  (:export #:gen-exam-files))
