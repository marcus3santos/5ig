;;;; package.lisp
(in-package :cl-user)

(defpackage #:sandbox
  (:use #:cl #:fiveam))

(defpackage #:utils
  (:use #:cl)
  (:export #:is-safe
           #:read-form-and-intern
           #:with-package
           #:add-prefix-to-symbol-in-form))

(defpackage #:sxm-compiler
  (:nicknames #:sxm)
  (:use #:cl
        #:utils
        #:fiveam)
  (:export #:gen-exam-files))

(defpackage #:5ig
  (:use #:cl #:sxm-compiler)
  (:export #:gen-exam-files))
