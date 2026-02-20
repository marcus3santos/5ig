;;;; package.lisp

(defpackage :sxm-compiler
  (:nicknames :sxm)
  (:use #:cl)
  (:export :gen-exam-files))

(defpackage #:5ig
  (:use #:cl :sxm-compiler)
  (:export :gen-exam-files))
