;;;; package.lisp
(in-package :cl-user)

(defpackage #:sandbox-utils
  (:use #:cl)
  (:export #:is-safe))

(defpackage #:sandbox
  (:use #:cl #:fiveam #:sandbox-utils))

(defpackage #:utils
  (:use #:cl)
  (:export #:read-form-and-intern
           #:with-package
           #:add-prefix-to-symbol-in-form
           #:safe-read-student-code
           #:get-call-graph))

(defpackage #:sxm-compiler
  (:nicknames #:sxm)
  (:use #:cl
        #:sandbox-utils
        #:fiveam)
  (:export #:gen-exam-files))

(defpackage #:gensymifier
  (:use #:cl )
  (:export #:gensymify #:normalize-gensyms))

(defpackage #:similarity-scorer
  (:use #:cl #:gensymifier)
  (:export #:normalize #:score-similarity))

(defpackage #:grader
  (:use #:cl #:utils)
  (:export #:used-forbidden-function-p
           #:grade-student
           #:check-assessment-violations
           #:critique-student-solution))

(defpackage #:5ig
  (:use #:cl #:sxm-compiler #:utils #:grader)
  (:export #:gen-exam-files))
