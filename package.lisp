;;;; package.lisp
(in-package :cl-user)


(defpackage #:utils
  (:use #:cl)
  (:export #:read-form-and-intern
           #:with-package
           #:add-prefix-to-symbol-in-form
           #:safe-read-student-code
           #:get-call-graph
           #:hash-std-id
           #:my-feedback-file))

(defpackage #:tester
  (:use #:cl #:utils)
  (:export #:summarize-results
           *tester-package*))

(defpackage #:gensymifier
  (:use #:cl)
  (:export #:gensymify
           #:normalize-gensyms))

(defpackage #:similarity
  (:use #:cl #:gensymifier #:utils)
  (:export #:normalize
           #:calc-similarity-score))


(defpackage #:grader
  (:use #:cl #:utils #:tester)
  (:export #:used-forbidden-function-p
           #:grade-student
           #:check-assessment-violations
           #:critique-student-solution-style
           #:calc-final-mark))

(defpackage #:sxm-compiler
  (:nicknames #:sxm)
  (:use #:cl #:tester)
  (:export #:gen-exam-files
           *parent-folder*))

(defpackage #:5ig
  (:use #:cl #:cl-user
        #:sxm-compiler
        #:utils
        #:grader
        #:similarity
        #:tester)
  (:export #:gen-exam-files #:chk-my-solution))

