;;;; 5ig.asd

(asdf:defsystem #:5ig
  :name "5IG - Sing Is a Grader"
  :description "5IG is a Grader for lisp program assessments that uses the fiveam testing framework."
  :author "Marcus Santos <m3santos@torontomu.ca>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:fiveam)
  :components ((:file "package")
               (:file "utils")
               (:file "sxm")
               (:file "5ig")))
