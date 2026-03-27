;;;; 5ig.asd

(asdf:defsystem #:5ig
  :name "5IG - Sing Is a Grader"
  :description "5IG is a Grader for lisp program assessments."
  :author "Marcus Santos <m3santos@torontomu.ca>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:lisp-critic)
  :components ((:file "package")
               (:file "utils")
	       (:file "gensymify")
               (:file "similarity-scorer")
               (:file "sxm")
               (:file "testing")
               (:file "grader")
               (:file "5ig")))
