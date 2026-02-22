;;;; 5ig.lisp

(in-package #:5ig)


#|
(defun load-assessment-data (from)
  (let* ((data (make-hash-table))
         (form (read-form-and-intern from :testing-runtime)))
    (dolist (q questions) 
      (let ((q-label (first q))
            (q-data (second q))
            (given-test-progn (assoc :given q-data))
            (hidden-test-progn (assoc :hidden q-data)))))
    ))
|#


