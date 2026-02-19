(ql:quickload :fiveam)

(defpackage :autograder
  (:use :cl :fiveam))

(in-package :autograder)

(defun fact (x)
  (if (< x 1) 1
      (* x (fact x))))

;; 0. A macro for capturing endless or memory exhausting computations

(defmacro is-safe (expression &key (timeout 1))
  "Evaluates the given expression and returns its result. If the expression
   does not complete within the specified timeout (in seconds), it returns
   a timeout error condition. If the expression triggers a stack overflow
   the store-condition captures that condition. Other kinds of errors are
   captured by the general ERROR condition"
  (let ((c (gensym)))
    `(handler-case
         (sb-ext:with-timeout ,timeout
           (fiveam:is ,expression))
       (sb-ext:timeout ()
         (fiveam:fail "Timeout: Infinite loop detected (> ~A sec)" ,timeout))
       (storage-condition ()
         (fiveam:fail "Memory error: Memory exhausted"))
       (error (,c)
         (fiveam:fail "Execution Error: ~A" ,c)))))


;; 1. Setup the test environment
(def-suite student-suite :description "Grading Suite")
(in-suite student-suite)

(test addition-test
  (is-safe (= (fact 3) (+ 2 2))))

(test logic-test
  ;;(is-safe (fiveam:is-true t))
  (is-safe (equal "apple" "orange"))) ; This will fail

;; 2. The Programmatic Runner
(defun grade-submission (suite-symbol)
  "Runs the suite and returns a list of plists for each check."
  (let ((results (fiveam:run suite-symbol))) ; Use 'run', not 'run!'
    (loop for res in results
          collect (list :test-name (fiveam::name (fiveam::test-case res))
                        :result-type (type-of res)
                        :passed-p (typep res 'fiveam::test-passed)
                        :expression (fiveam::test-expr res)
                        :reason (fiveam::reason res)))))

;; 3. Execute and inspect
(defparameter *final-results* (grade-submission 'student-suite))

;; Print the list to see the structure
(format t "~&Collected ~D results.~%" (length *final-results*))
(dolist (item *final-results*)
  (format t "~%@@@@@ Test: ~A~%  Passed: ~A~%  Expression: ~A~%  Error: ~A~%"
          (getf item :test-name)
          (getf item :passed-p)
          (getf item :expression)
          (getf item :reason)))
