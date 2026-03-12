(defconstant +timeout+ 1)

(defstruct test-result
  passed-p expr reason type)

(defun safe-read-student-code (file-path)
  "Reads a student's file in a restricted environment to prevent 
malicious reader macros or resource exhaustion."
  (with-open-file (in file-path)
    (let ((*read-eval* nil)           ;; Prevents #. execution
          (*read-base* 10)            ;; Ensures standard decimal reading
          ;;(*package* (find-package :sandbox))
          ;; Prevent circularity bombs
          (*read-circle* nil))        
      (handler-case
          ;; Reading the whole file as a list of forms
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                collect form)
        (error (c)
          (values nil (format nil "Reader Error: ~A" c)))))))

(defun run-in-subprocess (form student-code-path timeout)
  "Spawns a new SBCL process with a strict memory limit."
  (let* ((memory-limit-mb 256) ;; Set limit to 256MB
         (input-string 
           (format nil 
             "(handler-case 
                (sb-ext:with-timeout ~A 
                  (let ((result (progn ~S)))
                    (format t \"(~S . ~~S)~~%\" result)))
                (sb-ext:timeout () (format t \"(:error . :timeout)~~%\"))
                (error (c) (format t \"(:error . ~~S)~~%\" (format nil \"~~A\" c))))
              (sb-ext:exit)" 
             timeout form :ok))
         (command (list "sbcl" 
                        "--dynamic-space-size" (format nil "~A" memory-limit-mb)
                        "--noinform" 
                        "--disable-debugger" 
                        "--quit" 
                        "--load" (namestring student-code-path)
                        "--eval" input-string)))
    (handler-case
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program command :output :string :error-output :string :ignore-error-status t)
          (declare (ignore error-output))
          (if (/= exit-code 0)
              ;; Now we can be more specific about why it crashed
              (make-condition 'simple-error :format-control "Subprocess Terminated (Likely Heap Exhaustion or Timeout)")
              (let ((result (safe-read output)))
                (cond ((null result) (make-condition 'simple-error :format-control "No valid output from test"))
                      ((eq (car result) :ok) (cdr result))
                      (t (make-condition 'simple-error :format-control (cdr result)))))))
      (error (c) c))))

(defun report-result (result expected form)
  (let* ((passed (and (not (typep result 'condition))
                      (equal result expected)))
         (err-type (if (typep result 'condition) (type-of result) 'test-failure)))
    (make-test-result 
     :passed-p passed
     :expr form
     :reason (cond (passed "Passed")
                   ((typep result 'condition) (format nil "~A" result))
                   (t (format nil "Expected ~S but got ~S" expected result)))
     :type (if passed 'test-passed err-type))))

(defmacro is (expr student-path)
  "Each 'is' now requires the path to the student's source file."
  (let ((test-expr (second expr))
        (expected (third expr)))
    `(report-result (run-in-subprocess ',test-expr ,student-path ,+timeout+) 
                    ,expected 
                    ',test-expr)))

(defmacro test (name (student-path-var) &body body)
  "Defines a test function that accepts the path to the student's code."
  `(defun ,name (,student-path-var)
     (list ,@body)))

(defun summarize-results (q-label results)
  (let* ((total (length results))
         (passed (count-if #'test-result-passed-p results))
         (failures (remove-if #'test-result-passed-p results)))
    (list :q-label q-label
          :score (if (zerop total) 0 (float (* 100 (/ passed total))))
          :stats (list :total total :passed passed :failed (length failures))
          :feedback (mapcar (lambda (f)
                              (list :expr (test-result-expr f)
                                    :reason (test-result-reason f)
                                    :type (test-result-type f)))
                            failures))))

;; 1. Define the test suite
(test my-autograder-test (path)
  (is (equal (+ 2 2) 4) path)
  (is (equal (duplicate-elements '(1 2 3)) '(1 1 2 2 3 3)) path))

;; 2. Run it against a specific file
(defun run-grading-session (student-file-path)
  (let ((results (my-autograder-test student-file-path)))
    (summarize-results "Question 1" results)))

;; Example Call:
;; (run-grading-session #P"/tmp/student123/solution.lisp")
