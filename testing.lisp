(in-package :tester)

;; The package for regression testing

(defparameter *tester-package* (find-package :tester))


;;;; Modular Lisp Sandbox Runner

(defparameter +timeout+ 1)

;; --- 1. DATA STRUCTURES ---
(defstruct (execution-result (:type list))
  status       ; :ok, :error, :timeout, :overflow, :security-violation
  value        ; (:ok :PASSED-P <T/NIL> :GOT <returned-result> :EXPECTED <expected-result>))
  log          ; Combined stdout/stderr
  code)   ; Exit status code from the OS

(defstruct test-result
  passed-p ; test passed?, T/NIL
  expr     ; the form that was tested
  reason   ; The reason, a string
  status)    ; Either :ok, :timeout, :overflow, or :error

;; --- 2. SAFETY MODULE ---
(defparameter *forbidden-symbols*
  '(delete-file delete-directory rename-file 
    sb-ext:run-program uiop:run-program 
    cl-user::quit sb-ext:quit sb-sys:make-fd-stream
    cl:eval-when cl:load-time-value))

(defun %contains-forbidden-p (form)
  "Recursive check for blacklisted symbols."
  (cond ((symbolp form) 
         (member (symbol-name form) *forbidden-symbols* :test #'string-equal :key #'symbol-name))
        ((consp form) 
         (or (%contains-forbidden-p (car form))
             (%contains-forbidden-p (cdr form))))
        (t nil)))

(defun check-safety (path)
  "Statically analyzes a file. Returns (values is-safe-p forbidden-item)."
  (let ((*read-eval* nil)) ; CRITICAL: Prevents #. execution during READ
    (handler-case
        (with-open-file (in path)
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                when (%contains-forbidden-p form)
                  do (return-from check-safety (values nil form))))
      (error () (return-from check-safety (values nil :read-error))))
    (values t nil)))

(defun safe-read (output-string)
  "Scans a string for the (:OK ...)  marker, ignoring noise."
  (with-input-from-string (s output-string)
    (loop for form = (handler-case (read s nil :eof)
                       (error () :junk)) ; Skips student print statements or typos
          until (eq form :eof)
          when (and (consp form) (member (car form) '(:ok)))
            return form)))

;; --- 3. EXECUTION ENGINE ---

(defparameter *stack-limit-mb* 4)   ; Small stack to catch recursion fast
(defparameter *heap-limit-mb* 40)  ; Enough for small tasks, safe from "fork-bombs"

(defun %build-payload (form student-path timeout)
  "Generates the string of code to be passed to the subprocess."
  (format nil
          "(progn 
             (load ~S)
             (sb-ext:gc :full t)
             (handler-case
                 (sb-ext:with-timeout ~D
                   (handler-case
                       (let* ((expr (quote ~S))
                              (op (first expr))
                              (a1 (eval (second expr)))
                              (a2 (eval (third expr)))
                              (result (funcall op a1 a2)))
                         (format t \"~%(:OK :PASSED-P ~~S :GOT ~~S :EXPECTED ~~S)~%\" result a1 a2))
                     (storage-condition () (uiop:quit 101))
                     (error (e) (progn (format *error-output* \"~~A\" e) 
                                       (uiop:quit 102)))))
               (sb-ext:timeout (c) (progn (format *error-output* \"~~A\" e) 
                                          (uiop:quit 1)))))"
          student-path timeout form))

(defun %run-os-process (lisp-code timeout)
  "Handles the low-level UIOP system call."
  (handler-case
      (uiop:run-program 
       (list "sbcl"
             "--dynamic-space-size" (write-to-string *heap-limit-mb*)
             "--control-stack-size" (write-to-string *stack-limit-mb*)             
             "--noinform"
             "--non-interactive"
             "--eval" lisp-code "--quit")
       :output :string 
       :error-output :string 
       :timeout timeout 
       :ignore-error-status t)
    (uiop/run-program:subprocess-error () 
      (values nil "Process Timed Out" nil))))

;; --- 4. MAIN ORCHESTRATOR ---
(defun run-in-subprocess (form path timeout)
  "Main entry point: Validates, executes, and reports."
  (let ((student-code-path (truename path)))
    (multiple-value-bind (safe-p forbidden-item) (check-safety student-code-path)
      (cond ((eq forbidden-item :read-error)
             (make-execution-result
              :status :error
              :log (format nil "Read Error: Unbalanced parentheses or invalid character in program file ~A" path)))
            ((not safe-p)
             (make-execution-result 
              :status :security-violation
              :log (format nil "Security Error: Found ~A" forbidden-item)))
            (t
             (let ((payload (%build-payload form student-code-path timeout)))
               (multiple-value-bind (stdout stderr exit-code)
                  
                   (%run-os-process payload timeout)
                 
                 (let ((status (cond ((= exit-code 1)   :timeout)
                                     ((= exit-code 0)   :ok)
                                     ((= exit-code 101) :overflow)
                                     (t                 :error)))
                       (result (handler-case (safe-read stdout) (error () nil))))
                   (make-execution-result 
                    :status status
                    :value  (when (eq status :ok)
                              (if (eq (car result) :ok)
                                  (cdr result)
                                  (format nil "STUDENT ERROR: ~A" (getf (cdr result) :got))))
                    :log    (if (eq status :ok) stdout stderr)
                    :code   exit-code)))))))))

(defun report-result (result expected form)
  (let ((passed (getf (execution-result-value result) :passed-p)))
    (format t "~:[f~;.~]" passed)
    (make-test-result 
     :passed-p passed
     :expr form
     :reason (cond (passed "Passed")
                   ((eq :timeout (execution-result-status result)) "Execution Timed Out.")
                   ((eq :overflow (execution-result-status result)) "Memory Overflow.")
                   ((eq :error (execution-result-status result)) (execution-result-log result))
                   ((not passed)
                    (format nil "Expected ~S but got ~S" expected (getf (execution-result-value result) :GOT))))
     :status (execution-result-status result))))

(defmacro is (expr student-path)
  "Each 'is' now requires the path to the student's source file."
  (let ((test-expr (second expr))
        (expected (third expr)))
    `(report-result (run-in-subprocess ',expr ,student-path ,+timeout+)
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
                                    :status (test-result-status f)))
                            failures))))

#|

;; 1. Define the test suite
(test my-autograder-test (path)
  (is (equal (fact 0) 1) path)
  (is (equal (fact 4) 24) path)
  (is (equal (fact 3) 6) path))

;; 2. Run it against a specific file
(defun run-grading-session (student-file-path)
  (let ((results (my-autograder-test student-file-path)))
    (summarize-results "Question 1" results)))

;; Example Call:
;; (run-grading-session #P"/tmp/student123/solution.lisp")
|#
