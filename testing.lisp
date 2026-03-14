;;;; Modular Lisp Sandbox Runner

;; --- 1. DATA STRUCTURES ---
(defstruct (execution-result (:type list))
  status  ; :ok, :error, :timeout, :overflow, :security-violation
  value   ; The evaluated Lisp object
  log     ; Combined stdout/stderr
  code)   ; Exit status code from the OS

;; --- 2. SAFETY MODULE ---
(defparameter *forbidden-symbols* '(delete-file delete-directory rename-file 
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

;; --- 3. EXECUTION ENGINE ---
(defparameter *sbcl-bin* "sbcl")
(defparameter *mem-limit-mb* 256)

(defun %build-payload (form student-path)
  "Generates the string of code to be passed to the subprocess."
  (prin1-to-string
   `(handler-case
        (progn
          (load (pathname ,(namestring student-path)))
          (format t "~S" (eval ',form)))
      (storage-condition () (uiop:quit 101)) ; Handles Stack/Heap Overflow
      (error (e) (progn (format *error-output* "~A" e) 
                        (uiop:quit 102))))))

(defun %run-os-process (lisp-code timeout)
  "Handles the low-level UIOP system call."
  (handler-case
      (uiop:run-program 
       (list *sbcl-bin* "--noinform" "--non-interactive"
             "--dynamic-space-size" (write-to-string *mem-limit-mb*)
             "--eval" lisp-code "--quit")
       :output :string 
       :error-output :string 
       :timeout timeout 
       :ignore-error-status t)
    (uiop/run-program:subprocess-error () 
      (values nil "Process Timed Out" nil))))

;; --- 4. MAIN ORCHESTRATOR ---
(defun run-in-subprocess (form student-code-path timeout)
  "Main entry point: Validates, executes, and reports."
  (multiple-value-bind (safe-p forbidden-item) (check-safety student-code-path)
    (if (not safe-p)
        (make-execution-result 
         :status :security-violation
         :log (format nil "Security Error: Found ~A" forbidden-item))
        
        (let ((payload (%build-payload form student-code-path)))
          (multiple-value-bind (stdout stderr exit-code) 
              (%run-os-process payload timeout)
            
            (let ((status (cond ((null exit-code) :timeout)
                                ((= exit-code 0)   :ok)
                                ((= exit-code 101) :overflow)
                                (t                 :error))))
              (make-execution-result 
               :status status
               :value  (when (eq status :ok) 
                         (handler-case (read-from-string stdout) (error () nil)))
               :log    (if (eq status :ok) stdout stderr)
               :code   exit-code)))))))

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
