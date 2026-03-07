(in-package :grader)


(defun check-assessment-violations (target-func program forbidden-list)
  "Analyzes the program for forbidden function calls, even if nested 
   in helper functions. Returns a list of (FUNCTION . VIOLATIONS).
   Returns nil if program is nil"
  (let* ((graph (get-call-graph target-func program))
         (report '()))
    (dolist (node graph)
      (let* ((func-name (car node))
             (calls (second node))
             (violations (intersection calls forbidden-list)))
        (when violations
          (push (cons func-name violations) report))))
    report))

(defun summarize-results (q-label results)
  "Aggregates FiveAM result objects into a summarized report."
  (let ((total (length results))
        (passed (count-if (lambda (res) 
                            (typep res 'fiveam::test-passed)) 
                          results))
        (failures (remove-if (lambda (res) 
                               (typep res 'fiveam::test-passed)) 
                             results)))
    (list :q-label q-label
          :score (if (zerop total) 0 (float (* 100   (/ passed total))))
          :stats (list :total total :passed passed :failed (length failures))
          ;; Map the failures into a clean feedback list
          :feedback (mapcar (lambda (f)
                              (list :expr (fiveam::test-expr f)
                                    :reason (fiveam::reason f)
                                    :type (type-of f)))
                            failures))))

(defun critique-student-solution (sol)
  (let ((output (with-output-to-string (*standard-output*)
                  (lisp-critic:critique-file sol))))
    ;; We check if the output contains a hint (usually starts with a paren or keyword)
    ;; Adjust the search string based on what lisp-critic actually outputs
    (format t "~%--- Style Feedback ---~%~%Below is your 'pretty-printed' code. ")
    (if (search "----" output :test #'char-equal)
        (format t "The suggestions below your code can ~%help you write more 'Lisp-y' solutions:~%~%~A" output)
        (format t "No idiomatic improvements suggested.~%~%~A" output))))

(defun grade-student (student-file q-label fname kind)
  "Loads the student's program file in the sandbox,
   depending on KIND runs the given or hidden fiveam test cases, 
   and collects the results"
  (let ((runner-name (intern (format nil "RUN-~A-~A-~A-TEST" q-label fname kind) :sandbox)))
    ;; 1. Load student code
    
    (handler-case (with-package :sandbox
                    (load student-file))
      (error (c) (return-from grade-student
                   (list :q-label q-label :score 0.0 :error (format nil "Load Error: ~A" c)))))
    
    ;; 2. Execute the pre-compiled runner
    (let* ((raw-results (funcall runner-name))
           (summary (summarize-results q-label raw-results)))
      summary)))

