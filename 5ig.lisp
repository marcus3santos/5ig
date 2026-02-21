;;;; 5ig.lisp

(in-package #:5ig)


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
          :score (if (zerop total) 0 (* 100 (/ passed total)))
          :stats (list :total total :passed passed :failed (length failures))
          ;; Map the failures into a clean feedback list
          :feedback (mapcar (lambda (f)
                              (list :expr (fiveam::test-expr f)
                                    :reason (fiveam::reason f)
                                    :type (type-of f)))
                            failures))))

(defun grade-student (student-file q-label fname)
  (let ((runner-name (intern (format nil "RUN-~A-~A-TEST" q-label fname) :sxm-compiler)))
    ;; 1. Load student code
    (handler-case (load student-file)
      (error (c) (return-from grade-student 
                   (list :q-label q-label :error (format nil "Load Error: ~A" c)))))
    
    ;; 2. Execute the pre-compiled runner
    (let* ((raw-results (funcall runner-name))
           (summary (summarize-results q-label raw-results)))
      
      ;; 3. Log or Print the outcome
      (format t "~&Question ~A: ~D/~D passed (~A%)~%" 
              q-label 
              (getf (getf summary :stats) :passed)
              (getf (getf summary :stats) :total)
              (getf summary :score))
      
      summary)))

(defun process-entire-exam (student-file questions)
  "questions is a list of lists: ((:Q1 'fn1) (:Q2 'fn2))"
  (loop for (q-label fname) in questions
        collect (grade-student student-file q-label fname)))
