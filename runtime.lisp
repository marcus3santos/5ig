(in-package :testing-runtime)

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
  (let ((runner-name (intern (format nil "RUN-~A-~A-TEST" q-label fname))))
    ;; 1. Load student code
    
    (handler-case (with-package :sandbox
                    (load student-file))
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
      
      summary)
    ))

(defun process-entire-exam (student-file questions)
  "questions is a list of lists: ((:Q1 'fn1) (:Q2 'fn2))"
  (loop for (q-label fname) in questions
        collect (grade-student student-file q-label fname)))

(defun test-grade-student (student-file q-label fname)
  "Evaluates metadata in the :testing-runtime package and executes the grade."
  
  (let ((test-metadata
          (add-prefix-to-symbol-in-form 
           '(PROGN
             ;; Define the FiveAM test. Symbols like IS-SAFE will be 
             ;; resolved in the current package context during EVAL.
             (IT.BESE.FIVEAM:TEST Q1-LIST-OF-ARRAYS-TEST
               (IS-SAFE (EQUALP (LIST-OF-ARRAYS NIL NIL) NIL) :TIMEOUT 2)
               (IS-SAFE
                (EQUALP (LIST-OF-ARRAYS (LIST 1 2) (LIST 3 4)) (LIST #(1 3 4) #(2 4 6)))
                :TIMEOUT 2)
               (IS-SAFE
                (EQUALP (LIST-OF-ARRAYS (LIST -1 2 -3) (LIST 4 3 -2))
                        (LIST #(-1 4 3) #(2 3 5) #(-3 -2 -5)))
                :TIMEOUT 2)
               (IS-SAFE
                (EQUALP (LIST-OF-ARRAYS (LIST 10 20) (LIST 30 40))
                        (LIST #(10 30 40) #(20 40 60)))
                :TIMEOUT 2)
               (IS-SAFE (EQUALP (LIST-OF-ARRAYS (LIST 5) (LIST 10)) (LIST #(5 10 15)))
                        :TIMEOUT 2)
               (IS-SAFE
                (EQUALP (LIST-OF-ARRAYS (LIST 2 4 6 8) (LIST 0 0 0 0))
                        (LIST #(2 0 2) #(4 0 4) #(6 0 6) #(8 0 8)))
                :TIMEOUT 2))
             
             ;; Define the runner function
             (DEFUN RUN-Q1-LIST-OF-ARRAYS-TEST ()
               (UNWIND-PROTECT (IT.BESE.FIVEAM:RUN 'Q1-LIST-OF-ARRAYS-TEST)
                 (WHEN (FBOUNDP 'LIST-OF-ARRAYS) (FMAKUNBOUND 'LIST-OF-ARRAYS))
                 (WHEN (BOUNDP 'LIST-OF-ARRAYS) (MAKUNBOUND 'LIST-OF-ARRAYS))
                 (SETF (SYMBOL-PLIST 'LIST-OF-ARRAYS) NIL))))
           'list-of-arrays
           :sandbox)
          ))
    ;; 0. Export the function name from the sandbox
    (unintern 'list-of-arrays :testing-runtime)
    (with-package :sandbox
      (export 
       (mapcar (lambda (s)
                 (intern s :sandbox))
               '("LIST-OF-ARRAYS"))
       :sandbox))

    ;; 1. Set the evaluation context to :testing-runtime
    (with-package :testing-runtime
      ;; EVAL compiles/defines the test and runner in this package
      (eval test-metadata)
      
      ;; 2. Perform the grading
      ;; grade-student uses (intern (format ...)) to find the runner
      ;; in the *package* where it is called.
      (grade-student student-file q-label fname))))


