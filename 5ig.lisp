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

(defun q-label-p (label)
  (let ((str (symbol-name label)))
    (and (not (string= str "QUESTIONS"))
         (char= (aref str 0) #\Q))))

(defun get-assessment-test-case-data (assessment-data-file)
  "Return assoc list with relevant data from each question"
  (let* ((assessment-data (with-open-file (in assessment-data-file)
                            (read in))))
    (mapcar (lambda (qdata)
              `(,(first qdata)
                ,@(remove-if-not (lambda (data)
                                   (or (eq data :hidden)
                                       (eq data :given)
                                       (eq data :asked-function)))
                                 (rest qdata) :key #'first)))
            (remove-if-not  #'q-label-p  assessment-data :key #'first)) ))


(defun grade-student (student-file q-label fname kind)
  "Loads the student's program file in the sandbox,
   depending on KIND runs the given or hidden fiveam test cases, 
   and collects the results"
  (let ((runner-name (intern (format nil "RUN-~A-~A-~A-TEST" q-label fname kind))))
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
      
      summary)))

(defun process-entire-exam (student-file questions kind)
  "questions is a list of lists: ((:Q1 'fn1) (:Q2 'fn2))"
  (loop for (q-label fname) in questions
        collect (grade-student student-file q-label fname kind)))

(defun process-assessment-test-case-data (assessment-data-file q-labels-list)
  "Exposes the asked-functions in the sandbox, and adds prefix package
   to the function name in the test-cases code.
   Returns an a-list ((q given hidden) ...) containing the processed given and hidden 
   test-cases code"
  (let* ((assessment-data (get-assessment-test-case-data assessment-data-file))
         (testcase-data (mapcar (lambda (q)
                                  (list q
                                        :asked-function (second (assoc :asked-function (rest (assoc q assessment-data))))
                                        :given (second (assoc :given (rest (assoc q assessment-data))))
                                        :hidden (second (assoc :hidden (rest (assoc q assessment-data))))))
                              q-labels-list)))
    (mapc (lambda (d)
            (export (list (intern (symbol-name (getf (rest d) :asked-function)) :sandbox)) :sandbox))
          testcase-data)
    testcase-data))

(defun test-grade-student (student-file assessment-data-file q-label)
  "Evaluates metadata in the sandbox package and executes the grade."
  (let* ((assessment-test-case-data (process-assessment-test-case-data assessment-data-file (list q-label)))
         (q-testcase-data (progn (format t "~s @@@~%" (rest (assoc q-label assessment-test-case-data)))
                                 (rest (assoc q-label assessment-test-case-data))))
         (fname (getf q-testcase-data :asked-function))
         (given-testcases-metadata (getf q-testcase-data :given)))
    ;; 1. Set the evaluation context to :sandbox
    (with-package :sandbox
      ;; EVAL compiles/defines the test and runner in this package
      (eval given-testcases-metadata)
      
      ;; 2. Perform the grading
      ;; grade-student uses (intern (format ...)) to find the runner
      ;; in the *package* where it is called.
      
      (grade-student student-file q-label fname 'given))))

(defun derive-assessment-data-file (solution-file-path)
  "Derives the assessment data file path from the solution file path."
  (let* ((assessment-folder-name (car (last (pathname-directory solution-file-path))))
         (assessment-data-file (format nil "~a~a.data" *assessment-data-folder* assessment-folder-name)))
    assessment-data-file))

#|
(defun chk-my-solution (a#)
  "Checks a student's solution file against examples.
   a#: String identifying the solution file, e.g., '~/lab01/q1.lisp'."
  (unless (probe-assessment-file a#)
    (error "~%!!!!!!!! Error: You saved your file in the wrong folder. Please save it in the specified folder. !!!!!!!!"))
  (let* ((assessment-data-file (derive-assessment-data-file a#))
         (q-label (intern (string-upcase (pathname-name a#)) :keyword))
         (assessment-data (process-assessment-data assessment-data-file (list q-label)))
         (current-pckg *package*))
    (unwind-protect
         (format t "~V@{~A~:*~}~%Lisp generated load/compile messages:" *separators* "+")         
      (let* ((eval (load-and-evaluate-solution a# question-name assessment-data))
             (error-type (second eval)))
        (handle-evaluation-output error-type eval question-name)
        (critique-student-solution a#))
      (setf *package* current-pckg)))
  t)

|#
