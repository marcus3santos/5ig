;;;; 5ig.lisp

(in-package #:5ig)

;; Folder where assessment metadata files should be stored

(defparameter *assessment-data-folder* (merge-pathnames (make-pathname :directory '(:relative "quicklisp" "local-projects" "5ig" "Assessment-data"))
                                                        (user-homedir-pathname)))

;; Where the exam files are generated

(defparameter *parent-folder* "Gen-files/")

(defun q-label-p (label)
  (let ((str (symbol-name label)))
    (and (not (string= str "QUESTIONS"))
         (char= (aref str 0) #\Q))))

(defun get-assessment-test-case-data (assessment-data-file)
  "Return assoc list (:qi <tagged-data>) where :qi is a question label and
   <tagged-data> are forms of the kind (:tag <data>)"
  (let* ((assessment-data (with-package *tester-package*
                            (with-open-file (in assessment-data-file)
                              (read in)))))
    (mapcar (lambda (qdata)
              `(,(first qdata)
                ,@(remove-if-not (lambda (data)
                                   (or (eq data :hidden)
                                       (eq data :given)
                                       (eq data :asked-function)
                                       (eq data :forbidden-symbols)
                                       (eq data :solutions)))
                                 (rest qdata) :key #'first)))
            (remove-if-not  #'q-label-p  assessment-data :key #'first)) ))




(defun process-assessment-test-case-data (assessment-data-file q-labels-list)
  "Extracts and processes programming assessment data for a specific set of questions.

   This function retrieves test case data from ASSESSMENT-DATA-FILE and filters it 
   based on the provided Q-LABELS-LIST. For each question label in the list, it 
   assembles a property list containing the function name, the forbidden symbols.
   given test cases, and hidden test cases.

   Additionally, it performs a side effect: it interns the 'asked-function' symbols 
   into the :TESTER package and makes them accessible to packages that use it.

   ### Arguments:
   * ASSESSMENT-DATA-FILE: A file path or designator containing the assessment 
     definitions (passed to 'get-assessment-test-case-data').
   * Q-LABELS-LIST: A list of keys (labels) corresponding to the questions to 
     be processed.

   ### Returns:
   * An association list where each element is of the form:
     (LABEL :ASKED-FUNCTION function-symbol 
            :FORBIDDEN-SYMBOLS (:PENALTY p :SYMBOLS (symbs))
            :GIVEN (test-cases) :HIDDEN (test-cases)
            :SOLUTIONS (solutions))"
  (let* ((assessment-data (get-assessment-test-case-data assessment-data-file))
         (testcase-data (mapcar (lambda (q)
                                  (list q
                                        :asked-function (second (assoc :asked-function (rest (assoc q assessment-data))))
                                        :forbidden-symbols (rest (assoc :forbidden-symbols (rest (assoc q assessment-data))))
                                        :given (second (assoc :given (rest (assoc q assessment-data))))
                                        :hidden (second (assoc :hidden (rest (assoc q assessment-data))))
                                        :solutions (rest (assoc :solutions (rest (assoc q assessment-data))))))
                                q-labels-list)))
    (mapc (lambda (d)
            (export (list (intern (symbol-name (getf (rest d) :asked-function)) *tester-package*)) *tester-package*))
          testcase-data)

    testcase-data))

(defun reset-tester-package ()
  (when pkg
    ;; Unuse everything to break links
    (unuse-package *tester-package* pkg)
    ;; Delete and recreate or just unintern all symbols
    (do-symbols (s pkg)
      (unintern s pkg))))

#|
(defun test-grade-student (student-file assessment-data-file q-label)
  "Evaluates metadata in the sandbox package and executes the grade."
  (reset-sandbox-package)
  (let* ((assessment-test-case-data (process-assessment-test-case-data assessment-data-file (list q-label)))
         (q-testcase-data (rest (assoc q-label assessment-test-case-data)))
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
|#

(defun derive-assessment-data-file (solution-file-path)
  "Derives the assessment data file path from the solution file path."
  (let* ((assessment-folder-name (car (last (pathname-directory solution-file-path))))
         (assessment-data-file (format nil "~a~a.data" *assessment-data-folder* assessment-folder-name)))
    assessment-data-file))

(defun compile-test-cases (test-name test-form)
  "Compiles the fiveam test and its runner. The argument is a progn 
   containing a fiveam test and its checks and the defun for the 
   runner that runs the test and clears up the name space of the 
   tested functions."
  (with-package *tester-package*
    ;; Mute redefinition warnings during EVAL
    (handler-bind ((style-warning #'muffle-warning)
                   (warning #'muffle-warning))
      (eval test-form))))


;; --- Supporting Sub-functions ---

(defun perform-static-analysis (file fname tc-data)
  "Encapsulates the reading and analysis of student code.
   VIOLATIONS will be nil if the student's code triggers
   a loading error."
  
  (let* ((program    (safe-read-student-code file))
         (forbidden  (getf (getf tc-data :forbidden-symbols) :symbols))
         (graph      (get-call-graph fname program))
         (violations (check-assessment-violations fname program forbidden)))
    (values program violations graph)))

(defun apply-violation-penalty! (summary forbidden-info)
  "Modifies the summary score based on forbidden symbol violations."
  (let* ((penalty    (getf forbidden-info :penalty))
         (orig-score (getf summary :score))
         (multiplier (/ (- 100 penalty) 100)))
    (setf (getf summary :score) (float (* orig-score multiplier)))
    (setf (getf summary :penalty-applied) penalty)
    summary)) ; Store for the reporter

(defun render-grading-report (stream label score-history summary graph violations fname testcase-type)
  "Handles the final formatting of the grading results."
  ;; 1. Header
  (format stream "~%### Question ~A~%" (subseq (symbol-name label) 1))
  (format stream "~%--- Functional Correctness Analysis (% of test cases passed) ---~%")
  (format stream "~%Score: ~,2F, [0 to 100]" (getf score-history :functional-score))
  (unless (getf summary :error)
    ;; 2. Static Analysis / Violations
    (let ((violation-report (generate-forbidden-function-violation-report fname graph violations testcase-type)))
      (when violation-report
        (format stream "~%~A" violation-report)))

    ;; 3. Functional Scoring & Penalties
    (when (getf summary :penalty-applied)
        (format stream "~%!!! PENALTY APPLIED !!!~%Original Score: ~,2F, [0 to 100]~%Adjusted Score: ~,2F, [0 to 100] (-~D% Penalty)~%" 
                (getf score-history :functional-score) (getf score-history :violation-score) (getf summary :penalty-applied))
        ;;(format stream "~%Score: ~,2F, [0 to 100]~%" (getf score-history :functional-score))
        )

    ;; 4. Similarity & Style Feedback
    ;; Only render if it's a hidden test and feedback exists
    (let ((feedback (getf summary :similarity-feedback)))
      (when (and (eq testcase-type :hidden) feedback)
        (format stream "~%--- Final Score, Style & Logic Similarity Analysis ---~%~%")
        (format stream "~A~%" feedback)
        (format stream "------------------------------------------~%"))))

  ;; 5. Error messages if any
  (when (getf summary :error)
    (format stream "~%!!! EXECUTION ERROR: !!!~%~A~%" (getf summary :error))))


(defun generate-forbidden-function-violation-report (target-func graph violations testcase-type)
  "Produces a human-readable string summarizing any forbidden function 
   violations."
  (let ((report-stream (make-string-output-stream)))
    (format report-stream "~%--- Forbidden Function Violation Analysis for Function ~A ---~%" target-func)
    
    ;; 2. Violation Reporting
    (if (null violations)
        (format report-stream "~%SUCCESS: No forbidden functions detected.~%")
        (progn
          ;; 1. Structure Overview
          (format report-stream "Function Call Structure:~%")
          (dolist (node (reverse graph))
            (format report-stream "  [~A] calls -> ~{~A~^, ~}~%" 
                    (car node) (or (second node) '("no functions"))))
          
          (format report-stream "~%--- Violation Check ---~%")
    
          (format report-stream "~%FAILED: Forbidden functions found.~%")
          (dolist (v violations)
            (format report-stream "  - In function '~A': Used forbidden (~{~A~^, ~})~%" 
                    (car v) (cdr v)))
          (when (eq testcase-type :given)
            (format report-stream "~%Note: Rewrite the logic without using this/these helper(s)."))))
    
    (get-output-stream-string report-stream)))

(defun orchestrate-grading-of-one-solution (student-file metadata testcase-type &optional (stream t))
  "Orchestrates the grading of one solution file for a given question."
  (let* ((question-label (first metadata))
         (tc-data        (rest metadata))
         (fname          (getf tc-data :asked-function))
         (solutions      (getf tc-data :solutions))
         ;; 1. Execute Functional Testing
         (summary        (with-package *tester-package*
                           (grade-student student-file question-label fname testcase-type)))
         (score-history (list :functional-score (getf summary :score))))
    
    ;; 2. Static Analysis & Violation Checking
    (multiple-value-bind (program violations graph)
        (perform-static-analysis student-file fname tc-data)
      
      ;; 3. Apply Forbidden Symbol Penalties
      (when (and violations (> (getf summary :score) 0))
        (setf summary (apply-violation-penalty! summary (getf tc-data :forbidden-symbols))))
      (setf (getf score-history :violation-score) (getf summary :score))
      ;; 4. Similarity Grading (Only for :hidden testcases and if no functional errors)
      (when (and (eq testcase-type :hidden) 
                 (not (getf summary :error)))

        ;; score-similarity returns a plist: (:score :instructor-solution :student-solution) 
        (let* ((sim-results (calc-similarity-score fname program solutions)) 
               (sim-score   (getf sim-results :score))
               (prof-sol    (getf sim-results :instructor-solution)))
          
          ;; 5. Invoke refactored final-mark to get feedback and score
          (multiple-value-bind (feedback-string final-score)
              (calc-final-mark score-history sim-score prof-sol)
            
            ;; Update summary with the weighted/bonus score and the report string
            (setf (getf summary :score) final-score)
            (setf (getf summary :similarity-feedback) feedback-string)
            (setf (getf score-history :similarity-bonus-score) final-score))))
      
      ;; 6. Output Reporting
      (render-grading-report stream question-label score-history summary graph violations fname testcase-type))
    
    ;; 7. Style critique
    (unless (getf summary :error)
      (critique-student-solution-style student-file))
    summary))


(defun chk-my-solution (a# &optional (kind :given))
  "Checks a student's solution file. Performs safe reading, static analysis,
   and applies percentage penalties based on the :penalty metadata value."
  (unless (probe-file a#)
    (error "~%!!!!!!!! Error: File not found. !!!!!!!!"))
  
  (let* ((assessment-data-file (derive-assessment-data-file a#))
         (q-label (intern (string-upcase (pathname-name a#)) :keyword))
         (assessment-test-case-data (process-assessment-test-case-data assessment-data-file (list q-label)))
         (q-testcase-data (assoc q-label assessment-test-case-data))
         (testcases-metadata (getf (rest q-testcase-data) kind))
         (test-name (second (third testcases-metadata))))
    ;; Compiles test cases and the test cases runner
    (compile-test-cases test-name testcases-metadata)
    (orchestrate-grading-of-one-solution a# q-testcase-data kind)))

(defun orchestrate (assessment-folder assessment-name)
  (let* ((safe-path (uiop:ensure-directory-pathname assessment-folder)) ;; ensures path ends with /
         (student-lisp-program-files (directory (make-pathname :name :wild
                                                              :type "lisp"
                                                              :defaults safe-path)))
         (assessment-data-file (merge-pathnames (make-pathname :name assessment-name
                                                               :type "data")
                                                *assessment-data-folder*)))
     (let ((report-stream (make-string-output-stream)))
       (orchestrate-student-grading "~/pt1-v2/q1.lisp" )  
       (get-output-stream-string report-stream))

    student-lisp-program-files
    assessment-data-file))
