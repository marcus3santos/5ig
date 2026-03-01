;;;; 5ig.lisp

(in-package #:5ig)

;; Folder where assessment metadata files should be store

(defparameter *assessment-data-folder* "~/quicklisp/local-projects/CodeGrader/Assessment-data/")


(defun q-label-p (label)
  (let ((str (symbol-name label)))
    (and (not (string= str "QUESTIONS"))
         (char= (aref str 0) #\Q))))

(defun get-assessment-test-case-data (assessment-data-file)
  "Return assoc list (:qi <tagged-data>) where :qi is a question label and
   <tagged-data> are forms of the kind (:tag <data>)"
  (let* ((assessment-data (with-package :sandbox
                            (with-open-file (in assessment-data-file)
                              (read in)))))
    (mapcar (lambda (qdata)
              `(,(first qdata)
                ,@(remove-if-not (lambda (data)
                                   (or (eq data :hidden)
                                       (eq data :given)
                                       (eq data :asked-function)
                                       (eq data :forbidden-symbols)))
                                 (rest qdata) :key #'first)))
            (remove-if-not  #'q-label-p  assessment-data :key #'first)) ))




(defun process-assessment-test-case-data (assessment-data-file q-labels-list)
  "Extracts and processes programming assessment data for a specific set of questions.

   This function retrieves test case data from ASSESSMENT-DATA-FILE and filters it 
   based on the provided Q-LABELS-LIST. For each question label in the list, it 
   assembles a property list containing the function name, the forbidden symbols.
   given test cases, and hidden test cases.

   Additionally, it performs a side effect: it interns the 'asked-function' symbols 
   into the :SANDBOX package and makes them accessible to packages that use it.

   ### Arguments:
   * ASSESSMENT-DATA-FILE: A file path or designator containing the assessment 
     definitions (passed to 'get-assessment-test-case-data').
   * Q-LABELS-LIST: A list of keys (labels) corresponding to the questions to 
     be processed.

   ### Returns:
   * An association list where each element is of the form:
     (LABEL :ASKED-FUNCTION function-symbol 
            :FORBIDDEN-SYMBOLS (:PENALTY p :SYMBOLS (symbs))
            :GIVEN (test-cases) :HIDDEN (test-cases))"
  (let* ((assessment-data (get-assessment-test-case-data assessment-data-file))
         (testcase-data (mapcar (lambda (q)
                                  (list q
                                        :asked-function (second (assoc :asked-function (rest (assoc q assessment-data))))
                                        :forbidden-symbols (rest (assoc :forbidden-symbols (rest (assoc q assessment-data))))
                                        :given (second (assoc :given (rest (assoc q assessment-data))))
                                        :hidden (second (assoc :hidden (rest (assoc q assessment-data))))))
                                q-labels-list)))
    (mapc (lambda (d)
            (export (list (intern (symbol-name (getf (rest d) :asked-function)) :sandbox)) :sandbox))
          testcase-data)

    testcase-data))

(defun reset-sandbox-package ()
  (let ((pkg (find-package :sandbox)))
    (when pkg
      ;; Unuse everything to break links
      (unuse-package :sandbox pkg)
      ;; Delete and recreate or just unintern all symbols
      (do-symbols (s pkg)
        (unintern s pkg)))))

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

(defun derive-assessment-data-file (solution-file-path)
  "Derives the assessment data file path from the solution file path."
  (let* ((assessment-folder-name (car (last (pathname-directory solution-file-path))))
         (assessment-data-file (format nil "~a~a.data" *assessment-data-folder* assessment-folder-name)))
    assessment-data-file))

(defun critique-student-solution (sol)
  (let ((output (with-output-to-string (*standard-output*)
                  (lisp-critic:critique-file sol))))
    ;; We check if the output contains a hint (usually starts with a paren or keyword)
    ;; Adjust the search string based on what lisp-critic actually outputs
    (format t "~V@{~A~:*~}" 70 "+")
    (format t "~%### Style Feedback~%Below is your 'pretty-printed' code. ")
    (if (search "----" output :test #'char-equal)
        (format t "The suggestions below your code can ~%help you write more 'Lisp-y' solutions:~%~%~A" output)
        (format t "No idiomatic improvements suggested.~%~%~A" output))))


(defun chk-my-solution (a#)
  "Checks a student's solution file against examples.
   a#: String identifying the solution file, e.g., '~/lab01/q1.lisp'."
  (unless (probe-file a#)
    (error "~%!!!!!!!! Error: You saved your file in the wrong folder. Please save it in the specified folder. !!!!!!!!"))
  (let* ((assessment-data-file (derive-assessment-data-file a#))
         (q-label (intern (string-upcase (pathname-name a#)) :keyword))
         (assessment-test-case-data (process-assessment-test-case-data assessment-data-file (list q-label)))
         (q-testcase-data (rest (assoc q-label assessment-test-case-data)))
         (fname (getf q-testcase-data :asked-function))
         (forbidden-penalty (getf (getf q-testcase-data :forbidden-symbols) :penalty))
         (forbidden-symbs (getf (getf q-testcase-data :forbidden-symbols) :symbols))
         (given-testcases-metadata (getf q-testcase-data :given)))
    (with-package :sandbox
      ;; EVAL compiles/defines the test and runner in this package
      (eval given-testcases-metadata)
      ;; 2. Perform the grading
      ;; grade-student uses (intern (format ...)) to find the runner
      ;; in the *package* where it is called.
      (grade-student a# q-label fname 'given))
    (critique-student-solution a#)))
