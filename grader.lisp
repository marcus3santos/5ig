(in-package :grader)

(defun used-forbidden-function-p (q-func-name forbidden-functions student-forms)
  "Takes a function name Q-FUNC-NAME (a symbol), a list of function names FORBIDDEN-FUNCTIONS,
   and a list STUDENT-FORMS containing the forms present in the student's lisp program file. 
  Returns FUNC if Q-FUNC-NAME directly or indirectly calls FUNC and FUNC is in FORBIDDEN-FUNCTIONS."
  (let ((function-table (make-hash-table))
        (global-identifier-table (make-hash-table))
        (forbidden-found))
    ;; Build function and global identifier tables: name → body
    (dolist (form student-forms)
      (let ((form-wth-uniq-vars (gensymify form)))
        (cond ((and (consp form-wth-uniq-vars) (eq (first form-wth-uniq-vars) 'defun))
               (setf (gethash (second form-wth-uniq-vars) function-table)
                     (cdddr form-wth-uniq-vars)))
              ((and (consp form-wth-uniq-vars)
                    (or (eq (first form-wth-uniq-vars) 'defvar)
                        (eq (first form-wth-uniq-vars) 'defparameter)
                        (eq (first form-wth-uniq-vars) 'defconstant)))
               (setf (gethash (second form-wth-uniq-vars) global-identifier-table)
                     (cddr form-wth-uniq-vars))))))
    ;; Depth-first search
    (labels ((function-designator->symbol (fd)
               "Return a symbol if FD statically names a function, else NIL."
               (cond
                 ;; #'foo
                 ((and (consp fd) (eq (car fd) 'function))
                  (cadr fd))
                 ;; 'foo
                 ((and (consp fd) (eq (car fd) 'quote))
                  (cadr fd))
                 ;; bare symbol (e.g., (funcall foo ...))
                 ((symbolp fd)
                  fd)
                 (t nil)))
             (scan (aname fvisited gvvisited)
               (let ((name (function-designator->symbol aname)))
                 (cond
                   ;; direct forbidden call?
                   ((member name forbidden-functions)
                    (setf forbidden-found (list q-func-name name))
                    name)
                   ;; already visited? avoid infinite loops
                   ((or (member name fvisited)
                        (member name gvvisited)) ;; case of a weird naming cycle
                    nil)
                   ;; otherwise look at its body
                   (t
                    (let* ((fbody (gethash name function-table))
                           (fvisited (cons name fvisited))
                           (form (gethash name global-identifier-table))
                           (gvvisited (cons name gvvisited)))
                      (or (when fbody
                            (calls-forbidden-p fbody fvisited gvvisited))
                          (when form
                            (calls-forbidden-p form fvisited gvvisited))))))))
             (calls-forbidden-p (forms fvisited gvvisited)
               (cond
                 ((null forms)
                  nil)
                 ;; Direct call: (foo ....)
                 ((and (symbolp (first forms))
                       (scan (first forms) fvisited gvvisited))
                  (first forms))
                 ;; recur through subforms and rest
                 ((consp (first forms))
                  (or (calls-forbidden-p (first forms) fvisited gvvisited)
                      (calls-forbidden-p (rest forms) fvisited gvvisited)))
                 ((atom (first forms))
                  (calls-forbidden-p (rest forms) fvisited gvvisited))
                 (t nil))))
      ;; Start with q-func-name
      (scan q-func-name '() '())
      forbidden-found)))


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

(defun grade-student (student-file q-label fname kind)
  "Loads the student's program file in the sandbox,
   depending on KIND runs the given or hidden fiveam test cases, 
   and collects the results"
  (let ((runner-name (intern (format nil "RUN-~A-~A-~A-TEST" q-label fname kind) :sandbox)))
    ;; 1. Load student code
    
    (handler-case (with-package :sandbox
                    (load student-file))
      (error (c) (return-from grade-student 
                   (list :q-label q-label :error (format nil "Load Error: ~A" c)))))
    
    ;; 2. Execute the pre-compiled runner
    (let* ((raw-results (funcall runner-name))
           (summary (summarize-results q-label raw-results)))
      ;; 3. Log or Print the outcome

      (format t "~%### RESULTS~%### Question ~A: ~D/~D passed (~A%)~%~{~a~^~%~}~%" 
              q-label 
              (getf (getf summary :stats) :passed)
              (getf (getf summary :stats) :total)
              (getf summary :score)
              (mapcar (lambda (x) (getf x :reason)) (getf summary :feedback)))
      summary)))
