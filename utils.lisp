(in-package :utils)

(defparameter *salt* "A9f4XqZb!")

(defun simple-hash (string)
  "Compute a deterministic integer hash of STRING (portable Common Lisp)."
  (let ((hash 0))
    (loop for c across string
          do (setf hash (mod (+ (* hash 31) (char-code c)) #xffffffff)))
    hash))

(defun hash-std-id (stdid)
  (format nil "~A" (simple-hash (format nil "~A~A" stdid *salt*))))

(defun my-feedback-file (stdid)
  (format nil "~A.txt" (hash-std-id stdid)))

(defun safe-read-student-code (file-path)
  "Reads a student's file in a restricted environment to prevent 
malicious reader macros or resource exhaustion."
  (with-open-file (in file-path)
    (let ((*read-eval* nil) ;; Prevents #. execution
          (*read-base* 10)  ;; Ensures standard decimal reading
          (*package* (find-package (find-package :tester)))
          ;; Prevent circularity bombs
          (*read-circle* nil))        
      (handler-case
          ;; Reading the whole file as a list of forms
          (loop for form = (read in nil :eof)
                until (eq form :eof)
                collect form)
        (error (c)
          (values nil (format nil "Reader Error: ~A" c)))))))

(defun add-prefix-to-symbol-in-form (form symb package-designator)
  "Adds the PACKAGE-DESIGNATOR to the name of symb in form."
  (cond ((consp form)
         (mapcar #'(lambda (x) (add-prefix-to-symbol-in-form x symb package-designator)) form))
        ((keyword-symbol-p form) form)
        ((symbolp form)
         (if (eq form symb)
             (intern (symbol-name form) (find-package package-designator))
             form))
        (t form)))

(defmacro with-package (package &rest body)
  `(let ((*package* (find-package ,package)))
     ,@body))

(defun get-call-graph (target-func program)
  (let ((seen '())
        (graph '())
        (defun-table (make-hash-table :test 'eq))
        (ignored-symbols '(let let* symbol-macrolet if cond case ecase 
                           ccase when unless loop do do* dolist dotimes quote go throw)))

    (dolist (form program)
      (when (and (consp form) (eq (first form) 'defun))
        (setf (gethash (second form) defun-table) (cdddr form))))

    (labels 
        ((extract-calls (body)
           (let ((calls '()))
             (labels 
                 ((scan (item local-env)
                    (cond 
                      ((atom item) nil) ;; Atoms (symbols like A, B) are never calls
                      ((consp item)
                       (let ((head (car item)))
                         (cond
                           ((eq head 'quote) nil)

                           ((eq head 'function)
                            (let ((fn-name (cadr item)))
                              (when (and (symbolp fn-name) (not (member fn-name local-env)))
                                (pushnew fn-name calls))))

                           ((member head '(flet labels))
                            (let* ((definitions (second item))
                                   (new-locals (mapcar #'car definitions))
                                   (extended-env (append new-locals local-env))
                                   (inner-body (cddr item)))
                              ;; Scan the bodies of the local functions
                              (dolist (d definitions) 
                                (scan (cddr d) (if (eq head 'labels) extended-env local-env)))
                              ;; Scan the main body of the flet/labels
                              (scan inner-body extended-env)))

                           ;; If the head is a symbol, it's a potential function call
                           ((and (symbolp head) 
                                 (not (member head ignored-symbols))
                                 (not (member head local-env)))
                            (pushnew head calls)
                            ;; Now scan the arguments (the rest of the list)
                            (mapc (lambda (x) (scan x local-env)) (cdr item)))

                           ;; If the head is another list (like a lambda or a nested call)
                           ;; scan the head AND the rest of the list
                           (t (mapc (lambda (x) (scan x local-env)) item))))))))
               (scan body '())
               calls)))

         (build-graph (func)
           (unless (member func seen)
             (push func seen)
             (let* ((body (gethash func defun-table))
                    (calls (extract-calls body)))
               (push (cons func (list calls)) graph)
               (dolist (child calls)
                 (when (gethash child defun-table)
                   (build-graph child)))))))

      (build-graph target-func)
      graph)))




