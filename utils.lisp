(in-package :utils)

(defun safe-read-student-code (file-path)
  "Reads a student's file in a restricted environment to prevent 
malicious reader macros or resource exhaustion."
  (with-open-file (in file-path)
    (let ((*read-eval* nil)           ;; Prevents #. execution
          (*read-base* 10)            ;; Ensures standard decimal reading
          (*package* (find-package :sandbox))
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
                      ((null item) nil)
                      ((atom item) nil)
                      ((consp item)
                       (let ((head (car item)))
                         (cond
                           ((eq head 'quote) nil)

                           ;; Handle 'FUNCTION' / #': Check if shadowed
                           ((eq head 'function)
                            (let ((fn-name (cadr item)))
                              (when (and (symbolp fn-name) (not (member fn-name local-env)))
                                (pushnew fn-name calls))))

                           ;; NEW: Handle FUNCALL, APPLY, and Mapping functions
                           ;; We look at the first argument to these functions
                           ((member head '(funcall apply mapcar mapc mapcan maplist))
                            (pushnew head calls)
                            (let ((arg (second item)))
                              (cond 
                                ;; Case: (funcall 'reverse ...)
                                ((and (consp arg) (eq (first arg) 'quote))
                                 (pushnew (second arg) calls))
                                ;; Case: (funcall #'reverse ...)
                                ((and (consp arg) (eq (first arg) 'function))
                                 (pushnew (second arg) calls)))
                            ;; Continue scanning arguments for other calls
                            (mapc (lambda (x) (scan x local-env)) (cdr item)))

                           ((member head '(flet labels))
                            (let* ((definitions (second item))
                                   (new-locals (mapcar #'car definitions))
                                   (extended-env (append new-locals local-env))
                                   (body (cddr item)))
                              (if (eq head 'labels)
                                  (progn (dolist (d definitions) (scan (cdr d) extended-env))
                                         (scan body extended-env))
                                  (progn (dolist (d definitions) (scan (cdr d) local-env))
                                         (scan body extended-env)))))

                           ((and (symbolp head) 
                                 (not (member head ignored-symbols))
                                 (not (member head local-env)))
                            (pushnew head calls)
                            (mapc (lambda (x) (scan x local-env)) (cdr item)))

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
