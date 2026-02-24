;;;; 5ig.lisp

(in-package #:5ig)

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
