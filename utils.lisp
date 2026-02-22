(in-package :utils)

(defun read-form-and-intern (from package)
  (with-open-file (in from)
    (let ((*package* (find-package package))) 
      (read in))))
