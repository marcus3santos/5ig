(in-package :utils)

(defun read-form-and-intern (from package)
  (with-open-file (in from)
    (let ((*package* (find-package package))) 
      (read in))))

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

