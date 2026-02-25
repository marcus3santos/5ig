(in-package :utils)

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

