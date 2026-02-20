(in-package :testing-runtime)

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
