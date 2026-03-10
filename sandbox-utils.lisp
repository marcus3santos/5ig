(in-package :sandbox-utils)
#|
(defmacro is-safe (expression &key (timeout 1))
  (let ((result (gensym "RESULT"))
        (thread (gensym "THREAD"))
        (condition (gensym "CONDITION")))
    `(let* ((,result nil)
            (,thread (sb-thread:make-thread
                      (lambda ()
                        (handler-case
                            (locally (declare (optimize (safety 3)))
                              (setf ,result (fiveam:is ,expression)))
                          (sb-kernel::control-stack-exhausted ()
                            (setf ,result (fiveam:fail "Stack Overflow detected")))
                          (storage-condition ()
                            (setf ,result (fiveam:fail "Memory Exhausted")))
                          (error (,condition)
                            (setf ,result (fiveam:fail "Execution Error: ~A" ,condition))))))))
       
       ;; Join-thread returns the value of the thread, or NIL if it timeouts
       (let ((,condition (sb-thread:join-thread ,thread :timeout ,timeout :default :timeout-marker)))
         (if (eq ,condition :timeout-marker)
             (progn
               ;; The thread is stuck! Terminate it forcefully.
               (sb-thread:terminate-thread ,thread)
               (fiveam:fail "Timeout: Forced termination after ~A sec" ,timeout))
             ,result)))))

(defmacro is-safe (expression &key (timeout 1))
  (let ((c (gensym)))
    `(handler-case
         ;; We wrap the execution in an optimization block to ensure 
         ;; the timer interrupts are actually processed.
         (locally (declare (optimize (safety 3) (speed 0)))
           (sb-ext:with-timeout ,timeout
             (fiveam:is ,expression)))
       (sb-ext:timeout ()
         (fiveam:fail "Timeout: Execution exceeded ~A sec (likely infinite loop)" ,timeout))
       ;; SBCL specific stack overflow condition
       (sb-kernel::control-stack-exhausted ()
         (fiveam:fail "Stack Overflow: Infinite recursion detected"))
       ;; Fallback for other memory issues (Heap/GC)
       (storage-condition ()
         (fiveam:fail "Memory error: Storage exhausted"))
       ;; Catch-all for runtime errors
       (error (,c)
         (fiveam:fail "Execution Error: [~A] ~A" (type-of ,c) ,c)))))
|#

#|
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
|#

(defmacro is-safe (test-form &key (timeout 1))
  `(let* ((parent-thread (bt:current-thread))
          (result nil)
          (status :pending)
          (worker (bt:make-thread 
                   (lambda ()
                     (handler-case
                         (setf result ,test-form
                               status :success)
                       (storage-condition (c) 
                         (setf result c status :error))
                       (error (c) 
                         (setf result c status :error)))))))
     
     ;; Wait for the worker to finish, or time out
     (let ((start-time (get-internal-real-time)))
       (loop while (and (eq status :pending)
                        (< (/ (- (get-internal-real-time) start-time) 
                               internal-time-units-per-second) 
                           ,timeout))
             do (sleep 0.05)))

     (if (eq status :pending)
         ;; THE NUCLEAR STRIKE: Force the thread to stop NOW
         (progn
           (sb-thread:interrupt-thread 
            worker 
            (lambda () (error 'sb-ext:timeout)))
           (bt:destroy-thread worker)
           (fiveam:fail "TIMEOUT: Code entered an uninterruptible loop."))
         
         ;; Normal path
         (if (eq status :success)
             (fiveam:is (identity result))
             (fiveam:fail "CRASHED: ~A" result)))))

  
  


