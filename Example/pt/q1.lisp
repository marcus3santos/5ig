;; q1.lisp

#|
(defun dot-product-list (a b)
  (labels ((multiply-records (a b)
             (* a b)))
    (mapcar #'multiply-records a b)))
|#

(defun multiply-records (a b)
  (format t "~a" a)
  (* a (multiply-records a b))
  (* a b)
  )

(defun dot-product-list (a b)
  (mapcar #'multiply-records a b))
