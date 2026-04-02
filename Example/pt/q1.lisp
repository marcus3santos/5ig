;; q1.lisp


(defun dot-product-list (a b)
  (let ((res '()))
    (dotimes (i (length a))
      (setf res (cons (* (first a) (first b)) res)))
    res))
