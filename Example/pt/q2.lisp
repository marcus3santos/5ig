(defun square-evens (a)
  (cond ((null a) a)
        ((evenp (car a)) (cons (* (car a) (car a)) (square-evens (cdr a))))
        (t (square-evens (cdr a)))))
