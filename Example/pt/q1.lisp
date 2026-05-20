;; q1.lisp

(DEFUN DOT-PRODUCT-LIST (G1 G2)
  (LABELS ((G3 (G4 G5)
             (* G4 G4)))
    (MAPCAR #'G3 G1 G2)))
