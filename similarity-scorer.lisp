;;;; ============================================================
;;;; Robust Tree Edit Distance for Lisp forms
;;;; Counts node insert/delete costs correctly and handles atoms
;;;; ============================================================

(in-package :similarity)

;;;; Helpers: treat any Lisp form as a tree node (atoms are leaves)
(defun leafp (x) (not (consp x)))

(defun node-label (x) (if (consp x) (car x) x))
(defun node-children (x) (if (consp x) (cdr x) nil))

;;;; Count nodes in a tree (each atom or cons counts as 1 node)

(defun tree-size (tree)
  (cond ((null tree) 0)
        ((leafp tree) 1)
        ((listp (first tree))
         (+ (tree-size (first tree))
            (tree-size (rest tree))))
        (t (+ 1 (tree-size (rest tree))))))

;;;; Cost functions (unit relabel cost; insert/delete cost = number of nodes inserted/removed)
(defun cost-relabel (a b)
  (if (equal (node-label a) (node-label b)) 0 1))

(defun cost-delete-subtree (tree) (tree-size tree))
(defun cost-insert-subtree (tree) (tree-size tree))

;;;; Sequence edit (Levenshtein-like) where:
;;;; - deleting element i costs cost-delete-subtree(child-i)
;;;; - inserting element j costs cost-insert-subtree(child-j)
;;;; - replacing i->j costs tree-edit-distance(child-i, child-j)
(defun seq-edit-distance (xs ys)
  (let* ((m (length xs))
         (n (length ys))
         (d (make-array (list (1+ m) (1+ n)) :initial-element 0)))
    ;; initialize first column/row
    (setf (aref d 0 0) 0)
    (loop for i from 1 to m do
          (setf (aref d i 0) (+ (aref d (1- i) 0) (cost-delete-subtree (nth (1- i) xs)))))
    (loop for j from 1 to n do
          (setf (aref d 0 j) (+ (aref d 0 (1- j)) (cost-insert-subtree (nth (1- j) ys)))))
    ;; fill table
    (loop for i from 1 to m do
          (loop for j from 1 to n do
            (let* ((del (+ (aref d (1- i) j) (cost-delete-subtree (nth (1- i) xs))))
                   (ins (+ (aref d i (1- j)) (cost-insert-subtree (nth (1- j) ys))))
                   (rep (+ (aref d (1- i) (1- j)) (tree-edit-distance (nth (1- i) xs) (nth (1- j) ys))))
                   (best (min del ins rep)))
              (setf (aref d i j) best))))
    (aref d m n)))

;;;; Main tree-edit-distance

(defun type-order (x)
  "Assign an ordering value to a type."
  (cond
    ((numberp x) 0)
    ((stringp x) 1)
    ((symbolp x) 2)
    ((listp x) 3)
    (t 4)))

(defun form< (a b)
  "Compare two Common Lisp forms A and B.
Returns T if A is considered less than B."
  (cond
    ;; Numbers: compare numerically
    ((and (numberp a) (numberp b))
     (< a b))

    ;; Strings: compare lexicographically
    ((and (stringp a) (stringp b))
     (string< a b))

    ;; Symbols: compare by name lexicographically
    ((and (symbolp a) (symbolp b))
     (string< (symbol-name a) (symbol-name b)))

    ;; Lists: compare element by element recursively
    ((and (listp a) (listp b))
     (cond
       ((null a) (not (null b)))   ; empty list is less than non-empty
       ((null b) nil)           ; non-empty list is greater than empty
       (t (or (form< (first a) (first b))
              (and (equal (first a) (first b))
                   (form< (rest a) (rest b)))))))

    ;; Different types: define some arbitrary type ordering
    (t
     (< (type-order a) (type-order b)))))

(defun sort-form (f)
  (cond ((null f) f)
        ((atom f) f)
        ((and (listp f)
              (eq (first f) 'labels))
         `(,(first f)
           ,(sort (mapcar #'sort-form (second f)) #'form<)
           ,@(sort-form (cddr f))))
        ((and (listp f)
              (or (eq (car f) '*)
                  (eq (car f) '+)))
         (cons (car f)
               (sort (mapcar #'sort-form (cdr f)) #'form<)))
        (t (cons (sort-form (car f)) (mapcar #'sort-form (cdr f))))))


(defun tree-edit-distance (t1 t2)
  (cond
    ;; both leaves (atoms or small lists): treat as one replacement
    ((and (leafp t1) (leafp t2))
     (if (equal t1 t2) 0 1))
    ;; leaf vs non-leaf = insert/delete whole subtree
    ((leafp t1)
     (cost-insert-subtree t2))
    ((leafp t2)
     (cost-delete-subtree t1))
    ((and (listp t1) (listp t2))
     (+ (tree-edit-distance (first t1) (first t2))
        (seq-edit-distance (node-children t1)
                           (node-children t2))))
    ;; otherwise combine relabel + child sequence edit
    (t
     (let ((label-cost (cost-relabel t1 t2)))
       (+ label-cost
          (seq-edit-distance (node-children t1)
                             (node-children t2)))))))


(defun normalize (f)
  (sort-form (normalize-gensyms (gensymify f))))

(defun normalize-expand (f)
  (let* ((gensymified (gensymify f))
         (normed-gensyms (normalize-gensyms gensymified))
         (macroexpanded (macroexpand normed-gensyms)))
    (sort-form macroexpanded)))

#|
(defun similarity (qs ss &optional (lambda 0.1))
  (let* ((nqs (normalize-expand qs))
         (nss (normalize-expand ss))
         (distance (tree-edit-distance nqs nss))
         (size (tree-size nqs))
         ;; We set lambda so that a distance equal to the 
         ;; tree-size results in a ~36% score (e^-1).
         (dynamic-lambda (/ 1.0 size)))
    ;; Formula: e^(-lambda * distance)
    (exp (* (- dynamic-lambda) distance))))

(defun similarity (qs ss) 
  (let* ((nqs (normalize-expand qs)) 
         (nss (normalize-expand ss)) 
         (distance (tree-edit-distance nqs nss)) 
         ;; Measure both trees to find the maximum possible "cost"
         (size-qs (tree-size nqs))
         (size-ss (tree-size nss))
         (max-size (max size-qs size-ss)))
    (if (zerop max-size)
        1.0 ;; Two empty forms are 100% similar
        (max 0.0 (- 1 (/ (float distance) max-size))))))


(defun similarity (qs ss) 
  (let* ((nqs (normalize-expand qs)) 
         (nss (normalize-expand ss)) 
         (distance (tree-edit-distance nqs nss)) 
         ;; Measure both trees to find the maximum possible "cost"
         (size-qs (tree-size nqs))
         (size-ss (tree-size nss))
         (avg-size (/ (+ size-qs size-ss) 2.0)))
    (if (zerop avg-size)
        1.0 ;; Two empty forms are 100% similar
        (max 0.0 (- 1 (/ distance avg-size))))))
|#

(defun similarity (qs ss)
  "Calculates a similarity score between 0.0 and 1.0 for two Lisp forms.
   
   This function uses a normalized Tree Edit Distance approach based on the 
   Sørensen-Dice coefficient. It is specifically designed to be more lenient 
   than a simple linear ratio of distance to instructor-tree-size.

   ALGORITHM:
   1. Normalizes and expands both forms (qs = instructor, ss = student).
   2. Calculates the tree edit distance (D).
   3. Computes the total complexity as the sum of the sizes of both trees (S1 + S2).
   4. Applies the formula: 1.0 - (2 * D / (S1 + S2)).
   5. Clips the result at 0.0 to prevent negative scores.

   LENIENCY LOGIC:
   Unlike (1 - D/S1), which hits zero as soon as the distance equals the 
   instructor's code size, this version allows for 'structural drift.' 
   A student can write significantly more code (increasing S2) without 
   bottoming out the score, provided the distance doesn't grow faster 
   than the total complexity.

   EDGE CASES:
   - If both forms are empty, returns 1.0.
   - If one form is empty (e.g., NIL) and the other is a substantial 
     function, the score will approach 0.0.
   - Returns a FLOAT for precision."
  (let* ((nqs (normalize-expand qs))
         (nss (normalize-expand ss))
         (distance (tree-edit-distance nqs nss))
         (s1 (tree-size nqs))
         (s2 (tree-size nss))
         (total (+ s1 s2)))
    (cond 
      ((zerop total) 1.0)
      (t (max 0.0 (float (- 1 (/ (* 2.0 distance) total))))))))


(defun get-relevant-code (program call-graph)
  (let ((form-map (make-hash-table :test 'eq))
        (filtered-program
          (remove-if-not (lambda (f)
                           (and (consp f)
                                (member (car f) '(let let* defun defconstant defparameter defvar))))
                         program)))
    ;; Index the program once: O(N)
    (dolist (form filtered-program)
      (setf (gethash (second form) form-map) form))
    
    (append
     ;; Filter constants: O(N)
     (remove-if-not (lambda (f)
                      (and (consp f)
                           (member (car f) '(defconstant defparameter defvar))))
                    filtered-program)
     ;; Map call-graph nodes to forms: O(M)
     (loop for (node-name has-target) in call-graph
           when (and has-target (gethash node-name form-map))
             collect (gethash node-name form-map)))))

(defun embed-helpers (main-func-name program)
  (let* ((globals (loop for form in program
                        when (and  (consp form) (member (car form) '(defconstant defvar 'defparameter)))
                          collect form))
         (helpers (loop for form in program
                        when (and (consp form)
                                  (eq (first form) 'defun)
                                  (not (eq (second form) main-func-name)))
                          collect form))
         (main-def (first (member main-func-name program :key #'second)))
         (main-lamblist (third main-def))
         (main-bdy (cdddr main-def)))
    (if (and main-def helpers)
        (append  globals
                 (list`(defun ,main-func-name ,main-lamblist
                         (labels ,(mapcar #'rest helpers)
                           ,@main-bdy))))
        program)))

(defun filter-atoms (program)
  (cond ((null program) program)
        ((atom (car program)) (filter-atoms (cdr program)))
        (t (cons (car program) (filter-atoms (cdr program))))))

(defun closest-solution-to-the-profs-solution (std-sol prof-sols)
  (let ((dist most-positive-fixnum)
        res)
    (mapc (lambda (s)
            (let ((temp (tree-edit-distance (normalize-expand std-sol)
                                            (normalize-expand s))))
              (when (< temp dist)
                (setf dist temp)
                (setf res (normalize s)))))
          prof-sols)
    res))

(defun prepare-solution-for-comparison (target-func program)
  "Transforms a raw Lisp program into a standardized, self-contained functional 
   structure optimized for tree-edit-distance similarity analysis.

   The transformation pipeline consists of:
   1. Atom Filtering: Removes top-level atoms to ensure only valid S-expressions 
      remain.
   2. Call Graph Generation: Maps the functional dependencies starting from 
      TARGET-FUNC.
   3. Code Extraction: Isolates only the functions and global definitions (constants/vars) 
      identified in the call graph.
   4. Helper Embedding: Refactors the code by moving helper functions into a 
      local LABELS block inside the TARGET-FUNC definition, creating a single 
      unified tree structure.

   Returns a list of forms where the primary function encapsulates its 
   dependencies."
  (let* ((clean-code (filter-atoms program))
         (cg         (get-call-graph target-func clean-code))
         (relevant   (get-relevant-code clean-code cg)))
    (embed-helpers target-func relevant)))

(defun calc-similarity-score (target-func raw-student-solution instructor-solutions)
  "Calculates the maximum similarity between a student's implementation of TARGET-FUNC 
   and a list of instructor solutions.

   The function follows these steps:
   1. Prepares the student's solution by filtering atoms, generating a call graph, 
      isolating relevant code, and embedding helper functions.
   2. Iterates through the INSTRUCTOR-SOLUTIONS, filtering for those that contain 
      the TARGET-FUNC in their call graph.
   3. Prepares each valid instructor solution for comparison using the same 
      transformation pipeline as the student's code.
   4. Finds the best match by iterating through prepared instructor solutions 
      to find the highest similarity score.
   
   Returns a list containing the best similarity score, the normalized instructor 
   solution, and the normalized student solution. If no similarity is found (score is 0), 
   it returns the result of the closest identified instructor solution."
  (let* ((student-prepared (prepare-solution-for-comparison target-func raw-student-solution))
         (instructor-prepared-list
           (loop for s in instructor-solutions
                 for data = (rest s)
                 ;; Use a simple tree-search instead of flatten
                 when (tree-find target-func (get-call-graph target-func data))
                   collect (prepare-solution-for-comparison target-func data)))
         (best-match (find-best-similarity student-prepared instructor-prepared-list)))
    (destructuring-bind (score instructor-solution student-solution)
        (if (zerop (first best-match))
            (list 0.0 
                  (closest-solution-to-the-profs-solution student-prepared instructor-prepared-list) 
                  (third best-match))
            best-match)
      (list :score score
            :instructor-solution instructor-solution
            :student-solution student-solution))))

;; Utility for the above
(defun tree-find (item tree)
  "Recursively search for item in a nested list structure."
  (cond ((equal item tree) t)
        ((atom tree) nil)
        (t (some (lambda (sub) (tree-find item sub)) tree))))

(defun find-best-similarity (student-prepared instructor-prepared-list)
  "Iterates through instructor solutions to find the highest similarity score."
  (loop for instructor-sol in instructor-prepared-list
        for score = (similarity instructor-sol student-prepared)
        maximizing score into max-score
        when (= score max-score)
          do (setf best-result (list score 
                                     (normalize instructor-sol) 
                                     (normalize student-prepared)))
        finally (return (or best-result (list 0.0 nil nil)))))
