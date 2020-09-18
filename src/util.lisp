(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)

;;;;;;;;;; Basics
(defun pick (lst)
  (nth (random (length lst)) lst))

(defun shuffle (sequence)
  (let ((seq (copy-list sequence)))
    (loop for i from (length sequence) downto 2
       do (rotatef (elt seq (random i))
                   (elt seq (1- i))))
    seq))

(defun take (n lst)
  (loop repeat n for itm in lst
     collect itm))


;;;;;;;;;; Game specifics
(defun payoff-matrix (cc-a cc-b cd-a cd-b dc-a dc-b dd-a dd-b)
  (let ((tbl {(cons :cooperate :cooperate) (list cc-a cc-b)
	      (cons :defect :cooperate) (list dc-a dc-b)
	      (cons :cooperate :defect) (list cd-a cd-b)
	      (cons :defect :defect) (list dd-a dd-b)}))
    (lambda (a b) (lookup tbl (cons a b)))))

(defun combine (matrix-a matrix-b)
  (lambda (a b)
    (mapcar
     #'+
     (funcall matrix-a a b)
     (funcall matrix-b a b))))
