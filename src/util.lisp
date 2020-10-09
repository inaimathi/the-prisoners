(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)

;;;;;;;;;; Basics
(let ((ct 0))
  (defun log! (&rest message)
    (format t "-=~3d= ~s~%" (incf ct) message)))

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

(defun collapse (matrix a b)
  (funcall matrix a b))

(defun combine (matrix-a matrix-b)
  (lambda (a b)
    (mapcar
     #'+
     (collapse matrix-a a b)
     (collapse matrix-b a b))))

(defun get-by-prefix (lst prefix)
  (let ((l (length prefix)))
    (loop for elem in lst
       when (and (>= (length elem) l)
		 (== (subseq elem 0 l) prefix))
       do (return elem))))

(defun repackage-symbol (package thing)
  (if (and (symbolp thing)
	   (not (keywordp thing))
	   (not (== package (symbol-package thing))))
      (intern (symbol-name thing) package)
      thing))

(defun eval-into-package (package)
  (let ((*package* (find-package package))
	(form (read)))
    (walk
     (fn (k &optional (v nil v-supplied?))
	 (if v-supplied?
	     (cons (repackage-symbol *package* k)
		   (repackage-symbol *package* v))
	     (repackage-symbol *package* k)))
     form)))
