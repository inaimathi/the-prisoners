;;;; src/the-prisoners.lisp
(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)

;;;;;;;;;; Basics
(defun coop? (res) (eq :cooperate res))
(defun defe? (res) (eq :defect res))

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

(defun play! (game &rest players)
  (apply game players))

(defun matches (elems &key (mirror? t))
  (loop for (a . rest) on elems while rest
      if mirror? collect (cons a a)
      append (loop for b in rest collect (cons a b))))

(defun winner (results)
  (let ((max nil)
	(score nil))
    (loop for (k . v) in (as-list results)
       do (if (or (not score) (> v score))
	      (setf score v
		    max (cons k v))))
    max))
;(all-against-all! (iterated) (matches (list (cooperator) (defector) (gambler) (gambler :cooperate 5) (gambler :defect 5) (polo) (dantes) (robin))))

(defun all-against-all! (game matches)
  (reduce
   (lambda (memo res)
     (reduce
      (lambda (memo pair)
	(merge-with #'+ memo {(car pair) (cdr pair)}))
      res :initial-value memo))
   (loop for (a . b) in matches
      collect (let ((res (play! game a b)))
		(list (cons (lookup a :name) (first res)) (cons (lookup b :name) (second res)))))
   :initial-value {}))

;;;;;;;;;; Games
(defun one-time (player-a player-b)
  (let ((a (funcall (lookup player-a :strategy)))
	(b (funcall (lookup player-b :strategy))))
    (if-let (update (lookup player-a :update))
      (funcall update b))
    (if-let (update (lookup player-b :update))
      (funcall update a))
    (cond ((and (coop? a) (coop? b))
	   (list 3 3))
	  ((and (coop? a) (defe? b))
	   (list 1 5))
	  ((and (defe? a) (coop? b))
	   (list 5 1))
	  (t
	   (list 0 0)))))

(defun iterated (&key (iterations 10))
  (lambda (player-a player-b)
    (loop repeat iterations
       for (a b) = (one-time player-a player-b)
       sum a into a-sum sum b into b-sum
       finally (progn
		 (reset! player-a)
		 (reset! player-b)
		 (return (list a-sum b-sum))))))

;;;;;;;;;; Players
(defun reset! (player)
  (if-let (rst (lookup player :reset))
    (funcall rst))
  player)

(defun defector ()
  {:name :defector :strategy (lambda () :defect)})

(defun cooperator ()
  {:name :cooperator :strategy (lambda () :cooperate)})

(defun gambler (&key (cooperate 1) (defect 1))
  (let ((total (+ cooperate defect))
	(moves (concatenate
		'list
		(loop repeat cooperate collect :cooperate)
		(loop repeat defect collect :defect))))
    {:name (intern (format nil "GAMBLER~a/~a" cooperate defect) :keyword)
	   :strategy (lambda () (nth (random total) moves))}))

(defun robin ()
  (let ((prev :cooperate))
    {:name :robin
	   :strategy (lambda ()
		       (if (coop? prev)
			   (setf prev :defect)
			   (setf prev :cooperate)))}))

(defun polo ()
  (let ((prev nil))
    {:name :polo
     :update (lambda (opponent-action) (setf prev opponent-action))
     :strategy (lambda () (or prev :cooperate))
     :reset (lambda () (setf prev nil))}))

(defun dantes ()
  (let ((plan :cooperate))
    {:name :dantes
     :update (lambda (action) (when (defe? action) (setf plan :defect)))
     :strategy (lambda () plan)
     :reset (lambda () (setf prev nil))}))

(defun birth ()
  (funcall
   (pick
    (list
     #'dantes #'polo #'robin #'cooperator #'defector #'gambler
     (lambda () (gambler :cooperate 5))
     (lambda () (gambler :defect 5))))))
