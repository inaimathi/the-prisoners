;;;; src/the-prisoners.lisp
(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)

;;;;;;;;;; Basics
(defun coop? (res) (eq :cooperate res))
(defun defe? (res) (eq :defect res))

;;;;;;;;;; Scenarios
(defparameter dilemma
  (payoff-matrix
   3 3  1 5
   5 1  0 0))

(defparameter stag-hunt
  (payoff-matrix
   3 3  0 1
   1 0  1 1))

(defparameter trade
  (payoff-matrix
   3 3  0 0
   0 0  0 0))

(defparameter trap
  (payoff-matrix
   -3 -3  2 2
    2  2  0 0))

(defparameter mutual-prediction
  (payoff-matrix
   3 3  0 0
   0 0  3 3))

;;;;;;;;;; Games
(defun play! (game &rest players)
  (apply game players))

(defun matches (elems &key (mirror? t))
  (loop for (a . rest) on elems while rest
      if mirror? collect (cons a a)
      append (loop for b in rest collect (cons a b))))

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

(defun one-time (player-a player-b &key (scenario dilemma))
  (let ((a (play player-a))
	(b (play player-b)))
    (update! player-a b)
    (update! player-b a)
    (funcall scenario a b)))

(defun iterated (&key (iterations 10))
  (lambda (player-a player-b)
    (loop repeat iterations
       for (a b) = (one-time player-a player-b)
       sum a into a-sum sum b into b-sum
       finally (return (list a-sum b-sum)))))

;;;;;;;;;; Players
(defun update! (player action)
  (if-let (upd (lookup player :update))
    (funcall upd action))
  player)

(defun play (player)
  (funcall (lookup player :strategy)))


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
    (lambda () (nth (random total) moves))
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

;;;;;;;;;; Gameplay
(defun get-by-prefix (lst prefix)
  (let ((l (length prefix)))
    (loop for elem in lst
       when (and (>= (length elem) l)
		 (== (subseq elem 0 l) prefix))
       do (return elem))))

(defun repl! (adventure)
  (format t "~%~%~a~%~%" (lookup adventure :description))
  (when (contains? adventure :continue)
    (let* ((responses (list (lookup adventure :cooperate) (lookup adventure :defect)))
	   (resp-map {(lookup adventure :cooperate) :cooperate
		      (lookup adventure :defect) :defect})
	   (response ""))
      (loop until (and (symbolp response)
		       (get-by-prefix
			responses
			(string-downcase (symbol-name response))))
	 do (format
	     t "~a/~a:"
	     (string-capitalize (lookup adventure :cooperate))
	     (string-capitalize (lookup adventure :defect)))
	 do (setf response (read)))
      (let ((choice (lookup resp-map (get-by-prefix responses (string-downcase (symbol-name response))))))
	(repl! (funcall (lookup adventure :continue) choice))))))

(defun server! () :todo)
