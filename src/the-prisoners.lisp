;;;; src/the-prisoners.lisp
(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)

;;;;;;;;;; Basics
(defun c? (res) (eq :cooperate res))
(defun d? (res) (eq :defect res))

;;;;;;;;;; Scenarios
(defparameter dilemma
  (payoff-matrix
   1 1  -1 3
   3 -1  -2 -2))

(defparameter stag-hunt
  (payoff-matrix
   3 3  0 1
   1 0  1 1))

(defparameter trade
  (payoff-matrix
   3 3  0 0
   0 0  0 0))

(defparameter theft
  (payoff-matrix
   0 0   -3 3
   3 -3   -1 -1))

(defparameter trap
  (payoff-matrix
   -3 -3  1 1
    1  1  0 0))

(defparameter mutual-prediction
  (payoff-matrix
   1  1  -1 -1
  -1 -1   1  1))

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
		       (if (c? prev)
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
     :update (lambda (action) (when (d? action) (setf plan :defect)))
     :strategy (lambda () plan)
     :reset (lambda () (setf plan nil))}))

;;;;;;;;;; Gameplay
(defun server! () :todo)

(defun main ()
  (repl! (mk-adventure)))

(defun build! ()
  (sb-ext:save-lisp-and-die
   (or
    #+win32#p"the-prisoners.exe"
    #+darwin#p"the-prisoners.bin"
    #+linux#p"the-prisoners"
    (error "Unsupported build platform. Got ~A" *features*))
   :toplevel #'main :executable t))
