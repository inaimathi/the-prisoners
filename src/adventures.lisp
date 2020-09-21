(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)

(defparameter ending
  {:description "You have come to the end of your long, perilous journey."})

(defun random-encounter ()
  (let ((scenario (pick (list trade theft mutual-prediction stag-hunt trap dilemma)))
	(prisoner (pick (list (polo) (defector) (gambler) (dantes) (robin)))))
    (insert
     (!!scenario-choices {} prisoner scenario)
     :description (!!scenario-description {} prisoner scenario)
     :prisoner prisoner :scenario scenario)))

(defun wrap-encounter (adventure encounter)
  (insert
   encounter
   :continue
   (lambda (choice)
     (let* ((them (lookup encounter :prisoner))
	    (their-choice (play them))
	    (res (funcall (lookup encounter :scenario) choice their-choice)))
       (update! them choice)
       (insert adventure :score (first res))))))

(defun mk-adventure (&key (encounters 5))
  (let ((adventure ending))
    (loop repeat encounters
       do (setf adventure (wrap-encounter adventure (random-encounter))))
    adventure))
