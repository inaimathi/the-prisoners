(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)

(defun display-repl-choice (adventure)
  (format
   t "~a/~a:"
   (lookup adventure :cooperate)
   (lookup adventure :defect))
  (force-output))

(defun get-repl-choice (adventure)
  (let* ((responses (mapcar #'string-downcase (list (lookup adventure :cooperate) (lookup adventure :defect))))
	 (r-map {(string-downcase (lookup adventure :cooperate)) :cooperate
		 (string-downcase (lookup adventure :defect)) :defect})
	 (by-pref nil))
    (display-repl-choice adventure)
    (let ((resp (read-line)))
      (loop until (setf by-pref (get-by-prefix responses (string-downcase resp)))
	 do (display-repl-choice adventure)
	 do (setf resp (read-line))))
    (lookup r-map by-pref)))

(defun play-scenario! (player adventure)
  (format t "~%==============================~%")
  (format t "~a, score: ~a~%~%" (lookup player :name) (lookup player :score))
  (format t "~a~%~%" (lookup adventure :description))
  (cond ((contains? adventure :continue)
	 (let* ((scenario (lookup adventure :scenario))
		(prisoner (lookup adventure :prisoner))
		(choice (get-repl-choice adventure))
		(their-choice (play prisoner))
		(next (continue! adventure choice their-choice))
		(score (if-let (s (lookup next :score)) (prisoner-incf! player :score :by s))))
	   (prisoner-incf! player :encounters)
	   (format t "~%~a~%" (!!scenario-aftermath player prisoner scenario choice their-choice))
	   (play-scenario!
	    (insert player :score score)
	    (if (> score 0)
		next
		{:description (!!death player prisoner scenario)}))))
	((== (lookup adventure :ending) :success)
	 (prisoner-incf! player :adventures)
	 nil)))

(defun to-adventure! (adventure)
  (format t "~%           ▄▄▄█████▓ ██░ ██ ▓████
           ▓  ██▒ ▓▒▓██░ ██▒▓█  ▀
           ▒ ▓██░ ▒░▒██▀▀██░▒███
           ░ ▓██▓ ░ ░▓█ ░██ ▒█  ▄
             ▒██▒ ░ ░█▓ ░██▓░████▒
 ██▓███   ██▀███   ██▓  ██████  ▒█████   ███▄    █ ▓█████  ██▀███    █████
▓██░  ██▒▓██ ▒ ██▒▓██▒▒██    ▒ ▒██▒  ██▒ ██ ▀█   █ ▓█   ▀ ▓██ ▒ ██▒▒██
▓██░ ██▓▒▓██ ░▄█ ▒▒██▒░ ▓██▄   ▒██░  ██▒▓██  ▀█ ██▒▒███   ▓██ ░▄█ ▒░ ▓██▄
▒██▄█▓▒ ▒▒██▀▀█▄  ░██░  ▒   ██▒▒██   ██░▓██▒  ▐▌██▒▒▓█  ▄ ▒██▀▀█▄    ▒   ██
▒██▒ ░  ░░██▓ ▒██▒░██░▒██████▒▒░ ████▓▒░▒██░   ▓██░░▒████▒░██▓ ▒██▒▒██████▒
▒▓▒░ ░  ░░ ▒▓ ░▒▓░░▓  ▒ ▒▓▒ ▒ ░░ ▒░▒░▒░ ░ ▒░   ▒ ▒ ░░ ▒░ ░░ ▒▓ ░▒▓░▒ ▒▓▒ ▒
            ░      ░        ░      ░ ░           ░    ░  ░   ░           ░~%~%")

  (let ((prisoner (first-living-prisoner))
	(game-levels))
    (cond ((or (and prisoner (> (lookup prisoner :score) 10))
	       (== :strategies (lookup (game-settings) :game-stage)))
	   (level-up! :strategies)
	   (format t "~%You've done well. You can keep going. Or...~%")
	   (format t "I can let you shape the minds of your prisoners.~%")
	   (format t "If this is what you want, give me a function of type~%")
	   (format t "     Scenario -> (or :cooperate :defect)~%")
	   (format t "Otherwise, give me nothing")
	   (eval (read-into-package :logos)))
	  (prisoner
	   (format t "~%Welcome back, ~a. Prepare for your next adventure.~%~%" (lookup prisoner :name))
	   (play-scenario! prisoner adventure))
	  (t
	   (format t "~% What is your name?: ")
	   (let* ((player-name (read-line))
		  (prisoner (store-prisoner! {:name player-name :score 1 :adventures 0 :encounters 0})))
	     (format t "~%Welcome, ~a~%~%" player-name)
	     (format t "You may roam this world to your heart's content,~%but when you encounter your opposite number,~%know that you are the prisoners of your history.")
	     (play-scenario! prisoner adventure))))))

(defun repl! ()
  (to-adventure! (mk-adventure)))
