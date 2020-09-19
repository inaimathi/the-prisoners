(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)

(defparameter ending
  {:description "You have come to the end of your long, perilous journey."})

(defun random-scenario ()
  (pick
   (list
    {:description
     "A stranger approaches. \"I see you have baubles. Would you like to trade, that we both may enrich ourselves?\""
     :cooperate "Accept" :defect "Refuse" :prisoner (polo) :scenario trade}
    {:description
     "A muscled street thug approachs, knife drawn. \"Yer money or yer life, fop!\""
     :cooperate "surrender" :defect "run" :prisoner (defector) :scenario theft}
    {:description
     "As you walk through an expansive market square, a gambler motions you over. \"Fancy your chances at evens or odds?"
     :cooperate "Evens!" :defect "Odds!" :prisoner (gambler) :scenario mutual-prediction}
    {:description
     "A hunter approaches you in a forest clearing. \"Hallo there, young one. Would you help me hunt a deer? I've had enough hares for now, but I promise we'll eat well if we work together!\""
     :cooperate "<Nocks bow>" :defect "Rather go my own way" :prisoner (dantes) :scenario stag-hunt}
    {:description
     "\"Hey follow me into this bear trap!\""
     :cooperate "Sure; I've grown tired of living" :defect "No. No, I'd rather not."
     :prisoner (robin) :scenario trap}
    {:description
     "You see a merchant ahead of you, paying little attention to his overfull coin purse. You could cut it and run."
     :cooperate "It's too tempting" :defect "No"
     :prisoner (dantes) :scenario theft}
    {:description
     "At the end of your travails with your co-conspirator, you get to the treasure first and can pocket some if you want."
     :cooperate "Take it" :defect "Split fairly"
     :prisoner (gambler :defect 5) :scenario dilemma})))

(defun wrap-scenario (adventure scenario)
  (insert
   scenario
   :continue
   (lambda (choice)
     (let* ((them (lookup scenario :prisoner))
	    (their-choice (play them))
	    (res (funcall (lookup scenario :scenario) choice their-choice)))
       (update! them choice)
       (insert adventure :score (first res))))))

(defun mk-adventure (&key (scenarios 5))
  (let ((adventure ending))
    (loop repeat scenarios
       do (setf adventure (wrap-scenario adventure (random-scenario))))
    adventure))
