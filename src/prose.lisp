(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)

(defun !!death (player prisoner scenario)
  (declare (ignore player prisoner scenario))
  "You bleed out on the cobblestones of the city market.
Your hopes of fortune and glory fading along with sensation.
As a consolation, you have been freed.")

(defun !!scenario-description (player prisoner scenario)
  (declare (ignore player))
  (lookup
   {trade
    "A stranger approaches. \"I see you have baubles. Would you like to trade, that we both may enrich ourselves?\""

    theft
    (lookup
     {:defector "A muscled street thug approachs, knife drawn. \"Yer money or yer life, fop!\""}
     (lookup prisoner :name)
     :default "You see a merchant ahead of you, paying little attention to his overfull coin purse. You could cut it and run.")

    mutual-prediction
    "As you walk through an expansive market square, a gambler motions you over. \"Fancy your chances at evens or odds?"

    stag-hunt
    "A hunter approaches you in a forest clearing. \"Hallo there, young one. Would you help me hunt a deer? I've had enough hares for now, but I promise we'll eat well if we work together!\""

    trap
    "\"Hey follow me into this bear trap!\""

    dilemma
    "At the end of your travails with your co-conspirator, you get to the treasure first and can pocket some if you want."}
   scenario))

(defun !!scenario-choices (player prisoner scenario)
  (declare (ignore player))
  (lookup
   {trade
    {:cooperate "Accept" :defect "Refuse"}

    theft
    (lookup
     {:defector {:cooperate "Surrender" :defect "Run"}}
     (lookup prisoner :name)
     :default {:cooperate "It's too tempting" :defect "No"})

    mutual-prediction
    {:cooperate "Evens!" :defect "Odds!"}

    stag-hunt
    {:cooperate "<Nocks bow>" :defect "Rather go my own way"}

    trap
    {:cooperate "Sure; I've grown tired of living" :defect "No. No, I'd rather not."}

    dilemma
    {:cooperate  "Split fairly" :defect "Take it"}}
   scenario))

(defun !!scenario-aftermath (player prisoner scenario player-choice prisoner-choice)
  (declare (ignore player prisoner scenario player-choice prisoner-choice))
  "")
