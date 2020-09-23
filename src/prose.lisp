(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)

(defparameter *GRAMMARS*
  {:proper-name
   {:root
    '((:starter :link :ender) (:starter :partition :ender)
      (:starter :partition :link :ender) (:starter :partition :root)
      (:starter :link :link :ender) (:starter :ender))

    :starter
    '((:starter :link)
      "aa" "ae" "al" "an" "ao" "ar" "at" "az" "be" "bi" "ce" "di" "ed" "en" "er"
      "es" "ge" "in" "is" "la" "le" "ma" "on" "or" "qu" "ra" "re" "ri" "so" "te"
      "ti" "us" "ve" "xe" "za")
    :ender
    '((:link :ender)
      "aa" "al" "at" "di" "ti" "so" "ce" "re" "za" "in" "ed" "or" "an" "ma" "ab"
      "ge" "aq" "en" "ri" "ve" "ag" "qu" "us" "es" "ex" "ae" "on" "bi" "xe" "le"
      "is" "er" "be" "la" "ar" "az" "io" "sb" "te" "ra" "ia" "nb")
    :link
    '((:link :link) (:link :link)
      "at" "an" "ri" "es" "ed" "bi" "ce" "us" "on" "er" "ti" "ve" "ra" "la" "le"
      "ge" "i" "u" "xe" "in" "di" "so" "ar" "e" "s" "na" "is" "za" "re" "ma" "or"
      "be" "en" "qu" "a" "n" "r" "te" "t")
    :partition '("-" "'" " ")}

   :prisoner-name
   {:root
    '((:proper-name) (:proper-name :title))
    :title '((:of-title) (:the-title) (:of-title :the-title) (:the-title :of-title))
    :of-title '((" of " :proper-name))
    :the-title '((" the " :adjective))
    :adjective
    '("obsequious" "brilliant" "great" "glorious")}})

(defun expand-production (production grammar)
  (cond ((stringp production) production)
	((symbolp production)
	 (if-let (g (lookup grammar production))
	   (expand-production (pick g) grammar)
	   (generate-a production)))
	((listp production)
	 (reduce
	  (lambda (memo b)
	    (concatenate 'string memo (expand-production b grammar)))
	  (cons "" production)))))

(defun generate-a (kind-of-thing)
  (expand-production :root (lookup *GRAMMARS* kind-of-thing)))

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
  (declare (ignore player prisoner))
  (lookup
   {(list trade :cooperate :cooperate)
    "You trade some of the useless items you are carrying for some of the strangers' valuable goods. You suspect they see it the same way."
    (list trade :cooperate :defect)
    "\"Pardon; I mistakenly thought you had something I wanted\". They walk away haughtily."
    (list trade :defect :cooperate)
    "You refuse the trade."
    (list trade :defect :defect)
    "You refuse the trade. When the stranger hears this, they shout \"Well I didn't want your garbage anyway!\" and storms off."}
   (list scenario player-choice prisoner-choice) :default ""))
