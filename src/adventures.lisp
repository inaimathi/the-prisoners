(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)

(defun mk-scenario ()
  {:description "A stranger approaches. \"I see you have baubles. Would you like to trade, that we both may enrich ourselves?\""
		:cooperate "accept" :defect "refuse" :prisoner})

(defun mk-adventure ()
  (let ((prisoner (polo))
	(scenario free-trade))
    {:description
     "A stranger approaches. \"I see you have baubles. Would you like to trade, that we both may enrich ourselves?\""
     :cooperate "accept" :defect "refuse" :prisoner prisoner :scenario scenario
     :continue (lambda (choice)
		 (let* ((their-choice (play prisoner))
			(res (funcall scenario choice their-choice)))
		   (update! prisoner choice)
		   {:description
		    (format nil
			    "You have come to the end of your long, perilous journey.~%Your fortune of ~a will keep you well."
			    (first res)) }))}))
