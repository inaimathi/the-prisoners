;;;; the-prisoners.asd

(asdf:defsystem #:the-prisoners
  :description "A game-theoretic roguelikelike"
  :author "inaimathi <leo.zovic@gmail.com>"
  :license "AGPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:clj)
  :components ((:module
		src :components
		((:file "package")
		 (:file "util")
		 (:file "adventures")
		 (:file "the-prisoners")
		 (:file "repl")))))

(asdf:defsystem #:the-prisoners-test
  :description "Test suite for :the-prisoners"
  :author "inaimathi <leo.zovic@gmail.com>"
  :license "AGPL3"
  :serial t
  :depends-on (#:the-prisoners #:test-utils)
  :defsystem-depends-on (#:prove-asdf)
  :components ((:module
                test :components
                ((:file "package")
                 (:test-file "the-prisoners"))))
  :perform (test-op
	    :after (op c)
	    (funcall (intern #.(string :run) :prove) c)))
