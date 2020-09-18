;;;; the-prisoners.asd

(asdf:defsystem #:the-prisoners
  :description "Describe the-prisoners here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:clj)
  :components ((:module
		src :components
		((:file "package")
		 (:file "util")
		 (:file "adventures")
		 (:file "the-prisoners")))))

(asdf:defsystem #:the-prisoners-test
  :description "Test suite for :the-prisoners"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
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
