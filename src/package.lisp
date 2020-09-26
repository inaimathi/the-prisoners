;;;; src/package.lisp

(defpackage #:the-prisoners
  (:use #:cl #:clj)
  (:export #:build! #:main #:collapse #:log!))

(defpackage #:logos
  (:import-from #:cl #:lambda #:t #:nil)
  (:import-from #:clj #:fn #:fn #:lookup)
  (:import-from #:the-prisoners #:collapse #:log!))
