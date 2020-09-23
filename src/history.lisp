(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)

(defparameter *dir* (merge-pathnames (user-homedir-pathname) ".the-prisoners"))

(defparameter *base* (fact-base:base! (merge-pathnames *dir* "history.base")))

(defun facts->map (res)
  (format t "~s~%" res)
  (let ((id (caar res)))
    (insert
     (reduce
      (lambda (memo fact)
	(if (== id (first fact))
	    (insert memo (second fact) (third fact))
	    memo))
      res :initial-value {})
     :id id)))

(defun store-prisoner! (prisoner &key (strategy :manual) (source :local))
  (let ((strategy (lookup prisoner :strategy :default strategy)))
    (insert
     prisoner
     :id (fact-base:multi-insert!
	  *base*
	  (insert
	   (loop for (k . v) in (as-list (dissoc prisoner :strategy))
	      collect (list k v))
	   (list :strategy strategy)
	   (list :source source))))))

(defun prisoner-by (&key id strategy source)
  (if id
      (list
       (facts->map
	(fact-base:for-all `(,id ?k ?v) :in *base* :collect (list id ?k ?v))))
      (mapcan
       (fn (id) (prisoner-by :id id))
       (fact-base:for-all
	   `(,@(cond (strategy `(?id :strategy ,strategy))
		     (source `(?id :source ,source))))
	 :in *base* :collect ?id))))

(defun prisoner-incf! (prisoner key &key (by 1))
  (let ((id (lookup prisoner :id)))
    (if-let (old (first (fact-base:lookup *base* :a id :b key)))
      (let ((new (+ (third old) by)))

	(fact-base:change! *base* old (list id key new))
	new))))
