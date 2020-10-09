(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)

;; (cl-who:with-html-output-to-string (s nil :indent nil)
;;   (cl-who:str "<?xml version=\"1.0\" standalone=\"no\"?>")
;;   (cl-who:str "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
;;   \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")
;;   (:svg :width 200 :height 200 :version "1.1" :id "toplevel"
;; 	:xmlns "http://www.w3.org/2000/svg"
;; 	:xmlns\:xlink "http://www.w3.org/1999/xlink"
;; 	(cl-who:htm (reverse objects))))

;; '(:circle :cx 10 :cy 10 :r 30 :fill "white" :stroke "black" :stroke-width "0.5")

;; <?xml version=\"1.0\" standalone=\"no\"?><!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
;;   \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\"><svg width='200' height='200' version='1.1' id='toplevel' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'></svg>

(defun as-plist (alist)
  (loop for (a . b) in alist
     collect a collect b))

(defun write-tag (tag &key stream (level 0) (level-size 2))
  (let ((children (lookup tag :children))
	(self-terminate? (lookup tag :self-terminate?))
	(open (lookup tag :tag))
	(close (or (lookup tag :close) (lookup tag :tag)))
	(spacer (make-string (* level level-size) :initial-element #\space)))
    (apply #'format
	   stream
	   (cond
	     ((and self-terminate? (empty? children))
	      (list "~a<~a ~{~a=\"~a\"~^ ~} ~a>"
		    spacer open
		    (as-plist (as-list (lookup tag :properties)))
		    close))
	     ((empty? children)
	      (list "~a<~a~{ ~a=\"~a\"~}></~a>"
		    spacer open
		    (as-plist (as-list (lookup tag :properties)))
		    close))
	     ((typep children 'string)
	      (list "~a<~a~{ ~a=\"~a\"~}>~a</~a>"
		    spacer open
		    (as-plist (as-list (lookup tag :properties)))
		    children
		    close))
	     (t
	      (list "~a<~a~{ ~a=\"~a\"~}>~{~%~a~}~%~a</~a>"
		    spacer open
		    (as-plist (as-list (lookup tag :properties)))
		    (fmap (lambda (tag) (write-tag tag :level (inc level))) children)
		    spacer close))))))

(defun write-tags (tags &key stream)
  (if (null stream)
      (format stream "~{~a~^~%~}" (fmap (fn (tag) (write-tag tag :stream stream)) tags))
      (progn (fmap (fn (tag) (write-tag tag :stream stream) (format stream "~%")) tags) nil)))

(defun svg-tag (tag-name properties &rest children)
  {:tag tag-name
   :properties (fmap (fn (k v) (cons (if (keywordp k) (string-downcase (symbol-name k)) k) v)) properties)
   :children children})

(defun ?xml (&key (version 1.0))
  (insert
   (svg-tag "?xml" {"version" version})
   :self-terminate? t :close "?"))

(defun !doctype ()
  (insert
   (svg-tag "!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\"" {})
   :self-terminate? t :close ""))

(defun svg (properties &rest children)
  (apply #'svg-tag "svg" properties children))

;; (write-tags (list (?xml) (!doctype) (svg {:width 200 :height 200 :version 1.1} {:tag :p :properties {} :children (list {:tag :p :properties {} :children "blah"} {:tag :p :properties {} :children "blah"})})) :stream t)

(defun process-commands (commands)
  (format nil "~{~a~^ ~}" commands))

(defun g (properties &rest children)
  (apply #'svg-tag "g" properties children))

(defun path (options &rest commands)
  (insert
   (svg-tag
    "path"
    (fmap
     (lambda (k v) (when v (cons k v)))
     (insert options "d" (process-commands commands))))
   :self-terminate? t :close "/"))

(defun circle (options) (insert (svg-tag "circle" options) :self-terminate? t :close "/"))

(defun draw-person (person)
  (declare (ignore person))
  (g {:transform "translate(10 10)"}
   (circle {:cx 0 :cy 0 :r 5 :fill "cyan"})
   (path {:stroke "cyan" :fill "none"}
	 :m 0 0
	 :l 0 7 :l -10 17 :l 0 7
	 :l 10 17 :l 0 7
	 :l 0 30
	 :l -10 37 :l 0 30 :l 10 37)))

(defmacro draw! (options &rest objects)
  (declare (ignore options))
  `(cl-who:with-html-output-to-string (*standard-output* nil :indent nil)
     (cl-who:str "<?xml version=\"1.0\" standalone=\"no\"?>")
     (cl-who:str "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"
  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">")
     (:svg :width 200 :height 200 :version "1.1" :id "toplevel"
	   :xmlns "http://www.w3.org/2000/svg"
	   :xmlns\:xlink "http://www.w3.org/1999/xlink"
	   (:g (cl-who:htm (cl-who:with-html-output (s *standard-output*) ,@objects))))))

(defun draw-tree! (scene tree))

(defun draw! (scene object)
  )
