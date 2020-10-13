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

;; (with-open-file (s "test.svg" :direction :output :if-exists :supersede :if-does-not-exist :create :element-type 'character)
;;   (write-tags
;;    (list (?xml) (!doctype)
;; 	 (svg {:width 200 :height 200 :version 1.1 :xmlns "http://www.w3.org/2000/svg" "xmlns:xlink" "http://www.w3.org/1999/xlink"}
;; 	      (draw-person nil)
;; 	      (draw-tree nil)))
;;    :stream s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Tag basics
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; SVG Basics
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

(defun g (properties &rest children)
  (apply #'svg-tag "g" properties children))

(defun process-commands (commands)
  (format nil "~{~a~^ ~}" commands))

(defun path (options &rest commands)
  (insert
   (svg-tag
    "path"
    (fmap
     (lambda (k v) (when v (cons k v)))
     (insert options "d" (process-commands commands))))
   :commands commands
   :self-terminate? t :close "/"))

(defun ellipse (options)
  (insert (svg-tag "ellipse" options) :self-terminate? t :close "/"))

(defun circle (options)
  (insert (svg-tag "circle" options) :self-terminate? t :close "/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Drawing primitives
(defun bbox-+ (a b)
  (or (and
       a b
       (destructuring-bind (ax1 ay1 ax2 ay2) a
	 (destructuring-bind (bx1 by1 bx2 by2) b
	   (list (min ax1 bx1) (min ay1 by1)
		 (max ax2 bx2) (max ay2 by2)))))
      a b))

(defun bbox-x- (a b)
  (or (and
       a b
       (destructuring-bind (ax1 _ _ _) a
	 (destructuring-bind (bx1 _ _ _) b
	   (declare (ignore _))
	   (- ax1 bx1))))
      0))

(defun bbox-y- (a b)
  (or (and
       a b
       (destructuring-bind (_ ay1 _ _) a
	 (destructuring-bind (_ by1 _ _) b
	   (- ay1 by1))))
      0))

(defun bbox-of-commands (commands)
  (let ((cmds commands)
	(min-x nil)
	(min-y nil)
	(max-x nil)
	(max-y nil))
    (flet ((x! (x)
	     (setf min-x (min (or min-x x) x)
		   max-x (max (or max-x x) x)))
	   (y! (y)
	     (setf min-y (min (or min-y y) y)
		   max-y (max (or max-y y) y))))
      (loop while cmds
	 do (let ((c (pop cmds)))
    	      (cond
    		((contains? #{:m :l :t} c)
		 (x! (pop cmds))
		 (y! (pop cmds)))
		((contains? #{:s :q} c)
		 (loop repeat 2
		    do (progn
			 (x! (pop cmds))
			 (y! (pop cmds)))))
		((== :h c) (x! (pop cmds)))
		((== :v c) (y! (pop cmds)))
		((== :c c)
		 (loop repeat 3
		    do (progn
			 (x! (pop cmds))
			 (y! (pop cmds)))))
		((== :a c)
		 ;; FIXME - this might need to be more accurate :/
		 (x! (pop cmds))
		 (y! (pop cmds))
		 (loop repeat 3 do (pop cmds))
		 (x! (pop cmds))
		 (y! (pop cmds))))))
      (list (or min-x 0) (or min-y 0)
	    (or max-x 0) (or max-y 0)))))


(defun bbox-of (tag-tree)
  (if (listp tag-tree)
      (reduce #'bbox-+ (mapcar #'bbox-of tag-tree))
      (let ((props (lookup tag-tree :properties)))
	(case (intern (string-upcase (lookup tag-tree :tag)) :keyword)
	  (:circle
	   (let ((r (lookup props "r"))
		 (x (lookup props "cx"))
		 (y (lookup props "cy")))
	     (list (- x r) (- y r) (+ x r) (+ y r))))
	  (:ellipse
	   (let ((rx (lookup props "rx"))
		 (ry (lookup props "ry"))
		 (x (lookup props "cx"))
		 (y (lookup props "cy")))
	     (list (- x rx) (- y ry) (+ x rx) (+ y ry))))
	  (:path (bbox-of-commands (lookup tag-tree :commands)))
	  (:g (reduce #'bbox-+ (mapcar #'bbox-of (lookup tag-tree :children))))))))

(defun >dot (&optional (adjectives #{}))
  (declare (ignore adjectives))
  (circle {:cx 0 :cy 0 :r 5 :fill "black"}))
(defun >line (&optional (adjectives #{}))
    (declare (ignore adjectives))
    ;; (let ((adj (list->set adjectives)))
    ;;   (cond
    ;; 	((contains? adj :vertical))
    ;; 	((contains? adj :horizontal))
    ;; 	((contains? adj :diagonal))))
    (path {:stroke "black" :stroke-width 5}
	  :m 0 0 :l 30 -10))
(defun >arc (&optional (adjectives #{}))
    (declare (ignore adjectives))
    (path {:stroke "black" :stroke-width 5}
	  :m 0 0 :c 30 -10))
(defun >circle (&optional (adjectives #{}))
  (declare (ignore adjectives))
  (circle {:cx 0 :cy 0 :r 30 :stroke "black"}))
(defun >ellipse (&optional (adjectives #{}))
    (declare (ignore adjectives))
    (ellipse {:cx 0 :cy 0 :rx 30 :ry 20}))

(defun above (a b)
  ;; FIXME - ensure that the two elements are positioned above each other
  (g
   {:transform
    (format nil "rotate(0, ~a)"
	    (bbox-y- (bbox-of a) (bbox-of b)))}
   a b))

(defun left-of (a b)
  ;; FIXME - ditto but next to
  (let ((bbox-a (bbox-of a))
	(bbox-b (bbox-of b)))
    nil))

(defun inside (a b)
  ;; FIXME - ditto, but inside of each other
  (let ((bbox-a (bbox-of a))
	(bbox-b (bbox-of b)))
    nil))

;;;;;;;;;; Drawing wishlist
;; (let* ((>eye (above
;; 	      (>line :horizontal)
;; 	      (>dot :black)))
;;        (>nose (above
;; 	       (>line)
;; 	       (left-of (>dot) (>dot))))
;;        (>mouth (.wide (>line)))
;;        (>face (inside
;; 	       (above
;; 		(beside (beside (>eye) (>nose)) (>eye))
;; 		(>mouth))
;; 	       (>ellipse :vertical))))
;;   (>face))

;; should draw a basic face with variations on >eye. It should be possible to get a specific >eye out by asking, but I have no idea how. Maybe just easier to save the structure the first time through.

;;;;;;;;;; Drawing compounds
(defun draw-person (person)
  (declare (ignore person))
  (g {}
     (circle {:cx 0 :cy 0 :r 5 :fill "cyan"})
   (path {:stroke "cyan" :fill "none"}
	 :m 0 0
	 :l 0 7 :l -10 17 :l 0 7
	 :l 10 17 :l 0 7
	 :l 0 30
	 :l -10 47 :l 0 30 :l 10 47)))

(defun draw-tree (tree)
  (declare (ignore tree))
  (path {:stroke "green" :fill "none"}
	:m 0 0 :l 0 50
	:l 0 0 :l 10 10 :l 0 0 :l -10 10 :l 0 0
	:l 0 15 :l 15 25 :l 0 15 :l -15 25 :l 0 15
	:l 0 30 :l 20 40 :l 0 30 :l -20 40 :l 0 30))

(defun draw-canvas (&rest elems)
  (list
   (?xml) (!doctype)
   (apply
    #'svg
    {:width 200 :height 200 :version 1.1 :xmlns "http://www.w3.org/2000/svg" "xmlns:xlink" "http://www.w3.org/1999/xlink"}
    elems)))

(with-open-file (s "test.svg" :direction :output :if-exists :supersede :if-does-not-exist :create :element-type 'character)
  (write-tags
   (draw-canvas (draw-person nil) (draw-tree nil))
   :stream s))
