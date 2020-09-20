(in-package #:the-prisoners)
(named-readtables:in-readtable clj:syntax)

(defun display-repl-choice (adventure)
  (format
   t "~a/~a:"
   (lookup adventure :cooperate)
   (lookup adventure :defect))
  (force-output))

(defun get-repl-choice (adventure)
  (let* ((responses (mapcar #'string-downcase (list (lookup adventure :cooperate) (lookup adventure :defect))))
	 (r-map {(string-downcase (lookup adventure :cooperate)) :cooperate
		 (string-downcase (lookup adventure :defect)) :defect})
	 (by-pref nil))
    (display-repl-choice adventure)
    (let ((resp (read-line)))
      (loop until (setf by-pref (get-by-prefix responses (string-downcase resp)))
	 do (display-repl-choice adventure)
	 do (setf resp (read-line))))
    (lookup r-map by-pref)))

(defun to-adventure! (player adventure)
  (format t "~%==============================~%")
  (format t "~a, score: ~a~%~%" (lookup player :name) (lookup player :score))
  (format t "~a~%~%" (lookup adventure :description))
  (when (contains? adventure :continue)
    (let* ((choice (get-repl-choice adventure))
	   (next (funcall (lookup adventure :continue) choice))
	   (score (if-let (s (lookup next :score)) (+ s (lookup player :score)))))
      (to-adventure!
       (insert player :score score)
       (if (> score 0)
	   next
	   {:description (death-paragraph (dissoc adventure :continue))})))))

(defun repl! (adventure)
  (format t "~%-==E- The Prisoners -Ǝ==-~%~%")
  (format t "~% What is your name?: ")
  (let ((player-name (read-line)))
    (format t "~%Welcome, ~a~%~%" player-name)
    (format t "You may roam this world to your heart's content,~%but when you encounter your opposite number,~%know that you are the prisoners of your history.")
    (to-adventure!
     {:name player-name :score 1}
     adventure)))
