(defclass graph ()
  ((edges :accessor edges :initarg :edges)
   (nodes :accessor nodes :initarg :nodes)))

(defmethod print-object ((g graph) stream)
  (format stream "[~A nodes ~A edges]" 
	  (length (nodes g))
	  (length (edges g))))

(defun make-graph (edges nodes)
  (make-instance 'graph :edges edges :nodes nodes))

(defun iota (n &optional (start 0) (step 1))
  (loop repeat n
     for i from start by step
     collect i))

(defun sort-edges (list-edges)
  (mapcar #'(lambda (x)
	      (if (< (car x) (car (cdr x)))
		  (cons (car x) (cdr x))
		  (list (car (cdr x)) (car x) )))list-edges))

(defun delete-double (list-edges)
  (remove-duplicates list-edges :test #'equal))

(defmacro make-edges-oriented (nb-nodes exp)
  (when (symbolp exp)
    (setf exp (eval exp)))
  `(loop
      for i from 1 to ,nb-nodes
      nconc (loop for j from 1 to ,nb-nodes
	       unless (= i j)
	       when ,exp
	       collect (list i j))))

(defmacro make-edges-unoriented (nb-nodes expression)
  `(delete-double (sort-edges (make-edges-oriented ,nb-nodes ,expression))))

(defmacro to-graph-oriented (nb-nodes expressions)
  `(make-graph (make-edges-oriented ,nb-nodes ,expressions)
	      (make-nodes ,nb-nodes)))

(defun make-nodes (nb-nodes)
  (iota nb-nodes 1))

(defmacro to-graph-unoriented (nb-nodes expressions)
  `(make-graph (make-edges-unoriented ,nb-nodes ,expressions) 
	      (make-nodes ,nb-nodes)))


(defparameter *cycle* '(or (and (= i 1) (= j n)) (= j (1+ i))))
(defparameter *pn* '(= j (1+ i)))
(defparameter *kn* t)
(defparameter *empty* nil)
(defparameter *test1* '(or (= i (+ 1 j)) (= i (* 2 j))))
(defparameter *test2* '(or (= j (+ 1 i)) (= j (* 2 i))))
(defparameter *test3* '(or (= i (- j 1)) (= j (* 2 i))))
(defparameter *not* '(not (or (= i (+ 1 j)) (= i (* 2 j)))))
