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

(defun sort-nodes (list-edges)
  (mapcar #'(lambda (x)
	      (if (< (car x) (cadr x))
		  (cons (car x) (cdr x))
		  (list (cadr x) (car x) )))list-edges))

(defun delete-double (list-edges)
  (remove-duplicates list-edges :test #'equal))

(defmacro make-edges-oriented (nb-nodes exp)
  (when (symbolp exp)
    (setf exp (eval exp)))
  `(loop
      with n = ,nb-nodes
      for i from 1 to ,nb-nodes
      nconc (loop for j from 1 to ,nb-nodes
	       unless (= i j)
	       when ,exp
	       collect (list i j))))

(defmacro make-edges-unoriented (nb-nodes expression)
  `(sort (delete-double (sort-nodes (make-edges-oriented ,nb-nodes ,expression))) #'doublon))

(defmacro to-graph-oriented (nb-nodes expressions)
  `(make-graph (make-edges-oriented ,nb-nodes ,expressions)
	       (make-nodes ,nb-nodes)))

(defun doublon (list1 list2)
  (if (< (first list1) (first list2))
      t
      (and (= (first list1) (first list2)) (< (second list1) (second list2)))))

(defun make-nodes (nb-nodes)
  (iota nb-nodes 1))

(defmacro to-graph-unoriented (nb-nodes expressions)
  `(make-graph (make-edges-unoriented ,nb-nodes ,expressions) 
	       (make-nodes ,nb-nodes)))

(defmacro main (expression test n x0 x1 x2 x3 x4 x5 x6 x7 x8 x9)
  `(if ,test
       (to-graph-unoriented ,n (translate ,expression ,x0 ,x1 ,x2 ,x3 ,x4 ,x5 ,x6 ,x7 ,x8 ,x9))
       (print "error")))

(defun translate (expression x0 x1 x2 x3 x4 x5 x6 x7 x8 x9)
  (nsubst x0 'x0 expression)
  (nsubst x1 'x1 expression)
  (nsubst x2 'x2 expression)
  (nsubst x3 'x3 expression)
  (nsubst x4 'x4 expression)
  (nsubst x5 'x5 expression)
  (nsubst x6 'x6 expression)
  (nsubst x7 'x7 expression)
  (nsubst x8 'x8 expression)
  (nsubst x9 'x9 expression)
  )

(defun create (exp n x0 x1 x2 x3 x4 x5 x6 x7 x8 x9)
  (translate exp x0 x1 x2 x3 x4 x5 x6 x7 x8 x9)
  (to-graph-unoriented n exp)
  )


(defparameter *cycle* '(or (and (= i 1) (= j n)) (= j (1+ i))))
(defparameter *pn* '(= j (1+ i)))
(defparameter *kn* t)
(defparameter *empty* nil)
(defparameter *test1* '(or (= i (+ 1 j)) (= i (* 2 j))))
(defparameter *test2* '(or (= j (+ 1 i)) (= j (* 2 i))))
(defparameter *test3* '(or (= i (- j 1)) (= j (* 2 i))))
(defparameter *not* '(not (or (= i (+ 1 j)) (= i (* 2 j)))))
(defparameter *g*      '(or (= (+ 3  j) i) (and (= i (1+ j)) (/= (rem j 3 ) 0))))
(defparameter *grille* '(or (= (+ x0 j) i) (and (= i (1+ j)) (/= (rem j x0) 0)))) 

(defparameter *test-grille* '(= 0 (rem n x0)))

(defparameter *testra* '( x0 x1 x2 x3 x4 x5 x6 x8 x7 x9 x5 x4))

