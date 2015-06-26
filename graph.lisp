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

(defmacro main (expression test n listarg)
  `(progn (setf n ,n)
	  (if (eval (translate ,test ,listarg))
	      (create ,expression ,n ,listarg)
	      (print 'error))))

(defun translate (expression listarg)
  (do((l listarg (cdr l))
      (e expression (nsubst (car (cdr (car l))) (car(car l)) e)))
     ((= (length l) 0) e)))
  
(defmacro create (exp n listarg)
  `(progn(translate ,exp ,listarg)
	 (to-graph-unoriented ,n ,exp)))

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

(defparameter *testranslate* '( x0 x1 x2 x3 x4 x5 x6 x8 x7 x9 x5 x4))

