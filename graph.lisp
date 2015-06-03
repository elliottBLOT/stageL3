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

(defun eval-constant-expression (nb-nodes i j expression)
  (if (numberp expression)
      expression
      (case expression
	(n nb-nodes)
	(i i)
	(j j))))

(defun eval-arithmetic-expression (nb-nodes i j expression)
  (if (atom expression)
      (eval-constant-expression nb-nodes i j expression)
      (apply (car expression) 
	     (mapcar 
	      (lambda (arg) (eval-arithmetic-expression nb-nodes i j arg))
	      (cdr expression)))))

(defun eval-logical-expression (nb-nodes i j expression)
  (if (atom expression)
      expression
      (let ((operator (car expression))
	    (arguments (cdr expression)))
	(case operator
	  (or (some (lambda (arg) (eval-logical-expression nb-nodes i j arg))
		    arguments))
	  (and (every (lambda (arg) (eval-logical-expression nb-nodes i j arg))
		      arguments))
	  (not (not (eval-logical-expression nb-nodes i j (car arguments))))
	  (t (apply operator
		    (mapcar
		     (lambda (arg) (eval-arithmetic-expression nb-nodes i j arg))
		     arguments)))))))

  (defun make-edges-unoriented (nb-nodes expression)
    (loop
       for i from 1 below nb-nodes
       nconc (loop for j from (1+ i) to nb-nodes
		unless (= i j)
		when (eval-logical-expression nb-nodes i j expression)
		collect (list i j))))

  (defun make-nodes (nb-nodes)
    (iota nb-nodes 1))

  (defun to-graph (nb-nodes expressions)
    (make-graph (make-edges-unoriented nb-nodes expressions) 
		(make-nodes nb-nodes)))

(defparameter *cycle* '(or (and (= i 1) (= j n)) (= j (1+ i))))
(defparameter *pn* '(= j (1+ i)))
(defparameter *kn* t)
(defparameter *empty* nil)

