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

(defun order-edge (edge)
  (destructuring-bind (x y) edge
    (if (< x y)
	edge
	(reverse edge))))

(defun sort-nodes (edges)
  (mapcar #'order-edge edges))

(defun delete-double (list-edges)
  (remove-duplicates list-edges :test #'equal))

(defmacro m (nb-nodes expression)
  `(loop
      for i from 1 to ,nb-nodes
      nconc (loop for j from 1 to ,nb-nodes
	       unless (= i j)
	       when ,expression
	       collect (list i j))))

(defun make-edges-oriented (nb-nodes exp)
  (eval `(m ,nb-nodes ,exp)))

(defun make-edges-unoriented (nb-nodes expression)
  (sort
   (delete-double
    (sort-nodes
     (make-edges-oriented nb-nodes expression)))
   #'pair<))

(defun make-nodes (nb-nodes)
  (iota nb-nodes 1))

(defun to-graph-oriented (nb-nodes expressions)
  (make-graph (make-edges-oriented nb-nodes expressions)
	      (make-nodes nb-nodes)))

(defun pair< (pair1 pair2)
  (destructuring-bind (a1 b1) pair1
    (or (< a1 b1)
	(and (= a1 b1)
	     (< (second pair1) (second pair2))))))

(defun to-graph-unoriented (nb-nodes expressions)
  (make-graph (make-edges-unoriented nb-nodes expressions) 
	      (make-nodes nb-nodes)))

(defun main (nb-nodes expression test &rest args &key &allow-other-keys)
  (setq expression (copy-tree expression)) ;; for being non destructive
  (setq test (copy-tree test)) ;; for being non destructive
  (loop
     while args
     do (let ((var (pop args))
	      (val (pop args)))
	  (nsubst  nb-nodes 'n expression)
	  (nsubst nb-nodes 'n test) ;; is this useful?
	  (nsubst val var test)
	  (nsubst val var expression)))
  (if (eval test)
      (to-graph-unoriented nb-nodes expression)
      (warn "invalid test")))

(defparameter *pn* '(= j (1+ i)))
(defparameter *kn* t)
(defparameter *empty* nil)
(defparameter *cycle* 
  '(or (and (= i 1) (= j n)) (= j (1+ i))))
(defparameter *test1* 
  '(or (= i (+ 1 j)) (= i (* 2 j))))
(defparameter *test2* 
  '(or (= j (+ 1 i)) (= j (* 2 i))))
(defparameter *test3* 
  '(or (= i (- j 1)) (= j (* 2 i))))
(defparameter *not* 
  '(not (or (= i (+ 1 j)) (= i (* 2 j)))))
(defparameter *g*      
  '(or (= (+ 3 j) i) (and (= i (1+ j)) (/= (rem j 3 ) 0))))
(defparameter *grille* 
  '(or (= (+ :x0 j) i) (and (= i (1+ j)) (/= (rem j :x0) 0)))) 
(defparameter *gee*
  '(or (and (or (= j (+ (- n :x0 ) i)) (= i (+ :x0 j))) (= (mod (- j :x2) :x1) 0)) (and (= i (+ j :x4)) (= (mod (1- i) :x3) 0))(or (and (= i 1) (= j n)) (= j (1+ i)))))


(defparameter *test-grille* '(= 0 (rem n :x0)))

;; Example
;; (main 15 *grille* *test-grille* :x0 3) => [15 nodes 22 edges]

