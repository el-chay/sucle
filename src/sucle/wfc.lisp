(defpackage #:wfc
  (:use :cl))
(in-package #:wfc)

;;https://github.com/mxgmn/WaveFunctionCollapse
;;https://robertheaton.com/2018/12/17/wavefunction-collapse-algorithm/
;;https://exutumno.itch.io/wavefunctioncollapse
;;https://bitbucket.org/mxgmn/basic3dwfc/src/master/Model.cs
;;http://graphics.stanford.edu/%7Epmerrell/thesis.pdf
(defun player-pos ()
  (physics::pos sucle::*ent*))
(defparameter *hash* nil)
(defparameter *array* nil)
(defun compute-region (&aux (x0 -20) (y0 22) (z0 -30)
			 ;;(x0 0) (y0 -20) (z0 0)
			 (hash (make-hash-table :test 'eq)))
  (declare (optimize (speed 3) (safety 0)))
  (let (;;radius
	(r 50)
	(count 0))
    (declare (type fixnum count))
    (utility:dobox ((x1 (- r) r)
		    (y1 (- r) r)
		    (z1 (- r) r))
      (incf count)
      (let ((x (+ x1 x0))
	    (y (+ y1 y0))
	    (z (+ z1 z0)))
	;;#+nil
	(let ((3x3 (3x3-neighborhood x y z)))
	  (incf (gethash 3x3 hash 0)))
	#+nil
	(let ((id (getb x y z)))
	  (flet ((foo (direction x y z)
		   (let ((bid (getb x y z)))
		     (incf (gethash (pack2 id bid direction)
				    hash 0)))))
	    (progn
	      (foo 0 (1+ x0) y0      z0)
	      (foo 1 x0      (1+ y0) z0)
	      (foo 2 (1- x0) y0      z0)
	      (foo 3 x0      (1- y0) z0)
	      (foo 4 x0      y0      (1+ z0))
	      (foo 5 x0      y0      (1- z0)))))))
    (setf *hash* hash)
    (let ((array (make-array (hash-table-count hash))))
      (let ((id 0))
	(utility:dohash
	    (local count) hash
	    (setf (gethash local hash)
		  (cons id count))
	    (setf (aref array id) (cons local count))
	    (incf id)))
      (setf *array* array)
      (values hash
	      array
	      (locally (declare (optimize (speed 0) safety))
		(utility:floatify
		 (/ (hash-table-count hash)
		    count)))))))
#+nil
(defun opposing-p (a b direction)
  (or (and (= a 0) (= b 2) (= direction 0))
      (and (= a 2) (= b 0) (= direction 2))
      (and (= a 1) (= b 3) (= direction 1))
      (and (= a 3) (= b 1) (= direction 3))
      (and (= a 5) (= b 4) (= direction 5))
      (and (= a 4) (= b 5) (= direction 4))))

(defun vec+ (a b)
  (map 'vector '+ a b))

(defun aux (n)
  (aref
   #(#(1 0 0)
     #(0 1 0)
     #(-1 0 0)
     #(0 -1 0)
     #(0 0 1)
     #(0 0 -1))
   n))

(defun pack2 (b1 b2 direction)
  (dpb b1 (byte 2 6)
       (dpb b2 (byte 2 4) 
	    (dpb direction (byte 4 0)
		 0))))
(defun unpack2 (pack)
  (values (ldb (byte 2 6) pack)
	  (ldb (byte 2 4) pack)
	  (ldb (byte 4 0) pack)))

(defun getb (x y z)
  (translator
   (world::getblock x y z)))

(defun total-count ()
  (reduce '+ *array* :key 'cdr))

(defun 3x3-neighborhood (x0 y0 z0)
  (declare (optimize (speed 3) (safety 0))
	   (type fixnum x0 y0 z0))
  (let ((acc 0)
	(index 0))
    (declare (type fixnum acc)
	     (type (integer 0 54) index))
    (flet ((foo (x y z)
	     (declare (type fixnum x y z))
	     (let ((id (getb x y z)))
	       (setf acc (logior acc (the fixnum (ash (the (integer 0 3) id) index)))))
	     (incf index 2)))
      (progn
	(foo (1+ x0) y0      z0)
	(foo (1- x0) y0      z0)
	(foo x0      (1+ y0) z0)
	(foo x0      (1- y0) z0)
	(foo x0      y0      (1+ z0))
	(foo x0      y0      (1- z0))
	(foo x0      y0      z0))
      #+nil
      (utility:dobox
	  ((x (+ -1 x0) (+ 2 x0))
	   (y (+ -1 y0) (+ 2 y0))
	   (z (+ -1 z0) (+ 2 z0)))
	(foo x y z)))
    (values acc)))
;;#+nil
(defun explode-3x3 (3x3)
  (let ((array (make-array '(3 3 3)))
	(index 0))
    (flet ((foo (x y z)
	     (setf (aref array x y z)
		   (ldb (byte 2 index) 3x3))
	     (incf index 2)))
      (progn
	(foo 2 1 1)
	(foo 0 1 1)
	(foo 1 2 1)
	(foo 1 0 1)
	(foo 1 1 2)
	(foo 1 1 0)
	(foo 1 1 1))
      #+nil
      (utility:dobox
       ((x0 0 3)
	(y0 0 3)
	(z0 0 3))
       (foo x0 y0 z0)))
    array))
#+nil
(defparameter *adjacency* (make-hash-table :test 'equal))
#+nil
(defun add-3x3 (array count &optional (hash *adjacency*))
  (let ((center (aref array 1 1 1)))
    (utility:dobox
	((x 0 3)
	 (y 0 3)
	 (z 0 3))
      (unless (and (= x 1) (= y 1) (= z 1))
	(incf (gethash (list center (aref array x y z) x y z) hash 0)
	      count))))
  (values))

(defun compatible-p (w1 w2 n)
  (let ((center1 (aref w1 1 1 1))
	(center2 (aref w2 1 1 1)))
    (flet ((compat (side1 side2)
	     (and (= center1 side2)
		  (= center2 side1)))
	   (ref (arr x y z)
	     (aref arr (1+ x) (1+ y) (1+ z))))
      (ecase n
	(0 (compat (ref w1  1  0  0)
		   (ref w2 -1  0  0)))
	(1 (compat (ref w1  0  1  0)
		   (ref w2  0 -1  0)))
	(2 (compat (ref w1 -1  0  0)
		   (ref w2  1  0  0)))
	(3 (compat (ref w1  0 -1  0)
		   (ref w2  0  1  0)))
	(4 (compat (ref w1  0  0  1)
		   (ref w2  0  0 -1)))
	(5 (compat (ref w1  0  0 -1)
		   (ref w2  0  0  1)))))))

#+nil
(defun construct-adjacency (&optional (hash *hash*) (adj *adjacency*))
  (utility:dohash
      (3x3 count) hash
      (add-3x3 (explode-3x3 3x3) count adj)))

(defun types ()
  (remove-duplicates 
   (mapcar 'type-of
	   (alexandria:hash-table-keys *hash*))
   :test 'equalp))
(defun freq ()
  (subseq 
   (sort 
    (alexandria:hash-table-alist *hash*)
    '>
    :key 'cddr)
   0 100))
#+nil
(defun freqadj ()
  (subseq 
   (sort 
    (alexandria:hash-table-alist *adjacency*)
    '>
    :key 'cdr)
   0 ;;100
   ))

;;ground
;;air
;;plant
;;artificial

(defun translator (id)
  (ecase (block-data:data id :name)
    ;;Nothing
    ((:air) 0)
    ;;Land
    ((:grass :dirt :stone :sand :gravel) 1)
    ;;Plants
    ((:leaves :log) 2)
    ;;Artificial
    ((:planks) 3)))
"
bool[][][][] wave;
bool[][][] changes;
int[][][] observed;
double[] stationary;"
(defconstant +true+ 1)
(defconstant +false+ 0)

(defparameter *fmx* 20)
(defparameter *fmy* 20)
(defparameter *fmz* 20)
;;The number of possible outcomes
;;FIXME
;;The total pairs of tiles.
(defparameter *t* (length *array*))
;;The weight of each pairing. Just assign it to the weight of each tile.
(defparameter *stationary*
  (let ((total (total-count)))
    (map 'vector (lambda (pair)
		   (let ((count (cdr pair)))
		     (* 1.0 (/ count total))))
	 *array*)))
(defparameter *logprob* (map 'vector 'log *stationary*))
;;*propagator* determines whether two neighborhoods can coexist.
;;Currently only x+,x-,y+,y-,z+,z-, AKA cells 1 manhattan distance away.
(defparameter *test1*
  '((#(0 1 0) . 1)
    (#(0 0 2) . 1)
    (#(0 1 0) . 1)))
(defparameter *test2*
  '((#(0 1 0) . 1)
    (#(0 0 2) . 1)
    (#(3 0 0) . 1)))
(defparameter *test3*
  '((#(3 0 0) . 5)
    (#(0 0 2) . 1)
    (#(3 0 0) . 1)))

(defun set-type (alist)
  (let ((things nil)
	(type :independent))
    (dolist (pair alist)
      (let ((thing (assoc (car pair) things :test 'equalp)))
	;;(print things)
	;;(print (list pair thing))
	(cond (thing (if (equalp (cdr pair)
				 (cdr thing))
			 (setf type :overlap)
			 (return-from set-type :conflict)))
	      (t (push pair things)))))
    type))
;;Overlap
;;conflict
;;independent
(defun interference-p (pack1 pack2 direction)
  (multiple-value-bind (p11 p12 d1) (unpack2 pack1)   
    (multiple-value-bind (p21 p22 d2) (unpack2 pack2)
      (let* ((p11-pos #(0 0 0))
	     (p12-pos (vec+ p11-pos (aux d1)))
	     (p21-pos (vec+ p11-pos (aux direction)))
	     (p22-pos (vec+ p21-pos (aux d2))))
	(let ((foo
	       `((,p11-pos . ,p11)
		 (,p12-pos . ,p12)
		 (,p21-pos . ,p21)
		 (,p22-pos . ,p22))))
	  (let ((intersection-type (set-type foo)))
	    intersection-type))))))

;;#+nil
(defun compat (&optional (fun (constantly nil)))
  (let ((times 0))
    (utility:dohash
	(3x3_1 v1) *hash*
	(let ((w1 (explode-3x3 3x3_1)))
	  (utility:dohash
	      (3x3_2 v2) *hash*
	      (let ((w2 (explode-3x3 3x3_2)))
		(dotimes (side 6)
		  (when (compatible-p w1 w2 side)
		    (funcall fun side (car v1) (car v2))
		    (incf times)))))))
    (values times
	    (/ (* 1.0 times)
	       (* 6 (expt (hash-table-count *hash*) 2))))))

(defparameter *propagator*
  (let ((arr (make-array (list 6 *t* *t*) :element-type 'bit :initial-element +false+)))
    (let ((fun (lambda (side p1 p2)
		 (setf (aref arr side p1 p2)
		       +true+))))
      (compat fun)
      #+nil
      (utility:dohash
       (w1 v1) *hash*
       (utility:dohash
	(w2 v2) *hash*
	(dotimes (side 6)
	  (let ((foo (interference-p w1 w2 side)))
	    (when (member foo '(:overlap :independent))
	      (funcall fun side (car v1) (car v2))))))))
    arr))

;;How to set *t*, *stationary*, and *propagator?*

(defparameter *logt* (log *t*))
(defparameter *wave* (let ((arr (make-array (list *fmx* *fmy* *fmz*))))
		       (dotimes (i (array-total-size arr))
			 (setf (row-major-aref arr i)
			       (make-array *t* :element-type 'bit :initial-element +true+)))
		       arr))
(defparameter *changes* (make-array (list *fmx* *fmy* *fmz*) :element-type 'bit :initial-element +false+))
(defparameter *observed* (make-array (list *fmx* *fmy* *fmz*)))

(defparameter *changes-queue* (queue:make-uniq-q :hash (make-hash-table :test 'eq)))
;;FIXME::assumes fixnums contain at least 48 bits.
(defun pack16 (x y z)
  (dpb x (byte 16 0)
       (dpb y (byte 16 16) 
	    (dpb z (byte 16 32)
		 0))))
(defun unpack16 (pack)
  (values (ldb (byte 16 0) pack)
	  (ldb (byte 16 16) pack)
	  (ldb (byte 16 32) pack)))
(defun push-changes-queue (x y z)
  (let ((change (pack16 x y z)))
    (queue:uniq-push change *changes-queue*)))
(defun do-changes-queue (fun)
  (loop :named out :do
     (multiple-value-bind (value existp) (queue:uniq-pop *changes-queue*)
       (if existp
	   (multiple-value-call fun (unpack16 value))
	   (return-from out)))))

(defun observe ()
  ;;(declare (optimize speed (space 0)))
  (let ((min 1e+3) sum main_sum log_sum noise entropy
	(argminx -1)
	(argminy -1)
	(argminz -1)
	(amount 0)
	w)
    ;;FIXME::dobox code duplicated, iterating over arrays
    ;;Iterate through the box
    (utility:dobox
	((x 0 *fmx*)
	 (y 0 *fmy*)
	 (z 0 *fmz*))
      ;;w is the state at the point in wave     
      (setf w (aref *wave* x y z))
      (setf amount 0)
      (setf sum 0)

      (when (zerop (count +true+ w))
	(when (log:info)
	  (print "contradiction found, no possibilities.")
	  (return-from observe t)))
      
      ;;Iterate through the possible outputs
      (dotimes (_t *t*)
	(when (= +true+ (sbit w _t))
	  (incf amount)
	  (incf sum (aref *stationary* _t))))
      (when (zerop sum)
	(return-from observe nil))
      (setf noise (* (random 1.0) 1e-6))
      (cond ((= amount 1)
	     ;;(print "a")
	     (setf entropy 0))
	    ((= amount *t*)
	     ;;(print "b")
	     (setf entropy *logt*))
	    (t
	     (setf main_sum 0)
	     (setf log_sum (log sum))
	     (dotimes (_t *t*)
	       (when (= +true+ (sbit w _t))
		 (incf main_sum (* (aref *stationary* _t)
				   (aref *logprob* _t)))))
	     (setf entropy (- log_sum (/ main_sum sum)))))
      
      (when (and (plusp entropy)
		 (< (+ entropy noise)
		    min))
	(when (log:debu1)
	  (print "minimum being found"))
	(setf min (+ entropy noise)
	      argminx x
	      argminy y
	      argminz z)))
    ;;Nothing is observed, so end the entire algorithm.
    ;;argminx,y,z are unchanged, so nothing happens, just exit.
    (when (and (= -1 argminx)
	       (= -1 argminy)
	       (= -1 argminz))
      (utility:dobox
	((x 0 *fmx*)
	 (y 0 *fmy*)
	 (z 0 *fmz*))
	(let ((w (aref *wave* x y z)))
	  (block break
	    (dotimes (_t *t*)
	      (when (= +true+ (sbit w _t))
		(setf (aref *observed* x y z) _t)
		(return-from break))))))
      (when (log:info)
	(print "No changes observed, exiting"))
      (return-from observe t))
    ;; double[] distribution = new double[T];
    ;;         //Stationary[T] contains weights?
    ;; 	for (int t = 0; t < T; t++) distribution[t] = wave[argminx][argminy][argminz][t] ? stationary[t] : 0;
    ;; // selects a random element of the array?
    ;; 	int r = distribution.Random(random.NextDouble());
    ;;         //Collapse the lowest entropy spot?
    ;; 	for (int t = 0; t < T; t++) wave[argminx][argminy][argminz][t] = t == r;
    ;; //;Same as above?
    ;; 	for (int t = 0; t < T; t++) wave[argminx][argminy][argminz][t] = (t == r);
    ;; 	changes[argminx][argminy][argminz] = true;

    ;; 	return null;
    (let ((w (aref *wave* argminx argminy argminz))
	  distribution)
      (setf distribution
	    (map 'vector (lambda (bit stationary-value)
			   (if (= +true+ bit)
			       stationary-value
			       0))
		 w
		 *stationary*))
      ;;This picks a random selection and collapses it.
      (let ((r (pick-random distribution)))
	(dotimes (_t *t*)
	  (setf (sbit w _t) (if (= _t r) +true+ +false+))))
      (progn
	(setf (aref *changes* argminx argminy argminz) +true+)
	(push-changes-queue argminx argminy argminz))
      
      (return-from observe (values nil)))))

(defun pick-random (&optional (distribution #(1 0 20 3 0)))
  (let* ((total (reduce '+ distribution))
	 (stop (random (+ 0.0 total))))
    (dotimes (i (length distribution))
      (let ((probability (aref distribution i)))
	(decf stop probability)
	(when (<= stop 0)
	  (return-from pick-random i))))))

"
bool? Observe()
	{
		double min = 1E+3, sum, mainSum, logSum, noise, entropy;
		int argminx = -1, argminy = -1, argminz = -1, amount;
		bool[] w;

                //Iterate through the box
		for (int x = 0; x < FMX; x++) for (int y = 0; y < FMY; y++) for (int z = 0; z < FMZ; z++)
				{
					w = wave[x][y][z];
                                        //w is the state at the point in wave     
					amount = 0;
					sum = 0;
                                       
                                        //Iterate through the possible outputs
					for (int t = 0; t < T; t++) if (w[t])
						{
							amount += 1;
                                                        //is this the weight?
							sum += stationary[t];
						}
                                        
					if (sum == 0) return false;

					noise = 1E-6 * random.NextDouble();

					if (amount == 1) entropy = 0;
					else if (amount == T) entropy = logT;
					else
					{
						mainSum = 0;
						logSum = Math.Log(sum);
						for (int t = 0; t < T; t++) if (w[t]) mainSum += stationary[t] * logProb[t];
						entropy = logSum - mainSum / sum;
					}

					if (entropy > 0 && entropy + noise < min)
					{
						min = entropy + noise;
						argminx = x;
						argminy = y;
						argminz = z;
					}
				}

                //Nothing is observed, so end the entire algorithm.
                //argminx,y,z are unchanged, so nothing happens, just exit.
		if (argminx == -1 && argminy == -1 && argminz == -1)
		{
			for (int x = 0; x < FMX; x++) for (int y = 0; y < FMY; y++) for (int z = 0; z < FMZ; z++) for (int t = 0; t < T; t++) if (wave[x][y][z][t])
							{
								observed[x][y][z] = t;
								break;
							}						

			return true;
		}		

		double[] distribution = new double[T];
                //Stationary[T] contains weights?
		for (int t = 0; t < T; t++) distribution[t] = wave[argminx][argminy][argminz][t] ? stationary[t] : 0;
		int r = distribution.Random(random.NextDouble());
                //Collapse the lowest entropy spot?
		for (int t = 0; t < T; t++) wave[argminx][argminy][argminz][t] = t == r;
		changes[argminx][argminy][argminz] = true;

		return null;
	}
"
(defun c (direction)
  (ecase direction
    (:x+ 0)
    (:y+ 1)
    (:x- 2)
    (:y- 3)
    (:z+ 4)
    (:z- 5)))

(defun test89 ()
  (interference-p (pack2 2 4 (c :z+))
		  (pack2 1 1 (c :y+))
		  (c :z+)))

(defun propagate (&optional (periodic t) &aux (change nil))
  ;;(declare (optimize speed (safety 0)))
  (when (log:debug)
    (print "propagating"))
  (flet
      ((check (x1 y1 z1)
	 ;;#+nil
	 (macrolet ((_continue ()
		      `(return-from skip)))
	   (dotimes (d 6)
	     (block skip
	       (let ((x2 x1)
		     (y2 y1)
		     (z2 z1))
		 ;;0 -> 1 0 0
		 ;;1 -> 0 1 0
		 ;;2 -> -1 0 0
		 ;;3 -> 0 -1 0
		 ;;4 -> 0 0 1
		 ;;5 -> 0 0 -1
		 (ecase d
		   (0
		    (if (= x1 (- *fmx* 1))
			(if (not periodic)
			    (_continue)
			    (setf x2 0))
			(setf x2 (+ x1 1))))
		   (1
		    (if (= y1 (- *fmy* 1))
			(if (not periodic)
			    (_continue)
			    (setf y2 0))
			(setf y2 (+ y1 1))))
		   (2
		    (if (= x1 0)
			(if (not periodic)
			    (_continue)
			    (setf x2 (- *fmx* 1)))
			(setf x2 (- x1 1))))
		   (3
		    (if (= y1 0)
			(if (not periodic)
			    (_continue)
			    (setf y2 (- *fmy* 1)))
			(setf y2 (- y1 1))))
		   (4
		    (if (= z1 (- *fmz* 1))
			(if (not periodic)
			    (_continue)
			    (setf z2 0))
			(setf z2 (+ z1 1))))
		   (5
		    (if (= z1 0)
			(if (not periodic)
			    (_continue)
			    (setf z2 (- *fmz* 1)))
			(setf z2 (- z1 1)))))
		 (when (= +false+ (aref *changes* x2 y2 z2))
		   (_continue))
		 ;;This compares the states of adjacent cells.
		 ;;FIXME::*changes* is always checked????
		 (let ((w1 (aref *wave* x1 y1 z1)) ;;w1 has changed
		       (w2 (aref *wave* x2 y2 z2))) ;;so w2, an adjacent cell, might be updated
		   ;;constraing propagation
		   (dotimes (_t1 *t*)
		     (when (= +true+ (sbit w1 _t1))
		       (flet ((is-w1-possible-next-to-w2 ()
				(dotimes (_t2 *t*)
				  (when (= +true+ (sbit w2 _t2))
				    (when (= +true+ (aref *propagator* d _t1 _t2))
				      (return-from is-w1-possible-next-to-w2 t))))))
			 (when (not (is-w1-possible-next-to-w2))
			   ;;Eliminate this possibilitiy because it is not possible.
			   ;;It is not possible because it is not compatible with
			   ;;any possibilities found in w1.
			   (setf (sbit w1 _t1) +false+)
			   (progn
			     (setf (aref *changes* x1 y1 z1) +true+)
			     (progn
			       (push-changes-queue x1 y1 z1)
			       (push-changes-queue x2 y2 z2)))
			   (setf change t))))))))))))
    ;;#+nil
    (utility:dobox
     ((x 0 *fmx*)
      (y 0 *fmy*)
      (z 0 *fmz*))
     (check x y z))
    #+nil
    (do-changes-queue #'check))
  (return-from propagate (values change)))
	   

	   ;; bool[] w1 = wave[x1][y1][z1];
	   ;; 					bool[] w2 = wave[x2][y2][z2];
	   ;;                                         //Here we compare w1 to w2
	   ;;                                         //w2 is the block examined, w1 is the adjacent block
	   ;; 					for (int t2 = 0; t2 < T; t2++) if (w2[t2])
	   ;; 						{
	   ;;                                                         //d is the side
	   ;; 							bool[] prop = propagator[d][t2];
	   ;; 							b = false;

	   ;; 							for (int t1 = 0; t1 < T && !b; t1++) if (w1[t1]) b = prop[t1];
	   ;; 							if (!b)
	   ;; 							{
	   ;; 								w2[t2] = false;
	   ;; 								changes[x2][y2][z2] = true;
	   ;; 								change = true;
	   ;; 							}
	   ;; 						}

"
	bool Propagate()
	{
		bool change = false, b;
                //Iterate across every voxel and the 6 sides of the cube
		for (int x2 = 0; x2 < FMX; x2++) for (int y2 = 0; y2 < FMY; y2++) for (int z2 = 0; z2 < FMZ; z2++) for (int d = 0; d < 6; d++)
					{
						int x1 = x2, y1 = y2, z1 = z2;
						if (d == 0)
						{
                                                        //For edge
							if (x2 == 0)
							{
								if (!periodic) continue;
								else x1 = FMX - 1;
							}
							else x1 = x2 - 1;
						}
						else if (d == 1)
						{
                                                        //For edge
							if (y2 == FMY - 1)
							{
								if (!periodic) continue;
								else y1 = 0;
							}
							else y1 = y2 + 1;
						}
						else if (d == 2)
						{
                                                        //For edge
							if (x2 == FMX - 1)
							{
								if (!periodic) continue;
								else x1 = 0;
							}
							else x1 = x2 + 1;
						}
						else if (d == 3)
						{
                                                        //For edge
							if (y2 == 0)
							{
								if (!periodic) continue;
								else y1 = FMY - 1;
							}
							else y1 = y2 - 1;
						}
						else if (d == 4)
						{
                                                        //For edge
							if (z2 == FMZ - 1)
							{
								if (!periodic) continue;
								else z1 = 0;
							}
							else z1 = z2 + 1;
						}
						else
						{
                                                        //For edge/ wrap-around
							if (z2 == 0)
							{
								if (!periodic) continue;
								else z1 = FMZ - 1;
							}
							else z1 = z2 - 1;
						}
                                                //This spot did not change, so skip it.
						if (!changes[x1][y1][z1]) continue;

						bool[] w1 = wave[x1][y1][z1];
						bool[] w2 = wave[x2][y2][z2];
                                                //Here we compare w1 to w2
                                                //w2 is the block examined, w1 is the adjacent block
						for (int t2 = 0; t2 < T; t2++) if (w2[t2])
							{
                                                                //d is the side
								bool[] prop = propagator[d][t2];
								b = false;

								for (int t1 = 0; t1 < T && !b; t1++) if (w1[t1]) b = prop[t1];
								if (!b)
								{
									w2[t2] = false;
									changes[x2][y2][z2] = true;
									change = true;
								}
							}
					}				
                //When propagations are over
		return change;
	}
"
(defun run ()

  (clear)
  #+nil
  (dotimes (x 2)
    (seed))
  (catch :out
    (loop (per-frame)))
  #+nil
  (utility:dobox
   ((x 0 *fmx*)
    (y 0 *fmy*)
    (z 0 *fmz*))
   (let ((possibilities (aref *wave* x y z)))
     ;;(print (count +true+ possibilities))
     )))

(defun per-frame ()
  (let ((result (observe)))
    (when (log:debug)
      (write-out)
      (print (total-possibilities)))
    (when result
      (throw :out nil))
    (loop :while (Propagate))))

(defun total-possibilities ()
  (let ((acc 0)
	(non-one 0)
	(counts (make-hash-table :test 'eq)))
    (utility:dobox
     ((x 0 *fmx*)
      (y 0 *fmy*)
      (z 0 *fmz*))
     (let* ((possibilities (aref *wave* x y z))
	    (count (count +true+ possibilities)))
       (incf (gethash count counts 0))
       (when (not (= count 1))
	 (incf non-one)
	 (incf acc (1- count)))))
    (values
     acc
     ;;counts
     ;;(list acc non-one)
     )))

(defun seed ()
  (let
      ((x (random *fmx*))
       (y (random *fmy*))
       (z (random *fmz*)))
    (let* ((possibilities (aref *wave* x y z)))
      (setf (sbit possibilities (random *t*)) +false+))))

(defun ones ()
  (let (acc)
    (utility:dobox
     ((x 0 *fmx*)
      (y 0 *fmy*)
      (z 0 *fmz*))
     (let* ((possibilities (aref *wave* x y z))
	    (count (count +true+ possibilities)))
       (when (= count 1)
	 (push (position +true+ possibilities) acc))))
    (values acc)))

"
	public bool Run(int seed)
	{
		logT = Math.Log(T);
		logProb = new double[T];
		for (int t = 0; t < T; t++) logProb[t] = Math.Log(stationary[t]);

		Clear();

		random = new Random(seed);

		while (true)
		{
			bool? result = Observe();
			if (result != null) return (bool)result;
			while (Propagate()) ;
		}
	}
"
(defun clear ()
  (utility:dobox
   ((x 0 *fmx*)
    (y 0 *fmy*)
    (z 0 *fmz*))
   (fill (aref *wave* x y z) +true+)
   (dotimes (_t *t*)
     (setf (sbit *changes* x y z) +false+)))
  (setf *changes-queue* (queue:make-uniq-q))
  ;;Ground not implemented
  )
"
	void Clear()
	{
		for (int x = 0; x < FMX; x++) for (int y = 0; y < FMY; y++) for (int z = 0; z < FMZ; z++)
				{
					for (int t = 0; t < T; t++) wave[x][y][z][t] = true;
					changes[x][y][z] = false;
				}
		
		if (ground >= 0)
		{
			for (int x = 0; x < FMX; x++) for (int y = 0; y < FMY; y++)
				{
					for (int t = 0; t < T; t++) if (t != ground) wave[x][y][FMZ - 1][t] = false;
					changes[x][y][FMZ - 1] = true;

					for (int z = 0; z < FMZ - 1; z++)
					{
						wave[x][y][z][ground] = false;
						changes[x][y][z] = true;
					}
				}			
		}	
	}
"

(defun reverse-translator (id)
  (block-data:lookup
   (ecase id
     ;;Nothing
     (0 :air)
     ;;Land
     (1 :stone)
     ;;Plants
     (2 :leaves)
     ;;Artificial
     (3 :planks))))

(defun write-out ()
  (let ((basex 0)
	(basey 110)
	(basez 0))
    (utility:dobox
     ((x0 0 *fmx*)
      (y0 0 *fmy*)
      (z0 0 *fmz*))
     (let ((x (+ x0 basex))
	   (y (+ y0 basey))
	   (z (+ z0 basez)))
       (cond ((= 1 (count +true+ (aref *wave* x0 y0 z0)))
	      (let ((cell (aref *array* (aref *observed* x0 y0 z0))))
		;;(explode-3x3 )
		(;;multiple-value-bind (a b d) (unpack2 (car cell))
		 let* ((3x3 (explode-3x3 (car cell)))
		       (a (aref 3x3 1 1 1)))
		  ;;(print 3x3)
		  (let ((bid (reverse-translator a)))
		    (when (= 0 bid)
		      (setf bid (block-data:lookup :sand)))
		    (world:plain-setblock x y z bid)))))
	     (t (world:plain-setblock x y z (block-data:lookup :air))))))))
