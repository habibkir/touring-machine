#|
definizione dovrebbe essere sul

(defmacine machine-name
 (q1 ((symbols) (actions) final-config)
     ((symbols) (actions) final-config)
     (et cetera))
 (q2 ((symbols) (actions) final-config)
     (et cetera))
 (q3 ((symbols) (actions) final-config)
     (et cetera))
(et cetera))
|#

(defmacro while (condition &rest body)
  `(do ()
       ((not ,condition))
       ,@body))

(defparameter *default-tape-size* 100)
(defparameter *default-initial-pointer* 0)

(defstruct (turing-machine
	    (:conc-name tm-))
  (m-configs nil)
  (tape nil))

(defstruct (tape (:print-function print-tape))
  (buf nil)
  (ptr nil))

(defparameter *tape-max-cols-print* 12)

(defun print-tape (m-tape stream depth)
    (dotimes (index (length (tape-buf m-tape)))
      (when (eq 0 (mod index *tape-max-cols-print*))
	(fresh-line))
      (if (eq index (tape-ptr m-tape))
	  (format stream "[~A] " (elt (tape-buf m-tape) index))
	  (format stream " ~A  " (elt (tape-buf m-tape) index)))))

#|
tape-ptr e tape-buf non hanno effetti collaterali e posso permettermi una
doppia esecuzione senza troppi problemi
avrei fatto una funzione ma poi non si metteva bene col setf
|#
(macrolet ((tape-curr (tape) `(elt (tape-buf ,tape) (tape-ptr ,tape))))
	  (defun head-read (machine)
	    (tape-curr (tm-tape machine)))
	  (defun head-print (machine sym)
	    (setf (tape-curr (tm-tape machine)) sym)))

(defun make-blank-tape (&optional size ptr)
  (make-tape :buf (make-array (if size size *default-tape-size*)
			      :initial-element nil)
	     :ptr (if ptr ptr *default-initial-pointer*)))

(defmacro defmachine (machine-name &rest configs)
  `(defparameter ,machine-name
     (make-turing-machine :m-configs ',configs
			  :tape (make-blank-tape
				 *default-tape-size*
				 *default-initial-pointer*))))

;;; vedi specifica di m-configurazione in defmachine per vedere su che lista agisce
(defun find-among (lst tar)
  (if (null lst)
      nil
    (if (or (and (member 'any (caar lst)) tar) ;list has any, tar is not nil
	    (member tar (caar lst)))
	(car lst)
      (find-among (cdr lst) tar))))

(defun prompt (msg)
  (format t "~A~%> " msg)
  (read))

#|
le stringhe numeriche non vengono tradotte a numeri
il che non sarebbe sto gran problema ma rompe il cazzo con la scrittura
farei una bella macrona che ti legge 0 e mette |0| se sapessi e avesso lo sbatti
non solum nescio, sed sbatti etiam non habeo
|#
(defun what-to-print (name)
  (cond ((numberp (read-from-string name))
	 (read-from-string name))
	(t (intern name))))

(defun eval-atom (atom machine)
  (case atom
	((l) (decf (tape-ptr (tm-tape machine))))
	((r) (incf (tape-ptr (tm-tape machine))))
	((e) (head-print machine nil))
	(otherwise
	 (let ((name (symbol-name atom)))
	   (case (char name 0)
		 ((#\P) (head-print
			 machine
			 (what-to-print (subseq name 1)))))))))

;;; silentium compilerium!
(defun eval-complex-inst (machine instruction)
  (+ machine instruction))

(defun eval-single-inst (instruction machine)
  (if (atom instruction)
      (eval-atom instruction machine)
    (eval-complex-inst instruction machine)))

(defun eval-insts (machine instructions)
  (mapc #'(lambda (instruction)
	    (eval-single-inst instruction machine))
	instructions))

(defun tm-step (machine config-name)
  (let ((config-spec (assoc config-name (tm-m-configs machine))))
    (let ((config-row (find-among (cdr config-spec) (head-read machine))))
      (format t "config-spec => ~A~%reading : ~A~%config-row => ~A~%"
	      config-spec (head-read machine) config-row)
      (let ((instructions (cadr config-row))
	    (final-m-config (caddr config-row)))
	;; format per ui/debug, possono essere ignorati da chiunque stia debuggando
	(format t "current m-config : ~A~%next m-config : ~A~%tape:~%~A~%"
		config-name final-m-config (tm-tape machine))
	(eval-insts machine instructions)
	(if (prompt "Continue? [t/nil]")
	    (tm-step machine final-m-config))))))

;;; tests
(defmachine test
  (q0 ((nil) (p0 r) q1))
  (q1 ((nil) (p1 r) q0)))

(defmachine turing-1
  (b ((nil) (p0) b)
     ((1) (r r p0) b)
     ((0) (r r p1) b)))

(defmachine turing-fuckall
  (init
   ((any nil) (psh r psh r p0 r r p0 l l) o))
  (o
   ((1) (r px l l l) o)
   ((0) () q))
  (q
   ((0 1) (r r) q)
   ((nil) (p1 l) p))
  (p
   ((x) (e r) q)
   ((sh) (r) f)
   ((nil) (l l) p))
  (f
   ((any) (r r) f)
   ((nil) (p0 l l) o)))

(tm-step turing-fuckall 'init)
