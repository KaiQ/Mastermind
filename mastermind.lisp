(defparameter *Steckerfarben* 6)
(defparameter *Rundenanzahl* 10)
(defparameter *Steckeranzahl* 4)

(setf *random-state* (make-random-state t))

(defun Loesung (Steckeranzahl Steckerfarben)
(loop repeat Steckeranzahl collect (random Steckerfarben)))

(defun sortieren (liste)
(sort liste #'(lambda (X Y) (cond ((eql X 's) t) ((eql Y 'w) t) (t nil)))))

;(sortieren '(s w o s))
;(sortieren '(o o o o))
;(sortieren '(w w w w w w w w w s))

(defun Vergleich (Element pos Loesung) 
      (cond 	((eql Element (nth pos Loesung))    's)
		((member Element Loesung)           'w)
		 (t 			            'o)))

;(Vergleich 5 3 '(1 3 2 5))
;(Vergleich 5 2 '(5 1 4 6))
;(Vergleich 5 0 '(1 2 3 4))

(defun Vergleichslistetemp (Eingabeliste Loesungsliste)
(maplist #'(lambda (X) (Vergleich  (car X) 
				  (- (length Loesungsliste) (length X)) 
				  Loesungsliste)) Eingabeliste))

(defun Vergleichsliste (Eingabeliste Loesungsliste)
  (sortieren (Vergleichslistetemp eingabeliste loesungsliste)))

;(Vergleichsliste '(1 2 3 4) '(2 1 3 5))

(defun Eingabe (steckeranz) 
(format t "Eingabe:~%")
(let ((eing (ignore-errors (read-from-string (read-line)))))
    (cond ((not (listp eing)) (progn (format t "Bitte als Liste eingeben:~%")
				     (Eingabe steckeranz)))
	  ((not (every 'numberp eing)) (progn (format t "Bitte nur Zahlen eingeben:~%")
					      (Eingabe steckeranz)))
	  ((= steckeranz (length eing)) eing)
	  (t (progn (format t "Bitte ~d Listenelemente:~%" steckeranz)
		    (Eingabe steckeranz))))))

(defun mastermind (steckeranz steckerfarb rundenanz loesung spielfeld)
(format t "~%~{~A~%~}~%" spielfeld)
(let ((eing (eingabe steckeranz))) 
 (cond 	((equal loesung eing) (format t "~%Richtig gelöst~%"))
	((<= rundenanz 0) (format t "~%Rundenanzahl überschritten~%"))
	(t		(mastermind steckeranz steckerfarb (- rundenanz 1) loesung 
			(cons (cons eing (vergleichsliste eing loesung)) spielfeld ))))))

;(mastermind *Steckeranzahl* *Steckerfarben* *Rundenanzahl* (Loesung *Steckeranzahl* *Steckerfarben*) nil)
