;;;    (setq llung_m nil)
    (setq lungminlana 20)
    (setq lunglana15 80)
    (setq lunglana20 100)
    (cond ((and (not tagliver) (not taglihor)(= lung2 0) )
	   ;Pannello piegato senza tagli
	   (if (/= "Si" forisn) (setq lunglana15 lunglana20))
	   (foreach ll LLUNG_M_x_t
	     (setq l_lung (+ (car ll) (cadr (assoc 'diflungldr parametri))))
	     
	     (setq lx nil)
;;;	     (if (= (/ (cadr (assoc 'diflungldr parametri)) 2)) (setq a 1))
	     (setq l1 (list (car (read pieghe)) (nth 1 ll) (nth 2 ll) (nth 3 ll) (nth 4 ll) (nth 5 ll)))
	     (setq lungresto (- l_lung (nth 4 ll) (nth 4 ll) (car (read pieghe)) (cadr (read pieghe))))
	     (setq pos "S")
	       (setq pposs 0)
	       (setq pposd 0)
	     (while (> lungresto 0)
	       (if (> lungresto (+ lunglana15 lungminlana))
		 (progn
		   (setq lungresto (- lungresto lunglana15))
		   (if (= pos "S")
		     (progn
		       (setq lx (cons (list lunglana15 (nth 1 ll) (nth 2 ll) (nth 3 ll) (+ pposs (car (read pieghe)) (nth 4 ll))) lx))
		       (setq pos "D")
		       (setq pposs (+ lunglana15 pposs))
		       )
		     (progn
		       (setq lx (cons (list lunglana15 (nth 1 ll) (nth 2 ll) (nth 3 ll) (- l_lung pposd lunglana15 (cadr (read pieghe)) (nth 4 ll))) lx))
		       (setq pos "S")
		       (setq pposd (+ lunglana15 pposd))
		       
		       )

		     )
		   )
                  (progn
;;;		   (setq lungresto (- lungresto lunglana15))
		    (if (< lungresto lunglana15)
		      (if (= pos "S")
			(progn
			  (setq lx (cons (list lungresto (nth 1 ll) (nth 2 ll) (nth 3 ll) (+ pposs (car (read pieghe)) (nth 4 ll))) lx))
			  (setq pos "D")
			  (setq pposs (+ lungresto pposs))
			  		   (setq lungresto 0)

			  )
			(progn
			  (setq lx (cons (list lungresto (nth 1 ll) (nth 2 ll) (nth 3 ll) (- l_lung pposd lungresto (cadr (read pieghe)) (nth 4 ll))) lx))
			  (setq pos "S")
			  (setq pposd (+ lungresto pposd))
			  		   (setq lungresto 0)
			  )
			
			)
			(progn
			  (setq lx (cons (list (/ lungresto 2) (nth 1 ll) (nth 2 ll) (nth 3 ll) (+ pposs (car (read pieghe)) (nth 4 ll))) lx))
			  (setq lx (cons (list (/ lungresto 2) (nth 1 ll) (nth 2 ll) (nth 3 ll) (- l_lung pposd (/ lungresto 2) (cadr (read pieghe)) (nth 4 ll))) lx))
			  		   (setq lungresto 0)
			  )
		   )
	       )
		 )
	       )
	     (setq ln (list (cadr (read pieghe)) (nth 1 ll) (nth 2 ll) (nth 3 ll) (- l_lung (nth 4 ll) (cadr (read pieghe))) (nth 5 ll)))
	     )
	   )
	  ((and (not tagliver) (not taglihor)(/= lung2 0) )
	   ;Pannello piegato senza tagli
	   (if (/= "Si" forisn) (setq lunglana15 lunglana20))
	   (foreach ll LLUNG_M_x_t
	     (setq l_lung (+ (car ll) (/ (cadr (assoc 'diflungldr parametri)) 2)))
	     
	     (setq lx nil)
;;;	     (if (= (/ (cadr (assoc 'diflungldr parametri)) 2)) (setq a 1))
             (setq l1 (list (car (read pieghe)) (nth 1 ll) (nth 2 ll) (nth 3 ll) (nth 4 ll) (nth 5 ll)))
	     (setq ln (list (cadr (read pieghe)) (nth 1 ll) (nth 2 ll) (nth 3 ll) (- l_lung (nth 4 ll) (cadr (read pieghe))) (nth 5 ll)))
	     (if (< (nth 4 ll) lung)
	       (progn
		 (setq l1 (list (car (read pieghe)) (nth 1 ll) (nth 2 ll) (nth 3 ll) (nth 4 ll) (nth 5 ll)))
		 (setq ln (list 0 (nth 1 ll) (nth 2 ll) (nth 3 ll) (- l_lung (nth 4 ll) (cadr (read pieghe))) (nth 5 ll)))
		 
		 (setq lungresto (- l_lung (/ (cadr (assoc 'diflungldr parametri)) 2) (car (read pieghe)) ))
	     	       (setq pposs (+ (nth 4 ll) (car l1)))
		 (setq pposd (- (+ (nth 4 ll) (car ll)) 0))
		 )
	       (progn
		 (setq l1 (list 0 (nth 1 ll) (nth 2 ll) (nth 3 ll) (nth 4 ll) (nth 5 ll)))
		 (setq ln (list (cadr (read pieghe)) (nth 1 ll) (nth 2 ll) (nth 3 ll) (- l_lung (nth 4 ll) (cadr (read pieghe))) (nth 5 ll)))
	         (setq lungresto (- l_lung (/ (cadr (assoc 'diflungldr parametri)) 2)  (cadr (read pieghe))))
	          (setq pposd (- (+ (nth 4 ll) (car ll)) (car ln)))
		 (setq pposs (+ 0 (nth 4 ll)))
		 )
	       )
	     (setq pos "S")
;	       (setq pposs (/ (cadr (assoc 'diflungldr parametri)) 2))
;;;	       (setq pposd (- 0 (/ (cadr (assoc 'diflungldr parametri)) 2)))
	     
	     
	     (while (> lungresto 0)
	       (if (> lungresto (+ lunglana15 lungminlana))
		 (progn
		   (setq lungresto (- lungresto lunglana15))
		   (if (= pos "S")
		     (progn
		       (setq lx (cons (list lunglana15 (nth 1 ll) (nth 2 ll) (nth 3 ll) (+ pposs 0)) lx))
		       (setq pos "D")
		       (setq pposs (+ lunglana15 pposs))
		       )
		     (progn
		       (setq lx (cons (list lunglana15 (nth 1 ll) (nth 2 ll) (nth 3 ll) (- pposd lunglana15 )) lx))
		       (setq pos "S")
		       (setq pposd (- pposd lunglana15))
		       
		       )

		     )
		   )
                  (progn
;;;		   (setq lungresto (- lungresto lunglana15))
		    (if (< lungresto lunglana15)
		      (if (= pos "S")
			(progn
			  (setq lx (cons (list lungresto (nth 1 ll) (nth 2 ll) (nth 3 ll) (+ pposs (car (read pieghe)) )) lx))
			  (setq pos "D")
			  (setq pposs (+ lungresto pposs))
			  		   (setq lungresto 0)

			  )
			(progn
			  (setq lx (cons (list lungresto (nth 1 ll) (nth 2 ll) (nth 3 ll) (- pposd lungresto )) lx))
			  (setq pos "S")
			  (setq pposd (+ lungresto pposd))
			  		   (setq lungresto 0)
			  )
			
			)
			(progn
			  (setq lx (cons (list (/ lungresto 2) (nth 1 ll) (nth 2 ll) (nth 3 ll) (+ pposs 0)) lx))
			  (setq lx (cons (list (/ lungresto 2) (nth 1 ll) (nth 2 ll) (nth 3 ll) (- pposd (/ lungresto 2) 0)) lx))
			  		   (setq lungresto 0)
			  )
		   )
	       )
		 )
	       )
	     )
	   )
	  )
