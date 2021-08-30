(defun angolo_x (p1 p2 p3 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-Angoli" "")
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p2 p3)(setq ap2 (- $ap2$ (angle p3 p2) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2))
  (setq ap2 (/ pi 2))
  
	
  (setq p1d p1 p2d p2 p3d p3)
  (setq n1 (polar p1 (setq app1 (- (angle p2 p1) ap1)) 27.8))
  (setq n2 (polar n1 (- app1 (/ pi 2)) 28.5))
  (setq n3 (polar n2 (- app1 pi) 0.7))
  (SETQ N4 (POLAR N3 (- app1 (/ pi 2) pi) 27.8))
  
  
;;;  (SETQ N7 (POLAR N6 (- app1 PI ) 2.2))
;;;  
;;;  (SETQ N8 (POLAR N7 (+ app1 (/ pi 2)) 21.0))
  
  (SETQ N9 
  (inters n4
	  (POLAR N4 (- app1 PI ) 25.1)
	  (polar p1 (+ (angle p1 p2) (/ pi 2)) 0.7)
	  (polar p2 (+ (angle p1 p2) (/ pi 2)) 0.7)
	  nil))
  (if (null zpan) (setq zpan 0))
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" p2 "_non" p3
	  "_non" (setq pprofilo (setq px (polar p3 (setq app2 (+ (angle p2 p3) ap2)) 27.8)));piega1

	  "_NON" (POLAR P3 (+ (ANGLE P2 P3) ap2 pi) 1) "_non" px ;lineetta
	  
;;;	  "_non" (setq px (polar px (angle p2 p3) 4.181));punta
;;;	  "_non" (setq px (polar px (+ (angle p2 p3) 0.2618) 7.6126));vertice molla
;;;	  "_non" (setq pxx (setq px (polar px (- (angle p2 p3) 0.2618) 4.5119)))
;;;	  "_non" (setq pxx (setq px (polar px (- (angle p2 p3) 0.2618 (- 0 (/ pi 2))) 0.7)))
;;;	  "_non" (setq pxx (setq px (polar px (- (angle p3 p2) 0.2618) 4.7)))
;;;	  "_non" (setq pxx (setq px (polar px (+ (angle p3 p2) 0.2618) 7.8)))
	  "_non" (setq pxx (setq px (polar px (+ (angle p3 p2) 0) 0.7)))


	  "_non" (setq px (inters
			    (polar px (- app2 (* 2 (/ pi 2))) 26.6);torna
			 pxx
			 (polar p2 (+ (angle p2 p3) (/ pi 2)) 0.7)
			 (polar p3 (+ (angle p2 p3) (/ pi 2)) 0.7)
			 nil))
	  
	  "_non" (inters px (polar px (angle p3 p2) 0.1)
		   n9 (polar n9 (angle p1 p2) 0.1)
		   nil
		)
	  "_non" n9
;;;	  "_non" n8
;;;	  "_non" n7
;;;	  "_non" n6
;;;	  "_non" n5
	  "_non" (polar n1 (angle p1 p2) 0.7)
;;;	  "_non" n3
;;;	  "_non" n2
	  "_non" (setq pprofilo n1)
	  "_c"
	   )
    (allega (entlast) "PANFIN" fpan)
  (setq entlastpan (entlast))
  (if modop_d
    (progn
        (allega (entlast) "PANTIP" modop_d)
	(if (or (= modop_d "SPD")  (= modop_d "STD")) (command"_insert" "profilo1" "_non" pprofilo 1 1 (angtos (angle p2 p1)))))
    (if (or (= (car p1_nua) "AN1") (= tipoangolo 0) (= (rtos ang_prof 2 4) (rtos (angle p1 p2) 2 4))) (allega (entlast) "PANTIP" "STD") (allega (entlast) "PANTIP" "SPS"))
    )
  (allega (entlast) "PANRES" "B0")
    (if modop_d
    (allega (entlast) "PANTIP" modop_d)

    (if (or (= (car p1_nua) "AN1") (= tipoangolo 0) (= (rtos ang_prof 2 4) (rtos (angle p1 p2) 2 4))) (command"_insert" "profilo1" "_non" pprofilo 1 1 (angtos (angle p2 p1))))
    )

;;;  (allega (entlast) "PANFIN" fpan)
;;;  (if (or prk (= (car p1_nua) "AN1") (= tipoangolo 0) (= (rtos ang_prof 2 4) (rtos (angle p1 p2) 2 4))) (allega (entlast) "PANTIP" "STD") (allega (entlast) "PANTIP" "SPC"))
;;;  (allega (entlast) "PANRES" "B0")
;;;  (if (or prk (= (car p1_nua) "AN1") (= tipoangolo 0) (= (rtos ang_prof 2 4) (rtos (angle p1 p2) 2 4))) (command"_insert" "profilo1" "_non" pprofilo 1 1 (angtos (angle p2 p1))))

)



(DEFUN LANAPAN (ENTP ENTR)
  (SETQ MODO (LEGGI ENTP "PANTIP"))
  (IF (OR (= MODO "S3D") (= MODO "S4D") (= MODO "S4S")) (SETQ S 0.5)(SETQ S 0.3) )
  (SETQ P1_ (CDR (ASSOC '10 (ENTGET ENTP))))
  (SETQ P2_ (CDR (ASSOC '10 (CDR (MEMBER (ASSOC '10 (ENTGET ENTP)) (ENTGET ENTP))))))
  (SETQ PR1_ (CDR (ASSOC '10 (ENTGET ENTR))))
  (SETQ PR1__ PR1_)
    (setq lispt nil) (FOREACH N (ENTGET entr) (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))))
    (setq lispt (reverse lispt))
  (if (NOT (EQUAL '(0 0) POSPIEG))
    (SETQ pr2_ (nth 5 lispt))
    (SETQ PR2_ (nth 1 lispt))
    )
  (SETQ PR2__ PR2_)
  (SETQ P1_ (POLAR P1_ (ANGLE P1_ P2_) 0.5))
  (SETQ P1_ (POLAR P1_ (+ (ANGLE P1_ P2_) (/ PI 2)) 0.5))
  (SETQ P2_ (POLAR P2_ (ANGLE P2_ P1_) 0.5))
  (SETQ P2_ (POLAR P2_ (- (ANGLE P2_ P1_) (/ PI 2)) 0.5))
  (IF (OR (= MODO "SFS")(= MODO "S2S")(= MODO "S4S") (= MODO "SMS")) (SETQ PR1_X PR2_ PR2_ PR1_ PR1_ PR1_X))
  (SETQ PR1_ (POLAR PR1_ (ANGLE PR1_ PR2_) S))
  (IF (OR (= MODO "S2S")(= MODO "S4S")(= MODO "SFS") (= MODO "SMS")) (SETQ PR1_ (POLAR PR1_ (+ (ANGLE PR1__ PR2__) (/ PI 2)) S))
    (SETQ PR1_ (POLAR PR1_ (- (ANGLE PR1__ PR2__) (/ PI 2)) S)))
  (SETQ PR2_ (POLAR PR2_ (ANGLE PR2_ PR1_) S))
  (IF (OR (= MODO "S2S") (= MODO "S4S") (= MODO "SFS") (= MODO "SMS")) (SETQ PR2_ (POLAR PR2_ (- (ANGLE PR2__ PR1__) (/ PI 2)) S))
    (SETQ PR2_ (POLAR PR2_ (+ (ANGLE PR2__ PR1__) (/ PI 2)) S)))
  (SETQ DS1 0)
  (cond ((= modo "STD") (setq ds 0.3))
	((OR (= modo "SFD")) (setq ds 26.6))
	((OR (= modo "SFS")) (setq ds 26.6 DS1 0.8))
	
	((OR (= modo "SMD") ) (setq ds 0.8))
	((OR  (= modo "SMS")) (setq ds 0.8) (SETQ DS1 0.5))
	((OR (= modo "S2D") ) (setq ds 27.1))
	((OR  (= modo "S2S")) (setq ds 27.1) (SETQ DS1 1.3))
	((= modo "S3D") (setq ds 2.3))
	((OR (= modo "S4D") ) (setq ds 0.8))
	((OR  (= modo "S4S")) (setq ds 0.8) (SETQ DS1 0.5))
	)
	
  (COMMAND "_PLINE" "_NON" P1_)
  (if (OR (= modo "STD")(= modo "S4D")(= modo "S3D") (= modo "SFD") (= modo "SMD"))
    (COMMAND"_NON" (SETQ P (POLAR P1_ (+ (ANGLE P1_ P2_) (/ PI 2)) 11.0))
	    "_NON" (SETQ P (POLAR P (+ (ANGLE P1_ P2_) 0) 20.0))
	    "_NON" (SETQ P (POLAR P (+ (ANGLE P1_ P2_) (/ PI 2)) 2.2))
	    "_NON" (SETQ P (POLAR PR1_ (- (ANGLE P1_ P2_) (/ PI 2)) 11.0))
	    )
    )
  (IF (OR (= MODO "SFS")(= MODO "S2S")) (COMMAND "_NON" (POLAR P1_ (+ (ANGLE P1_ P2_) (/ PI 2)) (- 25 S 0.5))) (COMMAND  "_NON" PR1_))
  (IF (OR (= MODO "SFD") (= MODO "S2D"))
     (progn
       (if (NOT (EQUAL '(0 0) POSPIEG))
	 (progn
	   (setq ppl (getvar"lastpoint"))
	 (COMMAND "_non" (setq ppl (polar ppl (angle p1_ p2_) (+ DS1 (car pospieg))))
		  "_non" (setq ppl (polar ppl (- (angle p1_ p2_) (/ pi 2)) 10))
		  "_non" (setq ppl (polar ppl (- (angle p1_ p2_) 0) (-  (distance pr1_ pr2_) -0.3 DS (car pospieg) (cadr pospieg)) ))
  		  "_non" (setq ppl (polar ppl (+ (angle p1_ p2_) (/ pi 2)) 10))
;"100,30000"
		  )
	 )
	 )
       (command "_NON" (POLAR P2_ (+ (ANGLE P1_ P2_) (/ PI 2)) (- 25 S 0.5))))
    (progn
      (if (NOT (EQUAL '(0 0) POSPIEG))
	 (progn
	   (setq ppl (getvar"lastpoint"))
	 (COMMAND "_non" (setq ppl (polar ppl (angle p1_ p2_) (+ DS1 (car pospieg))))
		  "_non" (setq ppl (polar ppl (- (angle p1_ p2_) (/ pi 2)) 10))
		  "_non" (setq ppl (polar ppl (- (angle p1_ p2_) 0) (-  (distance pr1_ pr2_) -0.3 DS (car pospieg) (cadr pospieg)) ))
  		  "_non" (setq ppl (polar ppl (+ (angle p1_ p2_) (/ pi 2)) 10))
;"100,30000"
		  )
	 )
	 )
(command "_NON" PR2_)
      )
    )
    
  (if (OR (= modo "S3D")(= modo "S4S") (= modo "SFS") (= modo "SMS"))
    (COMMAND
            "_NON" (SETQ P (POLAR PR2_ (- (ANGLE P1_ P2_) (/ PI 2)) 11.0))
	    "_NON" (SETQ P (POLAR (SETQ PX2 (POLAR (SETQ PX1 (POLAR P2_ (+ (ANGLE P1_ P2_) (/ PI 2)) 11.0)) (+ (ANGLE P2_ P1_) 0) 20.0)) (+ (ANGLE P1_ P2_) (/ PI 2)) 2.2))
	    "_NON" PX2
	    "_NON" PX1
	    )
    )
	    
  (COMMAND "_NON" P2_ "_C")
  (ALLEGA ENTP "PANLANA" (CDR (ASSOC '5 (ENTGET (ENTLAST)))))
  (COMMAND"_LAYER" "_N" "pannelli-dritti-Lana" "")
  (command"_chprop" (entlast) "" "_la" "pannelli-dritti-Lana" "")
  )

(DEFUN RETROPAN (ENT)
  (SETQ POSPIEG '(60 100))
;;;  (SETQ POSPIEG '(0 0))
  (SETQ MODO (LEGGI ENT "PANTIP"))
  (SETQ P1 (CDR (ASSOC '10 (ENTGET ENT))))
  (SETQ P2 (CDR (ASSOC '10 (CDR (MEMBER (ASSOC '10 (ENTGET ENT)) (ENTGET ENT))))))
  (SETQ PR1 (POLAR P1 (ANGLE P1 P2) 1.0))
  (SETQ PR1 (POLAR PR1 (+ (ANGLE P1 P2) (/ PI 2)) (- 25.0 0)))
  (SETQ PR2 (POLAR P2 (ANGLE P2 P1) 1.0))
  (SETQ PR2 (POLAR PR2 (- (ANGLE P2 P1) (/ PI 2)) (- 25.0 0)))
  (COND ((= MODO "STD")
	 (COMMAND "_PLINE" "_NON" PR1)
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar pr1 (angle pr1 pr2) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 pr2) (/ pi 4)) 14.1421))
			   "_non" (setq pp (polar pp (angle pr1 pr2) (- (distance pr1 pr2) 20 (car pospieg) (cadr pospieg))))
   			   "_non" (setq pp (polar pp (+ (angle pr1 pr2) (/ pi 4)) 14.1421))
			   )
			   
	   )
	          (command "_NON" PR2
		  "_NON" (SETQ P (POLAR PR2 (- (ANGLE PR1 PR2) (/ PI 2)) 6.5))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 P1) 0.3)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PR1 PR2) (/ PI 2)) 6.2)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 pr1) (- (cadr pospieg) 0.42)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 pr1) (/ pi 4)) 14.1421))
			   "_non" (setq pp (polar pp (angle pr2 pr1) (- (distance pr1 pr2) 19.75 (car pospieg) (cadr pospieg))))
   			   "_non" (setq pp (polar pp (- (angle pr2 pr1) (/ pi 4)) 14.1421))
			   )
			   
	   )
	 (command
			   
		  ;LAMRETRO
;;;		  "_NON" (SETQ PP (POLAR (SETQ PP1 P) (ANGLE P2 P1) (CAR POSPIEG)))
			   
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE PR2 PR1) (- (DISTANCE PR1 PR2) 0.6))))
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PR2) (/ PI 2)) 6.2))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PR2 PR1) 0.3)))
		  

		  "_C")
	 )
	((= MODO "S3D")
	 (COMMAND "_PLINE" "_NON" (POLAR PR1 (ANGLE PR2 PR1) 1.0))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar (POLAR PR1 (ANGLE PR2 PR1) 0) (angle pr1 pr2) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 pr2) (/ pi 4)) 14.1421))
			   "_non" (setq pp (polar pp (angle pr1 pr2) (- (distance pr1 pr2) 20 (car pospieg) (cadr pospieg))))
   			   "_non" (setq pp (polar pp (+ (angle pr1 pr2) (/ pi 4)) 14.1421))
			   )
			   
	   )
	          (command
		  "_NON" (SETQ P (POLAR PR2 (ANGLE PR1 PR2) 1.0))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PR2) (/ PI 2)) 6.5))
		  "_NON" (SETQ P (POLAR P (ANGLE P2 P1) 0.5))
		  "_NON" (SETQ P (POLAR P (+ (ANGLE PR1 PR2) (/ PI 2)) 6.0)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 pr1) (+ 1.7129 (- (cadr pospieg) 1.42)) ))
			   "_non" (setq pp (polar pp (+ (angle pr2 pr1) (/ pi 4)) 14.1421))
			   "_non" (setq pp (polar pp (angle pr2 pr1) (- (distance pr1 pr2) 19.58 (car pospieg) (cadr pospieg))))
   			   "_non" (setq pp (polar pp (- (angle pr2 pr1) (/ pi 4)) 14.1421))
			   )
			   
	   )
	 (command
		  ;LAMRETRO
		  
		  "_NON" (SETQ P (POLAR P (ANGLE PR2 PR1) (+ (DISTANCE PR1 PR2) 1.0)))
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PR2) (/ PI 2)) 6.0))
		  "_NON" (SETQ P (POLAR P (ANGLE PR2 PR1) 0.5))
		  

		  "_C")
	 )
	((OR (= MODO "SFS") (= MODO "SFD"))
	 (COMMAND "_PLINE" "_NON" PR1)
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar pr1 (angle pr1 pr2) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 pr2) (/ pi 4)) 14.1421))
			   "_non" (setq pp (polar pp (angle pr1 pr2) (- (distance pr1 pr2) 20 (car pospieg) (cadr pospieg))))
   			   "_non" (setq pp (polar pp (+ (angle pr1 pr2) (/ pi 4)) 14.1421))
			   )
			   
	   )
	 (command "_NON" (SETQ P (POLAR PR1 (ANGLE PR1 PR2) (+ (DISTANCE PR1 PR2) 26.3)))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PR2) 1.6057029) 20.0))
		  "_NON" (SETQ P (POLAR P (- (- (ANGLE PR1 PR2) 1.6057029)(/ PI 2)) 0.3))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR2 PR1) 1.6057029 ) 19.69))
		  )
		  
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 pr1) (+ 26.29 (- (cadr pospieg) 0.42))) )
			   "_non" (setq pp (polar pp (+ (angle pr2 pr1) (/ pi 4)) 14.1421))
			   "_non" (setq pp (polar pp (angle pr2 pr1) (- (distance pr1 pr2) 19.75 (car pospieg) (cadr pospieg))))
   			   "_non" (setq pp (polar pp (- (angle pr2 pr1) (/ pi 4)) 14.1421))
			   )
			   
	   )
	 (command
		  ;LAMRETRO
		  
		  "_NON" (SETQ P (POLAR P (ANGLE PR2 PR1) (- (DISTANCE PR1 PR2) -26.3 0.59)))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PR2) (/ PI 2)) 6.2))
		  
		  "_NON" (SETQ P (POLAR P (ANGLE PR2 PR1) 0.3))
		  

		  "_C")
	 (IF (= MODO "SFS") (COMMAND"_MIRROR" (ENTLAST) "" "_NON" (SETQ P (POLAR PR1 (ANGLE PR1 PR2) (/ (DISTANCE PR1 PR2) 2)))
				    "_NON" (POLAR P (+ (ANGLE PR1 PR2) (/ PI 2)) 20) "_Y"))
	 
					    
	 )
	((OR (= MODO "S2S") (= MODO "S2D"))
	 (COMMAND "_PLINE" "_NON" (POLAR PR1 (ANGLE PR2 PR1) 0.5))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar (POLAR PR1 (ANGLE PR2 PR1) 0) (angle pr1 pr2) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 pr2) (/ pi 4)) 14.1421))
			   "_non" (setq pp (polar pp (angle pr1 pr2) (- (distance pr1 pr2) 20 (car pospieg) (cadr pospieg))))
   			   "_non" (setq pp (polar pp (+ (angle pr1 pr2) (/ pi 4)) 14.1421))
			   )
			   
	   )
	 (command	 
	         "_NON" (SETQ P (POLAR PR1 (ANGLE PR1 PR2) (+ (DISTANCE PR1 PR2) 26.3)))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PR2) 1.6057029) 20.0))
		  "_NON" (SETQ P (POLAR P (- (- (ANGLE PR1 PR2) 1.6057029)(/ PI 2)) 0.3))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR2 PR1) 1.6057029 ) 19.69))
		 )
		  
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 pr1) (+ 26.78 (- (cadr pospieg) 0.5 0.42))) )
			   "_non" (setq pp (polar pp (+ (angle pr2 pr1) (/ pi 4)) 14.1421))
			   "_non" (setq pp (polar pp (angle pr2 pr1) (- (distance pr1 pr2) 19.75 (car pospieg) (cadr pospieg))))
   			   "_non" (setq pp (polar pp (- (angle pr2 pr1) (/ pi 4)) 14.1421))
			   )
			   
	   )
	 (command		 
		  ;LAMRETRO
		  
		  "_NON" (SETQ P (POLAR P (ANGLE PR2 PR1) (- (DISTANCE PR1 PR2) -26.3 0.11)))
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PR2) (/ PI 2)) 19.7))
		  "_NON" (SETQ P (POLAR P (ANGLE PR2 PR1) 0.3))
		  

		  "_C")
	 (IF (= MODO "S2S") (COMMAND"_MIRROR" (ENTLAST) "" "_NON" (SETQ P (POLAR PR1 (ANGLE PR1 PR2) (/ (DISTANCE PR1 PR2) 2)))
				    "_NON" (POLAR P (+ (ANGLE PR1 PR2) (/ PI 2)) 20) "_Y"))
					    
	 )
	((OR (= MODO "SMD") (= MODO "SMS"))
	 (COMMAND "_PLINE" "_NON" PR1)
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar pr1 (angle pr1 pr2) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 pr2) (/ pi 4)) 14.1421))
			   "_non" (setq pp (polar pp (angle pr1 pr2) (- (distance pr1 pr2) 20 (car pospieg) (cadr pospieg))))
   			   "_non" (setq pp (polar pp (+ (angle pr1 pr2) (/ pi 4)) 14.1421))
			   )
			   
	   )
	 (command 	  
		  "_NON" (SETQ P (POLAR PR1 (ANGLE PR1 PR2) (+ (DISTANCE PR1 PR2) 0.5)))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PR2) (/ PI 2)) 20.0))
		  "_NON" (SETQ P (POLAR P (ANGLE P2 P1) 0.3))
		  "_NON" (SETQ P (POLAR P (+ (ANGLE PR1 PR2) (/ PI 2)) 19.7)))
		  
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 pr1) (+ 0.50 (- (cadr pospieg) 0.42))) )
			   "_non" (setq pp (polar pp (+ (angle pr2 pr1) (/ pi 4)) 14.1421))
			   "_non" (setq pp (polar pp (angle pr2 pr1) (- (distance pr1 pr2) 19.75 (car pospieg) (cadr pospieg))))
   			   "_non" (setq pp (polar pp (- (angle pr2 pr1) (/ pi 4)) 14.1421))
			   )
			   
	   )
	 (command		  
		  "_NON" (SETQ P (POLAR P (ANGLE PR2 PR1) (- (DISTANCE PR1 PR2) 0.1)))
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PR2) (/ PI 2)) 6.2))
		  "_NON" (SETQ P (POLAR P (ANGLE PR2 PR1) 0.3))
		  

		  "_C")
	 (IF (= MODO "SMS") (COMMAND"_MIRROR" (ENTLAST) "" "_NON" (SETQ P (POLAR PR1 (ANGLE PR1 PR2) (/ (DISTANCE PR1 PR2) 2)))
				    "_NON" (POLAR P (+ (ANGLE PR1 PR2) (/ PI 2)) 20) "_Y"))
	 
	 )
	((OR (= MODO "S4D") (= MODO "S4S"))
	 (COMMAND "_PLINE" "_NON" PR1 )
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar pr1 (angle pr1 pr2) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 pr2) (/ pi 4)) 14.1421))
			   "_non" (setq pp (polar pp (angle pr1 pr2) (- (distance pr1 pr2) 20 (car pospieg) (cadr pospieg))))
   			   "_non" (setq pp (polar pp (+ (angle pr1 pr2) (/ pi 4)) 14.1421))
			   )
			   
	   )
	          (command
		  "_NON" (SETQ P (POLAR PR1 (ANGLE PR1 PR2) (+ (DISTANCE PR1 PR2) 0.5)))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PR2) (/ PI 2)) 20.0))
		  "_NON" (SETQ P (POLAR P (ANGLE P2 P1) 0.5))
		  "_NON" (SETQ P (POLAR P (+ (ANGLE PR1 PR2) (/ PI 2)) 19.5)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 pr1) (+ 1.7129 (- (cadr pospieg) 1.92)) ))
			   "_non" (setq pp (polar pp (+ (angle pr2 pr1) (/ pi 4)) 14.1421))
			   "_non" (setq pp (polar pp (angle pr2 pr1) (- (distance pr1 pr2) 19.58 (car pospieg) (cadr pospieg))))
   			   "_non" (setq pp (polar pp (- (angle pr2 pr1) (/ pi 4)) 14.1421))
			   )
			   
	   )
	 (command
  		  ;LAMRETRO

		  "_NON" (SETQ P (POLAR P (ANGLE PR2 PR1) (- (DISTANCE PR1 PR2) 0.5)))
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PR2) (/ PI 2)) 6.0))
		  "_NON" (SETQ P (POLAR P (ANGLE PR2 PR1) 0.5))
		  

		  "_C")
	 (IF (= MODO "S4S") (COMMAND"_MIRROR" (ENTLAST) "" "_NON" (SETQ P (POLAR PR1 (ANGLE PR1 PR2) (/ (DISTANCE PR1 PR2) 2)))
				    "_NON" (POLAR P (+ (ANGLE PR1 PR2) (/ PI 2)) 20) "_Y"))
	 
	 )
	
	)
  (ALLEGA ENT "PANRETRO" (CDR (ASSOC '5 (ENTGET (ENTLAST)))))
  (COMMAND"_LAYER" "_N" "pannelli-dritti-Retro" "")
  (command"_chprop" (entlast) "" "_la" "pannelli-dritti-Retro" "")
  )