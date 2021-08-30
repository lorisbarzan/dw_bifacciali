;Rev 2020

(defun c:foro ()
  (IF (NULL PRESE) (setq prese (carica "prese" "")))
  (setq percorso_fori (cadr (assoc 'perc_fori parametri)))
  (setq nforo (getfiled "Selezionare Foro" percorso_fori "dwg" 2)) 
  (command"_insert" nforo pause "1" "1" pause "")
;;;  (setq gr (ssget "_C" (list (- (car pt) 1) (- (cadr pt) 1)) (list (+ (car pt) 1) (+ (cadr pt) 1))
;;;    (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))
  (setq pt (cdr (assoc '10 (entget (entlast)))))
  (setq hpresa (getint "Altezza da pavimento <500>"))
  (if (null hpresa) (setq hpresa 500))
  (setq entf (entnext (entlast)))
  (setq entg (subst (cons '1 (rtos hpresa 2 0)) (assoc '1 (entget entf)) (entget entf)))
  (entmod entg)
  (entupd entf)
  (setq presa (assoc (cdr (assoc '1 (entget (entnext entf)))) prese))
(setq gr (SSGET "_C" (list (- (car pt) 1) (- (cadr pt) 1)) (list (+ (car pt) 1) (+ (cadr pt) 1))
		  (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (if (/= (sslength gr) 1) (progn (alert"Errore inserimento foro!!")(exit)))
  (setq ent (ssname gr 0))
  (aggpann ent nil)
  )
(defun aggpann (ent tip)
  (IF (NULL PRESE) (setq prese (carica "prese" "")))
  (setq lispt nil) (FOREACH N (ENTGET ent) (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))))
  (setq lispt (reverse lispt))
  (setq p1 (nth 0 lispt))
  (setq p2 (nth 1 lispt))
  (setq p3 (nth 2 lispt))
  (command"_layer" "_off" "Hforo" "_off" "Hprese" "_off" "testo" "")
  (if (/= "pannelli-dritti" (cdr (assoc '8 (entget ent))))
      (progn
	(setq p1 (nth 0 lispt)) (setq p2 (nth 1 lispt)) (setq p3 (nth 2 lispt))
        (setq grf2 (ssget "_f" (list p2 p3) (list '(0 . "INSERT") '(2 . "FORO*"))))
	)
    (progn
	(setq p1 (nth 0 lispt)) (setq p2 (nth 2 lispt)) (setq p3 nil)
      (setq grf2 nil)  (setq p3 nil)

	)
    )
  (if (/= tip "Si")
    (progn
    (setq grf (ssget "_f" (list p1 p2) (list '(0 . "INSERT") '(2 . "FORO*"))))      
   (command"_layer" "_on" "Hforo" "_on" "Hprese" "_on" "testo" "")
  (setq nr -1)
  (setq delta (cadr (assoc 'RISP_FR_PG parametri)))
  
  (setq lgrf nil)
  (setq lgrf2 nil)
  (while (and grf (setq f (ssname grf (setq nr (+ 1 nr)))))
    (setq fn (cdr (assoc '1 (entget (entnext (entnext f))))))
    (setq da (+ delta (/ (nth  8 (assoc fn prese)) 2)))
    (setq db (+ delta (/ (nth  8 (assoc fn prese)) 2)))
    
    (if (or (null (nth 10 (assoc fn prese))) (= 0 (nth 10 (assoc fn prese))))
      (setq lgrf (cons (list (- (distance p1 (cdr (assoc '10 (entget f)))) da) fn) lgrf)))
    (if (null grf2) (if p3 (if (or (null (nth 10 (assoc fn prese))) (= 0 (nth 10 (assoc fn prese))))
			     (setq lgrf2 (cons (list (- (distance p3 (cdr (assoc '10 (entget f)))) db) fn) lgrf2)))
		      (if (or (null (nth 10 (assoc fn prese))) (= 0 (nth 10 (assoc fn prese))))
			(setq lgrf2 (cons (list (- (distance p2 (cdr (assoc '10 (entget f)))) db) fn) lgrf2)))))
    )

    (setq nr -1)
    (while (and grf2 (setq f (ssname grf2 (setq nr (+ 1 nr)))))
    (setq fn (cdr (assoc '1 (entget (entnext (entnext f))))))
    (setq da (+ delta (/ (nth  8 (assoc fn prese)) 2)))
    (setq db (+ delta (/ (nth  8 (assoc fn prese)) 2)))
    
;;;    (if (or (null (nth 10 (assoc fn prese))) (= 0 (nth 10 (assoc fn prese))))
;;;      (setq lgrf (cons (list (- (distance p1 (cdr (assoc '10 (entget f)))) da) fn) lgrf)))
    (if grf2 (if p3 (if (or (null (nth 10 (assoc fn prese))) (= 0 (nth 10 (assoc fn prese))))
			     (setq lgrf2 (cons (list (- (distance p3 (cdr (assoc '10 (entget f)))) db) fn) lgrf2)))
		      (if (or (null (nth 10 (assoc fn prese))) (= 0 (nth 10 (assoc fn prese))))
			(setq lgrf2 (cons (list (- (distance p3 (cdr (assoc '10 (entget f)))) db) fn) lgrf2)))))
    )
;;;  (if grf2
;;;    (progn
;;;
;;;     (setq lgrf2 nil)
;;;     (setq nr -1)
;;;      (while (and grf2 (setq f (ssname grf2 (setq nr (+ 1 nr)))))
;;;	          (setq fn (cdr (assoc '1 (entget (entnext (entnext f))))))
;;;    (setq da (+ delta (/ (nth  8 (assoc fn prese)) 2)))
;;;    (setq db (+ delta (/ (nth  8 (assoc fn prese)) 2)))
;;;
;;;         (setq lgrf2 (cons (list (- (distance p3 (cdr (assoc '10 (entget f)))) db) fn) lgrf2))
;;;      )
;;;      )
;;;    )
  (SETQ DISTB 0)
  (SETQ DISTa 0)
  (if lgrf
    (progn
      (setq lgrf (vl-sort lgrf (function (lambda (e1 e2) (< (car e1) (car e2))))))
      (setq dista (car (car lgrf)))
      )
    (setq distA 0)
    )
    (if lgrf2
    (progn
      (setq lgrf2 (vl-sort lgrf2 (function (lambda (e1 e2) (< (car e1) (car e2))))))
      (setq distb (car (car lgrf2)))
      )
    (setq distA 0)
    )
  (if (or (/= 0 dista) (/= 0 distb))
    (progn
      (if (> dista 100) (setq dista "100") (setq dista (rtos dista 2 0)))
      (if (> distb 100) (setq distb "100") (setq distb (rtos distb 2 0)))
      (if (= "0" dista) (setq dista "100"))
      (if (= "0" distb) (setq distb "100"))
      
      (if p3
	(progn
	 (command"_dimangular" "" "_non" p2 "_non" p1 "_non" p3 "_non" p3)
	 (setq angpan (atof (angtos (cdr (assoc '42 (entget (entlast)))) 0 0)))
	 (entdel (entlast))
	 (if (> angpan 180) (setq somma 25) (setq somma -05))
	 (if (< (- (distance p2 p3) delta somma ) (atoi distb)) (setq distb (rtos (- (distance p2 p3) delta somma) 2 0)))
	 (if (< (- (distance p1 p2) delta somma ) (atoi dista)) (setq dista (rtos (- (distance p1 p2) delta somma) 2 0)))
	 )
	)
	  
      )
    (setq dista "0" distb "0")
    )
  
  (setq pieghe (strcat "(" dista " " distb ")"))
  )
    (progn
      (IF (AND (ENTGET ENT)
	       (SETQ TMP (ENTGET ENT (LIST "PANRETRO")))
	       (SETQ TMP (ASSOC -3 TMP))
	       )
	(progn
	  (SETQ HAND (LEGGI ENT "PANRETRO"))
	  (if (= 'LIST (type (read hand)))
	    (setq rp (handent (cadr (READ hand))))
	    (setq rp (handent hand))
	    )
	  )
	)
        (setq lisptR nil) (FOREACH N (ENTGET RP) (IF (= '10 (CAR N)) (SETQ LISPTR (CONS (CDR N) LISPTR))))
  (setq lisptR (reverse lisptR))

      (SETQ TIPO (LEGGI ENT "PANTIP"))
(IF (= TIPO "STD") (SETQ DISTB (RTOS (+ (DISTANCE (NTH 5 LISPTR) (NTH 6 LISPTR)) 0) 2 0) DISTA (RTOS (+ (DISTANCE (NTH 0 LISPTR) (NTH 1 LISPTR)) 0)2 0)))
(IF (= TIPO "SFD") (SETQ DISTB (RTOS (+ (DISTANCE (NTH 5 LISPTR) (NTH 6 LISPTR)) -26) 2 0) DISTA (RTOS (+ (DISTANCE (NTH 0 LISPTR) (NTH 1 LISPTR)) 0)2 0)))
(IF (= TIPO "SFS") (SETQ DISTB (RTOS (+ (DISTANCE (NTH 5 LISPTR) (NTH 6 LISPTR)) 0) 2 0) DISTA (RTOS (+ (DISTANCE (NTH 0 LISPTR) (NTH 1 LISPTR)) -26)2 0)))
(IF (= TIPO "SMD") (SETQ DISTB (RTOS (+ (DISTANCE (NTH 5 LISPTR) (NTH 6 LISPTR)) -1) 2 0) DISTA (RTOS (+ (DISTANCE (NTH 0 LISPTR) (NTH 1 LISPTR)) 0)2 0)))
(IF (= TIPO "SMS") (SETQ DISTB (RTOS (+ (DISTANCE (NTH 5 LISPTR) (NTH 6 LISPTR)) 0) 2 0) DISTA (RTOS (+ (DISTANCE (NTH 0 LISPTR) (NTH 1 LISPTR)) -1)2 0)))
(IF (= TIPO "S2D") (SETQ DISTB (RTOS (+ (DISTANCE (NTH 5 LISPTR) (NTH 6 LISPTR)) -26) 2 0) DISTA (RTOS (+ (DISTANCE (NTH 0 LISPTR) (NTH 1 LISPTR)) -1)2 0)))
(IF (= TIPO "S2S") (SETQ DISTB (RTOS (+ (DISTANCE (NTH 5 LISPTR) (NTH 6 LISPTR)) -1) 2 0) DISTA (RTOS (+ (DISTANCE (NTH 0 LISPTR) (NTH 1 LISPTR)) -26)2 0)))
(IF (= TIPO "S3D") (SETQ DISTB (RTOS (+ (DISTANCE (NTH 5 LISPTR) (NTH 6 LISPTR)) -1) 2 0) DISTA (RTOS (+ (DISTANCE (NTH 0 LISPTR) (NTH 1 LISPTR)) -1)2 0)))
(IF (= TIPO "S4D") (SETQ DISTB (RTOS (+ (DISTANCE (NTH 5 LISPTR) (NTH 6 LISPTR)) -1) 2 0) DISTA (RTOS (+ (DISTANCE (NTH 0 LISPTR) (NTH 1 LISPTR)) 0)2 0)))
(IF (= TIPO "S4S") (SETQ DISTB (RTOS (+ (DISTANCE (NTH 5 LISPTR) (NTH 6 LISPTR)) 0) 2 0) DISTA (RTOS (+ (DISTANCE (NTH 0 LISPTR) (NTH 1 LISPTR)) -1)2 0)))
      (setq pieghe (strcat "(" dista " " distb ")"))
      )
    )
      
  (allega ent "PANPIEG" pieghe)
  (RETROPAN ENT)
  (LANAPAN ENT (ENTLAST))
  

		  
)
(defun c:aggiornapan ()
  (setq grent (ssget (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (setq nrk -1)
  (if (= 1 (sslength grent))
    (progn
      (initget "Si No")
      (setq mAntieni (getkword "\nLeggo le pieghe della gola dalla grafica [Si/No] <No>:"))
      )
    (progn
      (setq mantieni "no")
      )
    )
  (while (and grent (setq ent (ssname grent (setq nrk (+ 1 nrk)))))
  ;(setq ent (car (entsel)))
  (aggpann ent MANTIENI)
    )
  (C:ELIMINA_SPORCO)
  )
  
(DEFUN DIBFORO ()
  (setq listk_svi nil)
  (setq nrcalate 0)
  (setq elcalate nil)
  (command"_layer" "_off" "Hforo" "_off" "testo" "_off" "hprese" "")
  (setq grf (ssget "_f" LISPTFO (list '(0 . "INSERT") '(2 . "FORO*"))))
    (command"_layer" "_on" "Hforo" "_on" "testo" "_on" "hprese" "")

  (SETQ NRF 0)
  (SETQ LISFORP NIL)
  
  (if grf (setq lisforp (cons (list (list (setq foro (ssname grf 0)) (ATOI (CDR (ASSOC '1 (ENTGET (ENTNEXT (ssname grf 0)))))))) lisforp)))
  
;;;  (WHILE (AND grf (SSNAME GRF (SETQ NRF (+ 1 NRF))) (SETQ FOROd (substr (SSNAME GRF (SETQ NRF (+ 0 NRF)))1 8)))
  (WHILE (AND grf (SSNAME GRF (SETQ NRF (+ 1 NRF))) (SETQ FOROd (SSNAME GRF (SETQ NRF (+ 0 NRF)))))
    (if (< (distance (cdr (assoc '10 (entget foro))) (cdr (assoc '10 (entget forod)))) 25)
      
      (progn
	(setq lisforp (subst (cons (list forod (ATOI (CDR (ASSOC '1 (ENTGET (ENTNEXT forod)))))) (car lisforp))
			    (car lisforp)
			    lisforp
		      )
	)
      )
      (setq lisforp (cons (list (list forod (ATOI (CDR (ASSOC '1 (ENTGET (ENTNEXT FOROd))))))) lisforp))
    )
    (setq foro forod)
  )
  (setq lisf nil)
  (foreach n lisforp
    (setq n (vl-sort n (function (lambda (e1 e2) (> (CADR e1) (CADR e2))))))
    (setq lisf (cons n lisf))
  )
  (setq lisforp nil)
  (foreach gfori lisf
    (setq pfor (CDR (ASSOC '10 (ENTGET (car (car gFORi))))))
    (if (and (/= ldpos0_f 0) (> (distance pt pfor) ldpos0_f))
      (setq pxdf (+ (distance pt0x_f pfor) ldpos0_f))
      (setq pxdf (distance pt pfor))
    )
    (SETQ LISFORP (CONS (LIST ;(CDR (ASSOC '2 (ENTGET (car (car gfori)))))
			      (substr (CDR (ASSOC '2 (ENTGET (car (car gfori))))) 1 8)
 				   (cadr (car gfori))
				  'NX (atof (rtos pxdf 2 1))) LISFORP)
    )
    (setq nrxp 0)
    (foreach foro (cdr gfori)
       (setq pfor (CDR (ASSOC '10 (ENTGET (car foro)))))
       (if (and (/= ldpos0_f 0) (> (distance pt pfor) ldpos0_f))
           (setq pxdf (+ (distance pt0x_f pfor) ldpos0_f))
           (setq pxdf (distance pt pfor))
       )
       (setq nonx nil)
       (if (member (list (SETQ N1 (CDR (ASSOC '2 (ENTGET (car foro))))) (SETQ N2 (CDR (ASSOC '2 (ENTGET (car (nth nrxp gfori))))))) POSpon)
           (SETQ LISFORP (CONS (LIST ;(CDR (ASSOC '2 (ENTGET (car foro))))
				     (substr (CDR (ASSOC '2 (ENTGET (car foro)))) 1 8)
    				          (cadr foro)
				          'SP (atof (rtos pxdf 2 1))
				          (- (CADR (nth nrxp gfori)) (CADR FORO)
					     (/ (NTH 3 (ASSOC N1 PRESE)) 2)
					     (/ (NTH 3 (ASSOC N2 PRESE)) 2)
			                  )
			       ) LISFORP)

           )
           (if (and (= nonx t) (/= n2 "DFOR0020"))
               (SETQ LISFORP (CONS (LIST (substr (CDR (ASSOC '2 (ENTGET (car foro)))) 1 8)
    				          (cadr foro)
				          'SF (atof (rtos pxdf 2 1))) LISFORP) nonx t

               )
               (SETQ LISFORP (CONS (LIST (substr (CDR (ASSOC '2 (ENTGET (car foro)))) 1 8)
    				          (cadr foro)
				          'NX (atof (rtos pxdf 2 1))) LISFORP)

               )
	   )
       )
       (setq nrxp (+ 1 nrxp))
    )
 )
 (FOREACH FORO LISFORP
   (setq *error* nil)
   (setq posponti (carica "posponti" ""))
    (setq scatola (nth 1 (assoc (car foro) prese)))
    (if (< 3 (strlen scatola))
      (progn
         (SETQ LISTK (CONS (LIST scatola (nth 3 foro) 1  (nth 1 foro)
				    (nth 2 (assoc (car foro) prese)) (nth 3 (assoc (car foro) prese))) LISTK)
		 )
	)
      )
   (if (= profelet "DEM239") (setq -vinnest -70) (setq -vinnest 0))
    (FOREACH RIGA DISTFORI
      (IF (and (or (= "Aperto" (nth 1 riga)) (= "Variante" (nth 1 riga))) (= (CAR RIGA) (CAR FORO)))
	(progn
	  (setq var nil)
        (IF (= 'SF (nth 2 foro))
          (progn
	     (setq llh nil)
		     (foreach l lisforp (if (and (> (nth 1 l) (nth 1 foro)) (= (nth 3 foro) (nth 3 l)) ) (setq llh (cons (list (nth 1 l) (nth 0 l))llh))))
		     (setq llh (vl-sort llh (function (lambda (e1 e2) (< (car e1) (car e2))))))
		     (setq lhp (car (car llh)))
		     (if llh (setq lhp (- lhp (/ (nth 3 (assoc (cadr (car llh)) PRESE)) 2))))
		     (if (member (list (cadr (car llh)) (nth 0 riga) ) POSPONTI)
		       (setq Var nil) (setq var t))
	   ))
          (COND ((and (null var) (= "A" (substr (nth 2 riga)1 1)) (= "Tubo" (NTH 4 RIGA)) (/= -1 (nth 6 riga)) (/= -2 (nth 6 riga)))
		 (IF (/= 'SF (nth 2 foro))
		   (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga))
					   (atoi (rtos (- (- altkit (- 0 -vinnest) (+ (nth 6 riga) (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro))) 50.0) 2 0))
					   (+ (nth 6 riga) (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro))
				    (+ (NTH 3 foro) (nth 3 riga))
					   (+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro) (ATOI (RTOS (- ALTKIT (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0)))) LISTK))
			       (progn		     
		    (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga))
				(ATOI (RTOS (- lhp (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0)) (+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro))
				    (+ (NTH 3 foro) (nth 3 riga))
					    (+ (ATOI (RTOS (- lhp (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0)) (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro))) LISTK))
		    )
		   ))
		((and (null var) (= "A" (substr (nth 2 riga)1 1)) (or (= "Tubo" (NTH 4 RIGA)) (= "Curva" (NTH 4 RIGA))) (= -2 (nth 6 riga)))
		 (IF (/= 'SF (nth 2 foro))
		   (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ -64.5 (NTH 3 foro) (nth 3 riga))
					   1
					   altkit
				    65.5
					   0
					   ) LISTK))
			       
		   ))
		
                ((EQUAL (NTH 2 FORO) (NTH 2 RIGA))(SETQ LISTK (CONS (LIST (NTH 1 RIGA) (NTH 3 foro) 1 (nth 1 foro)
                                       (nth 2 (assoc (car foro) PRESE)) (nth 3 (assoc (car foro) PRESE) )) LISTK)))
		((and (null var) (= "A" (substr (nth 2 riga)1 1)) (= "Innesto" (NTH 4 RIGA)))
		 (IF (/= 'SF (nth 2 foro))
		   (if (/= 0 (nth 6 riga))
		     (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga)) 1 (+ (nth 1 foro) (nth 6 riga))
				    1 0) LISTK))
		     (progn
		       
		   (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga)) 1 (+ hpan -vinnest)
				    1 0) LISTK)
		 LISTK_SVI (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga)) 1 (+ hpan -vinnest)
				    1 0) LISTK_SVI))))
		   )
		 )
		((and (null var) (= "A" (substr (nth 2 riga)1 1)) (= "Passacavi" (NTH 4 RIGA)))
		 (if (= -2 (nth 6 riga)) (setq v6 -2) (setq v6 1))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga)) 1 (+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro))
				    v6 0) LISTK)))
		((and (null var) (= "A" (substr (nth 2 riga)1 1)) (= "Tubo" (NTH 4 RIGA)) (= -1 (nth 6 riga)))
		 (if (= -1.0 (nth 6 riga)) (setq v6 -1) (setq v6 1))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga)) 1 (+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro))
				    v6 0) LISTK)))
		
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "S" (substr (nth 2 riga)1 1)) (= "Tubo" (NTH 4 RIGA))(/= -2 (nth 6 riga)))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
		       (+  (nth 7 riga) (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
				(ATOI (RTOS (- ALTKIT 45 30 (- 0 -vinnest) (NTH 1 FORO)) 2 0))
;;;				(ATOI (RTOS (- ALTKIT 50.0 (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0))
				    (+ 30 (nth 1 foro));1		 
;;;				    (+ (nth 6 riga) (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro));1
					 (+ (nth 7 riga) (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
					(+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro)(ATOI (RTOS (- ALTKIT (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0)))
					 ) LISTK)))
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "S" (substr (nth 2 riga)1 1)) (or (= "Tubo" (NTH 4 RIGA)) (= "CurvaS" (NTH 4 RIGA)))(= -2 (nth 6 riga)))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
		(+ -64.5 (nth 7 riga) (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
				1
				    ALTKIT
				    65.5
					0
					 ) LISTK)))
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "S" (substr (nth 2 riga)1 1)) (= "Innesto" (NTH 4 RIGA)))
		 (if (/= 0 (nth 6 riga))
		   (progn 
		   (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
					   (+ (nth 7 riga) (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
					   1 (+ (nth 1 foro) (nth 6 riga)) 1 180) LISTK))
		   )
		  

		   (progn
		  (SETQ LISTK (CONS (LIST (NTH 5 RIGA)    (+ (nth 7 riga) (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2)) 1 (+ hpan -vinnest) 1 0) LISTK))
		  (SETQ LISTK_SVI (CONS (LIST (NTH 5 RIGA)(+ (nth 7 riga) (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2)) 1 (+ hpan -vinnest) 1 0) LISTK_SVI))
		  )
		   ))
		 
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "S" (substr (nth 2 riga)1 1)) (= "Passacavi" (NTH 4 RIGA)))
		 (if (= -1.0 (nth 6 riga)) (setq v6 -1) (setq v6 1))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
													     (+ (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
													     1
													     (- (+ 50 (nth 1 foro)) (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga)))
				    v6 90) LISTK)))
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "S" (substr (nth 2 riga)1 1)) (= "CurvaS" (NTH 4 RIGA)) (/= -2 (nth 6 riga)))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
													  (+ (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
													  1
													  (- (+ 50 (nth 1 foro)) 0)
				    (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga)) 90) LISTK)))
;;;		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "Tubo" (NTH 4 RIGA))(/= -2 (nth 6 riga)))
;;;		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
;;;		       (- (NTH 3 foro)(+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga))  (/ (nth 2 (assoc (car foro) PRESE)) 2))
;;;				(ATOI (RTOS (- ALTKIT (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0))
;;;				    (+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro));1
;;;				    (- (NTH 3 foro)(+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga))  (/ (nth 2 (assoc (car foro) PRESE)) 2))
;;;					(+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro)(ATOI (RTOS (- ALTKIT (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0)))) LISTK)))
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "Tubo" (NTH 4 RIGA))(/= -2 (nth 6 riga)))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
		       (-  (NTH 3 foro) (nth 7 riga)  (/ (nth 2 (assoc (car foro) PRESE)) 2))
					 
				(ATOI (RTOS (- ALTKIT 45 30 (- 0 -vinnest) (NTH 1 FORO)) 2 0))
;;;				(ATOI (RTOS (- ALTKIT 50.0 (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0))
				    (+ 30 (nth 1 foro));1
					 (- (NTH 3 foro)(nth 7 riga)  (/ (nth 2 (assoc (car foro) PRESE)) 2))
					(+ (/ (nth 3 (assoc (car foro) PRESE)) 2) (nth 1 foro)(ATOI (RTOS (- ALTKIT (/ (nth 3 (assoc (car foro) PRESE)) 2) (NTH 1 FORO)) 2 0)))
					 ) LISTK)))

;;;		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "Tubo" (NTH 4 RIGA))(= -2 (nth 6 riga)))
;;;		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
;;;		       (-  (NTH 3 foro)(+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga)) 65.0 (/ (nth 2 (assoc (car foro) PRESE)) 2))
;;;				1
;;;				    ALTKIT
;;;				    65.0
;;;					0
;;;					 ) LISTK)))
                 ((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (or (= "Tubo" (NTH 4 RIGA)) (= "CurvaD" (NTH 4 RIGA)))(= -2 (nth 6 riga)))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
		(- (NTH 3 foro) +64.5 (nth 7 riga)  (/ (nth 2 (assoc (car foro) PRESE)) 2))
				1
				    ALTKIT
				    65.5
					0
					 ) LISTK)))
;;;		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "Innesto" (NTH 4 RIGA)))
;;;		 (if (/= 0 (nth 6 riga))
;;;		   (progn
;;;		     (SETQ LISTK (CONS (LIST (NTH 5 RIGA)(- (NTH 3 foro) (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga))  (/ (nth 2 (assoc (car foro) PRESE)) 2)) 1 (+ (nth 1 foro) (nth 6 riga)) 1 180) LISTK))
;;;		     )
;;;		   (progn
;;;		  (SETQ LISTK (CONS (LIST (NTH 5 RIGA)(- (NTH 3 foro) (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga))  (/ (nth 2 (assoc (car foro) PRESE)) 2)) 1 (+ hpan 0) 1 0) LISTK))
;;;		  (SETQ LISTK_SVI (CONS (LIST (NTH 5 RIGA)(- (NTH 3 foro)(+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga))  (/ (nth 2 (assoc (car foro) PRESE)) 2)) 1 (+ hpan 0) 1 0) LISTK_SVI))
;;;		  )))
                 ((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "Innesto" (NTH 4 RIGA)))
		 (if (/= 0 (nth 6 riga))
		   (progn 
		   (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
					   (- (NTH 3 foro)(nth 7 riga)  (/ (nth 2 (assoc (car foro) PRESE)) 2))
					   1 (+ (nth 1 foro) (nth 6 riga)) 1 180) LISTK))
		   )
		  

		   (progn
		  (SETQ LISTK (CONS (LIST (NTH 5 RIGA)    (- (NTH 3 foro)(nth 7 riga)  (/ (nth 2 (assoc (car foro) PRESE)) 2)) 1 (+ hpan -vinnest) 1 0) LISTK))
		  (SETQ LISTK_SVI (CONS (LIST (NTH 5 RIGA)(- (NTH 3 foro)(nth 7 riga)  (/ (nth 2 (assoc (car foro) PRESE)) 2)) 1 (+ hpan -vinnest) 1 0) LISTK_SVI))
		  )
		   ))
;;;		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "Passacavi" (NTH 4 RIGA)))
;;;		 (if (= -1.0 (nth 6 riga)) (setq v6 -1) (setq v6 1))
;;;		                                                                       (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
;;;													     (- (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
;;;													     1
;;;													     (- (+ 50 (nth 1 foro)) (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga)))
;;;				    v6 -90) LISTK)))
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "Passacavi" (NTH 4 RIGA)))
		 (if (= -1.0 (nth 6 riga)) (setq v6 -1) (setq v6 1))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
					 (- (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
					 1
					 (- (+ 50 (nth 1 foro)) (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga)))
				    -1 -90) LISTK)));21.00
		((and (or (and (= "Variante" (NTH 1 RIGA)) var) (= "Aperto" (NTH 1 RIGA))) (= "D" (substr (nth 2 riga)1 1)) (= "CurvaD" (NTH 4 RIGA))(/= -2 (nth 6 riga))) (SETQ LISTK (CONS (LIST (NTH 5 RIGA)
													  (- (NTH 3 foro) (/ (nth 2 (assoc (car foro) PRESE)) 2))
													  1
													  (- (+ 50 (nth 1 foro)) 0)
				    (- 0 (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga))) 90) LISTK)))
		((and (= "B" (substr (nth 2 riga)1 1)) (= "Passacavi" (NTH 4 RIGA)))
		 (if (= -1.0 (nth 6 riga)) (setq v6 -1) (setq v6 1))
		 (SETQ LISTK (CONS (LIST (NTH 5 RIGA) (+ (NTH 3 foro) (nth 3 riga));(+ (NTH 3 foro) (+ (- 50 (/ (nth 3 (assoc (car foro) PRESE)) 2)) (nth 3 riga)))
					 1
													     (- (nth 1 foro) (/ (nth 3 (assoc (car foro) PRESE)) 2) )
				    v6 180) LISTK)))
          ))
	)
      )
    

  )
  (if listk_svi
    (progn
      (setq listk_svi (vl-sort listk_svi (function (lambda (e1 e2) (< (nth 1 e1) (nth 1 e2))))))
      (setq posk_svi (atof (rtos (/ (+ (nth 1 (car listk_svi)) (nth 1 (last listk_svi))) 2) 2 1)))
      
    (if (<= (length listk_svi) 8)
      (progn
	 (setq listk (cons (list profelet posk_svi 210 hpan 1 0 ) listk))
;;;         (alert "2 calate")
      )
      
      (progn
	 (setq listk (cons (list profelet (- posk_svi 105) 210 hpan 1 0 ) listk))
	(setq listk (cons (list profelet (+ posk_svi 105) 210 hpan 1 0 ) listk))
;;;         (alert "4 calate")
      )))
    )
;;;  (setq demf1 nil demf2 nil)
;;;  (foreach f lisforp
;;;    (if (< (nth 3 f) lung)(setq demf1 lung) (setq demf2 lung2))
;;;    )
;;;  (if demf1
;;;    (cond ((< lung 350) (setq listk (cons (list "DEM011" 1 (/ LUNG 2) 0 hpan 1 0 ) listk)))
;;;	  ((< lung 450) (setq listk (cons (list "DEM010" 1 (/ LUNG 2) 0 hpan 1 0 ) listk)))
;;;	  ((< lung 550) (setq listk (cons (list "DEM009" 1 (/ LUNG 2) 0 hpan 1 0 ) listk)))
;;;	  (T (setq listk (cons (list "DEM008" 1 (/ LUNG 2) 0 hpan 1 0 ) listk)))
;;;	  )
;;;    )
;;;  (if demf2
;;;    (cond ((< lung2 350) (setq listk (cons (list "DEM011" 1 (+ LUNG (/ LUNG2 2)) 0 hpan 1 0 ) listk)))
;;;	  ((< lung2 450) (setq listk (cons (list "DEM010" 1 (+ LUNG (/ LUNG2 2)) 0 hpan 1 0 ) listk)))
;;;	  ((< lung2 550) (setq listk (cons (list "DEM009" 1 (+ LUNG (/ LUNG2 2)) 0 hpan 1 0 ) listk)))
;;;	  (T (setq listk (cons (list "DEM008" 1 (+ LUNG (/ LUNG2 2)) 0 hpan 1 0 ) listk)))
;;;	  )
;;;    )
	  
  (setq lisfor_ (vl-sort lisforp (function (lambda (e1 e2) (< (nth 1 e1) (nth 1 e2))))))
  (setq lisfor nil)
  (foreach fr lisfor_
;;;    (if (/= "FORO0002" (nth 0 fr)) (setq lisfor (cons fr lisfor)))
    (if (= -1 (nth 7 (assoc (car fr) prese))) (setq lisfor (cons fr lisfor)))
    )
  (setq lisfor (reverse lisfor))
  (setq lfeltro nil)
  (foreach for lisfor
    (if (null feltro) (setq feltro (list (nth 1 for) (nth 1 for) (nth 3 for) "325"))
      (if (< (abs (- (nth 1 for) (car feltro))) 100)
	(setq feltro (list (car feltro) (nth 1 for) (nth 3 for) (rtos (- (+ lung lung2 50) piega1 piega2) 2 0)))
	(setq lfeltro (cons feltro lfeltro) feltro (setq feltro (list (nth 1 for) (nth 1 for) (nth 3 for) "325")))
	)
      )
    
    )
  (if feltro (setq lfeltro (cons feltro lfeltro) feltro nil))
    
    (print)
  (if (and lfeltro (= resis "B15"))
    (setq listk (cons (list (cadr (assoc 'feltro parametri)) (+ (/ (atof (rtos (- (+ lung lung2 50) piega1 piega2) 2 0))2) piega1 -25)
				(/ (* (atoi (rtos (- (+ lung lung2 50) piega1 piega2) 2 0)) halt) 1)
				(/ halt 2) (atoi (rtos (- (+ lung lung2 50) piega1 piega2) 2 0)) halt) listk))
    (foreach fel lfeltro
      (if (= "325" (nth 3 fel))
        (setq listk (cons (list (cadr (assoc 'feltro parametri)) (nth 2 fel) (/ (* (atoi (nth 3 fel)) 220) 1) (/ (+ (nth 0 fel) (nth 1 fel))2) (atoi (nth 3 fel)) 220) listk))
	(setq listk (cons (list (cadr (assoc 'feltro parametri)) (+ (/ (atof (nth 3 fel))2) piega1 -25)
				(/ (* (atoi (nth 3 fel)) 220) 1)
				(/ (+ (nth 0 fel) (nth 1 fel))2) (atoi (nth 3 fel)) 220) listk))
	)
      )
    )
;;;(print listk_svi)
;;;  (getpoint)
)