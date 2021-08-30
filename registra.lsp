;Rev 2020
;(setq profx (arcprof pprer1 "grezzo" pprerlg 0))

(defun c:regtmp ()
   (start)

  (setq parametri (carica "parametri" ""))
  
  (setq Lista (ssget "x" (list '(0 . "insert") '(2 . "codpas")'(8 . "siglepan"))))
  (if Lista
    (progn
      (setq nr -1)
      (setq lstcodpas nil)
      (while (setq ent (ssname Lista (setq nr (+ 1 nr))))
        (setq lstcodpas (cons (List (cdr (assoc '1 (entget (entnext (entnext ent))))) (cdr (assoc '10 (entget ent))) (cdr (assoc '50 (entget ent)))) lstcodpas))      
      )
    )
  )
  (initget "Si No")
  (setq mpr (getkword "\nMantenere posizioni di codifica attuali? [Si/No]<Si>:"))
  (if (= mpr "No")
    (setq lstcodpas nil)
  )
  
;;;  (setq nfilea (strcat (cadr (assoc 'percregtmp parametri)) "anagraf.txt"))
;;;  (setq nfiled (strcat (cadr (assoc 'percregtmp parametri)) "distbase.txt"))
  (IF (/= MODALITA (cadr (assoc 'percregtmp parametri))) (C:ANNTAB))
  (setq modalita (cadr (assoc 'percregtmp parametri)))
  (registra)
  (Stop)
)
(defun c:registra ()
  (start)
;;;  (c:ver_lic)
;;;  (if (null (verkit)) (exit))
(setq lrig-nomi nil)

  ;(solalet (cadr (assoc 'percregdef parametri)))
  (setq parametri (carica "parametri" ""))
  (initget "Si No")
  (if (= "Si" (getkword "\nVuoi procedere con la registrazione definitiva [Si/No] <No>:"))
    (progn
      (setq Lista (ssget "x" (list '(0 . "insert") '(2 . "codpas")'(8 . "siglepan"))))
      (if Lista
	(progn
	  (setq nr -1)
	  (setq lstcodpas nil)
	  (while (setq ent (ssname Lista (setq nr (+ 1 nr))))
	    (setq lstcodpas (cons (List (cdr (assoc '1 (entget (entnext (entnext ent))))) (cdr (assoc '10 (entget ent))) (cdr (assoc '50 (entget ent)))) lstcodpas))
	    )
	  )
	)
      (initget "Si No")
      (setq mpr (getkword "\nMantenere posizioni di codifica attuali? [Si/No]<Si>:"))
      (if (= mpr "No")
	(setq lstcodpas nil)
	)

;;;      (setq nfilea (cadr (assoc 'fileanagrafica parametri)))
;;;      (setq nfiled (cadr (assoc 'filedistbase parametri)))
;;;      (setq nfiled6 (strcat (cadr (assoc 'filedistbase parametri)) "6"))
;;;      (elim_cab_da_gaia)
      (IF (/= MODALITA (cadr (assoc 'percregdef parametri))) (C:ANNTAB))
      (setq modalita (cadr (assoc 'percregdef parametri)))
;;;      (if (= "No" (c:verfiltro)) (progn (alert "Filtro non Valido - Mancano altezze o finiture - All'uscita l'elenco delle altezze e finiture neccessarie !!")
;;;				   (print &lish) (print &lisf)
;;;				   (exit)
;;;				   ))
;;;
      (foreach n '("cabine" "cabpan" "cesoiati" "dibces" "cesoiati_r" "dibces_r" "DIBLDR" "dibpan" "dibpan_r" "dibpas" "dibrin"  "DIBsvi" "DIBsvi_r" "mater" "pannelli"
		   "pannellil" "pannellil_r" "sviluppi" "sviluppi_r" "tabrinf"
		   )
          (vl-file-delete (strcat modalita n ".bak"))
          (if (null (vl-file-copy (strcat modalita n ".txt") (strcat modalita n ".bak")))
	    (progn (alert (strcat "File " n " Non accessibile")) (exit))
	  )
      )
      (vl-file-delete
	(strcat (vl-filename-directory (cadr (assoc 'fileanagrafica parametri)))"/"(vl-filename-base (cadr (assoc 'fileanagrafica parametri)))".bak")
      )
		  
;;;      (if (null (vl-file-copy (cadr (assoc 'fileanagrafica parametri))
;;;	           (strcat (vl-filename-directory (cadr (assoc 'fileanagrafica parametri)))"/"(vl-filename-base (cadr (assoc 'fileanagrafica parametri)))".bak")
;;;                )
;;;	  )
;;;	  (progn (alert "File Anagrafica non accessibile") (exit))
;;;      )
;;;      (vl-file-delete
;;;	(strcat (vl-filename-directory (cadr (assoc 'filedistbase parametri)))"/"(vl-filename-base (cadr (assoc 'filedistbase parametri)))".bak")
;;;      )		  
;;;      (if (null (vl-file-copy (cadr (assoc 'filedistbase parametri))
;;;	           (strcat (vl-filename-directory (cadr (assoc 'filedistbase parametri)))"/"(vl-filename-base (cadr (assoc 'filedistbase parametri)))".bak")
;;;                )
;;;	  )
;;;	  (progn (alert "File Distbase non accessibile")(exit))
;;;      )
      (registra)
    )
  )
  (stop)
)
(defun S_OPEN (nome modo)
  (setq nff (findfile nome))
  (setq carnr 0)
  (while (null nff)
     (setq carnr (+ 1 carnr))
     (p_alert (strcat "Attesa scrittura prolungata " nome " !!!!"))
     (setq nff (findfile nome))
  )
  (print (strcat "attesa scrittura " nome " : " (rtos carnr 2 0)))
  (setq fperc (strcat (vl-filename-directory nff) "\\"))
  (setq fext (vl-filename-extension nff))
  (setq fbase (vl-filename-base nff))
  (while (null (vl-file-rename (strcat fperc fbase fext) (strcat fperc fbase ".tx_")))
      (p_alert (strcat "Attesa apertura " nome))
  )
  (while (null (findfile (strcat fperc fbase ".tx_"))))
  (open (strcat fperc fbase ".tx_") modo)
)
(DEFUN REGISTRA ()
    ;(start)
  (SETQ GRL (SSGET "x" (list  '(0 . "DIMENSION") '(8 . "Quotelana"))))
  (if grl (command"_erase" grl ""))
  (c:elimina_sporco)
  (setq pregd (selezpar 'profrdoga "Selezionare Profilo reggidoga: ")) ;profilo
  (setq pprer (selezpar 'profiloper "Selezionare Profilo Perimetrale: ")) ;profilo
  (setq pprer1 (selezpar 'profiloper1 "Selezionare Profilo Perimetrale: ")) ;profilo
    (setq progiu (selezpar 'prof_giu_ele "Selezionare Profilo prof_giu_ele :"));profilo
	  (setq profelet (selezpar 'Prof_elet "Selezionare Profilo Prof_elet :"));profilo

  (command"_layer" "_t" "*" "")
  (command"_regen")
  (SETVAR"TILEMODE" 1)
  (COMMAND"_ZOOM" "_E")
;;;  (setq seltravinf (selezpar 'traversinf "traversina inferiore")) 
;;;  (setq seltravsup (selezpar 'traverssup "traversina Superiore"))
;;;  (setq selrinfpiede (selezpar 'codrinfpiedep "Rinforzo piede"))
;;;  (setq selvelovetro (selezpar 'velovetro "Velovetro"))
;;;  (setq sellanacer (selezpar 'lanaceramica "Lana Ceramica"))  
;;;  (setq revisione (getstring "\nRevisione nr [.00/.01/.02/.03/.04] <.00>:"))
;;;  (if (= "" revisione) (setq revisione ".00"))
  (setq revisione ".00")
  ;(setq filea (S_OPEN nfilea "a"))
  ;(setq filed (S_OPEN nfiled "a"))
  (setq orainizio (getvar"cdate"))
  (C:REGPAN)
  ;(C:REGKIT)
  (c:regcab_p)
  (if filea (s_close filea nfilea))
  (if filed (s_close filed nfiled))
  (setq orafine (getvar"cdate"))
  (prompt (strcat "\nSecondi impiegati per registrazione :" (rtos (* 1000000 (- (atof (rtos orafine 2 6)) (atof (rtos orainizio 2 6)))) 2 1)))
  
)
(defun controlla_pan (gr / nr ent lispt)
  (setq nr 0)
  (setq nr -1)
  (while (and gr (setq ent (ssname gr (setq nr (+ 1 nr)))))
    (setq lispt nil)
    (setq nopan nil)
     (FOREACH N (ENTGET ent) (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))) )
     (setq lispt (reverse lispt))
    (if (= "pannelli-dritti" (cdr (assoc '8 (entget ent))))
      (progn
	(setq A1 (distance (nth 0 lispt) (nth 1 lispt)))
	(SETQ MODOP (LEGGI ENT "PANTIP"))
	(if (or (= modop "STD") (= modop "MTD") (= modop "SPS") (= modop "MPS"))
	  (if (or (> 60 a1) (< 600 a1)) (setq nopan "pannello 60>A1>600"))
	  (if (or (> 85 a1) (< 590 a1)) (setq nopan "pannello 85>A1>590"))
	  )
	)
      (progn
	(setq A1 (distance (nth 0 lispt) (nth 1 lispt)))
	(setq A2 (distance (nth 1 lispt) (nth 2 lispt)))
	(SETQ MODOP (LEGGI ENT "PANTIP"))
	(if (or (= modop "STD") (= modop "MTD") (= modop "SPS") (= modop "MPS"))
	  (PROGN
	     (if (or (< 600 (+ a1 A2))) (setq nopan "pannello A1+A2>600"))
	     (if (or (> 80 a1) (< 430 a1)) (setq nopan "pannello 80>A1>430"))
	     (if (or (> 80 a2) (< 430 a2)) (setq nopan "pannello 80>A2>430"))
	    )
	  (PROGN
	     (if (or  (< 590 (+ a1 A2))) (setq nopan  "pannello A1+A2>590"))
	     (if (or (> 80 a1) (< 430 a1)) (setq nopan "pannello 80>A1>430"))
	     (if (or (> 80 a2) (< 430 a2)) (setq nopan "pannello 80>A2>430"))

	    )
	  )
	)
      )
    
    (IF NOPAN
      (progn
	(command"_zoom" "_C" "_non" (polar (nth 1 lispt) (angle (nth 1 lispt) (nth 2 lispt)) (/ (distance (nth 1 lispt) (nth 2 lispt)) 2)) "800")
	(command"_circle" "_non" (polar (nth 1 lispt) (angle (nth 1 lispt) (nth 2 lispt)) (/ (distance (nth 1 lispt) (nth 2 lispt)) 2)) "200")
	(ALERT (strcat "Pannello fuori standard produttivi !! CASO " nopan )))
      )


    )
  )
(defun c:regpan ()
  ;(start)
  (setq grd (ssget "_x" (list '(8 . "siglepan"))))
  (if grd (command"_erase" grd ""))
  (command"_layer" "_of" "3dsimb" "")
  (setq gr (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (controlla_pan gr)
  (COMMAND"_ZOOM" "_E" )
  (regpan)
  ;(stop)
)
(defun carica_F (nfile perc)
  (if (findfile (strcat perc "$$" nfile ".txt"))
    (setq tbx (carica (strcat "$$" nfile) perc))
    (setq tbx (carica nfile perc))
    )
  tbx
  )
(defun carica_F+ (nfile perc)
  (if (findfile (strcat perc "$$" nfile ".txt"))
    (setq tbx (carica+ (strcat "$$" nfile) perc))
    (setq tbx (carica+ nfile perc))
    )
  tbx
  )
(defun C:ripris ()
  (setq *error* nil)
  (if file (close file))
  (if filea (close filea))
  (if filed (close filed))
  (if filer (close filer))
  (if filew (close filew))
  (if filet (close filet))
  (setq file (open "tmp.bat" "w"))
  (write-line (strcat "rename " modalita "*.tx_ " "*.txt") file)
  (close file)
  (command"_sh" "tmp.bat")
)
(defun regpan () ;dopo
    (setq *error* nil)

;;;  (setq Profl (selezpar 'ProfFissLana "Profilo fissaggio lana: ")) ;profilo
  ;(setq giuntopan (selezpar 'giuntopan "Giunto pannello: "));profilo
  (SETQ NR 0)
  (IF (NULL lispan) (progn (setq lispan (carica_f "pannellil" modalita) lispn $tb$)
;;;		      (if (= filtrafori "Si") (setq lispan_sf tb_sf lispan_nf tb_nf lispn_sf $tb_sf$ lispn_nf $tb_nf$))
		      ))
  (IF (NULL lispan_r) (progn (setq lispan_r (carica_f "pannellil_r" modalita) lispn_r $tb$)
;;;		      (if (= filtrafori "Si") (setq lispan_sf tb_sf lispan_nf tb_nf lispn_sf $tb_sf$ lispn_nf $tb_nf$))
		      ))

  (IF (NULL DIBpan) (progn (setq DIBpan (carica_f "DIBpan" modalita) DIBpn $tb$)
;;;		      (if (= filtrafori "Si") (setq DIBpan_sf tb_sf DIBpan_nf tb_nf DIBpn_sf $tb_sf$ DIBpn_nf $tb_nf$))
		      ))
  (IF (NULL DIBpan_r) (progn (setq DIBpan_r (carica_f "DIBpan_r" modalita) DIBpn_r $tb$)
;;;		      (if (= filtrafori "Si") (setq DIBpan_sf tb_sf DIBpan_nf tb_nf DIBpn_sf $tb_sf$ DIBpn_nf $tb_nf$))
		      ))

  (IF (NULL lispas) (progn (setq lispas (carica_F "pannelli" modalita) lisps $tb$)
;;;		      (if (= filtrafori "Si") (setq lispas_sf tb_sf lispas_nf tb_nf lisps_sf $tb_sf$ lisps_nf $tb_nf$))
		      ))

  (IF (NULL DIBpaS) (progn (setq DIBpaS (carica_f "DIBpaS" modalita) DIBpS $tb$)
		      (setq distpas dibpas)
;;;		      (if (= filtrafori "Si") (setq DIBpas_sf tb_sf DIBpas_nf tb_nf DIBps_sf $tb_sf$ DIBps_nf $tb_nf$))
		      ))
;;;  (IF (or (null fogl) (NULL fogli)) (setq fogli (carica "fogli" "") fogl $tb$));rinfogli

  (IF (NULL sviluppi) (progn (setq sviluppi (carica_F "sviluppi" modalita) svilupp $tb$)
			))
  (IF (NULL sviluppi_r) (progn (setq sviluppi_r (carica_F "sviluppi_r" modalita) svilupp_R $tb$)
;;;			(if (= filtrafori "Si") (setq sviluppi_sf tb_sf sviluppi_nf tb_nf svilupp_sf $tb_sf$ svilupp_nf $tb_nf$))
			))

  (IF (NULL DIBsvi) (progn (setq DIBsvi (carica_F "DIBsvi" modalita) DIBsv $tb$)
		      ))
  (IF (NULL DIBsvi_r) (progn (setq DIBsvi_R (carica_F "DIBsvi_r" modalita) DIBsv_r $tb$)
;;;		      (if (= filtrafori "Si") (setq DIBsvi_sf tb_sf DIBsvi_nf tb_nf DIBsv_sf $tb_sf$ DIBsv_nf $tb_nf$))
		      ))

  (IF (NULL cesoiati) (setq cesoiati (carica "cesoiati" modalita) cesoiat $tb$))
  (IF (NULL cesoiati_r) (setq cesoiati_R (carica "cesoiati_r" modalita) cesoiat_r $tb$))
  (IF (NULL lanadir) (setq lanadir (carica "lanadir" "") lanadi $tb$))
;;;  (IF (NULL lanacer) (setq lanacer (carica "lanacer" modalita) lanace $tb$))
  (IF (NULL mater) (setq mater (carica "mater" modalita) mate $tb$))
;;;  (IF (NULL rinfpiede) (setq rinfpiede (carica "rinfpiede" modalita) rinfpied $tb$))
  (IF (NULL tabrinfor) (setq tabrinfor (carica "tabrinf" modalita) tabrinfo $tb$))
  (IF (NULL DISTFORI) (SETQ DISTFORI (CARICA "DISTFORI" "")))
  (IF (NULL POSPON) (setq POSPON (carica "POSPONTI" "")))
  (IF (NULL RINFORZI) (setq RINFORZI (carica "RINFORZI" "")))
  (setq nbasepas (substr (car (nth 1 LISPAS)) 1 3))
  (setq gr (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
    (setq nrtotpannelli (sslength gr))
  (WHILE (SETQ ENT (SSNAME GR NR))
     (setq listapp nil)
     (setq listappr nil)
     (setq listap nil)
     (setq listapr nil)
     (setq listaps nil)
     (setq codrp (cdr (assoc '5 (entget ent))))
     (SETQ LISPT NIL)
     (FOREACH N (ENTGET ent)
        (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT)))
     )
     (setq lispt (reverse lispt))
    (setq lispt_or lispt)
     (setq lung (distance (car lispt) (nth 1 lispt)))
    
     (if (= "pannelli-dritti" (cdr (assoc '8 (entget ent))))
       (setq angpan 0 lung2 0 SPES (distance (nth 1 lispt) (nth 2 lispt)) cart_ang "")
       (progn
	 (command"_dimangular" "" "_non" (nth 1 lispt) "_non" (nth 0 lispt) "_non" (nth 2 lispt) "_non" (nth 2 lispt))
	 (setq angpan (atof (angtos (cdr (assoc '42 (entget (entlast)))) 0 0)))
	 (entdel (entlast))
	 (setq lung2 (distance (nth 1 lispt) (nth 2 lispt)) SPES (distance (nth 2 lispt) (nth 3 lispt)))
	 (setq cart_ang "P")
       )
     )
     (SETQ HALT (CDR (ASSOC '39 (ENTGET ENT))))

    (SETQ controlcava (READ (leggi ent "PANPIEG")))
    (if (equal '(0 0) controlcava)
      (setq retrocava nil)
      (setq retrocava t)
      )
    
     (setq finit (leggi ent "PANFIN"))
    (if (null finit) (setq finit "A"))
     (SETQ RESIS (LEGGI ENT "PANRES"))    
     (SETQ MODOP (LEGGI ENT "PANTIP"))
     (SETQ MODOP_ NIL)
     (IF (NULL SVILPAN) (setq svilpan (carica "svilpan" "")))
     (if (null modop_) (setq modop_ modop))
     (setq hpan halt)
    (SETQ LUNGLAM NIL)
     (SETQ FORISN "No")(SETQ scatoleSN "No")
    (command"_layer" "_off" "Hforo" "_off" "Hprese" "_off" "testo" "")
     (setq grf (ssget "_f" (list (nth 0 lispt) (nth 1 lispt) (nth 2 lispt)) (list '(0 . "INSERT") '(2 . "FORO*"))))
        (command"_layer" "_on" "Hforo" "_on" "Hprese" "_on" "testo" "")
    (if grf  (SETQ FORISN "Si"))
    (if retrocava
      (setq Inclunggola (cadr (assoc 'Inclunggola parametri)))
      (setq inclunggola 0.0)
      )
    (if (/= angpan 0)
      
      (progn
	(if (= forisn "Si")
	  (progn
	    (setq lung_retro (+ lung (setq inc_x_f_lato2 (* 15 (sin (* (/ angpan 180.0) pi))))))
	    (setq lung2_retro (+ lung2 (setq inc_x_f_lato2 (* 15 (sin (* (/ angpan 180.0) pi))))))
	    )
	  (progn
	    (setq lung_retro (+ lung (setq inc_x_f_lato2 (* 25 (sin (* (/ angpan 180.0) pi))))))
	    (setq lung2_retro (+ lung2 (setq inc_x_f_lato2 (* 25 (sin (* (/ angpan 180.0) pi))))))
	    )
	  )
	)
      (progn
	(if (= forisn "Si")
	  (progn
	    (setq lung_retro lung )
	    (setq inc_x_f_lato2 0)
	    (setq lung2_retro lung2)
	    )
	  (progn
	    (setq lung_retro lung)
	    (setq inc_x_f_lato2 0)
	    (setq lung2_retro lung2)
	    )
	  )
	)
      )
	    
			
     (foreach m svilpan
       (if (and (= modop_ (car m)) (= finit (cadr m)) (= RESIS (NTH 2 M)))
	 (progn
	   (setq svilcorr m)
	   (setq halt_f (+ halt (nth 14 m) (nth 15 m)))
	   (setq lunglam (+ lung lung2 (nth 3 m) (nth 4 m)))
	   (setq lungretro (+ lung_retro lung2_retro inclunggola inclunggola (nth 6 m) (nth 7 m)(nth 8 m) (nth 9 m)))
	   )
	 )
     )
     (if (= modop_ "MAN")
       (progn
	 (setq rigeman (read (setq rigemant (leggi ent "SVILMAN"))))
	 (setq halt_f (+ halt (atof (rtos (nth 2 rigeman) 2 1))  (atof (rtos (nth 3 rigeman) 2 1))))
	 (setq lunglam (+ lung lung2 (atof (rtos (nth 0 rigeman) 2 1)) (atof (rtos (nth 1 rigeman) 2 1))))
	 )
       )
     (SETQ MODOP (LEGGI ENT "PANTIP"))
    (setq modop_ modop)
     (if (= modop "SPS") (setq modop_ "STD"))
     (if (= modop "SPD") (setq modop_ "SPC"))
    (SETQ LEN_F NIL)
     (if (null lunglam) (progn (alert "Riga mancante in Svilpan.txt.!!") (exit)))
;cesoiato frontale
    (setq len_f (list 1 2 3 (rtos lunglam)))
    (setq rig_f (list (strcat "Pannello cesoiato ") (nth 5 svilcorr) (atof (rtos lunglam 2 1)) (atof (rtos halt_f 2 1)) (nth 12 svilcorr)
		      (atoi (rtos (* (atof (rtos lunglam 2 1)) (atof (rtos halt_F 2 1))) 2 0)) "Nr" ))
    (if (setq percod_F (member rig_f cesoiat))
      (progn
	(setq nomepan_f (car (nth (- (LENGTH cesoiat) (length percod_f)) cesoiati
				  )))
	)
      (progn
	(setq file (S_OPEN (strcat modalita "cesoiati.txt") "a"))
	(setq newcod (rtos (+ 1 (atoi (substr (car (last cesoiati)) 4))) 2 0))
	(repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	(setq n_base (substr (car (last cesoiati)) 1 3))
	(setq rig-F (strcat "\"" n_base newcod "\"\t\"Pannello cesoiato " "\"\t\"" (NTH 1 RIG_F)"\"\t" (rtos (NTH 2 RIG_f) 2 1)
			    "\t" (rtos (NTH 3 RIG_f)2 1) "\t" (RTOS (NTH 4 RIG_f) 2 1) "\t" (rtos (NTH 5 RIG_f) 2 0)
			    "\t\"" (NTH 6 RIG_f) "\"")
	      )
	(SETQ nomepan_F (strcat n_base newcod))
	(IF REGP (SETQ RIG_A-f (strcat nomepan_F "|Pannello cesoiato "(nth 3 len_f)"|Nr.|" (rtos (nth 2 rig_F) 2 1) "|" (rtos (nth 3 rig_f) 2 1) "|"(rtos (nth 4 rig_f) 2 1)
				       "|Cesoiato|" "|" finit "|" "|" "|0|0||||"
				       )
		       )
	  (SETQ RIG_A-f (strcat nomepan_F "|Pannello cesoiato "(nth 3 len_f)"|Nr.|" (rtos (nth 2 rig_F) 2 1) "|" (rtos (nth 3 rig_f) 2 1) "|"(rtos (nth 4 rig_f) 2 1)
				"|Cesoiato|" "|" finit "|" "|" "|"
				)
		)
	  )
	(write-line rig-f file)
	(s_close file (strcat modalita "cesoiati.txt"))
	(setq cesoiati (REVERSE (CONS (READ (STRCAT "(" rig-f ")")) (REVERSE cesoiati))))
	(setq cesoiat (REVERSE (CONS (CDR (READ (STRCAT "(" rig-f ")"))) (REVERSE cesoiat))))
	
	(if filea (write-line rig_A-f filea))	 ; (inquad rig_A-f)
	(setq file (S_OPEN (strcat modalita "DIBces.txt") "a"))
	(setq rig (strcat "\"" nomePAN_f "\"\t\"" (nth 5 svilcorr) "\"\t" (RTOS (NTH 2 rig_f) 2 0)
			  "\t" (RTOS (NTH 3 rig_f) 2 0) "\t" (RTOS (NTH 4 rig_f) 2 0) "\t" (RTOS (NTH 5 rig_f) 2 0)))
	(write-line rig file)
	(SETQ RIG_A (STRCAT NOMEPAN_f "|" (NTH 3 LEN_f) "|" (RTOS (/ (atof (rtos (NTH 5 rig_f))) 1000000) 2 2) "||"))
	(if filed (WRITE-LINE RIG_A FILED))
	(s_close file (strcat modalita "DIBces.txt"))
	)
      )
    (setq listap (cons (list nomepan_f 0 0 1 0 0) listap))
    (setq listapp (cons (list nomepan_f 0 0 1 0 0) listapp))
;cesoiato retro
    (setq len_f (list 1 2 3 (rtos lungretro)))
;;;    (if (= Forisn "Si") (setq nomelamretro (nth 11 svilcorr)) (setq nomelamretro (nth 10 svilcorr)))
    (if (or (/= lung2 0) (< (+ lung lung2) 100)) (setq nomelamretro (nth 11 svilcorr)) (setq nomelamretro (nth 10 svilcorr)))
    (setq rig_f (list (strcat "Pannello cesoiato ") nomelamretro (atof (rtos lungretro 2 1)) (atof (rtos halt_f 2 1)) (nth 13 svilcorr)
		      (atoi (rtos (* (atof (rtos lungretro 2 1)) (atof (rtos halt_F 2 1))) 2 0)) "Nr" ))
    (if (setq percod_Fr (member rig_f cesoiat_r))
      (progn
	(setq nomepan_fr (car (nth (- (LENGTH cesoiat_R) (length percod_fr)) cesoiati_r)))
	)
      (progn
	(setq file (S_OPEN (strcat modalita "cesoiati_r.txt") "a"))
	(setq newcodr (rtos (+ 1 (atoi (substr (car (last cesoiati_r)) 4))) 2 0))
	(repeat (- 5 (strlen newcodr)) (setq newcodr (strcat "0" newcodr)))
	(setq n_base (substr (car (last cesoiati_r)) 1 3))
	(setq rig-F (strcat "\"" n_base newcodr "\"\t\"Pannello cesoiato " "\"\t\"" (NTH 1 RIG_F)"\"\t" (rtos (NTH 2 RIG_f) 2 1)
			    "\t" (rtos (NTH 3 RIG_f)2 1) "\t" (RTOS (NTH 4 RIG_f) 2 1) "\t" (rtos (NTH 5 RIG_f) 2 0)
			    "\t\"" (NTH 6 RIG_f) "\"")
	      )
	(SETQ nomepan_Fr (strcat n_base newcodr))
	(IF REGP (SETQ RIG_A-f (strcat nomepan_Fr "|Pannello cesoiato "(nth 3 len_f)"|Nr.|" (rtos (nth 2 rig_F) 2 1) "|" (rtos (nth 3 rig_f) 2 1) "|"(rtos (nth 4 rig_f) 2 1)
				       "|Cesoiato|" "|" finit "|" "|" "|0|0||||"
				       )
		       )
	  (SETQ RIG_A-f (strcat nomepan_Fr "|Pannello cesoiato "(nth 3 len_f)"|Nr.|" (rtos (nth 2 rig_F) 2 1) "|" (rtos (nth 3 rig_f) 2 1) "|"(rtos (nth 4 rig_f) 2 1)
				"|Cesoiato|" "|" finit "|" "|" "|"
				)
		)
	  )
	(write-line rig-f file)
	(s_close file (strcat modalita "cesoiati_r.txt"))
	(setq cesoiati_r (REVERSE (CONS (READ (STRCAT "(" rig-f ")")) (REVERSE cesoiati_R))))
	(setq cesoiat_R (REVERSE (CONS (CDR (READ (STRCAT "(" rig-f ")"))) (REVERSE cesoiat_R))))
	
	(if filea (write-line rig_A-f filea))	 ; (inquad rig_A-f)
	(setq file (S_OPEN (strcat modalita "DIBces_R.txt") "a"))
	(setq rig (strcat "\"" nomePAN_fr "\"\t\"" nomelamretro "\"\t" (RTOS (NTH 2 rig_f) 2 0)
			  "\t" (RTOS (NTH 3 rig_f) 2 0) "\t" (RTOS (NTH 4 rig_f) 2 0) "\t" (RTOS (NTH 5 rig_f) 2 0)))
	(write-line rig file)
	(SETQ RIG_A (STRCAT NOMEPAN_fr "|" (NTH 3 LEN_f) "|" (RTOS (/ (atof (rtos (NTH 5 rig_f))) 1000000) 2 2) "||"))
	(if filed (WRITE-LINE RIG_A FILED))
	(s_close file (strcat modalita "DIBces_R.txt"))
	)
      )
    (setq listapr (cons (list nomepan_fr 0 0 1 0 0) listapr))
    (setq listappr (cons (list nomepan_fr 0 0 1 0 0) listappr))
    
;fori
     (SETQ FORISN "No")(SETQ scatoleSN "No")
      (command"_layer" "_off" "Hforo" "_off" "Hprese" "_off" "testo" "")
     (setq grf (ssget "_f" (list (nth 0 lispt) (nth 1 lispt) (nth 2 lispt)) (list '(0 . "INSERT") '(2 . "FORO*"))))
        (command"_layer" "_on" "Hforo" "_on" "Hprese" "_on" "testo" "")
    (if grf  (SETQ FORISN "Si"))


     (IF (NULL PRESE) (setq prese (carica "prese" "")))
     (setq cart_for "")
         (SETQ FORISN "No" scatoleSN "No")

     (IF GRF
       (PROGN
	      (setq cart_for "F")

         (SETQ FORISN "Si")
	 (SETQ NRF 0)
  	 (WHILE (SETQ ENTF (SSNAME GRF NRF))
	    (SETQ NFOR (CDR (ASSOC '2 (ENTGET ENTF))))
	    (setq nfor (substr nfor 1 8))
	    (if (> lung (DISTANCE (NTH 0 LISPT) (CDR (ASSOC '10 (ENTGET ENTF)))))
	      (SETQ XFOR (ATOF (RTOS (DISTANCE (NTH 0 LISPT) (CDR (ASSOC '10 (ENTGET ENTF)))) 2 1)))
	      (SETQ XFOR (ATOF (RTOS
				 (+ (DISTANCE (NTH 0 LISPT) (NTH 1 LISPT))
				    (DISTANCE (NTH 1 LISPT) (CDR (ASSOC '10 (ENTGET ENTF))))
				 )
				 2 1;massimofori
				)
			  )
      	      )
	    )
	    (SETQ YFOR (ATOF (CDR (ASSOC '1 (ENTGET (ENTNEXT ENTF))))))
	    (SETQ DFOR "Scatola portafrutti")
	    (SETQ NRF (+ 1 NRF))
;;;	    (SETQ LFOR_R (LIST "000000000" (NTH 8 (ASSOC NFOR PRESE)) (NTH 9 (ASSOC NFOR PRESE)) 1
;;;			       (ATOF (RTOS (+ (nth 3 svilcorr) XFOR) 2 1))
;;;			       (ATOF (RTOS (+ (nth 15 svilcorr) YFOR) 2 1))))
;;;            (SETQ LFOR (LIST "000000000" (NTH 5 (ASSOC NFOR PRESE)) (NTH 6 (ASSOC NFOR PRESE)) 1
;;;			     (ATOF (RTOS (+ (nth 6 svilcorr) inclunggola (nth 8 svilcorr) XFOR) 2 1))
;;;			     (ATOF (RTOS (+ (nth 15 svilcorr) YFOR) 2 1))))
;;;            (SETQ LISTAP (CONS LFOR LISTAP))
;;;            (SETQ LISTAPR (CONS LFOR_R LISTAPR))
	    (if (/= "No scatola" (NTH 1 (ASSOC NFOR PRESE))) (SETQ scatoleSN "Si"))
	    (SETQ LFOR (LIST "000000000" (NTH 5 (ASSOC NFOR PRESE)) (NTH 6 (ASSOC NFOR PRESE)) 1
			       (ATOF (RTOS (+ (nth 3 svilcorr) XFOR) 2 1))
			       (ATOF (RTOS (+ (nth 15 svilcorr) YFOR) 2 1))))
            (SETQ LFOR_R (LIST "000000000" (NTH 8 (ASSOC NFOR PRESE)) (NTH 9 (ASSOC NFOR PRESE)) 1
			     (ATOF (RTOS (+ (nth 6 svilcorr) inclunggola (nth 8 svilcorr) XFOR) 2 1))
			     (ATOF (RTOS (+ (nth 15 svilcorr) YFOR) 2 1))))
            (SETQ LISTAP (CONS LFOR LISTAP))
            (SETQ LISTAPR (CONS LFOR_R LISTAPR))
	   	    (SETQ LFORp (LIST "000000000" (NTH 5 (ASSOC NFOR PRESE)) (NTH 6 (ASSOC NFOR PRESE)) 1
			       (ATOF (RTOS (+ 0 XFOR) 2 1))
			       (ATOF (RTOS (+ 0 YFOR) 2 1))))
            (SETQ LFOR_pR (LIST "000000000" (NTH 8 (ASSOC NFOR PRESE)) (NTH 9 (ASSOC NFOR PRESE)) 1
			     (ATOF (RTOS (+ 0  (nth 8 svilcorr) XFOR) 2 1))
			     (ATOF (RTOS (+ 0 YFOR) 2 1))))
            (SETQ LISTAPp (CONS LFORp LISTAPp))
            (SETQ LISTAPpR (CONS LFOR_pR LISTAPpR))

	   
	  )     
       )
     (SETQ FORISN "No" scatoleSN "No")
       )
    (setq cart_rinf "")
    (setq listapr_new nil)
    (if (/= inc_x_f_lato2 0)
      (progn
      (foreach n listapr
	(if (and (= "000000000" (nth 0 n)) (> (nth 4 n) lung_retro))
	  (setq listapr_new (cons (list (nth 0 n) (nth 1 n) (nth 2 n) (nth 3 n) (+ inc_x_f_lato2 inc_x_f_lato2 (nth 4 n)) (nth 5 n)) listapr_new))
	  (setq listapr_new (cons (list (nth 0 n) (nth 1 n) (nth 2 n) (nth 3 n) (nth 4 n) (nth 5 n)) listapr_new))
	  )
	)
          (setq listapr (reverse listapr_new))
      )

      )
    
	  
;fine fori
;INIZIO RINFOEZI

;;;     (if (= filtrafori "Si")
;;;       (if (= forisn "Si") (setq lispan_& lispan_sf lispn_& lispn_sf dibpan_& dibpan_sf dibpn_& dibpn_sf
;;;			       lispas_& lispas_sf lisps_& lisps_sf dibpas_& dibpas_sf dibps_& dibpn_sf
;;;			       sviluppi_& sviluppi_sf svilupp_& svilupp_sf dibsvi_& dibsvi_sf dibsv_& dibsv_sf
;;;			       )
;;;                          (setq lispan_& lispan_nf lispn_& lispn_nf dibpan_& dibpan_nf dibpn_& dibpn_nf
;;;			       lispas_& lispas_nf lisps_& lisps_nf dibpas_& dibpas_nf dibps_& dibpn_nf
;;;			       sviluppi_& sviluppi_nf svilupp_& svilupp_nf dibsvi_& dibsvi_nf dibsv_& dibsv_nf
;;;			       )
;;;       )
;;;       (setq lispan_& lispan lispn_& lispn dibpan_& dibpan dibpn_& dibpn
;;;			       lispas_& lispas lisps_& lisps dibpas_& dibpas dibps_& dibpn
;;;			       sviluppi_& sviluppi svilupp_& svilupp dibsvi_& dibsvi dibsv_& dibsv
;;;			       )
;;;       )
     (setq rinfsn "No")
     (setq angr (+ (angle (nth 0 lispt) (nth 1 lispt)) (/ pi 2)))
     (setq angr1 (+ (angle (nth 1 lispt) (nth 2 lispt)) (/ pi 2)))
     (if (= 0 lung2)
         (setq grr (ssget "_f" (list (polar (nth 0 lispt) angr 1.5) (polar (nth 1 lispt) angr 1.5)) (list '(0 . "MLINE") '(8 . "rinforzi"))))
         (setq grr (ssget "_f" (list (polar (nth 0 lispt) angr 1.5) (polar (nth 1 lispt) angr 1.5) (polar (nth 2 lispt) angr1 1.5)) (list '(0 . "MLINE") '(8 . "rinforzi"))))
     )
       
     (IF GRr
       (PROGN
         (SETQ rinfsn "Si")
(setq cart_rinf "R")
	  (SETQ NRF 0)
	  (setq listar nil)
	  (WHILE (SETQ ENTF (SSNAME GRr NRF))
	    (SETQ Nrin (STRCAT "" (substr (cdr (assoc '2 (entget entf))) 10)))
	    (SETQ LISPTf NIL)
	    (FOREACH Nf (ENTGET entf)
	        (IF (= '11 (CAR Nf)) (SETQ LISPTf (CONS (CDR Nf) LISPTf)))
	    )
	    (setQ lisptf (reverse lisptf))
	    (setq lgrin (atoi (rtos (distance (nth 0 lisptf) (nth 1 lisptf)) 2 0)))
	    (setq drin (strcat "Rinforzo " Nrin))
	    (setq lrin (list nrin drin lgrin))
	    (if (> lung (DISTANCE (NTH 0 LISPT) (nth 0 lisptf)))
	      (SETQ Xrin (ATOI (RTOS (DISTANCE (NTH 0 LISPT) (nth 0 lisptf)) 2 0)))
	      (SETQ Xrin (ATOI (RTOS
				 (+ (DISTANCE (NTH 0 LISPT) (NTH 1 LISPT))
				    (DISTANCE (NTH 1 LISPT) (nth 0 lisptf))
				 )
				 2 0
				)
			  )
	      )
    	    )
    	    (setq listar nil)
	    (setq hrinforzo (leggi entf "HRIN"))
	    (if hrinforzo (setq hrinforzo (atoi hrinforzo)) (setq hrinforzo 80))
;reg rinforzi
            (setq rig_p (list "Rinforzo pannello" nrin lgrin hrinforzo "HOR" "Nr"))
	    
            (if (setq percod_p (member rig_p tabrinfo))
                (progn
                   (setq nomerin (car (nth (- (LENGTH tabrinfo) (length percod_p)) tabrinfor)))
                )  
                (progn
  	           (setq file (S_OPEN (strcat modalita "tabrinf.txt") "a"))
  	           (setq newcod (rtos (+ 1 (atoi (substr (car (last tabrinfor)) 4))) 2 0))
	           (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
		   (setq n_base (substr (car (last tabrinfor)) 1 3))
                   (setq rig-p (strcat "\"" n_base newcod "\"\t\"Rinforzo pannello\"\t\"" (NTH 1 RIG_p)"\"\t" (rtos (NTH 2 RIG_p) 2 1) "\t" (rtos  (NTH 3 RIG_p) 2 0) "\t\"HOR\"\t\"Nr\""))
                   (SETQ nomerin (strcat n_base newcod))
                   (IF REGP (SETQ RIG_A-p (strcat nomerin "|Rinforzo pannello|Nr.|" (rtos (nth 2 rig_p) 2 1) "|||Rinforzo|||||0|0||||"))
		     (SETQ RIG_A-p (strcat nomerin "|Rinforzo pannello|Nr.|" (rtos (nth 2 rig_p) 2 1) "|||Rinforzo|||||"))
		     )
                   (write-line rig-p file)
                   (s_close file (strcat modalita "tabrinf.txt"))
		   (setq TABRINFor (REVERSE (CONS (READ (STRCAT "(" RIG-P ")")) (REVERSE TABRINFor))))
                   (setq TABRINfo (REVERSE (CONS (CDR (READ (STRCAT "(" RIG-P ")"))) (REVERSE TABRINfo))))
		  
                   (if filea (write-line rig_A-p filea)) ;(inquad rig_A-p)
                   (setq file (S_OPEN (strcat modalita "DIBrin.txt") "a"))
                   (setq rig (strcat "\"" nomerin "\"\t\"" (NTH 1 rig_p) "\"\t" (rtos (nth 2 rig_p) 2 1)))
                   (write-line rig file)
                   (s_close file (strcat modalita "DIBrin.txt"))
	           (SETQ RIG_A (STRCAT NOMErin "|" (NTH 1 rig_P) "|" (RTOS (/ (atof (rtos (NTH 2 rig_P))) 1000) 2 2) "||"))
		   (SETQ RIG_A6 (STRCAT NOMErin "|" (NTH 1 rig_P) "|" (RTOS (/ (atof (rtos (NTH 2 rig_P))) 1000) 2 2) "||||"))
		   
                   (if filed (WRITE-LINE RIG_A FILED));ok
                   (if filed (WRITE-LINE RIG_A6 FILED6));ok
                )
             )
             (setq LISTAPs (cons (list NOMERIN 1 XRIN (atoi (rtos (caddr (car lisptf)) 2 0)) 0 0) LISTAPs))
	    
    	    (SETQ NRF (+ 1 NRF))
  	  )         
       )
       (SETQ rinfsn "No")
     )
;FINE RINFORZI



;INIZIO RINFORZI PUNTIFORMI$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
     (setq rfPANsn "No")

     (if (= 0 lung2)
         (setq grr (ssget "_f" (list (setq pll1 (polar (nth 0 lispt) angr 1.5)) (setq pll2 (polar (nth 1 lispt) angr 1.5))) (list '(0 . "INSERT") '(2 . "rinforzo-*"))))
         (setq grr (ssget "_f" (list (setq pll1 (polar (nth 0 lispt) angr 1.5)) (setq pll2 (polar (nth 1 lispt) angr 1.5)) (setq pll3 (polar (nth 1 lispt) angr1 2)) (setq pll4 (polar (nth 2 lispt) angr1 2)))
			  (list '(0 . "INSERT") '(2 . "rinforzo-**"))))
     )
 	 
       
     (IF GRr
       (PROGN
	 (setq cart_rinf "R")
         (SETQ rfPANsn "Si")
	  (SETQ NRF 0)
	  (setq listar nil)
	  (WHILE (SETQ ENTF (SSNAME GRr NRF))
	    (SETQ +nomer (cdr (assoc '2 (entget entf))))
	    (setq +ptrf (cdr (assoc '10(entget entf))))
;;;	    (setq yrin (cdr (assoc '1 (entget (entnext  entf)))))
	    (if (> lung (DISTANCE (NTH 0 LISPT) +ptrf))
	      (SETQ Xrin (ATOI (RTOS (DISTANCE (NTH 0 LISPT) +ptrf) 2 0)))
	      (SETQ Xrin (ATOI (RTOS
				 (+ (DISTANCE (NTH 0 LISPT) (NTH 1 LISPT))
				    (DISTANCE (NTH 1 LISPT) +ptrf)
				 )
				 2 0
				)
			  )
	      )
    	    )
	        	    (setq listar nil)
	    (setq hrinforzo (leggi entf "HRIN"))
	    (if hrinforzo (setq hrinforzo (atoi hrinforzo)) (setq hrinforzo 80))
;reg rinforzi
	    (setq nrin (substr +nomer 10))
	    (setq lgrin (- hpan (cadr (assoc 'diffaltrinv parametri))))
            (setq rig_p (list "Rinforzo pannello" nrin lgrin hrinforzo "VER" "Nr"))
	    
            (if (setq percod_p (member rig_p tabrinfo))
                (progn
                   (setq nomerin (car (nth (- (LENGTH tabrinfo) (length percod_p)) tabrinfor)))
                )  
                (progn
  	           (setq file (S_OPEN (strcat modalita "tabrinf.txt") "a"))
  	           (setq newcod (rtos (+ 1 (atoi (substr (car (last tabrinfor)) 4))) 2 0))
	           (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
		   (setq n_base (substr (car (last tabrinfor)) 1 3))
                   (setq rig-p (strcat "\"" n_base newcod "\"\t\"Rinforzo pannello\"\t\"" (NTH 1 RIG_p)"\"\t" (rtos (NTH 2 RIG_p) 2 1) "\t" (rtos  (NTH 3 RIG_p) 2 0) "\t\"VER\"\t\"Nr\""))
                   (SETQ nomerin (strcat n_base newcod))
                   (IF REGP (SETQ RIG_A-p (strcat nomerin "|Rinforzo pannello|Nr.|" (rtos (nth 2 rig_p) 2 1) "|||Rinforzo|||||0|0||||"))
		     (SETQ RIG_A-p (strcat nomerin "|Rinforzo pannello|Nr.|" (rtos (nth 2 rig_p) 2 1) "|||Rinforzo|||||"))
		     )
                   (write-line rig-p file)
                   (s_close file (strcat modalita "tabrinf.txt"))
		   (setq TABRINFor (REVERSE (CONS (READ (STRCAT "(" RIG-P ")")) (REVERSE TABRINFor))))
                   (setq TABRINfo (REVERSE (CONS (CDR (READ (STRCAT "(" RIG-P ")"))) (REVERSE TABRINfo))))
		  
                   (if filea (write-line rig_A-p filea)) ;(inquad rig_A-p)
                   (setq file (S_OPEN (strcat modalita "DIBrin.txt") "a"))
                   (setq rig (strcat "\"" nomerin "\"\t\"" (NTH 1 rig_p) "\"\t" (rtos (nth 2 rig_p) 2 1)))
                   (write-line rig file)
                   (s_close file (strcat modalita "DIBrin.txt"))
	           (SETQ RIG_A (STRCAT NOMErin "|" (NTH 1 rig_P) "|" (RTOS (/ (atof (rtos (NTH 2 rig_P))) 1000) 2 2) "||"))
		   (SETQ RIG_A6 (STRCAT NOMErin "|" (NTH 1 rig_P) "|" (RTOS (/ (atof (rtos (NTH 2 rig_P))) 1000) 2 2) "||||"))
		   
                   (if filed (WRITE-LINE RIG_A FILED));ok
                   (if filed (WRITE-LINE RIG_A6 FILED6));ok
                )
             )
	                 (setq LISTAPs (cons (list NOMERIN 1 XRIN (atoi (rtos (/ (cadr (assoc 'diffaltrinv parametri)) 2) 2 0)) 0 0) LISTAPs))

    	    (SETQ NRF (+ 1 NRF))
  	  )         
       )
       (SETQ rfPANsn "No")
     )
      
;FINE RINFORZI PUNTIFORMI$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
    
     (setq listapl nil)
     (setq finit (leggi ent "PANFIN"))
     (SETQ RESIS (LEGGI ENT "PANRES"))    
     (SETQ MODOP (LEGGI ENT "PANTIP"))
     (setq modop_ modop)
     (if (/= 0 lung2) (progn (setq valsv (strcat (rtos angpan 2 0) " " (rtos lung2 2 1)))) (setq valsv "-"))
     (if (= "MAN" modop_) (setq nn rigemant) (setq nn "NN"))
;sviluppo fronte    
     (if (= "Si" (leggi ent "BLOCCOSVI"))
         (setq lrig (list (strcat "Sviluppo Pannello Anteriore" ) (atof (rtos lunglam 2 1)) (atoi (rtos spes 2 0))
		      (atof (rtos halt_F 2 1)) (nth 5 svilcorr) forisn valsv 1500 MODOP_ "Nr"))
         (setq lrig (list (strcat "Sviluppo Pannello Anteriore" ) (atof (rtos lunglam 2 1)) (atoi (rtos spes 2 0))
		      (atof (rtos halt_F 2 1)) (nth 5 svilcorr) forisn valsv (atoi (rtos (length listap) 2 0)) MODOP_ "Nr"))
       )
    
     (setq newpan nil)
     (SETQ LIS1 NIL)
     (FOREACH PANDL sviluppi
        (IF (equal (CDR PANDL) LRIG)
          (SETQ LIS1 (CONS PANDL LIS1))
        )
     )
;sviluppi
    (IF (NULL LIS1) (SETQ NEWPAN "T"))
    (SETQ LIS LIS1)
    (if (AND LISTAP (null newPAN))
      (progn
	(SETQ LIS LIS1)
	(FOREACH PN (reverse LISTAP);sono solo fori
          (SETQ LISZ NIL)
	  (FOREACH N (reverse LIS  )
;;;	    (SETQ PK (CONS (CAR N) PN))
;;;	    (setq pk (list (car n) (nth 0 pn) halt finit forisn (nth 1 pn) (nth 2 pn) (nth 3 pn) (nth 4 pn) (nth 5 pn)))
	    (setq pk (list (car n) (nth 0 pn) (atoi (rtos halt 2 0)) (nth 5 svilcorr) forisn (nth 1 pn) (nth 2 pn) (nth 3 pn) (nth 4 pn) (nth 5 pn)))
	    (IF (MEMBER PK dibsvi) (PROGN  (SETQ LISZ (CONS N LISZ))))
	    (SETQ LIS LISZ)
	  )
	)
      )
    )
    (IF LIS (SETQ NEWPAN NIL NOMEPAN (CAR (LAST LIS))) (SETQ NEWPAN "T"))
    (if newPAN
       (progn
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last sviluppi)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq file (S_OPEN (strcat modalita "sviluppi.txt") "a"))
	 (if (findfile (strcat modalita "$$sviluppi.txt")) (setq filet (S_OPEN (strcat modalita "$$sviluppi.txt") "a")) (setq filet nil))

	 (setq n_base (substr (car (last sviluppi)) 1 3))
	 (if (= "Si" forisn) (setq dessvi "SvilFor") (setq dessvi "SvilnonFor"))
;;;	 (setq rig (strcat "\""n_base newcod "\"\t\"Sviluppo Pannello " (nth 2 (assoc finit fogl)) "\"\t" (rtos lunglam 2 1) "\t" (rtos spes 2 0) "\t"
;;;			   (rtos halt_F 2 1) "\t\"" finit "\"\t\"" forisn "\"\t\"" valsv "\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
;;;			   )
;;;	       nomepan (strcat n_base newcod))
	 (if (= "Si" (leggi ent "BLOCCOSVI"))
	   (setq rig (strcat "\""n_base newcod "\"\t\"Sviluppo Pannello Anteriore" "\"\t" (rtos lunglam 2 1) "\t" (rtos spes 2 0) "\t"
			   (rtos halt_F 2 1) "\t\"" (nth 5 svilcorr) "\"\t\"" forisn "\"\t\"" valsv "\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
			   )
	       nomepan (strcat n_base newcod))
	   (setq rig (strcat "\""n_base newcod "\"\t\"Sviluppo Pannello Anteriore" "\"\t" (rtos lunglam 2 1) "\t" (rtos spes 2 0) "\t"
			   (rtos halt_F 2 1) "\t\"" (nth 5 svilcorr) "\"\t\"" forisn "\"\t\"" valsv "\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
			   )
	       nomepan (strcat n_base newcod))
	   )
	 
	 (IF REGP (SETQ RIG_A (strcat nomepan "|Sviluppo Pannello Anteriore"  (nth 5 svilcorr)  "|Nr.|" (rtos lunglam 2 1) "|"  "|"(rtos halt_F 2 0)
			     "|" dessvi "|" MODOP_ "|" finit "||" nomepan ".00|0|0||||"
			   )
	       )
	   (SETQ RIG_A (strcat nomepan "|Sviluppo Pannello Anteriore"  (nth 5 svilcorr)  "|Nr.|" (rtos lunglam 2 1) "|"  "|"(rtos halt_F 2 0)
			     "|" dessvi "|" MODOP_ "|" finit "||" nomepan ".00|"
			   )
	       )
	 )
 	 (setq $sviluppi (cons nomepan $sviluppi))

         (write-line rig file)
	 (if filet (write-line  rig filet))

         (s_close file (strcat modalita "sviluppi.txt"))
	 (if filet (s_close filet (strcat modalita "$$sviluppi.txt")))

		   (setq sviluppi (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE sviluppi))))
                   (setq svilupp (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE svilupp))))
		   (if (= filtrafori "Si") (progn
	     (setq sviluppi_sf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE sviluppi_sf))))
                   (setq svilupp_sf (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE svilupp_sf))))
		   (setq sviluppi_nf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE sviluppi_nf))))
                   (setq svilupp_nf (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE svilupp_nf))))
					     ))
         (if filea (write-line rig_A filea));	 (inquad rig_A)
         (setq file (S_OPEN (strcat modalita "dibsvi.txt") "a"))
	 (if (findfile (strcat modalita "$$dibsvi.txt")) (setq filet (S_OPEN (strcat modalita "$$dibsvi.txt") "a")) (setq filet nil))

 	 (foreach n LISTAP
           (setq rig_& (strcat "\"" nomePAN "\"\t\""(NTH 0 N) "\"\t" (rtos halt 2 0) "\t\"" (nth 5 svilcorr) "\"\t\"" forisn
			     "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)));massimofori
	   (setq rig (strcat "\"" nomePAN "\"\t\""(NTH 0 N) "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)))
           (write-line rig_& file)
	   (if filet (write-line  rig_& filet))

	   	   (setq dibsvi (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE dibsvi))))
                   (setq dibsv (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE dibsv))))
	   	   (if (= filtrafori "Si") (progn (setq dibsvi_sf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE dibsvi_sf))))
                   (setq dibsv_sf (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE dibsv_sf))))
	   	   (setq dibsvi_nf (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE dibsvi_nf))))
                   (setq dibsv_nf (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE dibsv_nf))))
					     ))

	   (SETQ RIG_A (STRCAT NOMEPAN "|" (NTH 0 N) "|" (RTOS (NTH 3 N) 2 0) "||"))
	   (SETQ RIG_A6 (STRCAT NOMEPAN "|" (NTH 0 N) "|" (RTOS (NTH 3 N) 2 0) "||||"))
	   
	   (IF (/= "000000000" (NTH 0 N)) (if filed (WRITE-LINE RIG_A FILED)))
;;;	   (IF (/= "000000000" (NTH 0 N)) (WRITE-LINE RIG_A6 FILED6))
	 )
	 
         (s_close file (strcat modalita "dibsvi.txt"))
	 (if filet (s_close filet (strcat modalita "$$dibsvi.txt")))


	 
       )
    )
    (setq nomesvi nomepan)
;sviluppi retro    
     (if (= "Si" (leggi ent "BLOCCOSVI"))
         (setq lrig (list (strcat "Sviluppo Pannello Posteriore" ) (atof (rtos lungretro 2 1)) (atoi (rtos spes 2 0))
		      (atof (rtos halt_F 2 1)) nomelamretro forisn valsv 1500 MODOP_ "Nr"))
         (setq lrig (list (strcat "Sviluppo Pannello Posteriore" ) (atof (rtos lungretro 2 1)) (atoi (rtos spes 2 0))
		      (atof (rtos halt_F 2 1)) nomelamretro forisn valsv (atoi (rtos (length listap) 2 0)) MODOP_ "Nr"))
       )
    
     (setq newpan nil)
     (SETQ LIS1 NIL)
     (FOREACH PANDL sviluppi_r
        (IF (equal (CDR PANDL) LRIG)
          (SETQ LIS1 (CONS PANDL LIS1))
        )
     )
    (IF (NULL LIS1) (SETQ NEWPAN "T"))
    (SETQ LIS LIS1)
    (if (AND LISTAPr (null newPAN))
      (progn
	(SETQ LIS LIS1)
	(FOREACH PN (reverse LISTAPr);sono solo fori
          (SETQ LISZ NIL)
	  (FOREACH N (reverse LIS  )
;;;	    (SETQ PK (CONS (CAR N) PN))
;;;	    (setq pk (list (car n) (nth 0 pn) halt finit forisn (nth 1 pn) (nth 2 pn) (nth 3 pn) (nth 4 pn) (nth 5 pn)))
	    (setq pk (list (car n) (nth 0 pn) (atoi (rtos halt 2 0)) nomelamretro forisn (nth 1 pn) (nth 2 pn) (nth 3 pn) (nth 4 pn) (nth 5 pn)))
	    (IF (MEMBER PK dibsvi_r) (PROGN  (SETQ LISZ (CONS N LISZ))))
	    (SETQ LIS LISZ)
	  )
	)
      )
    )
    (IF LIS (SETQ NEWPAN NIL NOMEPAN (CAR (LAST LIS))) (SETQ NEWPAN "T"))
    (if newPAN
       (progn
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last sviluppi_r)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq file (S_OPEN (strcat modalita "sviluppi_r.txt") "a"))
	 (setq n_base (substr (car (last sviluppi_r)) 1 3))
	 (if (= "Si" forisn) (setq dessvi "SvilFor") (setq dessvi "SvilnonFor"))
;;;	 (setq rig (strcat "\""n_base newcod "\"\t\"Sviluppo Pannello " (nth 2 (assoc finit fogl)) "\"\t" (rtos lunglam 2 1) "\t" (rtos spes 2 0) "\t"
;;;			   (rtos halt_F 2 1) "\t\"" finit "\"\t\"" forisn "\"\t\"" valsv "\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
;;;			   )
;;;	       nomepan (strcat n_base newcod))
	 (if (= "Si" (leggi ent "BLOCCOSVI"))
	   (setq rig (strcat "\""n_base newcod "\"\t\"Sviluppo Pannello Posteriore" "\"\t" (rtos lungretro 2 1) "\t" (rtos spes 2 0) "\t"
			   (rtos halt_F 2 1) "\t\"" nomelamretro "\"\t\"" forisn "\"\t\"" valsv "\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
			   )
	       nomepan (strcat n_base newcod))
	   (setq rig (strcat "\""n_base newcod "\"\t\"Sviluppo Pannello Posteriore" "\"\t" (rtos lungretro 2 1) "\t" (rtos spes 2 0) "\t"
			   (rtos halt_F 2 1) "\t\"" nomelamretro "\"\t\"" forisn "\"\t\"" valsv "\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
			   )
	       nomepan (strcat n_base newcod))
	   )
	 
	 (IF REGP (SETQ RIG_A (strcat nomepan "|Sviluppo Pannello Posteriore"  nomelamretro  "|Nr.|" (rtos lungretro 2 1) "|"  "|"(rtos halt_F 2 0)
			     "|" dessvi "|" MODOP_ "|" finit "||" nomepan ".00|0|0||||"
			   )
	       )
	   (SETQ RIG_A (strcat nomepan "|Sviluppo Pannello Posteriore"  nomelamretro  "|Nr.|" (rtos lungretro 2 1) "|"  "|"(rtos halt_F 2 0)
			     "|" dessvi "|" MODOP_ "|" finit "||" nomepan ".00|"
			   )
	       )
	 )
 	 (setq $sviluppi (cons nomepan $sviluppi))

         (write-line rig file)
	 (if filet (write-line  rig filet))

         (s_close file (strcat modalita "sviluppi_r.txt"))
	 (if filet (s_close filet (strcat modalita "$$sviluppi.txt")))

		   (setq sviluppi_r (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE sviluppi_r))))
                   (setq svilupp_r (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE svilupp_r))))
		   (if (= filtrafori "Si") (progn
	     (setq sviluppi_sf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE sviluppi_sf))))
                   (setq svilupp_sf (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE svilupp_sf))))
		   (setq sviluppi_nf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE sviluppi_nf))))
                   (setq svilupp_nf (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE svilupp_nf))))
					     ))
         (if filea (write-line rig_A filea));	 (inquad rig_A)
         (setq file (S_OPEN (strcat modalita "dibsvi_r.txt") "a"))
	 (if (findfile (strcat modalita "$$dibsvi.txt")) (setq filet (S_OPEN (strcat modalita "$$dibsvi.txt") "a")) (setq filet nil))

 	 (foreach n LISTAPr
           (setq rig_& (strcat "\"" nomePAN "\"\t\""(NTH 0 N) "\"\t" (rtos halt 2 0) "\t\"" nomelamretro "\"\t\"" forisn
			     "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)));massimofori
	   (setq rig (strcat "\"" nomePAN "\"\t\""(NTH 0 N) "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)))
           (write-line rig_& file)
	   (if filet (write-line  rig_& filet))

	   	   (setq dibsvi_r (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE dibsvi_r))))
                   (setq dibsv_r (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE dibsv_r))))
	   	   (if (= filtrafori "Si") (progn (setq dibsvi_sf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE dibsvi_sf))))
                   (setq dibsv_sf (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE dibsv_sf))))
	   	   (setq dibsvi_nf (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE dibsvi_nf))))
                   (setq dibsv_nf (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE dibsv_nf))))
					     ))

	   (SETQ RIG_A (STRCAT NOMEPAN "|" (NTH 0 N) "|" (RTOS (NTH 3 N) 2 0) "||"))
	   (SETQ RIG_A6 (STRCAT NOMEPAN "|" (NTH 0 N) "|" (RTOS (NTH 3 N) 2 0) "||||"))
	   
	   (IF (/= "000000000" (NTH 0 N)) (if filed (WRITE-LINE RIG_A FILED)))
;;;	   (IF (/= "000000000" (NTH 0 N)) (WRITE-LINE RIG_A6 FILED6))
	 )
	 
         (s_close file (strcat modalita "dibsvi_r.txt"))
	 (if filet (s_close filet (strcat modalita "$$dibsvi.txt")))


	 
       )
    )
    (setq nomesvir nomepan)
    
;pannello-------------------------------------------------------------------------------------------------------------
    (setq listapx listap)
    (setq listappx listapp)
    (setq listap nil)
    (setq listapp nil)
    (setq listapxr listapr)
    (setq listappxr listappr)
    (setq listapr nil)
    (setq listappr nil)

    (foreach n listapx
      	   (IF (= "000000000" (NTH 0 N))
	     (setq listap (cons n listap))
	     (setq listap (cons (subst nomesvi (car n) n) listap))
	   )
    )
(foreach n listapxr
      	   (IF (= "000000000" (NTH 0 N))
	     (setq listapr (cons n listapr))
	     (setq listapr (cons (subst nomesvir (car n) n) listapr))
	   )
    )    
    (foreach n listappx
      	   (IF (= "000000000" (NTH 0 N))
	     (setq listapp (cons n listapp))
	     (setq listapp (cons (subst nomesvi (car n) n) listapp))
	   )
    )
(foreach n listappxr
      	   (IF (= "000000000" (NTH 0 N))
	     (setq listappr (cons n listappr))
	     (setq listappr (cons (subst nomesvir (car n) n) listappr))
	   )
    )    
;pieghe su giunti
    (if (= lung2 0)
      (progn
         (command"_dimangular" "" "_non" (nth 1 lispt) "_non" (nth 2 lispt) "_non" (nth 0 lispt) "_non" (nth 2 lispt))
	 (setq angpan2 (atof (angtos (cdr (assoc '42 (entget (entlast)))) 0 0)))
	 (entdel (entlast))
	 (command"_dimangular" "" "_non" (nth 0 lispt) "_non" (nth 1 lispt) "_non" (last lispt) "_non" (nth 1 lispt))
	 (setq angpan1 (atof (angtos (cdr (assoc '42 (entget (entlast)))) 0 0)))
	 (entdel (entlast))

      )
      (progn
         (command"_dimangular" "" "_non" (nth 2 lispt) "_non" (nth 3 lispt) "_non" (nth 1 lispt) "_non" (nth 3 lispt))
	 (setq angpan2 (atof (angtos (cdr (assoc '42 (entget (entlast)))) 0 0)))
	 (entdel (entlast))
	 (command"_dimangular" "" "_non" (nth 0 lispt) "_non" (nth 1 lispt) "_non" (last lispt) "_non" (nth 1 lispt))
	 (setq angpan1 (atof (angtos (cdr (assoc '42 (entget (entlast)))) 0 0)))
	 (entdel (entlast))

      )
      
    )
    (setq pieghe (leggi ent "PANPIEG"))
    (SETQ PIEGA1 (CAR (READ PIEGHE)))
    (SETQ PIEGA2 (CADR (READ PIEGHE)))
;lamiera frontale
     (if (= lung2 0)
       (if (= "Si" (leggi ent "BLOCCOLAM"))
         (setq lrig (list "Pannello" (atoF (rtos lung 2 1)) (atoF (rtos lung2 2 1)) 0 0 (atoi (rtos spes 2 0)) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" 1500 MODOP_ "Nr"))
	 (setq lrig (list "Pannello" (atoF (rtos lung 2 1)) (atoF (rtos lung2 2 1)) 0 0 (atoi (rtos spes 2 0)) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" (atoi (rtos (length listap) 2 0)) MODOP_ "Nr"))
	 )
       (if (= "Si" (leggi ent "BLOCCOLAM"))
         (setq lrig (list "Angolo" (atoF (rtos lung 2 1)) (atoF (rtos lung2 2 1)) 0 0 (atoi (rtos spes 2 0)) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" 1500 MODOP_ "Nr"))
	 (setq lrig (list "Angolo" (atoF (rtos lung 2 1)) (atoF (rtos lung2 2 1)) 0 0 (atoi (rtos spes 2 0)) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" (atoi (rtos (length listap) 2 0)) MODOP_ "Nr"))
	 )
     )
     (setq newpan nil)
     (SETQ LIS1 NIL)
     (FOREACH PANDL LISPAN
        (IF (equal (CDR PANDL) LRIG)
          (SETQ LIS1 (CONS PANDL LIS1))
        )
     )
     (IF (NULL LIS1) (SETQ NEWPAN "T"))
     (SETQ LIS LIS1)
    
    (if (AND LISTAP (null newPAN))
      (progn
	(SETQ LIS LIS1)
	(FOREACH PN LISTAPp;sono solo fori
          (SETQ LISZ NIL)
	  (FOREACH N LIS  
	    (setq pk (list (car n) (nth 0 pn) (atoi (rtos halt 2 0)) finit forisn (nth 1 pn) (nth 2 pn) (nth 3 pn) (nth 4 pn) (nth 5 pn)))
	    (IF (MEMBER PK DIBPAN) (PROGN  (SETQ LISZ (CONS N LISZ))))
	    (SETQ LIS LISZ)
	  )
	)
      )
    )
    (IF LIS (SETQ NEWPAN NIL NOMEPAN (CAR (LAST LIS))) (SETQ NEWPAN "T"))
    (if newPAN
       (progn
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last lispan)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq file (S_OPEN (strcat modalita "pannellil.txt") "a"))
	 (if (findfile (strcat modalita "$$pannellil.txt")) (setq filet (S_OPEN (strcat modalita "$$pannellil.txt") "a")) (setq filet nil))
	 (setq n_base (substr (car (last lispan)) 1 3))
         
	 (if (= lung2 0)
	   (PROGN
	     (if (= "Si" (leggi ent "BLOCCOLAM"))
             (setq rig (strcat "\""n_base newcod "\"\t\"Pannello\"\t" (rtos lung 2 1) "\t" (rtos lung2 2 1) "\t0\t0\t" (rtos spes 2 0) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan (strcat n_base newcod))
	       (setq rig (strcat "\""n_base newcod "\"\t\"Pannello\"\t" (rtos lung 2 1) "\t" (rtos lung2 2 1) "\t0\t0\t" (rtos spes 2 0) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan (strcat n_base newcod))
	       )
	     (IF REGP (SETQ RIG_A (strcat nomepan "|Pannello Dritto  |Nr.|" (rtos lung 2 1) "|" (rtos lung2 2 1) "|"(rtos halt 2 0)
                           "|PanLamiera|" MODOP_ "|" finit "||" nomepan ".00|0|0||||"
                         )		   
	     )
	       (SETQ RIG_A (strcat nomepan "|Pannello Dritto  |Nr.|" (rtos lung 2 1) "|" (rtos lung2 2 1) "|"(rtos halt 2 0)
                           "|PanLamiera|" MODOP_ "|" finit "||" nomepan ".00|"
                         )		   
	     )
	       )
	   )
	   (PROGN
	     (if (= "Si" (leggi ent "BLOCCOLAM"))
             (setq rig (strcat "\"" n_base newcod "\"\t\"Angolo\"\t" (rtos lung 2 1) "\t" (rtos lung2 2 1)  "\t0\t0\t" (rtos spes 2 0) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan (strcat n_base newcod))
	       (setq rig (strcat "\"" n_base newcod "\"\t\"Angolo\"\t" (rtos lung 2 1) "\t" (rtos lung2 2 1)  "\t0\t0\t"(rtos spes 2 0) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan (strcat n_base newcod))
	       
	       )
	   (IF REGP (SETQ  rig_a (strcat nomepan "|Pannello piegato |Nr.|" (rtos lung 2 1) "|" (rtos lung2 2 1) "|"(rtos halt 2 0)
		            "|AngLamiera|" MODOP_ "|" finit "||" nomepan ".00|0|0||||" ;00
                         )		   
             )
	     (SETQ  rig_a (strcat nomepan "|Pannello piegato |Nr.|" (rtos lung 2 1) "|" (rtos lung2 2 1) "|"(rtos halt 2 0)
		            "|AngLamiera|" MODOP_ "|" finit "||" nomepan ".00|" ;00
                         )		   
             )
	     )
	   )
	 )
 	 (setq $pannellil (cons nomepan $pannellil))
         (write-line rig file)
	 (if filet (write-line rig filet))

         (s_close file (strcat modalita "pannellil.txt"))
	 (if filet (s_close filet (strcat modalita "$$pannellil.txt")))
		   (setq lispan (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE lispan))))
                   (setq lispn (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE lispn))))
		   (if (= filtrafori "Si") (progn (setq lispan_sf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE lispan_sf))))
                   (setq lispn_sf (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE lispn_sf))))
		   (setq lispan_nf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE lispan_nf))))
                   (setq lispn_nf (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE lispn_nf))))
					     ))
	 
         (if filea (write-line rig_A filea));	 (inquad rig_A)
         (setq file (S_OPEN (strcat modalita "DIBpan.txt") "a"))
	 (if (findfile (strcat modalita "$$DIBpan.txt")) (setq filet (S_OPEN (strcat modalita "$$DIBpan.txt") "a")) (setq filet nil))

 	 (foreach n LISTAPp
           (setq rig_& (strcat "\"" nomePAN "\"\t\""(NTH 0 N) "\"\t" (rtos halt 2 0) "\t\"" finit "\"\t\"" forisn
			     "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)));massimofori
           (setq rig (strcat "\"" nomePAN "\"\t\""(NTH 0 N) "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)))	   
           (write-line rig_& file)
	   (if filet (write-line  rig_& filet))

  	   	   (setq DIBpan (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpan))))
                   (setq DIBpn (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBpn))))
  	   	   (if (= filtrafori "Si") (progn (setq DIBpan_sf (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpan_sf))))
                   (setq DIBpn_sf (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBpn_sf))))
  	   	   (setq DIBpan_nf (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpan_nf))))
                   (setq DIBpn_nf (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBpn_nf))))
					     ))

	   (SETQ RIG_A (STRCAT NOMEPAN "|" (NTH 0 N) "|" (RTOS (NTH 3 N) 2 0) "||"))
	   (SETQ RIG_A6 (STRCAT NOMEPAN "|" (NTH 0 N) "|" (RTOS (NTH 3 N) 2 0) "||||"))
	   
	   (IF (/= "000000000" (NTH 0 N)) (if filed (WRITE-LINE RIG_A FILED)))
	   (IF (/= "000000000" (NTH 0 N)) (if filed (WRITE-LINE RIG_A6 FILED6)))
	 )
         (s_close file (strcat modalita "DIBpan.txt"))
	 (if filet (s_close filet (strcat modalita "$$DIBpan.txt")))
       )
    )
    (setq nomepanf nomepan)
 ;lamiera retro
(if (= lung2 0)
    (progn (setq lungr (+ lung INC_X_F_LATO2 (nth 8 svilcorr) (nth 9 svilcorr)))
           (setq lung2r (+ lung INC_X_F_LATO2 (nth 9 svilcorr)))
      )
    (progn (setq lungr (+ lung INC_X_F_LATO2 (nth 8 svilcorr)))
           (setq lung2r (+ lung2 INC_X_F_LATO2 (nth 9 svilcorr)))
      )
  )
    (if (= lung2 0) (setq lung2r 0))
    (setq spesr spes)
    (if (= lung2 0)
       (if (= "Si" (leggi ent "BLOCCOLAM"))
         (setq lrig (list "P retro" (atoF (rtos lungr 2 1)) (atoF (rtos lung2r 2 1)) (atoI (rtos PIEGA1 2 0)) (atoI (rtos PIEGA2 2 0)) (atoi (rtos spesr 2 0)) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" 1500 MODOP_ "Nr"))
	 (setq lrig (list "P retro" (atoF (rtos lungr 2 1)) (atoF (rtos lung2r 2 1)) (atoI (rtos PIEGA1 2 0)) (atoI (rtos PIEGA2 2 0)) (atoi (rtos spesr 2 0)) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" (atoi (rtos (length listap) 2 0)) MODOP_ "Nr"))
	 )
       (if (= "Si" (leggi ent "BLOCCOLAM"))
         (setq lrig (list "A retro" (atoF (rtos lungr 2 1)) (atoF (rtos lung2r 2 1)) (atoI (rtos PIEGA1 2 0)) (atoI (rtos PIEGA2 2 0)) (atoi (rtos spesr 2 0)) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" 1500 MODOP_ "Nr"))
	 (setq lrig (list "A retro" (atoF (rtos lungr 2 1)) (atoF (rtos lung2r 2 1)) (atoI (rtos PIEGA1 2 0)) (atoI (rtos PIEGA2 2 0)) (atoi (rtos spesr 2 0)) (atoi (rtos halt 2 0)) (atoi (rtos angpan 2 0)) finit forisn "_" (atoi (rtos (length listap) 2 0)) MODOP_ "Nr"))
	 )
     )
     (setq newpan nil)
     (SETQ LIS1 NIL)
     (FOREACH PANDL LISPAN_r
        (IF (equal (CDR PANDL) LRIG)
          (SETQ LIS1 (CONS PANDL LIS1))
        )
     )
     (IF (NULL LIS1) (SETQ NEWPAN "T"))
     (SETQ LIS LIS1)
    
    (if (AND LISTAP (null newPAN))
      (progn
	(SETQ LIS LIS1)
	(FOREACH PN LISTAPpr;sono solo fori
          (SETQ LISZ NIL)
	  (FOREACH N LIS  
	    (setq pk (list (car n) (nth 0 pn) (atoi (rtos halt 2 0)) finit forisn (nth 1 pn) (nth 2 pn) (nth 3 pn) (nth 4 pn) (nth 5 pn)))
	    (IF (MEMBER PK DIBPAN_r) (PROGN  (SETQ LISZ (CONS N LISZ))))
	    (SETQ LIS LISZ)
	  )
	)
      )
    )
    (IF LIS (SETQ NEWPAN NIL NOMEPAN (CAR (LAST LIS))) (SETQ NEWPAN "T"))
    (if newPAN
       (progn
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last lispan_r)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq file (S_OPEN (strcat modalita "pannellil_r.txt") "a"))
	 (if (findfile (strcat modalita "$$pannellil_r.txt")) (setq filet (S_OPEN (strcat modalita "$$pannellil_r.txt") "a")) (setq filet nil))
	 (setq n_base (substr (car (last lispan_r)) 1 3))
         
	 (if (= lung2 0)
	   (PROGN
	     (if (= "Si" (leggi ent "BLOCCOLAM"))
             (setq rig (strcat "\""n_base newcod "\"\t\"P retro\"\t" (rtos lungr 2 1) "\t" (rtos lung2r 2 1)   "\t" (rtos piega1 2 0) "\t" (rtos piega2 2 0) "\t" (rtos spes 2 0) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan (strcat n_base newcod))
	       (setq rig (strcat "\""n_base newcod "\"\t\"P retro\"\t" (rtos lungr 2 1) "\t" (rtos lung2r 2 1)   "\t" (rtos piega1 2 0) "\t" (rtos piega2 2 0) "\t" (rtos spes 2 0) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan (strcat n_base newcod))
	       )
	     (IF REGP (SETQ RIG_A (strcat nomepan "|Pannello Dritto  |Nr.|" (rtos lungr 2 1) "|" (rtos lung2r 2 1) "|"(rtos halt 2 0)
                           "|PanLamiera|" MODOP_ "|" finit "||" nomepan ".00|0|0||||"
                         )		   
	     )
	       (SETQ RIG_A (strcat nomepan "|Pannello Dritto  |Nr.|" (rtos lungr 2 1) "|" (rtos lung2r 2 1) "|"(rtos halt 2 0)
                           "|PanLamiera|" MODOP_ "|" finit "||" nomepan ".00|"
                         )		   
	     )
	       )
	   )
	   (PROGN
	     (if (= "Si" (leggi ent "BLOCCOLAM"))
             (setq rig (strcat "\"" n_base newcod "\"\t\"A retro\"\t" (rtos lungr 2 1) "\t" (rtos lung2r 2 1)  "\t" (rtos piega1 2 0) "\t" (rtos piega2 2 0)  "\t" (rtos spes 2 0) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" "0" "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan (strcat n_base newcod))
	       (setq rig (strcat "\"" n_base newcod "\"\t\"A retro\"\t" (rtos lungr 2 1) "\t" (rtos lung2r 2 1)  "\t" (rtos piega1 2 0) "\t" (rtos piega2 2 0)  "\t" (rtos spes 2 0) "\t"
	                  (rtos halt 2 0) "\t" (rtos angpan 2 0) "\t\"" finit "\"\t\"" forisn "\"\t\"_\"\t" (rtos (length listap) 2 0) "\t\"" MODOP_ "\"\t\"Nr\""
	               )
	           nomepan (strcat n_base newcod))
	       
	       )
	   (IF REGP (SETQ  rig_a (strcat nomepan "|Pannello piegato |Nr.|" (rtos lungr 2 1) "|" (rtos lung2r 2 1) "|"(rtos halt 2 0)
		            "|AngLamiera|" MODOP_ "|" finit "||" nomepan ".00|0|0||||" ;00
                         )		   
             )
	     (SETQ  rig_a (strcat nomepan "|Pannello piegato |Nr.|" (rtos lungr 2 1) "|" (rtos lung2r 2 1) "|"(rtos halt 2 0)
		            "|AngLamiera|" MODOP_ "|" finit "||" nomepan ".00|" ;00
                         )		   
             )
	     )
	   )
	 )
 	 (setq $pannellil_r (cons nomepan $pannellil_r))
         (write-line rig file)
	 (if filet (write-line rig filet))

         (s_close file (strcat modalita "pannellil_r.txt"))
	 (if filet (s_close filet (strcat modalita "$$pannellil_r.txt")))
		   (setq lispan_r (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE lispan_R))))
                   (setq lispn_R (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE lispn_R))))
		   (if (= filtrafori "Si") (progn (setq lispan_sf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE lispan_sf))))
                   (setq lispn_sf (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE lispn_sf))))
		   (setq lispan_nf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE lispan_nf))))
                   (setq lispn_nf (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE lispn_nf))))
					     ))
	 
         (if filea (write-line rig_A filea));	 (inquad rig_A)
         (setq file (S_OPEN (strcat modalita "DIBpan_r.txt") "a"))
	 (if (findfile (strcat modalita "$$DIBpan_r.txt")) (setq filet (S_OPEN (strcat modalita "$$DIBpan_R.txt") "a")) (setq filet nil))

 	 (foreach n LISTAPpr
           (setq rig_& (strcat "\"" nomePAN "\"\t\""(NTH 0 N) "\"\t" (rtos halt 2 0) "\t\"" finit "\"\t\"" forisn
			     "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)));massimofori
           (setq rig (strcat "\"" nomePAN "\"\t\""(NTH 0 N) "\"\t" (rtos (NTH 1 N) 2 1) "\t" (RTOS (NTH 2 N) 2 1)
                              "\t" (RTOS (NTH 3 N) 2 0) "\t" (RTOS (NTH 4 N) 2 1) "\t" (RTOS (NTH 5 N) 2 1)))	   
           (write-line rig_& file)
	   (if filet (write-line  rig_& filet))

  	   	   (setq DIBpan_r (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpan_R))))
                   (setq DIBpn_R (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBpn_R))))
  	   	   (if (= filtrafori "Si") (progn (setq DIBpan_sf (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpan_sf))))
                   (setq DIBpn_sf (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBpn_sf))))
  	   	   (setq DIBpan_nf (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpan_nf))))
                   (setq DIBpn_nf (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBpn_nf))))
					     ))

	   (SETQ RIG_A (STRCAT NOMEPAN "|" (NTH 0 N) "|" (RTOS (NTH 3 N) 2 0) "||"))
	   (SETQ RIG_A6 (STRCAT NOMEPAN "|" (NTH 0 N) "|" (RTOS (NTH 3 N) 2 0) "||||"))
	   
	   (IF (/= "000000000" (NTH 0 N)) (if filed (WRITE-LINE RIG_A FILED)))
	   (IF (/= "000000000" (NTH 0 N)) (if filed (WRITE-LINE RIG_A6 FILED6)))
	 )
         (s_close file (strcat modalita "DIBpan_r.txt"))
	 (if filet (s_close filet (strcat modalita "$$DIBpan_r.txt")))
       )
    )
    
    (setq nomepanr nomepan)
    (setq nomepan nomepanf)

;assieme pannelllo



(setq llung_m nil)

;    integ01
;;;;;;;;;;;;;;;;;;;;;;;;
    (SETQ LIS NIL)
    (SETQ LIS1 NIL)
;;;    (setq resisor resis)
;;;    (IF (AND (= RESIS "B15") (= FORISN "Si")) (PROGN  (SETQ RESIS "B0")))
;;;    (IF (= RESIS "B0+") (PROGN (SETQ RESIS "B15")))
    (If (/= "KIT" (cadr (assoc 'montaggiofori parametri)))
      (progn
	(SETQ ALTKIT halt)
	(setq listk nil)
	(setq pt (car lispt))
	(setq pt0x_f pt)
	(if (= 0 lung2)
	  (setq LISPTFO (list (nth 0 lispt) (nth 1 lispt)))
	  (progn
	    (setq LISPTFO (list (nth 0 lispt) (nth 1 lispt) (nth 3 lispt)))
	    (setq pt0x_f (nth 1 lispt))
	  )
	)
	(setq ldpos0_f lung)
	(setq hpan halt)
	(IF (= fORISN "Si") (DIBFORO) (setq listk nil))
	
	(foreach k listk (setq listaps (cons (list (nth 0 k)
						   (nth 2 k)
						   (nth 1 k)
						   (nth 3 k)
						   (nth 4 k)
						   (nth 5 k)
					     )
						   listaps)))
	
      )
    )
;lanacer
;;;    (IF (AND (or (= RESisor "B15") (= RESisor "B15+")) (= FORISN "Si") (/= "KIT" (cadr (assoc 'montaggiolanace parametri))))
;;;	(PROGN
;;;          (if (= "pannelli-Angoli" (cdr (assoc '8 (entget ent))))
;;;	    (setq llun (list (list (distance (nth 0 lispt) (nth 1 lispt)) 0)
;;;			     (list (distance (nth 1 lispt) (nth 2 lispt)) (distance (nth 0 lispt) (nth 1 lispt)))))
;;;	    (setq llun (list (list (distance (nth 0 lispt) (nth 1 lispt)) 0)))
;;;	  )
;;;	  (if (or (= resisor "B15")(= resisor "B15+"))
;;;	      (foreach lunl llun
;;;		(setq lun (car lunl))
;;;                (setq listaps (cons (list selvelovetro
;;;					  (atoi (rtos (* (atof (rtos lun 2 1)) (atof (rtos halt 2 1))) 2 0))
;;;					  (atoi (rtos (cadr lunl) 2 0)) 0 0 0) listaps))
;;;	        (setq rig_m (list "Materassino Ceramico" sellanacer
;;;				  (atof (rtos lun 2 1)) (atoi (rtos halt 2 0)) 0 (atoi (rtos (* (atof (rtos lun 2 1))
;;;												(atof (rtos halt 2 1))
;;;												) 2 0)) "Nr" ))
;;;		(if (setq percod_m (member rig_m lanace))
;;;		  (setq NOMEPAN_m (car (nth (- (LENGTH lanace) (length percod_m)) lanacer)))
;;;		  (progn
;;;		    (setq file (S_OPEN (strcat modalita "lanacer.txt") "a"))
;;;		    (setq newcod (rtos (+ 1 (atoi (substr (car (last lanacer)) 4))) 2 0))
;;;		    (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
;;;		    (setq n_base (substr (car (last lanacer)) 1 3))
;;;		    (setq rig-m (strcat "\"" n_base newcod "\"\t\"Materassino Ceramico\"\t\"" (NTH 1 RIG_m)"\"\t" (rtos (NTH 2 RIG_m) 2 1)
;;;					"\t" (rtos (NTH 3 RIG_m)2 0) "\t" (RTOS (NTH 4 RIG_m) 2 0) "\t" (rtos (NTH 5 RIG_m) 2 0)
;;;					"\t\"" (NTH 6 RIG_m) "\"")
;;;			  )
;;;		    (SETQ nomepan_m (strcat n_base newcod))
;;;		    (IF REGP (SETQ RIG_A-m (strcat nomepan_m "|Materassino Ceramico|Nr.|" (rtos (nth 2 rig_m) 2 1) "|" (rtos (nth 3 rig_m) 2 1) "|"(rtos (nth 4 rig_m) 2 0)"|"
;;;					  "lanaceramica" "||" "|" "|" "|0|0||||"
;;;					  )
;;;			  )
;;;		      (SETQ RIG_A-m (strcat nomepan_m "|Materassino Ceramico|Nr.|" (rtos (nth 2 rig_m) 2 1) "|" (rtos (nth 3 rig_m) 2 1) "|"(rtos (nth 4 rig_m) 2 0)"|"
;;;					  "lanaceramica" "||" "|" "|" "|"
;;;					  )
;;;			  )
;;;		    )
;;;		    (write-line rig-m file)
;;;		    (s_close file (strcat modalita "lanacer.txt"))
;;;		   (setq lanacer (REVERSE (CONS (READ (STRCAT "(" rig-m ")")) (REVERSE lanacer))))
;;;                   (setq lanace (REVERSE (CONS (CDR (READ (STRCAT "(" rig-m ")"))) (REVERSE lanace))))
;;;		    
;;;		    (if filea (write-line rig_A-m filea)) (inquad rig_A-m)
;;;		    (setq file (S_OPEN (strcat modalita "DIBlcr.txt") "a"))
;;;		    (setq rig (strcat "\"" nomePAN_m "\"\t\"" (NTH 1 rig_m) "\"\t" (RTOS (NTH 2 rig_m) 2 0)
;;;				      "\t" (RTOS (NTH 3 rig_m) 2 0) "\t" (RTOS (NTH 4 rig_m) 2 0) "\t" (RTOS (NTH 5 rig_m) 2 0)))
;;;		    (write-line rig file)
;;;		    (SETQ RIG_A (STRCAT NOMEPAN_m "|" (NTH 1 rig_m) "|" (RTOS (/ (atof (rtos (NTH 5 rig_m))) 1000000) 2 2) "||"))
;;;		    (SETQ RIG_A6 (STRCAT NOMEPAN_m "|" (NTH 1 rig_m) "|" (RTOS (/ (atof (rtos (NTH 5 rig_m))) 1000000) 2 2) "||||"))
;;;		    
;;;		    (if filed (WRITE-LINE RIG_A FILED))
;;;		    (if filed (WRITE-LINE RIG_A6 FILED6))
;;;		    (s_close file (strcat modalita "DIBlcr.txt"))
;;;		  )
;;;		)
;;;  	        (setq listaps (cons (list nomepan_m 1 (atoi (rtos (cadr lunl) 2 0)) 0 0 0) listaps))
;;;	      )
;;;	  )
;;;	)
;;;      )

;mater
    (if (= resis "B0")
        (setq deskldr "Foglio lana di roccia Sp. " spmnan 15)
        (setq deskldr "Foglio lana di roccia Sp. " spmnan 25)
    )
;;;;;;    (if (/= resis  "B0") (setq resisk "B15") (setq resisk "B0"))
;;;    (setq len_m (member (list resis (atoi (rtos halt 2 0))) lanadi))
;;;    (if (null len_m) (setq len_m  (member (list resis 0) lanadi)))
;;;    (if (null len_m) (progn (alert "Riga mancante in lanadir.txt!!") (exit)))
;;;    (setq cod_m (car (nth (- (LENGTH LANADI) (length len_m)) lanadir)))
     (setq lung1 lung)
   (IF (/= (RTOS LUNG2 2 0) "0")
      (if (< 180 angpan)
	  (if (< lung lung2)
	    (SETQ LLUNG_M (LIST (- LUNG (/ (cadr (assoc 'diflungldr parametri)) 2))
                                (- LUNG2 spmnan (/ (cadr (assoc 'diflungldr parametri))2))"a")
            )
	    (SETQ LLUNG_M (LIST (- LUNG2 (/ (cadr (assoc 'diflungldr parametri)) 2))
                                (- LUNG spmnan (/ (cadr (assoc 'diflungldr parametri))2))"a")
            )
	  )
	  (if (< lung lung2)
	    (SETQ LLUNG_M (LIST (+ spmnan (- LUNG (/ (cadr (assoc 'diflungldr parametri)) 2)))
                                (- LUNG2 (/ (cadr (assoc 'diflungldr parametri))2))"a")
            )
	    (SETQ LLUNG_M (LIST (- LUNG (/ (cadr (assoc 'diflungldr parametri))2))
				(+ spmnan (- LUNG2 (/ (cadr (assoc 'diflungldr parametri)) 2))) "a"
                          )
            )
	  )
       )
       (SETQ LLUNG_M (LIST (- LUNG (cadr (assoc 'diflungldr parametri)))))
    )
(if (/= PIEGA1 0) (setq spl 14 cod_m (nth 2 (assoc "lanaSiEle" lanadir)) deskldr (nth 1 (assoc "lanaSiEle" lanadir))) (setq spl 24 cod_m (nth 2 (assoc "lanaNoEle" lanadir)) deskldr (nth 1 (assoc "lanaNoEle" lanadir))))
    
    (COND ((= ANGPAN 0) (SETQ LLUNG_M (LIST (LIST (- LUNG (cadr (assoc 'diflungldr parametri))) 0  0 (ATOI (RTOS HPAN 2 0)) 2 0 "p"))))
	   ;[ang]=90;IIf([lung1]>[lung2];[lung1]-2+15;[lung1]-2);
	   ;[ang]=90;IIf([lung1]>[lung2];[lung2]-2;[lung2]-2+15);
	  
	  ((= ANGPAN 90) (IF (> LUNG1 LUNG2) (SETQ LLUNG_M (LIST (LIST (+ spl (- LUNG (/ (cadr (assoc 'diflungldr parametri))2))) 0 0 (ATOI (RTOS HPAN 2 0)) 2 0 "1")
								 (LIST (+ 0  (- LUNG2 (/ (cadr (assoc 'diflungldr parametri))2)))  0  0 (ATOI (RTOS HPAN 2 0)) (atoi (rtos lung 2 0)) 0 "2")
								 ) )
			                     (SETQ LLUNG_M (LIST (LIST (+ 0 (- LUNG (/ (cadr (assoc 'diflungldr parametri))2))) 0 0(ATOI (RTOS HPAN 2 0)) 2 0 "1")
								 (LIST (+ spl  (- LUNG2 (/ (cadr (assoc 'diflungldr parametri))2)))  0  0 (ATOI (RTOS HPAN 2 0)) (atoi (rtos lung 2 0)) 0 "3")
								 ))))
	  ;[ang]=270;IIf([lung1]>[lung2];[lung1]-2-15;[lung1]-2);
	  ;[ang]=270;IIf([lung1]>[lung2];[lung2]-2;[lung2]-2-15);
	  ((= ANGPAN 270) (IF (> LUNG1 LUNG2) (SETQ LLUNG_M (LIST (LIST (- LUNG (/ (cadr (assoc 'diflungldr parametri))2) spl) 0 0 (ATOI (RTOS HPAN 2 0))2 0 "1")
								  (LIST (- LUNG2 (/ (cadr (assoc 'diflungldr parametri))2) 0)  0  0 (ATOI (RTOS HPAN 2 0)) (atoi (rtos lung 2 0)) 0 "4")
								 ))
			                     (SETQ LLUNG_M (LIST (LIST (- LUNG (/ (cadr (assoc 'diflungldr parametri))2) 0) 0 0 (ATOI (RTOS HPAN 2 0))2 0 "1")
								 (LIST (- LUNG2 (/ (cadr (assoc 'diflungldr parametri))2) spl)  0  0 (ATOI (RTOS HPAN 2 0))(atoi (rtos lung 2 0)) 0 "4")
								 ))))
	  (T                           (SETQ LLUNG_M (LIST (LIST (- LUNG (/ (cadr (assoc 'diflungldr parametri))2) 0) 0 (ATOI (RTOS (- (/ ANGPAN 2) 90 0.01) 2 0)) (ATOI (RTOS HPAN 2 0)) 2 0 "a")
						           (LIST (- LUNG2 (/ (cadr (assoc 'diflungldr parametri))2) 0)  (ATOI (RTOS (+ (- 0 (/ ANGPAN 2))90 0.01) 2 0))  0 (ATOI (RTOS HPAN 2 0))(atoi (rtos lung 2 0)) 0 "b")
								 ))
			                     ))

         (setq tagliver nil taglihor nil)
	 (foreach rf listaps (if (and (= "RF-" (substr (car rf) 1 3))
				      (or (= "R69" (nth 2 (assoc (car rf) tabrinfor)))
					  (= "R74" (nth 2 (assoc (car rf) tabrinfor)))
					  (= "R1" (nth 2 (assoc (car rf) tabrinfor)))
					  )
				      )
			       (if (= "VER" (nth 5 (assoc (car rf) tabrinfor)))
				 (setq tagliver (cons (list (nth 2 rf) (nth 4 (assoc (car rf) tabrinfor))) tagliver))
				 (setq taglihor (cons (list (nth 3 rf) (nth 4 (assoc (car rf) tabrinfor))) taglihor))
				 )
			       )
	   )
;   (setq taglihor '( (1770 80) (1270 80)))
    (setq taglihor (vl-sort taglihor
             (function (lambda (e1 e2)
                         (> (car e1) (car e2))))))
;;;    (setq taglihor mil)
   ;(setq taglihor (vl-sort taglihor '>)) 
    (foreach htag__ taglihor
      (setq htag (car htag__))
      (setq dtag (cadr htag__))
;      (setq LLUNG_M_to LLUNG_M)
      (setq llung_m_to nil)
      (foreach lung_m llung_m
	(if (< htag (nth 5 lung_m))
	  (setq LLUNG_M_to (cons lung_m LLUNG_M_to))
	  (progn
	    (setq LLUNG_M_to (cons (list (nth 0 lung_m) (nth 1 lung_m) (nth 2 lung_m) (- htag (/ dtag 2)) (nth 4 lung_m) 0 (strcat (nth 6 lung_m) "")) LLUNG_M_to))
	    (setq LLUNG_M_to (cons (list (nth 0 lung_m) (nth 1 lung_m) (nth 2 lung_m) (- (nth 3 lung_m) htag (/ dtag 2)) (nth 4 lung_m) (+ (/ dtag 2) htag) (strcat (nth 6 lung_m) "")) LLUNG_M_to))
	    )
	  )
	)
      (setq llung_m LLUNG_M_to)
      )
    










    
;;;    (setq tagliver '( (318 80) ));nota1
;;;    (setq tagliver '( (235 80)));nota1
;;;    (setq tagliver nil)
;;;    (setq tagliver (vl-sort tagliver '>)) 
    (setq tagliver (vl-sort tagliver
             (function (lambda (e1 e2)
                         (< (car e1) (car e2))))))

    (foreach dtag__ tagliver
      (setq dtag (car dtag__))
      (setq ltag (cadr dtag__))
       (setq llung_m_to nil)

       (foreach lung_m llung_m
;;;	 (if (/= lung2 6)
;;;         (if (> dtag (+ (nth 0 lung_m) (nth 4 lung_m)))
	   (if (> dtag lung)
	 (if (not (and (> dtag (nth 4 lung_m)) (< dtag (+ (nth 0 lung_m) (nth 4 lung_m)))))
  
        	(setq LLUNG_M_to (cons lung_m LLUNG_M_to))
	   (progn
;	    (setq LLUNG_M_to (cons (list (- (nth 0 lung_m)   (- (+ lung lung2) dtag 2 )(/ ltag 2)) (nth 1 lung_m) 0 (nth 3 lung_m) (nth 4 lung_m) (nth 5 lung_m)) LLUNG_M_to))
	    (setq LLUNG_M_to (cons (list (- (nth 0 lung_m) (- (+ lung lung2) dtag (/ ltag 2) 2) ltag)  (nth 1 lung_m) 0 (nth 3 lung_m) (nth 4 lung_m) (nth 5 lung_m)(strcat (nth 6 lung_m) "v1")) LLUNG_M_to) )
;	    (setq LLUNG_M_to (cons (list (- (+ lung lung2)  dtag 2 (/ ltag 2)) 0 (nth 2 lung_m) (nth 3 lung_m) (+ dtag (/ ltag 2)) (nth 5 lung_m)) LLUNG_M_to))
	    (setq LLUNG_M_to (cons (list (- (+ lung lung2) dtag (/ ltag 2) 2) 0 (nth 2 lung_m) (nth 3 lung_m) (+ dtag (/ ltag 2)) (nth 5 lung_m) (strcat (nth 6 lung_m) "v2")) LLUNG_M_to))
	    ))
	 (if (not (and (> dtag (nth 4 lung_m)) (< dtag (+ (nth 0 lung_m) (nth 4 lung_m)))))
  
        	(setq LLUNG_M_to (cons lung_m LLUNG_M_to))
	   (progn
;	    (setq LLUNG_M_to (cons (list (- (nth 0 lung_m)   (- (+ lung lung2) dtag 2 )(/ ltag 2)) (nth 1 lung_m) 0 (nth 3 lung_m) (nth 4 lung_m) (nth 5 lung_m)) LLUNG_M_to))
	    (setq LLUNG_M_to (cons (list (- (nth 0 lung_m) ltag (- (+ (nth 4 lung_m) (nth 0 lung_M) ) (/ ltag 2) dtag)) (nth 1 lung_m) 0 (nth 3 lung_m) (nth 4 lung_m) (nth 5 lung_m)(strcat (nth 6 lung_m) "v1")) LLUNG_M_to))
;	    (setq LLUNG_M_to (cons (list (- (+ lung lung2)  dtag 2 (/ ltag 2)) 0 (nth 2 lung_m) (nth 3 lung_m) (+ dtag (/ ltag 2)) (nth 5 lung_m)) LLUNG_M_to))
	    (setq LLUNG_M_to (cons (list (- (+ (nth 4 lung_m) (nth 0 lung_M) ) (/ ltag 2) dtag) 0 (nth 2 lung_m) (nth 3 lung_m) (+ dtag (/ ltag 2)) (nth 5 lung_m) (strcat (nth 6 lung_m) "v2")) LLUNG_M_to))
	    ))
	     )

	   
;;;	   (if (< dtag (nth 4 lung_m))
;;;        	(setq LLUNG_M_to (cons lung_m LLUNG_M_to))
;;;	   (progn
;;;	    (setq LLUNG_M_to (cons (list (- dtag 2 (/ ltag 2)) (nth 1 lung_m) 0 (nth 3 lung_m) (nth 4 lung_m) (nth 5 lung_m)) LLUNG_M_to))
;;;	    (setq LLUNG_M_to (cons (list (- (+ 2 (nth 0 lung_m)) dtag (/ ltag 2)) 0 (nth 2 lung_m) (nth 3 lung_m) (+ dtag (/ ltag 2)) (nth 5 lung_m)) LLUNG_M_to))
;;;	    ))
;;;	   )
	 )
         (setq llung_m LLUNG_M_to)

      )
        (setq LLUNG_M_x_t LLUNG_M)

    (setq ll_m nil)
    (setq lungminlana (cadr (assoc 'lungminlana parametri)))
    (setq lunglana14 (cadr (assoc 'lunglana14 parametri)))
    (setq lunglana24 (cadr (assoc 'lunglana24 parametri)))

    (foreach ll LLUNG_M
      (cond ((or (= "p" (nth 6 ll)) (= "2" (nth 6 ll))(= "3" (nth 6 ll))(= "4" (nth 6 ll)) (= "1" (nth 6 ll))(= "a" (nth 6 ll)) (= "b" (nth 6 ll)))
             (if (= 0 piega1) (setq lunglana lunglana24 tlana 24) (setq lunglana lunglana14 tlana 14))

	     (if (and (or (= (nth 6 ll) "p")(= (nth 6 ll) "1") (= (nth 6 ll) "a")) (/= (car (read pieghe)) 0))
	       (progn (setq ll_m (cons (list (car (read pieghe)) (nth 1 ll) (nth 2 ll) (nth 3 ll) (nth 4 ll) (nth 5 ll) 24 (nth 6 ll)) ll_m)) (setq poss (+ (car (read pieghe)) (nth 4 ll))) (setq resto (- (nth 0 ll) (car (read pieghe)))))
	       (progn (setq poss (nth 4 ll)) (setq resto (nth 0 ll)))
	       )
	     (if (and (or (= (nth 6 ll) "p")(= (nth 6 ll) "2")(= (nth 6 ll) "b")(= (nth 6 ll) "3")(= (nth 6 ll) "4")) (/= (cadr (read pieghe)) 0))
	       (progn (setq ll_m (cons (list (cadr (read pieghe)) (nth 1 ll) (nth 2 ll) (nth 3 ll) (setq posd (- (+ (nth 0 ll) (nth 4 ll)) (cadr (read pieghe))))  (nth 5 ll) 24 (nth 6 ll)) ll_m))
		 (setq resto (- resto (cadr (read pieghe)))))
	       (progn (setq posd (+ (nth 0 ll) (nth 4 ll))) )
	       )
	     (if (or (= (nth 6 ll) "1")(= (nth 6 ll) "a")) (setq pos "D") (setq pos "S"))
	     
	     (while (> resto  0)
		 (if (> resto (+ lunglana lungminlana))
		  (progn
		   (if (= pos "S")
		     (progn (setq ll_m (cons (list lunglana (nth 1 ll) (nth 2 ll) (nth 3 ll) poss (nth 5 ll) tlana (nth 6 ll)) ll_m)) (setq resto (- resto lunglana)) (setq pos "D") (setq poss (+ lunglana poss)))
		     (progn (setq ll_m (cons (list lunglana (nth 1 ll) (nth 2 ll) (nth 3 ll) (setq posd (- posd lunglana)) (nth 5 ll) tlana (nth 6 ll)) ll_m)) (setq resto (- resto lunglana)) (setq pos "S"))
		     )
		   )
		   (if (< resto lunglana)
		      (if (= pos "S")
			(progn (setq ll_m (cons (list resto (nth 1 ll) (nth 2 ll) (nth 3 ll)  poss (nth 5 ll) tlana (nth 6 ll)) ll_m)) (setq pos "D") (setq poss (+ resto poss)) (setq resto 0))
			(progn (setq ll_m (cons (list resto (nth 1 ll) (nth 2 ll) (nth 3 ll) (setq posd (- posd resto)) (nth 5 ll) tlana (nth 6 ll)) ll_m)) (setq resto 0) (setq pos "S"))
			)
			(progn (setq ll_m (cons (list (/ resto 2) (nth 1 ll) (nth 2 ll) (nth 3 ll) poss (nth 5 ll) tlana (nth 6 ll)) ll_m)) (setq poss (- poss (/ resto 2)))
			       (setq ll_m (cons (list (/ resto 2) (nth 1 ll) (nth 2 ll) (nth 3 ll) (setq posd (- posd (/ resto 2))) (nth 5 ll) tlana (nth 6 ll)) ll_m)) (setq resto 0))
		     )
		   )
		 
	       )
	     )
	     
	    )
      )
    (setq pamin 10000)
    (setq pamax 0)
(foreach n ll_m
  (if (and (/= (nth 1 n) 0) (< (nth 4 n) pamin)) (setq pamin (nth 4 n)))
  (if (and (/= (nth 2 n) 0) (> (nth 4 n) pamax)) (setq pamax (nth 4 n)))
  )
    (setq ll_mx ll_m)
    (setq ll_m nil)
  (foreach n ll_mx
    (cond ((and (/= (nth 1 n) 0) (= (nth 4 n) pamin))
	      (setq ll_m (cons n ll_m)))
	      ((and (/= (nth 2 n) 0) (= (nth 4 n) pamax))
	      (setq ll_m (cons n ll_m)))
	      (t (setq ll_m (cons (list (nth 0 n) 0 0 (nth 3 n) (nth 4 n) (nth 5 n) (nth 6 n) (nth 7 n)) ll_m)))
	      )
    )
    	(if (= 0 lung2)
	  (setq p1 (nth 0 lispt) p2 (nth 1 lispt) p3 nil)
          (setq p1 (nth 0 lispt) p2 (nth 1 lispt) p3 (nth 2 lispt))
	)

    (setq pqs (polar p1 (+ (angle p1 p2) (/ pi 2)) 8))
    (if p3
      (progn
        (setq pqd (polar p3 (- (angle p3 p2) (/ pi 2)) 8))
        (setq grq (ssget "_f" (list pqs (setq prova1 (polar p2 (+ (angle p1 p2) (/ pi 2)) 8)) (setq prova2 (polar p2 (- (angle p3 p2) (/ pi 2)) 8)) (setq prova (polar p3 (- (angle p3 p2) (/ pi 2)) 8))) (list '(0 . "DIMENSION") '(8 . "Quotelana"))))
	)
        (setq grq (ssget "_f" (list pqs (polar p2 (+ (angle p1 p2) (/ pi 2)) 8)) (list '(0 . "DIMENSION") '(8 . "Quotelana"))))
      )
    (if grq (command "_erase" grq ""))
    (command "_layer" "_n" "Quotelana" "")
    (command "_dimstyle" "_r" "iso-25")
    (foreach q ll_m
      (if (or (= "1" (nth 7 q))(= "a" (nth 7 q))(= "p" (nth 7 q)))
	(progn
	  (command"_dimaligned" "_non" (polar pqs (angle p1 p2) (nth 4 q)) "_non" (polar pqs (angle p1 p2) (+ (nth 0 q) (nth 4 q))) "_t"
		  (strcat "<>-" (rtos (nth 6 q) 2 0)"-" (nth 7 q) "*" (rtos (nth 1 q) 2 0)"*" (rtos (nth 2 q) 2 0)) "_non" (polar pqs (angle p1 p2) (+ (nth 0 q) (nth 4 q))) )
	  (command"_chprop" (entlast) "" "_la" "Quotelana" "")
	  )
	(progn
	  (cond ((= "3" (nth 7 q))
        	   (command"_dimaligned" "_non" (setq pq (polar pqd (angle p2 p3) (- (nth 4 q) (nth 6 q) lung1 lung2))) "_non"
			   (polar pq (angle p2 p3) (+ (nth 0 q) 0)) "_t"
			   (strcat "<>-" (rtos (nth 6 q) 2 0)"-" (nth 7 q) "*" (rtos (nth 1 q) 2 0)"*" (rtos (nth 2 q) 2 0)) "_non" (polar pqd (angle p3 p2) (+ (nth 0 q) (nth 4 q))) ))
               ((= "4" (nth 7 q))
        	   (command"_dimaligned" "_non" (setq pq (polar pqd (angle p2 p3) (+ (- (nth 4 q)  lung1 lung2) (nth 6 q)))) "_non"  (polar pq (angle p2 p3) (+ (nth 0 q) 0)) "_t"
			   (strcat "<>-" (rtos (nth 6 q) 2 0)"-" (nth 7 q)"*" (rtos (nth 1 q) 2 0)"*" (rtos (nth 2 q) 2 0)) "_non" (polar pqd (angle p3 p2) (+ (nth 0 q) (nth 4 q))) ))
		(t (command"_dimaligned" "_non" (setq pq (polar pqd (angle p2 p3) (- (nth 4 q) lung1 lung2))) "_non"                 (polar pq (angle p2 p3) (+ (nth 0 q) 0)) "_t"
			   (strcat "<>-" (rtos (nth 6 q) 2 0)"-" (nth 7 q) "*" (rtos (nth 1 q) 2 0)"*" (rtos (nth 2 q) 2 0)) "_non" (polar pqd (angle p3 p2) (+ (nth 0 q) (nth 4 q))) ))
	    )
	  (command"_chprop" (entlast) "" "_la" "Quotelana" "")
	  )
	)
      )
    (setq LLUNG_M ll_m)
    
    
	 (SETQ LLUNG_M+H LLUNG_M)  
    (setq LLUNG_M+H_ nil)
    (foreach lm LLUNG_M+H
      (if (> (car lm) 1) (setq LLUNG_M+H_ (cons lm LLUNG_M+H_)))
      )
    (setq LLUNG_M+H LLUNG_M+H_)
    (FOREACH LUNG_M+H+A LLUNG_M+H
      (SETQ LUNG_M (CAR LUNG_M+H+A))
      
      (setq rig_m (list (strcat deskldr (rtos (nth 6 LUNG_M+H+A) 2 0)) cod_m (atoI (rtos lung_M 2 1)) (atoi (rtos (- (NTH 3 LUNG_M+H+A)  (cadr (assoc 'diffaltldr parametri))) 2 0))
			(nth 6 LUNG_M+H+A) (NTH 1 LUNG_M+H+A) (NTH 2 LUNG_M+H+A)
			(atoi (rtos (* (atoI (rtos lung_M 2 1))
				         (atoi (rtos (- (NTH 3 LUNG_M+H+A)  (cadr (assoc 'diffaltldr parametri))) 2 0))) 2 0)) "Nr" ))
      (if (setq percod_m (member rig_m mate))
	(setq NOMEPAN_m (car (nth (- (LENGTH MATE) (length percod_m)) mater)))
	(progn
	  (setq file (S_OPEN (strcat modalita "mater.txt") "a"))
	  (setq newcod (rtos (+ 1 (atoi (substr (car (last mater)) 4))) 2 0))
	  (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	  (setq n_base (substr (car (last mater)) 1 3))
	  (setq rig-m (strcat "\""n_base newcod "\"\t\"" deskldr (rtos (nth 6 LUNG_M+H+A) 2 0)  "\"\t\"" (NTH 1 RIG_m)"\"\t" (rtos (NTH 2 RIG_m) 2 0)
			      "\t" (rtos (NTH 3 RIG_m)2 0) "\t" (RTOS (NTH 4 RIG_m) 2 0) "\t" (RTOS (NTH 5 RIG_m) 2 0)
			      "\t" (RTOS (NTH 6 RIG_m) 2 0)
			      "\t" (rtos (NTH 7 RIG_m) 2 0)
			      "\t\"" (NTH 8 RIG_m) "\"")
		)
	  (SETQ nomepan_m (strcat n_base newcod))
;;;	  (IF REGP (SETQ RIG_A-m (strcat nomepan_m "|" deskldr "|Nr.|" (rtos (nth 2 rig_m) 2 1) "|" (rtos (nth 3 rig_m) 2 1) "|"(rtos (nth 4 rig_m) 2 0)
;;;				"|materassino|" "|" resis "|" "|" "|0|0||||"
;;;				)
;;;		)
;;;	    (SETQ RIG_A-m (strcat nomepan_m "|" deskldr "|Nr.|" (rtos (nth 2 rig_m) 2 1) "|" (rtos (nth 3 rig_m) 2 1) "|"(rtos (nth 4 rig_m) 2 0)
;;;				"|materassino|" "|" resis "|" "|" "|"
;;;				)
;;;		)
;;;	  )
	  (write-line rig-m file)
	  (s_close file (strcat modalita "mater.txt"))
		   (setq mater (REVERSE (CONS (READ (STRCAT "(" rig-m ")")) (REVERSE mater))))
                   (setq mate (REVERSE (CONS (CDR (READ (STRCAT "(" rig-m ")"))) (REVERSE mate))))
	  (if filea (write-line rig_A-m filea)) ;(inquad rig_A-m)
	  (setq file (S_OPEN (strcat modalita "DIBldr.txt") "a"))
	  (setq rig (strcat "\"" nomePAN_m "\"\t\"" (NTH 1 rig_m) "\"\t" (RTOS (NTH 2 rig_m) 2 0)
			    "\t" (RTOS (NTH 3 rig_m) 2 0) "\t" (RTOS (NTH 4 rig_m) 2 0) "\t" (RTOS (NTH 7 rig_m) 2 0)))
	  (write-line rig file)
	  (SETQ RIG_A (STRCAT NOMEPAN_m "|" (NTH 1 rig_m) "|" (RTOS (/ (atof (rtos (NTH 5 rig_m))) 1000000) 2 2) "||"))
	  (SETQ RIG_A6 (STRCAT NOMEPAN_m "|" (NTH 1 rig_m) "|" (RTOS (/ (atof (rtos (NTH 5 rig_m))) 1000000) 2 2) "||||"))
	
	  (if filed (WRITE-LINE RIG_A FILED))
	  (if filed (WRITE-LINE RIG_A6 FILED6))
	  (s_close file (strcat modalita "DIBldr.txt"))
	  )
	)
      (setq listaps (cons (list nomepan_m 1 (NTH 4 LUNG_M+H+A) (NTH 5 LUNG_M+H+A) 0 0) listaps))
   )
    (if (or (= modop "STD") (= modop "SPS") (= modop "MTD") (= modop "MPS"))
      (progn
;;;        (setq Profl (cadr (assoc 'ProfFissLana parametri)))
;;;        (setq listaps (cons (list Profl 1 0 (- halt 20.0) 0 0) listaps))
;;;        (setq listaps (cons (list Profl 1 0 20.0 0 0) listaps))
	)
      )
    (cond 
          
	  
	  
          ((and (= resis "B15") (= "Si" forisn) (= "No" scatoleSN))  (setq desctec "7"))
	  ((and (= resis "B15") (= "Si" scatoleSN))  (setq desctec "8"))
          ((and (= resis "B15")  (= "No" scatoleSN)) (setq desctec "5"))
          (t (setq desctec "7"))
    )
;;;    (if (and (= resisor "B0+") (= "No" forisn)) (setq resis "B15"))
;;;    (if (and (= resisor "B0+") (= "Si" forisn)) (setq resis "B0"))
;;;    (if (= resis "B15+") (setq resis "B15"))
    (if (or (= modop "SPD") (= modop "STD"))
      (progn
	(if (>= halt (cadr (assoc 'altgiuntoPanL parametri))) (setq hgiunto (cadr (assoc 'altgiuntopan1 parametri)));profilo
	  (setq hgiunto (cadr (assoc 'altgiuntopan2 parametri))))
;;;	(setq listaps (cons (list giuntopan hgiunto 0 0 0 0) listaps))
	)
      )
    (SETQ CART_SX "")
    (if (and (= "" cart_ang) (= "" cart_for) (= "" cart_rinf)
	     (or (= "SPC" MODOP) (= "SPD" MODOP) (= "MPC" MODOP) (= "MPD" MODOP) (= "MAN" MODOP)
	     ))
      (SETQ CART_SX "X")
      )
    (if (and (= "" cart_ang) (= "" cart_for) (= "" cart_rinf)
	     (or (= "STD" MODOP) (= "SPS" MODOP) (= "MPS" MODOP) (= "MPS" MODOP)
	     ))
      (SETQ CART_SX "S")
      )
    (if (and (= "" cart_ang) (= "" cart_for) (= "" cart_rinf)
	     (or (= "MPM" MODOP) (= "SPM" MODOP)
	     ))
      (SETQ CART_SX "Y")
      )
     (setq listaps (cons (list nomepan 1 0 0 0 0) listaps))
     (setq listaps (cons (list nomepanr 1 -1 0 0 0) listaps))
     (if (= "Si" (leggi ent "BLOCCOPAN"))
    (setq lrigs (list nomepan (strcat "Assieme pannello " modop " " CART_SX cart_for cart_ang cart_rinf) resis "Nr" 1500 desctec finit (atoi (rtos halt))
		      forisn modop nomepanr))
	(setq lrigs (list nomepan (strcat "Assieme pannello " modop " " CART_SX cart_for cart_ang cart_rinf) resis "Nr" (atoi (rtos (length listaps) 2 0)) desctec finit (atoi (rtos halt))
		      forisn modop nomepanr))
	)
    (setq newpas nil)
    (FOREACH PANDL LISPAs
        (IF (equal (CDR PANDL) LRIGs)
          (SETQ LIS1 (CONS PANDL LIS1
			   ))
        )
     )
     (IF (NULL LIS1) (SETQ NEWPAs "T"))
     (SETQ LIS LIS1)
    (if (AND LISTAPs (null newPAs))
      (progn
        ;(alert "pp")
	(SETQ LIS LIS1)
	(FOREACH Ps LISTAPs
          (SETQ LISZ NIL)
	  (FOREACH N LIS
;;;	    (SETQ PK (CONS (CAR N) Ps))
	    (setq ps1 (list (car ps)))
	    (foreach v (cdr ps)
	      (if (= 0 (- v (atoi (rtos v 2 1)))) (setq ps1 (cons (atoi (rtos v 2 1)) ps1)) (setq ps1 (cons (atof (rtos v 2 1)) ps1)))
	      )
	    (setq ps (reverse ps1))

	    (setq pk (list (car n) (nth 0 ps) (atoi (rtos halt 2 0)) finit forisn (nth 1 ps) (nth 2 ps) (nth 3 ps) (nth 4 ps) (nth 5 ps)))
;;;	    (if (null (MEMBER PK DIBPAs)) (alert "pp"))
	    (IF (MEMBER PK DIBPAs) (PROGN  (SETQ LISZ (CONS N LISZ))))
	    (SETQ LIS LISZ)
	  )
	)
      )
    )
	  
    (IF LIS (SETQ NEWPAs NIL NOMEPAns (CAR (LAST LIS))) (SETQ NEWPAs "T"))
    (if newPAs

      (progn
        (setq newcodx (rtos (+ 1 (atoi (substr (car (last lispas)) 4))) 2 0))
	(repeat (- 5 (strlen newcodx)) (setq newcodx (strcat "0" newcodx)))
	(setq n_base (substr (car (last lispas)) 1 3))
	(if (= "Si" (leggi ent "BLOCCOPAN"))
          (setq rig (strcat "\"" n_base newcodx "\"\t\"" nomepan "\"\t\"Assieme pannello " modop " " CART_SX cart_for cart_ang cart_rinf "\"\t\"" resis "\"\t\"Nr\"\t" "0" "\t\"" desctec "\"\t\"" finit "\"\t" (rtos halt 2 0) "\t\"" forisn "\"\t\"" modop "\"\t\"" nomepanr "\"" ))
	  (setq rig (strcat "\"" n_base newcodx "\"\t\"" nomepan "\"\t\"Assieme pannello " modop " " CART_SX cart_for cart_ang cart_rinf "\"\t\"" resis "\"\t\"Nr\"\t"(rtos (length listaps) 2 0)"\t\"" desctec "\"\t\"" finit "\"\t" (rtos halt 2 0) "\t\"" forisn "\"\t\"" modop "\"\t\"" nomepanr "\"" ))
	  )
	(setq $pannelli (cons (strcat n_base newcodx) $pannelli))
        (setq nomepans (strcat n_base newcodx))
     	(setq file (S_OPEN (strcat modalita "pannelli.txt") "a"))
	(if (findfile (strcat modalita "$$pannelli.txt")) (setq filet (S_OPEN (strcat modalita "$$pannelli.txt") "a")) (setq filet nil))

        (write-line rig file)
	(if filet (write-line rig filet))
        (s_close file (strcat modalita "pannelli.txt"))
	(if filet (s_close filet (strcat modalita "$$pannelli.txt")))
		   (setq lispas (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE lispas))))
                   (setq lisps (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE lisps))))
		   (if (= filtrafori "Si") (progn (setq lispas_sf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE lispas_sf))))
                   (setq lisps_sf (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE lisps_nf))))
		   (setq lispas_nf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE lispas_nf))))
                   (setq lisps_nf (REVERSE (CONS (CDR (READ (STRCAT "(" rig ")"))) (REVERSE lisps_nf))))
					     ))

	(pesopan (strcat n_base newcodx))
	
	(setq file (S_OPEN (strcat modalita "DIBpaS.txt") "a"))
	
;;;	(if (findfile (strcat modalita "$$DIBpaS.txt")) (setq filet (S_OPEN (strcat modalita "$$DIBpaS.txt") "a")) (setq filet nil))
;;;        (write-line  (setq rig_& (strcat "\"" n_base newcodx "\"\t\"" nomepan "\"\t" (rtos halt 2 0) "\t\"" finit "\"\t\"" forisn "\"\t1")) file)
;;;  	   	   (setq DIBpas (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpas))))
;;;                   (setq DIBps (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBps))))
;;;  	   	   (if (= filtrafori "Si") (progn (setq DIBpas_sf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE DIBpas_sf))))
;;;                   (setq DIBps_sf (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBps_sf))))
;;;  	   	   (setq DIBpas_nf (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpas_nf))))
;;;                   (setq DIBps_nf (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBps_nf))))
;;;					     ))
;;;	
;;;	(if filet (write-line  (strcat "\"" n_base newcodx "\"\t\"" nomepan "\"\t" (rtos halt 2 0) "\t\"" finit "\"\t\"" forisn "\"\t1") filet))
;;;        (IF REGP (setq rig_a (strcat n_base newcodx "|Assieme pannello|Nr.|0|0|0|Pannello|" resis "|" desctec "||" NBASEPAS newcodx".00|" (rtos mqpan 2 2)"|" (rtos pesop 2 2)"||||" )) ;calcolopeso
;;;	  (setq rig_a (strcat n_base newcodx "|Assieme pannello|Nr.|0|0|0|Pannello|" resis "|" desctec "||" NBASEPAS newcodx".00|"))
;;;	  ) ;delta
;;;	(if filea (write-line rig_a filea))
;;;	(inquad rig_A)
;;;        (if filed (write-line  (strcat n_base newcodx "|" nomepan "|1||") filed))
;;;	(if filed (write-line  (strcat n_base newcodx "|" nomepan "|1||||") filed6))
;;;	
	(foreach db listaps
           (setq rig_& (strcat "\"" n_base newcodx "\"\t\"" (NTH 0 db) "\"\t" (rtos halt 2 0) "\t\"" finit "\"\t\"" forisn
			     "\"\t" (rtos (NTH 1 db) 2 1) "\t" (RTOS (NTH 2 db) 2 1)
                              "\t" (RTOS (NTH 3 db) 2 1) "\t" (RTOS (NTH 4 db) 2 1) "\t" (RTOS (NTH 5 db) 2 1)))
           (setq rig (strcat "\"" n_base newcodx "\"\t\"" (NTH 0 db) "\"\t" (rtos (NTH 1 db) 2 1) "\t" (RTOS (NTH 2 db) 2 1)
                              "\t" (RTOS (NTH 3 db) 2 1) "\t" (RTOS (NTH 4 db) 2 1) "\t" (RTOS (NTH 5 db) 2 1)))

	  (if (member (nth 0 db) (cdr (assoc 'tubiinkit parametri)))
(progn
	       (setq rig_a (strcat n_base newcodx "|" (NTH 0 db) "|" (rtos (/ (NTH 1 db) 1000.0) 2 2) "||"))
	       (setq rig_a6 (strcat n_base newcodx "|" (NTH 0 db) "|" (rtos (/ (NTH 1 db) 1000.0) 2 2) "||||"))
	       )
             (if (= (nth 0 db) selvelovetro)
	       (progn
		 (setq rig_a (strcat n_base newcodx "|" (NTH 0 db) "|" (rtos (/ (NTH 1 db) 1000000.0) 2 2) "||"))
		 (If (= "RP" (substr (NTH 0 db) 4 2))
		   (setq rig_a6 (strcat n_base newcodx "|" (NTH 0 db) "|" (rtos (/ (NTH 1 db) 1000000.0) 2 2) "||" (rtos (NTH 2 db) 2 2) "|" (rtos (NTH 3 db) 2 2) "|"))
		   (setq rig_a6 (strcat n_base newcodx "|" (NTH 0 db) "|" (rtos (/ (NTH 1 db) 1000000.0) 2 2) "||||"))
		   )
		 )

	       (progn (setq rig_a (strcat n_base newcodx "|" (NTH 0 db) "|" (rtos (NTH 1 db) 2 1) "||"))
		 (If (= "RP" (substr (NTH 0 db) 4 2))
		   (setq rig_a6 (strcat n_base newcodx "|" (NTH 0 db) "|" (rtos (NTH 1 db) 2 1) "||"(rtos (NTH 2 db) 2 2) "|" (rtos (NTH 3 db) 2 2) "|"))
		   (setq rig_a6 (strcat n_base newcodx "|" (NTH 0 db) "|" (rtos (NTH 1 db) 2 1) "||||"))
		   )
		 )
	       
	     )
	   )
	   (if filed (write-line rig_a filed))
	   (if filed (write-line rig_a6 filed6))
           (write-line rig_& file)
	  (if filet (write-line rig_& filet))
  	   	   (setq DIBpas (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpas))))
                   (setq DIBps (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBps))))
  	   	   (if (= filtrafori "Si") (progn (setq DIBpas_sf (REVERSE (CONS (READ (STRCAT "(" rig ")")) (REVERSE DIBpas_sf))))
                   (setq DIBps_sf (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBps_sf))))
  	   	   (setq DIBpas_nf (REVERSE (CONS (READ (STRCAT "(" rig_& ")")) (REVERSE DIBpas_nf))))
                   (setq DIBps_nf (REVERSE (CONS (CDR (READ (STRCAT "(" rig_& ")"))) (REVERSE DIBps_nf))))
					     ))

	  
        )
        (s_close file (strcat modalita "DIBpaS.txt"))
	(if filet (s_close filet (strcat modalita "$$DIBpaS.txt")))
      )	
    )

    
    (setq ptn (polar (nth 0 lispt) (setq ang (angle (nth 0 lispt) (nth 1 lispt))) (/ lung 2)))
    (setq ptn (polar ptn (+ ang (/ pi 2)) 230))

    (if lstcodpas
      (foreach n lstcodpas
	(if (= codrp (nth 0 n))
	  (PROGN
	    (setq ptn1 (nth 1 n))
	    (setq ptn ptn1)
	    (setq ang (nth 2 n))
	    )
	  )
	)
      )
    
    (setq ptna (polar ptn (+ ang (/ pi 2)) 120))
    (setq ptna_r (polar ptn (+ ang (/ pi 2)) 60))
    (setq ptns (polar ptn (+ ang (/ pi 2)) -60))
    (setq ptns_r (polar ptn (+ ang (/ pi 2)) -120))
    
    (command "_layer" "_t" "siglepan*" "")
    (command "_layer" "_m" "siglepan" "")
;;;    (command"_insert" "codpas" "_non" ptn  "1" "1" (angtos ang) nomepans codrp)
;;;    (command"_insert" "codpan" "_non" ptna "1" "1" (angtos ang) nomepan codrp)
;;;    (command"_insert" "codsvi" "_non" ptns   "1" "1" (angtos ang) nomesvi codrp)
;;;    (command"_insert" "codpan_r" "_non" ptna_r "1" "1" (angtos ang) nomepanr codrp)
;;;    (command"_insert" "codsvi_R" "_non" ptns_r   "1" "1" (angtos ang) nomesvir codrp)
        (command"_insert" "codpas" "_non" ptn  "1" "1" (angtos ang) nomepans codrp)
  (setq gent1 (entlast))

    (command"_insert" "codpan" "_non" ptna "1" "1" (angtos ang) nomepan codrp)
  (setq gent2 (entlast))
    (command"_insert" "codsvi" "_non" ptns   "1" "1" (angtos ang) nomesvi codrp)
  (setq gent3 (entlast))
    (command"_insert" "codpan_r" "_non" ptna_r "1" "1" (angtos ang) nomepanr codrp)
  (setq gent4 (entlast))
    (command"_insert" "codsvi_R" "_non" ptns_r   "1" "1" (angtos ang) nomesvir codrp)
  (setq gent5 (entlast))
            (command"_group" "_c" "*" "codici" gent1 gent2 gent3 gent4 gent5 "")

    (SETQ NR (+ 1 NR))
  )
  (command"_layer" "_m" "0" "")
;;;  (close file)
  (stop)
)

(DEFUN C:ANNTAB ()
;;;  (alert "pp")
  (FOREACH N '(profili profil CABPAS cabpan CABPAS distcab lispan lispn DIBpan DIBpn lispas lisps DIBpaS PORTE PRESE DISTPAN TABRIN LISKIT LISKT kitpan kitpn POSPON POSPN travinf
travin travsup travsu lanacer lanace DISTFORI LISRIN fogli fogl sviluppi svilupp DIBsvi DIBsv cesoiati cesoiat lanadir lanadi
mater mate rinfpiede rinfpied tabrinfor tabrinfo POSPON svilpan datikit distpan distpaS pannelli
	       sviluppi_r svilupp_R DIBsvi_r DIBsv_R cesoiati_r cesoiat_r lispan_R lispn_R DIBpan_r DIBpn_R)
    (SET N NIL)
  )
)
(defun s_close (file nome)
  (close file)
  (setq fperc (strcat (vl-filename-directory nome) "\\"))
  (setq fext (vl-filename-extension nome))
  (setq fbase (vl-filename-base nome))
  (while (null (vl-file-rename (strcat fperc fbase ".tx_") (strcat fperc fbase fext)))
    (p_alert (strcat "Attesa chiusura " nome))
  )
)
(defun pesopan (rpan)
  (IF (NULL PESI) (setq pesi (carica "pesi" "")))
  (if (null lispas) (setq lispas (carica "pannelli" modalita)))
  (if (null lispan) (setq lispan (carica "pannellil" modalita)))
  (setq finit (nth 9 (assoc (nth 1 (assoc rpan lispas)) lispan)))
  (setq lung1  (nth 2 (assoc (nth 1 (assoc rpan lispas)) lispan)))
  (setq lung2 (nth 3 (assoc (nth 1 (assoc rpan lispas)) lispan)))
  (setq halt (nth 7 (assoc (nth 1 (assoc rpan lispas)) lispan)))
  (setq resis (nth 3 (assoc rpan lispas)))
;;;  (if (or (= resis "B15") (= resis "B15+")) (setq resisk "B15"))
;;;  (if (or (= resis "B0") (= resis "B0+")) (setq resisk "B0"))

  (FOREACH N PESI
    (IF (AND (= FINIT (NTH 0 N))
	     (= RESIS (NTH 1 N))
	)
      (SETQ pesouk (NTH 2 N))
    )
  )
  (setq mqpan (* (/ (+ lung1 lung2) 1000.0) (/ halt 1000.0)))
  
  (setq pesop (* mqpan pesouk))
)
(defun c:newfin ()
  (setq gr (ssget (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (setq fpan (getstring (strcat "\Finitura pannello [" (cadr (assoc 'Finiture parametri)) "]<"(cadr (assoc 'Finitura parametri)) ">:")))
  (if (= "" fpan) (setq fpan (cadr (assoc 'Finitura parametri))))

  (setq nr -1)
  (while (setq ent (ssname gr (setq nr (+ 1 nr))))
    (allega ent "PANFIN" fpan)
  )
)
(defun c:regcab_p ()
(setq giuntopan (selezpar 'giuntopan "Giunto pannello: "))
;;;      (setq alf (ssget "x" (list '(0 . "INSERT") '(2 . "$POS"))))
;;;  (SETQ LALF NIL)
;;;  (IF ALF
;;;    (PROGN
;;;      (SETQ NR -1)
;;;      (WHILE (SETQ ENT (SSNAME ALF (SETQ NR (+ 1 NR))))
;;;	(SETQ LALF (CONS (LIST (CDR (ASSOC '1 (ENTGET (ENTNEXT (ENTNEXT ENT)))))
;;;			       (CDR (ASSOC '1 (ENTGET (ENTNEXT ENT))))
;;;			       (CDR (ASSOC '1 (ENTGET (ENTNEXT (ENTNEXT (ENTNEXT ENT))))))
;;;			       (CDR (ASSOC '10 (ENTGET ENT)))
;;;			       )
;;;			 
;;;			 LALF))
;;;	)
;;;      )
;;;    )
  (setq cabina (getvar "dwgname"))
  (setq cabina (substr cabina 1 (- (strlen cabina) 4)))
  (SETQ CABINA (SUBSTR CABINA 1 10))
  (setq revisione (substr (getvar "dwgname") 16 1))
  (setq lista nil)
;;;  (setq gr (ssget "x" (list '(0 . "INSERT") '(2 . "codkit"))))
  (setq grpn (ssget "x" (list '(0 . "INSERT") '(2 . "codpas"))))
  (setq lista nil)
  (SETQ LISTA_P NIL)
;;;  (setq nr 0)
;;;  (while (setq ent (ssname gr nr))
;;;     (setq lista (cons
;;;		   (cdr (assoc '1 (entget (entnext ent))))
;;;		   lista
;;;		 )
;;;     )
;;;     (IF (AND LALF (ASSOC (cdr (assoc '1 (entget (ENTNEXT (entnext ent))))) LALF))
;;;       (SETQ POS (NTH 1 (ASSOC (cdr (assoc '1 (entget (ENTNEXT (entnext ent))))) LALF)))
;;;       (SETQ POS "0")
;;;     )
;;;     (setq lista_P (cons
;;;		   (LIST (cdr (assoc '1 (entget (entnext ent)))) POS)
;;;		   lista_P 
;;;		 )
;;;     )
;;;    
;;;    (setq nr (+ 1 nr))
;;;  )

  
;;;  (setq gr (ssget "x" (list '(0 . "INSERT") '(2 . "CONGKIT?"))))
;;;  (setq nr 0)
;;;  (while (AND GR (setq ent (ssname gr nr)))
;;;     (if (AND (= "ATTRIB" (cdr (assoc '0 (entget (entnext ent)))))
;;;	      (/= "" (cdr (assoc '1 (entget (entnext ent)))))
;;;	 )
;;;       
;;;       (progn
;;;         (setq lista (cons (setq codpez (cdr (assoc '1 (entget (entnext ent))))) lista))
;;;	 (IF (AND LALF (ASSOC (cdr (assoc '5 (entget ent))) LALF))
;;;            (SETQ POS (NTH 1 (ASSOC (cdr (assoc '5 (entget ent))) LALF)))
;;;            (SETQ POS "0")
;;;         )
;;;         (setq lista_P (cons (LIST (setq codpez (cdr (assoc '1 (entget (entnext ent))))) pos) lista_P))
;;;	 
;;;       )
;;;     )
;;;    (setq nr (+ 1 nr))
;;;  )



  
;copia bak
  (vl-file-delete (strcat modalita "cabine_.bak"))
  (vl-file-copy  (strcat modalita "cabine.txt")  (strcat modalita "cabine_.bak"))
  (setq filer (S_OPEN (strcat modalita "cabine_.bak") "r"))
  (setq filew (S_OPEN (strcat modalita "cabine.txt") "w"))
  (while (setq rig (read-line filer))
;    (print (car (read (strcat "(" rig ")"))))
    (if (/= cabina (car (read (strcat "(" rig ")"))))
      (progn
         (write-line rig filew)
;	 (print rig)
	(car (read (strcat "(" rig ")")))
      )
    )
  )
  (s_close filer (strcat modalita "cabine_.bak"))
  (s_close filew (strcat modalita "cabine.txt"))



  
  (vl-file-delete (strcat modalita "cabpan_.bak"))
  (vl-file-copy  (strcat modalita "cabpan.txt")  (strcat modalita "cabpan_.bak"))
  (setq filer (S_OPEN (strcat modalita "cabpan_.bak") "r"))
  (setq filew (S_OPEN (strcat modalita "cabpan.txt") "w"))
  (while (setq rig (read-line filer))
;    (print (car (read (strcat "(" rig ")"))))
    (if (/= cabina (car (read (strcat "(" rig ")"))))
      (progn
         (write-line rig filew)
;	 (print rig)
	(car (read (strcat "(" rig ")")))
      )
    )
  )
  (s_close filer (strcat modalita "cabpan_.bak"))
  (s_close filew (strcat modalita "cabpan.txt"))

  (setq liscabk lista)
  (SETQ liscabk_P LISTA_P)
;;;  (setq file (S_OPEN (strcat modalita "cabine.txt") "a"))
;;;  (write-line (strcat "\"" cabina "\"\t\"NR\"\t\"Assieme pareti\"") file)
;;;  (s_close file (strcat modalita "cabine.txt"))
  (setq nr -1)
  (setq lista_pn nil)
  (while (setq ent (ssname grpn (setq nr (+ 1 nr))))
    (setq lista_pn (cons (list (cdr (assoc '1 (entget (entnext ent)))) 1) lista_pn))
    )
  (setq grpr (ssget "x" (list '(0 . "INSERT") '(2 . "profilo1"))))
  (setq nr -1)
  (setq lista_pr nil)
  (while (and grpr (setq ent (ssname grpr (setq nr (+ 1 nr)))))
    (setq lista_pr (cons (list (cdr (assoc '2 (entget ent))) 1) lista_pr))
    )
  (if (null finit) (setq finit "Non Definita"))
  (if (null ALTKIT) (setq ALTKIT 0))
   (setq porte (carica "porte" ""))
 (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))

  (SETQ nrPnfor 0)
  (setq nrpntot 1)
  (Setq ltrave 0)
  (foreach n lista_pn
     (if (= "Si" (nth 9 (assoc (car n) lispas))) (setq nrPnfor (+ 1 nrPnfor)))
    (setq nrpntot (+ 1 nrpntot))
     (setq ltrave (+ ltrave (nth 2 (assoc (nth 1 (Assoc (car n) lispas)) lispan)) (nth 3 (assoc (nth 1 (Assoc (car n) lispas)) lispan))))
     (setq rig_a (strcat cabina "|" (CAR n) "|" (rtos (cadr n) 2 0) "|" "0" "|"))
;;;     (setq rig_a6 (strcat cabina "|" (car (CAR n)) "|" (rtos (cdr n) 2 0)  "|" (CADR (CAR N)) "|||"))
    
    (IF (NULL NOREGORA )(if filed (write-line rig_A filed))) ;ok)
    (setq rig (strcat "\"" cabina "\"\t\"" (car n) "\"\t" (rtos (cadr n) 2 0) "\t\"" "0" "\""))
    (write-line rig file)
  )
  
  (s_close file (strcat modalita "cabpan.txt"))
  (setq nr- -1)
  (setq gr-p (ssget "x" (list '(0 . "INSERT") '(2 . "Porta*"))))
  (while (and gr-p (setq ent- (ssname gr-p (setq nr- (+ nr- 1)) )))
    (setq nome- (cdr (assoc '2  (entget ent-))))
    (if (assoc nome- porte) (setq lporta (nth 3 (assoc nome- porte))))
    (if lporta (setq ltrave (+ ltrave lporta)))
    )
 ; (setq pprer (cadr (assoc 'profiloper parametri)))
  ;rif selezpar
  (setq pprerlg  (cadr (assoc 'profiloperqnt parametri))) ;profilo
  (setq profrdogaAgg  (cadr (assoc 'profrdogaAgg parametri))) ;profilo
  (setq profiloperAgg  (cadr (assoc 'profiloperAgg parametri))) ;profilo
  (setq qntprof (+ (fix (* 1 (/ ltrave pprerlg))) 0))
  

(if (/= qntprof 0)
    (progn
      (setq profx (arcprof pprer "grezzo" pprerlg 0))
      (setq rig (strcat "\"" cabina "\"\t\"" profx "\"\t" (rtos (+ profiloperAgg (/ qntprof 1)) 2 0) "\t\"" "0" "\""))
      (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))
      (write-line rig file)
      (s_close file (strcat modalita "cabpan.txt"))

      (setq profx (arcprof pprer1 "grezzo" pprerlg 0))
      (setq rig (strcat "\"" cabina "\"\t\"" profx "\"\t" (rtos (+ profiloperAgg (/ qntprof 1)) 2 0) "\t\"" "0" "\""))
      (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))
      (write-line rig file)
      (s_close file (strcat modalita "cabpan.txt"))
      (if (assoc 'Fin_profrdoga parametri) (setq Fin_nn (cadr (assoc 'Fin_profrdoga parametri))) (setq fin_nn "NN"))
      (setq profx (arcprof pregd fin_NN pprerlg 0))
      (setq rig (strcat "\"" cabina "\"\t\"" profx "\"\t" (rtos (+ profrdogaAgg (/ qntprof 1)) 2 0) "\t\"" "0" "\""))
      (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))
      (write-line rig file)
      (s_close file (strcat modalita "cabpan.txt"))
      )
    )
  
    ;rif selezpar
;giunto pannello
  ;(setq giuntopan (selezpar 'giuntopan "Giunto pannello: "))
  (if (/= nrPntot 0)
    (progn
 (setq proftx (arcprof giuntopan "grezzo" (- hpan 10) 0))
      (setq rig (strcat "\"" cabina "\"\t\"" proftx "\"\t" (rtos nrPntot 2 0) "\t\"" "0" "\""))
      (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))
      (write-line rig file)
      (s_close file (strcat modalita "cabpan.txt"))

      )
    )
  ;fine giunto pannelli
  (if (/= nrPnfor 0)
    (progn
      (setq rig_a (strcat cabina "|" progiu "|" (rtos (* 2 nrPnfor) 2 0) "|" "1" "|"))
      (IF (NULL NOREGORA )(if filed (write-line rig_A filed)))
      (setq rig (strcat "\"" cabina "\"\t\"" progiu "\"\t" (rtos (* 2 nrPnfor) 2 0) "\t\"" "0" "\""))
            (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))

      (write-line rig file)
            (s_close file (strcat modalita "cabpan.txt"))

      )
    )
;;;  (setq grrif (ssget "x" (list '(0 . "INSERT") '(2 . "riferimento"))))
;;;  (setq nr -1)
;;;  (while (and grrif (setq rif (ssname grrif (setq nr (+ 1 nr)))))
;;;      (setq rig_a (strcat cabina "|" (cdr (assoc 1 (entget (entnext rif)))) "|" (cdr (assoc 1 (entget (entnext (entnext rif)))))  "|" "1" "|"))
;;;      (IF (NULL NOREGORA )(if filed (write-line rig_A filed)))
;;;      (setq rig (strcat "\"" cabina "\"\t\"" (cdr (assoc 1 (entget (entnext rif)))) "\"\t" (cdr (assoc 1 (entget (entnext (entnext rif))))) "\t\"" "0" "\""))
;;;      (write-line rig file)
;;;)

    (setq grrif (ssget "x" (list '(0 . "INSERT") '(2 . "riferimento"))))
  (setq nr -1)
            (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))

  (while (and grrif (setq rif (ssname grrif (setq nr (+ 1 nr)))))
      (if (and (entnext (entnext (entnext rif))) (cdr (assoc '1 (entget (entnext (entnext (entnext rif))))))
	       (/= (cdr (assoc '1 (entget (entnext (entnext (entnext rif)))))) ""))
	(progn
	  (setq __tip (cdr (assoc '1 (entget (entnext (entnext (entnext rif)))))))
	  (setq __fam (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext rif))))))))
	  (setq __dim (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext (entnext rif)))))))))
	  (setq __alt (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext (entnext (entnext rif))))))))))
	  (setq __cer (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext rif)))))))))))
	  (setq __fin (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext rif))))))))))))
	  (setq __cla (cdr (assoc '1 (entget (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext (entnext rif)))))))))))))
            (setq rig (strcat "\"" cabina "\"\t\"" (cdr (assoc 1 (entget (entnext rif)))) "\"\t" (cdr (assoc 1 (entget (entnext (entnext rif))))) "\t\"" "0" "\"\t\""
			      __tip "\"\t\"" __fam "\"\t\"" __dim "\"\t\"" __alt "\"\t\"" __cer "\"\t\"" __fin "\"\t\"" __cla "\""))
	  )
            (setq rig (strcat "\"" cabina "\"\t\"" (cdr (assoc 1 (entget (entnext rif)))) "\"\t" (cdr (assoc 1 (entget (entnext (entnext rif))))) "\t\"" "0" "\""))
	)
;;;         (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))
     (write-line rig file)
)

    (s_close file (strcat modalita "cabpan.txt"))

    (setq grrif (ssget "x" (list '(0 . "INSERT") '(2 . "profilo"))))
  (setq nr -1)
  (while (and grrif (setq rif (ssname grrif (setq nr (+ 1 nr)))))
      (setq profx (arcprof (cdr (assoc 1 (entget (entnext (entnext rif)))))
			   (cdr (assoc 1 (entget (entnext (entnext (entnext rif))))))
			   (atof (cdr (assoc 1 (entget (entnext (entnext (entnext (entnext rif))))))))
			   (atoi (cdr (assoc 1 (entget (entnext  (entnext (entnext (entnext (entnext rif)))))))))
			   ))
      (setq entx (entnext rif))
      (setq entm (subst (cons '1 profx) (assoc '1 (entget entx)) (entget entx)))
      (entmod entm)
      (entupd entx)
      (setq rig_a (strcat cabina "|" profx "|"   "|" "1" "|"))
      (IF (NULL NOREGORA )(if filed (write-line rig_A filed)))
      (setq rig (strcat "\"" cabina "\"\t\"" profx "\"\t" (cdr (assoc 1 (entget (entnext (entnext  (entnext (entnext (entnext (entnext rif))))))))) "\t\"" "0" "\""))
      (setq file (S_OPEN (strcat modalita "cabpan.txt") "a"))
    
    
      (write-line rig file)
      (s_close file (strcat modalita "cabpan.txt"))
)
;;;  (cadr (assoc 'prof_g_pannelli parametri)) ;profilo
;;;  (setq rig (strcat "\"" cabina "\"\t\"" (cadr (assoc 'prof_g_pannelli parametri)) "\"\t" (rtos (- nrtotpannelli 1) 2 0) "\t\"" "0" "\""))
;;;      (write-line rig file)
;;;  (s_close file (strcat modalita "cabpan.txt"))
;;;
  (pesocab cabina)
  (IF REGP (setq rig_ax (strcat cabina "|Cabina |Nr.|||" (rtos altkit 2 0) "|Cab||"finit "||" cabina revisione "|"(rtos mqcab 2 2) "|" (rtos pesoc 2 2) "||||")) ;calcolopeso
   (setq rig_ax (strcat cabina "|Cabina |Nr.|||" (rtos altkit 2 0) "|Cab||"finit "||" cabina revisione "|"))
  )
  (IF (NULL NOREGORA )
    (progn
       (if filea (write-line rig_ax filea)) ;(inquad rig_Ax)
       (setq file (S_OPEN (strcat modalita "cabine.txt") "a"))
       (if (null revisione) (setq revisione ""))
       (write-line (strcat "\"" cabina "\"\t\"NR\"\t\"Assieme pareti\"\t\"" finit "\"\t" (Rtos altkit 2 0) "\t" (rtos mqcab 2 1) "\t" (rtos pesoc 2 2) "\t\"" revisione "\"") file)
       (s_close file (strcat modalita "cabine.txt")))
			))

(defun pesocab (cab)
  (IF (NULL PESI) (SETQ PESI (CARICA "PESI" "")))
  (IF (NULL lispas) (setq lispas (carica "pannelli" modalita)))
  (IF (NULL cabpan) (setq cabpan (carica "cabpan" modalita)))
    (setq nbasepas (substr (car (nth 1 lispas)) 1 3))

;;;  (setq nr 1)
;;;  (while (and (setq kit (nth nr cabkit))
;;;	      (/= cab (nth 0 kit))
;;;	 )
;;;         (setq nr (+ 1 nr))
;;;  )
;;;  (while (and (setq kit (nth nr cabkit))
;;;	      (= cab (nth 0 kit))
;;;              (/= nbasekit (substr (nth 1 kit) 1 3))
;;;	 )
;;;         (setq nr (+ 1 nr))
;;;  )
;;;  (setq kit (nth 1 (nth nr cabkit)))
  (setq lpas nil)
  (setq pesoc 0)
  (setq mqcab 0)
  (foreach pan (cdr cabpan)
    (if (and (= (strcase cab) (strcase (nth 0 pan)))
	     (= nbasepas (substr (nth 1 pan) 1 3))
	)
      (progn
        (setq lpas (cons (nth 1 pan) lpas))
	(setq pesoc (+ (pesopan (nth 1 pan)) pesoc))
	(setq mqcab (+ mqpan mqcab))
      )
    )
  )
  pesoc
)

(defun arcprof (nomex finiturax lunghezzax larghezzax)

     ;(setq rig_f (list (strcat "Profilo " nomex " "finiturax " l:" (rtos lunghezzax 2 0)) nomex finiturax lunghezzax))
  (setq rig_f (list (strcat "Profilo " nomex) nomex finiturax lunghezzax larghezzax))
     (setq profili (carica "profili" modalita) profil $tb$)
     (if (setq percod_F (member rig_f profil))

       (progn
         (setq profilo (car (nth (- (LENGTH profil) (length percod_f)) profili)))
       )  
       (progn
	 (setq xfilexy (OPEN (strcat modalita "profili.txt") "a"))
	 (setq newcod (rtos (+ 1 (atoi (substr (car (last profili)) 4))) 2 0))
	 (repeat (- 5 (strlen newcod)) (setq newcod (strcat "0" newcod)))
	 (setq n_base (substr (car (last profili)) 1 3))
         (setq rig-F (strcat "\"" n_base newcod "\"\t\"" (nth 0 rig_f) "\"\t\"" (NTH 1 RIG_F)"\"\t\"" (NTH 2 RIG_f)
			     "\"\t" (rtos (NTH 3 RIG_f)2 1) "\t" (rtos (NTH 4 RIG_f)2 1) ))
         (SETQ profilo (strcat n_base newcod))
         (write-line rig-f xfilexy)
         (close xfilexy)
         (setq profili (REVERSE (CONS (READ (STRCAT "(" rig-f ")")) (REVERSE profili))))
         (setq profil (REVERSE (CONS (CDR (READ (STRCAT "(" rig-f ")"))) (REVERSE profil))))
	 )
	 
       )
     profilo
     )