f;Rev 2020
(setq adl 1)
(load"dispas")
(load"dissvi")
(load"registra")
(load"fori")
(load"rinforzi")
(load"distbase")
(load"aggpan")
(load"3dsimb")
;(setq modalita (cadr (assoc 'percregtmp parametri)))
(defun c:ripregtmp ()
 (command "_sh" (strcat "c:\\windows\\system32\\xcopy.exe " (cadr (assoc 'percregdef parametri)) "*.* " (cadr (assoc 'percregtmp parametri)) "*.* /v /c /EXCLUDE:" (cadr (assoc 'perc_programma parametri)) "esclusioni.par"))
  ;(command "_sh" (strcat "del " (cadr (assoc 'percregdef parametri)) "$$*.*"))
  (command "_sh" (strcat "del " (cadr (assoc 'percregtmp parametri)) "$$*.*"))
 (C:ANNTAB)
)
(defun c:tracciasp ()
  (start)
  (command"_layer" "_new" "traccia" "_on" "traccia" "_set" "traccia" "")
  (setq hpani (getdist (strcat "\Altezza pannello inferiore <cabina>:")))
  (setq hpans (getdist (strcat "\Altezza pannello superiore <cabina>:")))
  (setvar "cmdecho" 1)
  (command"_elev" "" 0)
  (command"_pline")
  (while (> (getvar "cmdactive") 0)
    (command pause)
  )
  (setvar "cmdecho" 0)
  (if hpani (allega (entlast) "HPAN" (rtos hpani 2 0)))
  (if hpans (allega (entlast) "HPANS" (rtos hpans 2 0)))
  (stop)
)
(defun p_Alert (msg)
  (setq nrdl_ (load_dialog "blatt"))
  (new_dialog "P_Alert" nrdl_)
  (set_tile "text" msg)
  (setq adl (start_dialog))
  (if (= adl 0) (exit))
)

(defun carica+ (nfile perc);AGGIORANMENTO CON SELEZIONE SIGLA A O B
    (setq r1 nil)
    (setq r2 nil)
    (setq r3 nil)
  (setq tt nfile)
  (setq nff (findfile (strcat perc nfile ".txt")))
  (setq carnr 0)
  (while (null nff)
     (setq carnr (+ 1 carnr))
     (if (> carnr 10000) (p_alert (strcat "Attesa lettura prolungata " nfile " !!!!")))
     (setq nff (findfile (strcat perc nfile ".txt")))
  )
  (print (strcat "attesa lettura " nfile " : " (rtos carnr 2 0)))
  (setq perc (strcat (vl-filename-directory nff) "\\"))
  
  (while (null (setq file (open (findfile (strcat perc nfile ".txt")) "r")))
    (alert "Tabella aperta:")
  )
  (setq filet nil)
  (while (setq r (read-line file))
    (setq rx (read (strcat "(" r ")")))
    (setq rxt r)
    (setq rr (read (strcat "(" r ")")))
    (setq rold nil)
    (setq rxt r)
    (if (and rx)
      (progn
	(setq tb (cons rx  tb))
	(setq $tb$ (cons (cdr rx) $tb$))
	)
      )
    )
  (close file)
    (if (and &filtro (/= "$$" (substr nfile 1 2))) (close filet))
  (setq $tb$ (reverse $tb$))
  (reverse tb)
)

(defun carica (nfile perc);AGGIORANMENTO CON SELEZIONE SIGLA A O B
    (setq tb_nf nil)
    (setq tb_sf nil)
    (setq $tb_nf$ nil)
    (setq $tb_sf$ nil)
    (setq r1 nil)
    (setq r2 nil)
    (setq r3 nil)
  (setq tt nfile)
  (setq nff (findfile (strcat perc nfile ".txt")))
  (setq carnr 0)
  (while (null nff)
     (setq carnr (+ 1 carnr))
     (if (> carnr 10000) (p_alert (strcat "Attesa lettura prolungata " nfile " !!!!")))
     (setq nff (findfile (strcat perc nfile ".txt")))
  )
  (print (strcat "attesa lettura " nfile " : " (rtos carnr 2 0)))
  (setq perc (strcat (vl-filename-directory nff) "\\"))
  
  (while (null (setq file (open (findfile (strcat perc nfile ".txt")) "r")))
    (alert "Tabella aperta:")
  )
  (setq filet nil)
  (setq $tb$ nil)
  (setq tb nil)
  (setq $tb$- nil)
  (setq tb- nil)
  (setq $tb$_ nil)
  (setq tb_ nil)
  (while (setq r (read-line file))
    (setq rx (read (strcat "(" r ")")))
    (setq rxt r)
    (setq rr (read (strcat "(" r ")")))
    (setq rold nil)
    (setq rxt r)
    (if (and rx)
      (progn
	(setq tb (cons rx  tb))
	(setq $tb$ (cons (cdr rx) $tb$))
	)
      )
    )
  (close file)
    (if (and &filtro (/= "$$" (substr nfile 1 2))) (close filet))
  (setq $tb$ (reverse $tb$))
  (reverse tb)
)

(setq parametri (carica "parametri" ""))
(defun  sp ()
  (command"_layer" "_n" "traccia" "_on" "traccia" "_set" "traccia" "")
  (setvar "cmdecho" 1)
  (command"_elev" "" 0)
  (command"_pline")
  (while (> (getvar "cmdactive") 0)
    (command pause)
  )
  (setvar "cmdecho" 0)
)
(defun c:par_par ()
  (command"_sh" (findfile (strcat "par_par" ".accdb")))
)

(DEFUN C:PORTA ()
  (start)
;;;  (SETQ TIPO (GETSTRING "\nPorta tipo: "))
  (commanD"_layer" "_on" "traccia" "")
  (setq percorso_porte (cadr (assoc 'perc_porte	parametri)))
  (setq porta (getfiled "Selezionare porte" percorso_porte "dwg" 2)) 
  (setq ptpor (getpoint "\nPunto di inserimento: "))
  (setq angolop (getangle "\nAngolo: " ptpor))
  (setq tipo (substr porta (+ 1 (strlen percorso_porte)) (- (strlen porta) (strlen percorso_porte) 4)))
  (command"_insert" porta "_non" ptpor "1" "1" (ANGTOS angolop))
  (IF (NULL PORTE) (setq porte (carica "PORTE" "")))
  (COMMAND"_INSERT" "DIVIDISX" "_NON" (POLAR PTPOR ANGOLOp (NTH 3 (ASSOC TIPO porte))) "1" "1" (ANGTOS (+ (/ PI 2) angolop)))
  (COMMAND"_INSERT" "DIVIDIDX" "_NON" (POLAR PTPOR (+ PI ANGOLOp) (NTH 3 (ASSOC TIPO porte))) "1" "1" (ANGTOS (+ (/ PI 2) angolop)))
  (if (and (NTH 4 (ASSOC TIPO porte)) (NTH 5 (ASSOC TIPO porte)))
    (command"_insert" "riferimento" "_non" ptpor "1" "1" (ANGTOS (- angolop pi))  (NTH 4 (ASSOC TIPO porte)) (rtos (NTH 5 (ASSOC TIPO porte)) 2 1) ))
  (stop)
)
(defun my_err (err_msg)
    (if (/= err_msg "Function cancelled")
      (prompt (strcat "\nError: " err_msg))
    )

  
    (stop)
    (command "._U")
    (setq *error* olderr)
  (princ)
)
(DEFUN START ()
  (setq *error* my_err)
  (*push-error-using-command*)
  (COMMAND "_UCS" "_W" "_UNDO" "_GROUP")
  (setvar"cmdecho" 0)
  (SETVAR"AUPREC" 4)
  (SETVAR"PLINEWID" 0)
  (command"_layer" "_on" "0" "_set" "0")
  (foreach n (cdr (assoc 'listalaysof parametri))
    (command "_off" n)
  )
  (foreach n (cdr (assoc 'listalaypar parametri))
    (command "_on" n)
  )
  (command "")
  (SETQ LISTA-LAY NIL)
  (SETQ LISTA-LAY (CONS (TBLNEXT "LAYER" "") LISTA-LAY))
  (WHILE (SETQ LAY (TBLNEXT "LAYER"))
     (SETQ LISTA-LAY (CONS LAY LISTA-LAY))
  )
  ;(SETQ *ERROR* MYERR)
  (SETVAR "PICKSTYLE" 0)
  (setq parametri (carica "parametri" ""))
  (setq $lpan (cadr (assoc 'lungpannello parametri)))
)
(DEFUN MYERR (ST)
  (STOP)
)
(DEFUN STOP ()
  (SETVAR "PICKSTYLE" 1)
  (COMMAND "_UCS" "_W" "_UNDO" "_END")
  (SETVAR "EXPERT" 1)
  (COMMAND "_LAYER")
  (FOREACH N LISTA-LAY
     (IF (> 0 (CDR (ASSOC '62 N))) (COMMAND "_OF" (CDR (ASSOC '2 N))) (COMMAND "_ON" (CDR (ASSOC '2 N))))
  )
  (COMMAND "")
  (SETVAR "EXPERT" 0)
;  (COMMAND"'ZOOM" "_P")
  (COMMAND "_UCS" "_W" "_UNDO" "_END")
  (command"_layer" "_on" "0" "_set" "0" "")
;;;  (command"_layer" "_of" "traccia" "")
  (setq *error* nil)                ; Restore old *error* handler
    ;(*pop-error-mode*)
)
(defun blatt (tex lis)
    (setq grc (ssget "x" lis))
    (if grc
        (progn
            (setq nrdl (load_dialog "blatt"))
            (new_dialog "blatt" nrdl)
            (set_tile "blatt" tex) 
            (setq adl (start_dialog))
            (if (= adl 1) 
                (command "_erase" grc "")
                (exit)
            )
       )
   )
)
(defun blcanc (lis)
    (setq grc (ssget "x" lis))
    (if grc
        (command "_erase" grc "")
    )
)

(defun c:PAnnELLo ()
  (setq modop_d nil)
  (command"_layer" "_of" "tracciaold*" "")
  (START)
  (if (/= 0 (cadr (assoc 'ArretangALT parametri)))
    (progn
  (initget "Si No")
  (setq new_ang (getkword "\nAngolo alternativo? [Si/No]<No>:"))
  (if (= new_ang  "Si")
    (progn
        (setq grdel (ssget "x" (list '(0 . "INSERT") '(2 . "NEWANG"))))
        (if grdel (command "_erase" grdel ""))
    )
  )
  )(setq new_ang "No"))
  (Initget "0 1")
  (setq tipoangolo (getkword (strcat "\nTipo Angolo [0/1] <" (rtos (cadr (assoc 'tipoangolo parametri)) 2 0) ">:")))
  (if (null tipoangolo) (setq tipoangolo (cadr (assoc 'tipoangolo parametri)))
     (if (= tipoangolo "1") (setq tipoangolo 1) (setq tipoangolo 0))
    )
  (setq lnew_ang nil)
  (command"_insert" "dividi=wdividi" (getvar"viewctr") "0.001" "" "0")
  (entdel (entlast))
  (command"_insert" "dividi_pg=wdividi_pg" (getvar"viewctr") "0.001" "" "0")
  (entdel (entlast))
  (command"_insert" "divididx=wdivididx" (getvar"viewctr") "0.001" "" "0")
  (entdel (entlast))
  (command"_insert" "dividisx=wdividisx" (getvar"viewctr") "0.001" "" "0")
  (entdel (entlast))
  (command"_zoom" "_E")
  (setq hpan (getdist (strcat "\Altezza pannello [" (cadr (assoc 'Altezze parametri)) "]<" (rtos (cadr (assoc 'altezzapannello parametri)) 2 0) ">:")))
  (if (null hpan) (setq hpan (cadr (assoc 'altezzapannello parametri))))
;;;  (command"_elev" 0 hpan)
  
  (setq fpan (getstring (strcat "\Finitura pannello [" (cadr (assoc 'Finiture parametri)) "]<"(cadr (assoc 'Finitura parametri)) ">:")))
  (if (= "" fpan) (setq fpan (cadr (assoc 'Finitura parametri))))

  (setq $lpan (cadr (assoc 'lungpannello parametri)))
  (blatt "Attenzione sono gia definiti i panelli\nLI ELIMINO ?"
         (list '(0 . "LWPOLYLINE") '(8 . "pannelli*"))
  )
  (setq grdd (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "pan-lana"))))
  (if grdd (command "_erase" grdd ""))
  (setq grdd (ssget "x"  (list '(0 . "LWPOLYLINE") '(8 . "pan-Retro"))))
  (if grdd (command "_erase" grdd ""))
  
  (setq grprof (ssget "x" (list '(0 . "INSERT") '(2 . "profilo1"))))
  (if grprof (command"_erase" grprof ""))
  (blcanc (list '(8 . "esplosi*")))
  (blcanc (list '(8 . "siglepan")))
  (blcanc (list '(8 . "rif_kit")))
  (blcanc (list '(0 . "INSERT") '(2 . "codkit")))
  (blcanc (list '(0 . "INSERT") '(2 . "CONGKIT*")))
;;;  (blcanc (list '(0 . "MLINE") '(2 . "TERMAX*")))
;;;  (blcanc (list '(0 . "MLINE") '(2 . "TRAVER*")))
;;;  (blcanc (list '(0 . "MLINE") '(2 . "VELOVE*")))
  (IF (NULL grdivdac) (blcanc (list '(0 . "MLINE") '(2 . "RINFOR*"))))
;;;
;;;  (blcancex)
  (command"_layer" "_on" "traccia" "")
  (setq ktsel (ssget (list '(0 . "LWPOLYLINE") '(8 . "TRACCIA")) ))
  (if ktsel (setq kcsel nil) (setq kcsel "t"))
  (command"_layer" "_off" "traccia" "")

 (riltraccia)
 (setq hpanor hpan)
;;;  (setq llisfinh (car llisfinh))
 (foreach lisfinh llisfinh
;   (setq lisfin (car Llisfin))
   (setq hpan (car (cadr lisfinh)))
   (setq zpan (cadr (cadr lisfinh)))
   (setq hpans (caddr (cadr lisfinh)))
   (setq zpans (cadddr (cadr lisfinh)))
   (if (null hpan) (setq hpan hpanor))
   (command"_elev" zpan hpan)
   (setq lisfin (car lisfinh))
   (setq nr 0)
   (setq entr nil)
   (setq lsang1 nil)
   (setq lsang2 (ssadd))
   (setq dsr (cadr (assoc 'panSPCricerca parametri)))
   (setq $arr1STD (cadr (assoc 'ArretangSTD1 parametri)))
   (setq $arr2STD (cadr (assoc 'ArretangSTD2 parametri)))
   (setq $arr1SPC (cadr (assoc 'ArretangSPC1 parametri)))
   (setq $arr2SPC (cadr (assoc 'ArretangSPC2 parametri)))
   
   ;;; (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli" "_OF" "TRACCIA" "")
   (SETQ NONFARE NIL)
   (setq $na nil)
   (repeat (- (length lisfin) 1)
     (setq p1x nil)
     (setq p2x nil)
     (if (> nr 0) (setq p0 (nth (- nr 1) lisfin)) (setq p0 nil))
     (setq p1 (nth nr lisfin))
     (setq p1_Nua p1)
     (setq p2 (nth (+ 1 nr) lisfin))
     (setq p2_Nua p2)
     (setq p3 (nth (+ 2 nr) lisfin))
     (SETQ P4 (nth (+ 3 nr) lisfin))
     ;risega
     ;;;   (IF (AND (= "NUA" (car p1)) (= "AN1" (car p2)) (= "AN1" (car p3)) P4 (= "NUA" (car p4))
     ;;;	    (> $LPAN (+ (DISTANCE (CDR P1) (CDR P2)) (DISTANCE (CDR P2) (CDR P3)) (DISTANCE (CDR P3) (CDR P4))))
     ;;;       )
     ;;;       (PROGN
     ;;;         (SETQ NONFARE 3)
     ;;;	 (COMMAND"_TEXT" "_NON" (CDR P1) "100" "0" "VERIFICARE")
     ;;;       )
     ;;;   )
     ;;;	   (print p1) (print p2) (print p3) (print p4) (getint)
     ; PORTA BAGNO MENO DI UN PANNELLO
     (IF (AND (= "IN" (car p1)) (= "NUA" (car p2)) (= "FI" (car p3))
	      (> $LPAN (+ (DISTANCE (CDR P1) (CDR P2)) (DISTANCE (CDR P2) (CDR P3))))
	      (= 'Si (cadr (assoc 'Panregole parametri)))
	      )
       (PROGN
	 (SETQ NONFARE 2)
	 ;;;       (if (cercaspc (SETQ PPP1 (CDR P3)) (SETQ PPP2 (CDR P2)))
	 ;;;   	    (angolos (cdr P1) (cdr P2) (cdr P3) 27)
	 (angolo_s(cdr P1) (cdr P2) (cdr P3) 27)
	 ;;;       )
	 )
       )
     (if (= "FI" (car p3)) t)
     ; PORTA BAGNO MENO DI DUE PANNELLO
     (IF (AND (= "IN" (car p1)) (= "NUA" (car p2)) (= "FI" (car p3));elimina
	      (<= $LPAN (+ (DISTANCE (CDR P1) (CDR P2)) (DISTANCE (CDR P2) (CDR P3))))
	      (> (* 2 $LPAN) (+ (DISTANCE (CDR P1) (CDR P2)) (DISTANCE (CDR P2) (CDR P3))))
	      (= 'Si (cadr (assoc 'Panregole parametri)))
	      )
       (PROGN
	 (SETQ NONFARE 2)
	 (if (<= $lpan (distance (cdr p1) (cdr p2)))
	   (progn
	     (PANNELLO_s (CDR P1) (SETQ PXX1 (POLAR (CDR P1) (ANGLE (CDR P1) (CDR P2)) $LPAN)) 27)
	     (angolo_s PXX1 (cdr P2) (cdr P3) 27)
	     )
	   (progn
	     (angolo_s (cdr p1) (cdr p2) (setq pxx1 (polar (cdr p3) (angle (cdr p3) (cdr p2)) $lpan)) 27)
	     (pannello_s pxx1 (cdr p3) 27)
	     ;;;	   (PANNELLO (CDR P1) (SETQ PXX1 (POLAR (CDR P1) (ANGLE (CDR P1) (CDR P2)) $LPAN)) 27)
	     ;;;           (angolo PXX1 (cdr P2) (cdr P3) 27)
	     )
	   )
	 )
       )
     
     (if (and (NULL NONFARE) (or (= "NU" (car p1)) (= "NUA" (car p1)) (= "IN" (car p1)))
	      (or (= "NU" (car p2)) (= "NUA" (car p2)) (= "FI" (car p2)))
	      )
       (progn
	 ;       (montang (cdr p1)(cdr p2) 27)
	 ;;;       (alert "pp")
	 (setq pannellox pannello)
	 ;;;       (if (and (= "FI" (car p2)) (cercaspc (cdr p2) (cdr p1)))
	 ;;;	 (setq pannellox pannellos)
	 ;;;	 (setq pannellox pannello)
	 ;;;       )
	 
	 (if (= "NUA" (car p1))
	   (progn
	     (if (= "Si" new_ang) (progn (setq $na T $arr1std (cadr (assoc 'ArretangALT parametri))) ))
	    (IF (SSGET "_C" (list (- (CAR (CDR p1)) dsr) (- (cadr (CDR p1)) dsr)) (list (+ (CAR (CDR p1)) dsr) (+ (cadr (CDR p1)) dsr)) (list '(0 . "INSERT") '(2 . "ANGOLOSPC")))
	     (setq lpar (distance (setq p1f (polar (cdr p1) (angle (cdr p2) (cdr p1)) $arr1SPC))  (cdr p2)))
	     (setq lpar (distance (setq p1f (polar (cdr p1) (angle (cdr p2) (cdr p1)) $arr1STD))  (cdr p2)))
	     ))
	   (setq lpar (distance (setq p1f (cdr p1)) (cdr p2)) $na nil)
	   )
	 (if (= "NUA" (car p2))
	   (progn
;;;	     (setq $na T)
	    (IF (SSGET "_C"  (list (- (CAR (cdr p2)) dsr) (- (cadr (cdr p2)) dsr))
		      (list (+ (CAR (cdr p2)) dsr) (+ (cadr (cdr p2)) dsr))
		      (list '(0 . "INSERT") '(2 . "ANGOLOSPC")))
	     (setq lpar (distance p1f (setq p2f (polar (cdr p2) (angle (cdr p1) (cdr p2)) $arr2SPC))))
	     (setq lpar (distance p1f (setq p2f (polar (cdr p2) (angle (cdr p1) (cdr p2)) $arr2STD))))
	     ))
	   (setq lpar (distance p1f (setq p2f (cdr p2))))
	   )
	 (while (< $lpan (distance p1f p2f))
	   (pannello_s p1f (setq p1f (polar p1f (angle p1f (cdr p2)) $lpan)) 27)
	   )
	 ;       (pannellox p1f p2f 27)
	 (if (= "NUA" (car p2_nua)) (pannello_s (setq ppp p1f) p2f 0)  (pannello_s (setq ppp p1f) p2f 27))
	 )
       )
     
     (if (and (NULL NONFARE) (= "AN" (car p1)) (= "AN" (car p2)))
       (progn
	 (setq lpar (distance (cdr p1) (cdr p2)))
	 (setq pm (polar (cdr p1) (angle (cdr p1) (cdr p2)) (/ lpar 2)))
	 (if (> lpar $lpan)
	   (progn
	     (pannello_s (setq p1x (polar pm (angle pm (cdr p1)) (/ $lpan 2))) (setq p2x (polar pm (angle pm (cdr p2)) (/ $lpan 2))) 27)
	     (while (< $lpan (distance p1x (cdr p1)))
	       (setq p1xx p1x)
	       (pannello_s (setq p1x (polar p1x (angle p1x (cdr p1)) $lpan)) p1xx 27)
	       (pannello_s p2x (setq p2x (polar p2x (angle p2x (cdr p2)) $lpan)) 27)
	       )
	     )
	   (setq p1x pm p2x pm)
	   )
	 )
       )
     
     (if (and (NULL NONFARE) (= "AN1" (car p1)) (= "AN1" (car p2)))
       (progn
	 ;       (alert "pippo")
	 (if (and p0 (< (distance (cdr p0) (cdr p1)) (/ $lpan 2)));new
	   (setq respan1 (distance (cdr p0) (cdr p1)));new
	   (setq respan1 nil);new
	   );new
	 (if (and p3 (< (distance (cdr p3) (cdr p2)) (/ $lpan 2)));new
	   (setq respan2 (distance (cdr p3) (cdr p2)));new
	   (setq respan2 nil);new
	   );new
	 (setq lpar (distance (cdr p1) (cdr p2)))
	 (setq pm (polar (cdr p1) (angle (cdr p1) (cdr p2)) (/ lpar 2)))
	 (if (> lpar $lpan)
	   (progn
	     ;;;	   (if respan1;new
	     ;;;	     (setq p1t (setq p1x (polar (cdr p1) (angle (cdr p1) (cdr p2)) (- $lpan respan1))));new
	     ;;;	     (setq p1t (setq p1x (polar (cdr p1) (angle (cdr p1) (cdr p2)) (/ $lpan 2))))
	     ;;;	   );new
	     (if respan2;non valido solo per verifica di distanza
	       (setq p2t (setq p2x (polar (cdr p2) (angle (cdr p2) (cdr p1)) (- $lpan respan2))));new
	       (setq p2t (setq p2x (polar (cdr p2) (angle (cdr p2) (cdr p1)) (/ $lpan 2))))
	       );new
	     (if respan1;new
	       (if (< (- $lpan respan1) (distance (cdr p1) p2t))
		 (setq p1t (setq p1x (polar (cdr p1) (angle (cdr p1) (cdr p2)) (- $lpan respan1))));new
		 (setq p1t p2t p1x p2t)
		 )
	       (setq p1t (setq p1x (polar (cdr p1) (angle (cdr p1) (cdr p2)) (/ $lpan 2))))
	       );new
	     (if respan2;new
	       (if (< (- $lpan respan2) (distance (cdr p2) p1t))
		 (setq p2t (setq p2x (polar (cdr p2) (angle (cdr p2) (cdr p1)) (- $lpan respan2))));new
		 (setq p2t p1t p2x p1t)
		 )
	       (setq p2t (setq p2x (polar (cdr p2) (angle (cdr p2) (cdr p1)) (/ $lpan 2))))
	       );new
	     
	     (while (> (distance p1t p2t) $lpan)
	       (pannello_s p1t (setq p1t (polar p1t (angle p1t p2t) $lpan)) 27)
	       )
	     (if (> (distance p1t p2t) 0) (pannello_s (setq pppp p1t) p2t 27))
	     )
	   (setq p1x pm p2x pm)
	   )
	 )
       )
     (if (and (NULL NONFARE) (or (= "NU" (car p1)) (= "NUA" (car p1)) (= "IN" (car p1))) (= "AN" (car p2)))
       (progn
	 ;(montang (cdr p1)(cdr p2) 27)
	 (if (= "NUA" (car p1))
	   (IF (SSGET "_C" (list (- (CAR (cdr p1)) dsr) (- (cadr (cdr p1)) dsr))
		      (list (+ (CAR (cdr p1)) dsr) (+ (cadr (cdr p1)) dsr)) (list '(0 . "INSERT") '(2 . "ANGOLOSPC")))
	     (setq lpar (distance (setq p1f (polar (cdr p1) (angle (cdr p2) (cdr p1)) $arr1SPC))  (cdr p2)))
	     (setq lpar (distance (setq p1f (polar (cdr p1) (angle (cdr p2) (cdr p1)) $arr1STD))  (cdr p2)))
	     )
	   (setq lpar (distance (setq p1f (cdr p1)) (cdr p2)))
	   )
	 ;       (setq lpar (distance (cdr p1) (cdr p2)))
	 (if (> lpar $lpan)
	   (progn
	     (pannello_s p1f (setq p2x (polar (cdr p1) (angle (cdr p1) (cdr p2)) $lpan)) 27)
	     (while (< $lpan (distance p2x (cdr p2)))
	       (pannello_s p2x (setq p2x (polar p2x (angle p2x (cdr p2)) $lpan)) 27)
	       )
	     )
	   (setq p2x (cdr p1))
	   )
	 )
       )
     
     (if (and (NULL NONFARE) (or (= "NU" (car p1)) (= "NUA" (car p1)) (= "IN" (car p1))) (= "AN1" (car p2)))
       (progn
	 (if (= "Si" new_ang) (progn (setq $na T $arr1std (cadr (assoc 'ArretangALT parametri))) ))
	 ;(montang (cdr p1)(cdr p2) 27)
	 (if (and p3 (< (distance (cdr p3) (cdr p2)) (/ $lpan 2)));new
	   (setq respan2 (distance (cdr p3) (cdr p2)));new
	   (setq respan2 nil);new
	   );new
	 ;      (setq lpar (distance (cdr p1) (cdr p2)))
	 (if (= "NUA" (car p1))
	   ;mod 20.03.00
	   (IF (SSGET "_C" (list (- (CAR (cdr p1)) dsr) (- (cadr (cdr p1)) dsr))
		      (list (+ (CAR (cdr p1)) dsr) (+ (cadr (cdr p1)) dsr)) (list '(0 . "INSERT") '(2 . "ANGOLOSPC")))
	     (setq lpar (distance (setq p2x (setq p1f (polar (cdr p1) (angle (cdr p2) (cdr p1)) $arr1SPC)))  (cdr p2)))
	     (setq lpar (distance (setq p2x (setq p1f (polar (cdr p1) (angle (cdr p2) (cdr p1)) $arr1STD)))  (cdr p2)))
	     )
	   (setq lpar (distance (setq p1f (cdr p1)) (cdr p2)))
	   )
	 (if (> lpar (/ $lpan 2))
	   (progn
	     (if respan2;new
	       (setq p2x (polar (cdr p2) (angle (cdr p2) (cdr p1)) (- $lpan respan2)));new
	       (setq p2x (polar (cdr p2) (angle (cdr p2) (cdr p1)) (/ $lpan 2)))
	       );new
	     (setq pt1 p1f)
	     (while (> (distance pt1 p2x) $lpan)
	       (pannello_s pt1 (setq pt1 (polar pt1 (angle (cdr p1) (cdr p2)) $lpan)) 27)
	       )
	     (pannello_s (setq pppp pt1) p2x 27) 
	     )
	   ;mod 20.03.00
	   (if (null p2x) (setq p2x (cdr p1)))
	   )
	 )
       )
     
     (if (and (NULL NONFARE) (= "AN" (car p1)) (or (= "NU" (car p2))(= "NUA" (car p2))(= "FI" (car p2))))
       (progn
	 (setq pannellox pannello)
	 ;;;       (if (and (= "FI" (car p2)) (cercaspc (cdr p2) (cdr p1)))
	 ;;;	 (setq pannellox pannellos)
	 ;;;	 (setq pannellox pannello)
	 ;;;       )
	 (if (= "NUA" (car p2))
	   (IF (SSGET "_C" (list (- (CAR (cdr p2)) dsr) (- (cadr (cdr p2)) dsr))
		      (list (+ (CAR (cdr p2)) dsr) (+ (cadr (cdr p2)) dsr)) (list '(0 . "INSERT") '(2 . "ANGOLOSPC")))
	     (setq lpar (distance (cdr p1) (setq p2f (polar (cdr p2) (angle (cdr p1) (cdr p2)) $arr2SPC))))
	     (setq lpar (distance (cdr p1) (setq p2f (polar (cdr p2) (angle (cdr p1) (cdr p2)) $arr2STD))))
	     )
	   (setq lpar (distance (cdr p1) (setq p1f (cdr p2))))
	   )
	 ;       (setq lpar (distance (cdr p1) (cdr p2)))
	 (if (> lpar $lpan)
	   (progn
	     (setq p1x (cdr p1))
	     (setq p2f (cdr p2))
	     ;	   (pannellox (setq p1x (polar p2f (angle (cdr p2) (cdr p1)) $lpan)) p2f 27)
	     (pannello_s (setq p1x (polar p2f (angle (cdr p2) (cdr p1)) $lpan)) p2f 27)
	     (while (< $lpan (distance p1x (cdr p1)))
	       (setq p1xx p1x)
	       (pannello_s (setq p1x (polar p1x (angle p1x (cdr p1)) $lpan)) p1xx 27)
	       )
	     )
	   (setq p1x p1f)
	   )
	 )
       )
     
     (if (and (NULL NONFARE) (or (= "ANx" (car p1)) (= "AN1" (car p1))) (or (= "NU" (car p2))(= "NUA" (car p2))(= "FI" (car p2))))
       (progn
	 (setq pannellox pannello)
	 ;;;       (if (and (= "FI" (car p2)) (cercaspc (cdr p2) (cdr p1)))
	 ;;;	 (setq pannellox pannellos)
	 ;;;	 (setq pannellox pannello)
	 ;;;       )
	 ;       (alert "pippo")
	 (if (and p0 (< (distance (cdr p0) (cdr p1)) (/ $lpan 2)));new
	   (setq respan1 (distance (cdr p0) (cdr p1)));new
	   (setq respan1 nil);new
	   );new
	 (if (= "NUA" (car p2))
	   (IF (SSGET "_C" (list (- (CAR (cdr p2)) dsr) (- (cadr (cdr p2)) dsr))
		      (list (+ (CAR (cdr p2)) dsr) (+ (cadr (cdr p2)) dsr)) (list '(0 . "INSERT") '(2 . "ANGOLOSPC")))
	     (setq lpar (distance (cdr p1) (setq p2f (polar (cdr p2) (angle (cdr p1) (cdr p2)) $arr2SPC))))
	     (setq lpar (distance (cdr p1) (setq p2f (polar (cdr p2) (angle (cdr p1) (cdr p2)) $arr2STD))))
	     )
	   (setq lpar (distance (cdr p1) (setq p2f (cdr p2))))
	   ;            (setq lpar (distance (cdr p1) (setq p1f (cdr p2))))
	   )
	 ;       (setq lpar (distance (cdr p1) (cdr p2)))
	 (if (> lpar (/ $lpan 2))
	   (progn
	     (if respan1;new
	       (setq p1x (setq pt1 (polar (cdr p1) (angle (cdr p1) (cdr p2)) (- $lpan respan1))));new
	       (setq p1x (setq pt1 (polar (cdr p1) (angle (cdr p1) (cdr p2)) (/ $lpan 2))))
	       );new
	     (while (> (distance p2f pt1) $lpan)
	       (pannello_s pt1 (setq pt1 (polar pt1 (angle (cdr p1) (cdr p2)) $lpan)) 27)
	       )
	     ;           (pannellox pt1 p2f 27)
	     (if (= (car p2_nua) "NUA") (pannello_s (setq ppp pt1) p2f 0) (pannello_s (setq ppp pt1) p2f 27))
	     )
	   (setq p1x p2f)
	   )
	 )
       )
     (COND ((= NONFARE 3) (SETQ NONFARE 2))
	   ((= NONFARE 2) (SETQ NONFARE 1))
	   ((= NONFARE 1) (SETQ NONFARE NIL))
	   ("T" (SETQ NONFARE NIL))
	   )
     (setq nr (+ 1 nr))
     (if p1x
       (progn
	 (command "_color" 2 "_line" "_non" (list (car p1x) (cadr p1x) 0.0) "_non" (cdr p1) "")
	 (if entr
	   (progn
	     ;	  (print "pippo")
	     ;;;	  (if (cercaspc (cdr (assoc '10 (entget (setq entx (entlast))))) (cdr (assoc '11 (entget entr))))
	     ;;;   	    (angolos (cdr (assoc '10 (entget entr))) (cdr (assoc '11 (entget entr))) (cdr (assoc '10 (entget (setq entx (entlast))))) 27)
	     (angolo_s (cdr (assoc '10 (entget entr))) (cdr (assoc '11 (entget entr))) (cdr (assoc '10 (entget (setq entx (entlast))))) 27)
	     ;;;	  )
	     (entdel entr)
	     (entdel entx)
	     (setq entr nil)
	     )
	   (setq entr (entlast))
	   )
	 )
       )
     (if p2x
       (progn
	 (command "_color" 3 "_line" "_non" (list (car p2x) (cadr p2x) 0.0) "_non" (cdr p2) "")
	 (setq entr (entlast))
	 )
       )
     )
 )
 (setq hpan hpanor)
 (foreach p1 lnew_ang
   (command"_insert" "NEWANG" p1 "1" "1" "0")
 )
   
;;; (PANFONDO T)
 (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-dritti" "_OF" "TRACCIA" "")
 (stop)
 (if grdivdac (progn (command"_erase" grdivdac "") (setq grdivdac nil)))
 (IF (SETQ GRAGG (SSGET "x" (LIST '(0 . "INSERT") '(2 . "RINDIV"))))
   (AGGRINDIV GRAGG)
 )
 (command"_insert" "dividi=dividi" (getvar"viewctr") "0.001" "" "0")
  (entdel (entlast))
;;; (command"_insert" "dividi_pg=dividi_pg" (getvar"viewctr") "0.001" "" "0")
;;;  (entdel (entlast))
  (command"_insert" "divididx=divididx" (getvar"viewctr") "0.001" "" "0")
  (entdel (entlast))
  (command"_insert" "dividisx=dividisx" (getvar"viewctr") "0.001" "" "0")
  (entdel (entlast))
  (setq grent (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (setq nrk -1)
  (while (and grent (setq ent (ssname grent (setq nrk (+ 1 nrk)))))
  ;(setq ent (car (entsel)))
  (aggpann ent t)
    )

  (stop)
)

(defun riltraccia ()
  (setq nrtrac -1)
  (if (null ktsel) (setq kcsel t))
  
  (if kcsel
     (setq gr (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "TRACCIA"))))
     (setq gr ktsel)
    )
  (if (or (null gr) (< (sslength gr) 1)) (er "nessuna traccia" "EXIT"))
  (setq llisfin nil)
  (setq llisfinh nil)
  (while (SETQ TRACCIA (SSNAME GR (setq nrtrac (+ 1 nrtrac))))
    (if (null kcsel)
  (progn

    (setq riflspt- nil)
    (setq rpold (list 0 0 0))
    (foreach n (entget traccia)
       (IF (= '10 (CAR N))
	 (progn
	   (if (> (distance rpold (cdr n)) 1)
	     (SETQ riflspt- (CONS (CDR n) riflspt-))
	     )
	   (setq rpold (cdr n))
	   )
	 )
      )
    (setvar"pdmode" 32)
    (setvar"pdsize" 100)
    (setq riflspt- (cons (last riflspt-) riflspt-))
    (setq rilgr (ssget "_f" riflspt- (list '(0 . "POINT") '(8 . "RIF_KIT"))))
    (if rilgr (command"_erase" rilgr ""))

      (setq rilgr (ssget "_f" riflspt- (list '(0 . "INSERT") '(2 . "CONGKIT*"))))
  (IF rilgr (COMMAND"_ERASE" rilgr ""))

    (setvar"pdmode" 0)
    (setvar"pdsize" 0)
))    
;;;    (setq hpani (leggi traccia "HPAN"))
;;;    (setq hpans (leggi traccia "HPANS"))
    (setq zpan1 (cdr (assoc '38 (entget traccia))))
    (SETQ LISPT NIL)
    (SETQ PRIMPT NIL)
    (FOREACH N (ENTGET TRACCIA)
      (IF (= '10 (CAR N))
	(PROGN
	  (SETQ LISPT (CONS (CDR N) LISPT))
	  (IF (NULL PRIMPT) (SETQ PRIMPT (CDR N)))
	  )
	)
    )
;____________________________________________________________________
    (IF (= '128 (CDR (ASSOC '70 (ENTGET TRACCIA))))
      (PROGN
       (IF (NULL (SSGET "_C" (LIST (- (CAR (CAR LISPT)) 1) (- (CADR (CAR LISPT)) 1)) (LIST (+ (CAR (CAR LISPT)) 1) (+ (CADR (CAR LISPT)) 1))
			 (LIST '(0 . "INSERT") '(2 . "DIVIDIDX*"))
	         )
	   )
 	   (COMMAND"_INSERT" "DIVIDIDX" (CAR LISPT) "1" "1" (ANGTOS (- (ANGLE (CAR LISPT) (CADR LISPT)) (/ PI 2))0 2))
       )
       (IF (NULL (SSGET "_C" (LIST (- (CAR (LAST LISPT)) 1) (- (CADR (LAST LISPT)) 1)) (LIST (+ (CAR (LAST LISPT)) 1) (+ (CADR (LAST LISPT)) 1))
			 (LIST '(0 . "INSERT") '(2 . "DIVIDISX*"))
	         )
	   )
	   (COMMAND"_INSERT" "DIVIDISX" (LAST LISPT) "1" "1" (ANGTOS (+ (ANGLE (CAR (REVERSE LISPT)) (CADR (REVERSE LISPT))) (/ PI 2))0 2))
       )
      )
      (SETQ LISPT1 (REVERSE (CONS PRIMPT LISPT)))
    )
;____________________________________________________________________    
    (SETQ LISPT1 (REVERSE (CONS PRIMPT LISPT)))
    (SETQ LISPT (REVERSE LISPT))
    (SETQ DISTOT 0)
    (SETQ LISDIV (LIST (CAR LISPT)))
    (SETQ NR 1)
    (FOREACH N LISPT1
      (IF (NTH NR LISPT1)
	(SETQ GRD (SSGET "_F" (LIST N (NTH NR LISPT1))
			 (LIST '(0 . "INSERT") '(2 . "DIVIDI*"))
			 )
	      )
	)
      (IF GRD
	(PROGN
	  (SETQ NE 0)
	  (SETQ LSPT NIL)
	  (WHILE (SETQ ENT (SSNAME GRD NE))
	    (COND ((OR (= "DIVIDISX" (strcase (CDR (ASSOC '2 (ENTGET ENT))))) (= "DIVIDISXF" (strcase (CDR (ASSOC '2 (ENTGET ENT)))))
		       (= "DIVIDISX28" (strcase (CDR (ASSOC '2 (ENTGET ENT)))))
		   )
		   (SETQ PR (CONS "IN" (CDR (ASSOC '10 (ENTGET ENT))))
			 LSPT (CONS (CDR (ASSOC '10 (ENTGET ENT))) LSPT)
			 )
		   )
		  ((OR (= "DIVIDIDX" (strcase (CDR (ASSOC '2 (ENTGET ENT))))) (= "DIVIDIDXF" (strcase (CDR (ASSOC '2 (ENTGET ENT)))))
		       (= "DIVIDIDX28" (strcase (CDR (ASSOC '2 (ENTGET ENT)))))
		   )
		   (SETQ PR (CONS "FI" (CDR (ASSOC '10 (ENTGET ENT))))
			 LSPT (CONS (CDR (ASSOC '10 (ENTGET ENT))) LSPT)
			 )
		   )
		  ((= "DIVIDI" (strcase (CDR (ASSOC '2 (ENTGET ENT)))))
		   (SETQ PR (CONS "NU" (CDR (ASSOC '10 (ENTGET ENT))))
			 LSPT (CONS (CDR (ASSOC '10 (ENTGET ENT))) LSPT)
			 )
		   )
		  ((= "DIVIDI_PG" (strcase (CDR (ASSOC '2 (ENTGET ENT)))))
		   (SETQ PR (CONS "NU" (CDR (ASSOC '10 (ENTGET ENT))))
			 LSPT (CONS (CDR (ASSOC '10 (ENTGET ENT))) LSPT)
			 )
		   )
		  ((= "DIVIDIF" (strcase (CDR (ASSOC '2 (ENTGET ENT)))))
		   (SETQ PR (CONS "NU" (CDR (ASSOC '10 (ENTGET ENT))))
			 LSPT (CONS (CDR (ASSOC '10 (ENTGET ENT))) LSPT)
			 )
		   )
		  )
	    (SETQ LISDIV (CONS PR LISDIV))
	    (SETQ NE (+ 1 NE))
	    )
	  )
	)
      (SETQ GRD NIL)
      (IF (NTH NR LISPT)
	(SETQ LISDIV (CONS (NTH NR LISPT) LISDIV))
	)
      (SETQ LSPT NIL)
      (SETQ NR (+ 1 NR))
      )
    (SETQ LISDIV (REVERSE LISDIV))
    (SETQ NO (CAR LISDIV))
    (IF (= 'STR (TYPE (CAR No))) (SETQ Pold (CDR No))(SETQ Pold No))
    (SETQ LISDIVN NIL)
    (SETQ NC (NTH 1 LISDIV))
    (IF (= 'STR (TYPE (CAR NC))) (SETQ PCOR (CDR NC))(SETQ PCOR NC))
    (IF (< 1 (DISTANCE PCOR POLD)) (SETQ LISDIVN (CONS no LISDIVN)))
    (FOREACH NC (CDR LISDIV)
      (IF (= 'STR (TYPE (CAR NC))) (SETQ PCOR (CDR NC))(SETQ PCOR NC))
      (IF (< 1 (DISTANCE POLD PCOR))
	(SETQ LISDIVN (CONS NC LISDIVN))
	(PROGN
	  (COND ((= 'STR (TYPE (CAR NO))) (SETQ LISDIVN (CONS NO (cdr LISDIVN))))
		((= 'STR (TYPE (CAR NC))) (SETQ LISDIVN (CONS NC (cdr LISDIVN))))
		("T" (SETQ LISDIVN (CONS nc (cdr LISDIVN))))
		)
	  )
	)
      (setq pold pcor no nc)
      )
    (SETQ LISDIV (REVERSE LISDIVN))
    (setq lisfin (reverse (member (assoc "IN" lisdiv) lisdiv)))
    (setq nr -1)
    (repeat (- (length lisdiv) (length lisfin))
      (setq lisfin (cons (nth (setq nr (+ nr 1)) lisdiv) lisfin))
      )
    (setq lisfin (reverse lisfin))

;;;    (if (= (cadr (assoc 'tipoangolo parametri)) 1)
    (if (= tipoangolo 1)
      (progn
	(setq nr 1)
	(repeat (- (length lisfin) 2)
	  (setq n (nth nr lisfin))
	  (if (/= 'STR (type (car n)))
	    (progn
	      (command"_dimangular" "" "_non" n)
	      (if (/= 'STR (type (car (nth (- nr 1) lisfin))))
		(command "_non" (nth (- nr 1) lisfin))
		(command "_non" (cdr (nth (- nr 1) lisfin)))
		)
	      (if (/= 'STR (type (car (nth (+ nr 1) lisfin))))
		(command "_non" (nth (+ nr 1) lisfin))
		(command "_non" (cdr (nth (+ nr 1) lisfin)))
		)
	      (if (/= 'STR (type (car (nth (+ nr 1) lisfin))))
		(command "_non" (nth (+ nr 1) lisfin))
		(command "_non" (cdr (nth (+ nr 1) lisfin)))
		)
	      (setq angpan (atof (angtos (cdr (assoc '42 (entget (entlast)))) 0 0)))
	      (entdel (entlast))
	      (if (= angpan 90.0) 
		(setq lisfin (subst (cons "NUA" n) n lisfin))
		(setq lisfin (subst (cons "AN1" n) n lisfin))
		)
	      )
	    )
	  (setq nr (+ 1 nr))
	  )
	)
      (foreach n lisfin
	(if (/= 'STR (type (car n)))
	  (setq lisfin (subst (cons "AN" n) n lisfin))
	  )
	)
      
      )
    (setq lisfinxx lisfin lisfin nil)
    (setq metti "t")
    (foreach n lisfinxx
      (cond ((= "FI" (car n)) (setq metti nil) (setq lisfin (cons n lisfin)))
	    ((= "IN" (car n)) (setq metti "t") (setq lisfin (cons n lisfin)))
	    (metti (setq lisfin (cons n lisfin)))
	    )
      )
    (setq lisfin (reverse lisfin))
    (setq kkprec nil)
    (setq lisfin_ nil)
    (setq l300 (/ (cadr (assoc 'lungpannello parametri)) 2))
    
;;;    (foreach kk lisfin
;;;      (if (and  (= "AN" (car kkprec)) (> (distance (cdr kk) (cdr kkprec)) l300))
;;;	(setq lisfin_ (cons (list "NU" (car (polar (cdr kkprec) (angle (cdr kkprec) (cdr kk)) l300))
;;;				      (cadr (polar (cdr kkprec) (angle (cdr kkprec) (cdr kk)) l300))) lisfin_))
;;;	)
;;;      (if (and kkprec (= "AN" (car kk)) (> (distance (cdr kk) (cdr kkprec)) l300))
;;;	(setq lisfin_ (cons (list "NU" (car (polar (cdr kk) (angle (cdr kk) (cdr kkprec)) l300))
;;;				      (cadr (polar (cdr kk) (angle (cdr kk) (cdr kkprec)) l300))) lisfin_))
;;;	)
;;;      
;;;      (setq kkprec kk)
;;;      (setq lisfin_ (cons kk lisfin_))
;;;    )
    (setq lpan (cadr (assoc 'lungpannello parametri)))
    (setq lpanSPC (cadr (assoc 'lungpannelloSPC parametri)))
    (setq langmin 30)
    (foreach kk lisfin
      (if (and  (= "IN" (car kkprec)) (= "FI" (car kk)) (> (distance (cdr kk) (cdr kkprec)) (+ 0 lpan)))
	(progn (setq resto lpan)
	 (while (>= resto lpan)
	  (setq lisfin_ (cons (list "NU" (car (polar (cdr kkprec) (angle (cdr kkprec) (cdr kk)) lpanSPC))
				      (cadr (polar (cdr kkprec) (angle (cdr kkprec) (cdr kk)) lpanSPC))) lisfin_))
	   (setq resto 0)
	   )
	  )
	)
      (if (and  (= "IN" (car kkprec)) (= "AN" (car kk)))
	(if (> (distance (cdr kk) (cdr kkprec)) (+ langmin lpan))
	  (progn (setq resto lpan)
	    (while (>= resto (distance (cdr kk) (cdr kkprec)))
	      (setq lisfin_ (cons (list "NU" (car (polar (cdr kkprec) (angle (cdr kkprec) (cdr kk)) lpanSPC))
					(cadr (polar (cdr kkprec) (angle (cdr kkprec) (cdr kk)) lpanSPC))) lisfin_))
	      (setq resto 0)
	      )
	    )
	  (progn (setq resto 0)
	    (if (>= (distance (cdr kk) (cdr kkprec)) lpan)
	      (setq lisfin_ (cons (list "NU" (car (polar (cdr kk) (angle (cdr kk) (cdr kkprec)) langmin))
					(cadr (polar (cdr kk) (angle (cdr kk) (cdr kkprec)) langmin))) lisfin_)
		    resto 0)
	      )
	    )
	  )
	  
;;;	(setq lisfin_ (cons kk lisfin_))
	)
      (if (and  (= "AN" (car kkprec)) (= "FI" (car kk)))
	(if (> (distance (cdr kk) (cdr kkprec)) (+ langmin lpan))
	  (progn (setq resto lpan)
	    (while (>= resto lpan)
	      (setq lisfin_ (cons (list "NU" (car (polar (cdr kk) (angle (cdr kk) (cdr kkprec)) lpanSPC))
					(cadr (polar (cdr kk) (angle (cdr kk) (cdr kkprec)) lpanSPC))) lisfin_))
	      (setq resto 0)
	      )
	    )
	  (progn (setq resto 0)
	    (if (>= (distance (cdr kk) (cdr kkprec)) lpan)
	      (setq lisfin_ (cons (list "NU" (car (polar (cdr kkprec) (angle (cdr kkprec) (cdr kk)) langmin))
					(cadr (polar (cdr kkprec) (angle (cdr kkprec) (cdr kk)) langmin))) lisfin_))
	      )
	    )
	  )
	)
      (if (and  (= "AN" (car kkprec)) (= "AN" (car kk)) (> (distance (cdr kk) (cdr kkprec)) (+ langmin langmin lpan)))
	(progn (setq resto lpan)
	  (setq lang1 (/ (distance (cdr kk) (cdr kkprec)) lpan))
	  (setq lang1 (fix lang1))
	  (setq lang1 (* lang1 lpan))
	  (setq distang (/ (- (distance (cdr kk) (cdr kkprec)) lang1) 2))
	  (setq resto 0)
	 (if (>= distang langmin)
	  (setq lisfin_ (cons (list "NU" (car (polar (cdr kkprec) (angle (cdr kkprec) (cdr kk)) distang))
				      (cadr (polar (cdr kkprec) (angle (cdr kkprec) (cdr kk)) distang))) lisfin_))
	   (progn
	     	     
	     (setq lisfin_ (cons (list "NU" (car (polar (cdr kkprec) (angle (cdr kkprec) (cdr kk)) langmin))
				      (cadr (polar (cdr kkprec) (angle (cdr kkprec) (cdr kk)) langmin))) lisfin_))
	     (setq lisfin_ (cons (list "NU" (car (polar (cdr kk) (angle (cdr kk) (cdr kkprec)) langmin))
				      (cadr (polar (cdr kk) (angle (cdr kk) (cdr kkprec)) langmin))) lisfin_))
	     )

	   
	   )
	  )
	)
;;;      (if (and kkprec (= "AN" (car kk)) (> (distance (cdr kk) (cdr kkprec)) l300))
;;;	(setq lisfin_ (cons (list "NU" (car (polar (cdr kk) (angle (cdr kk) (cdr kkprec)) l300))
;;;				      (cadr (polar (cdr kk) (angle (cdr kk) (cdr kkprec)) l300))) lisfin_))
;;;	)
      
      (setq kkprec kk)
      (setq lisfin_ (cons kk lisfin_))
    )
    (setq lisfin (reverse lisfin_))
    (setq llisfin (cons lisfin llisfin))

    (if hpans
      (setq llisfinh (cons (list lisfin (list hpani zpan1 hpans (+ zpan1 (- hpan (atof hpans))))) llisfinh))
      (setq llisfinh (cons (list lisfin (list hpani zpan1 hpans nil)) llisfinh))
    )
      
  )
  (setq ktsel nil)
)
(defun angolo_s (p1 p2 p3 sp)
  (if (and hpans (/= hpans 0))
 
      (cond
;;;	((and (cercaspf p3 p2) (cercaspf p1 p2))
;;;	     (angolosd_f p1 p2 p3 sp) (setq $__$ (cdr (assoc '5 (entget (entlast)))))(command"_elev" "" hpans) (setq zpanf zpan) (setq zpan zpans)
;;;	     (angolosd_f p1 p2 p3 sp) (allega (entlast) "PSUP" $__$)(command"_elev" "" hpan) (setq zpan zpanf)(setq $__$ nil)
;;;	    )
;;;            ((cercaspF p3 p2)
;;;	     (angolos_f p1 p2 p3 sp)(setq $__$ (cdr (assoc '5 (entget (entlast))))) (command"_elev" "" hpans)(setq zpanf zpan)(setq zpan zpans)
;;;	     (angolos_f p1 p2 p3 sp)(allega (entlast) "PSUP" $__$)(command"_elev" "" hpan)(setq zpan zpanf) (setq $__$ nil)
;;;	    )
;;;	    ((cercaspF p1 p2)
;;;	     (angolod_f p1 p2 p3 sp)(setq $__$ (cdr (assoc '5 (entget (entlast)))))(command"_elev" "" hpans)(setq zpanf zpan)(setq zpan zpans)
;;;	     (angolod_f p1 p2 p3 sp)(allega (entlast) "PSUP" $__$)(command"_elev" "" hpan)(setq zpan zpanf)(setq $__$ nil)
;;;	    )
	    ("T"
	     (angolon_f p1 p2 p3 sp)(setq $__$ (cdr (assoc '5 (entget (entlast)))))(command"_elev" "" hpans)(setq zpanf zpan)(setq zpan zpans)
	     (angolon_f p1 p2 p3 sp)(allega (entlast) "PSUP" $__$)(command"_elev" "" hpan)(setq zpan zpanf)(setq $__$ nil)
	    )
       )
      (if (= hpans 0)
	(cond
;;;	  ((and (cercaspf p3 p2) (cercaspf p1 p2)) (angolosd_f p1 p2 p3 sp))
;;;	      ((cercaspf p3 p2) (angolos_f p1 p2 p3 sp))
;;;	      ((cercaspf p1 p2) (angolod_f p1 p2 p3 sp))
	      ("t" (angolon p1 p2 sp))
	)
	(cond
;;;	  ((and (cercaspFx p3 p2) (cercaspFX p1 p2)) (angolosd_F p1 p2 p3 sp))
;;;      	      ((cercaspFX p3 p2) (angolos_F p1 p2 p3 sp))
;;;	      ((cercaspFX p1 p2) (angolod_F p1 p2 p3 sp))
;;;	      ((and (cercasp28x p3 p2) (cercasp28x p1 p2)) (angolosd28 p1 p2 p3 sp))
;;;	      ((cercasp28x p3 p2) (angolos28 p1 p2 p3 sp))
;;;	      ((cercasp28x p1 p2) (angolod28 p1 p2 p3 sp))
	      ((and (cercaspc p3 p2) (cercaspc p1 p2)) (angolosd p1 p2 p3 sp)) ;angolosd
	      ((cercaspc p3 p2) (angolomd p1 p2 p3 sp))
	      ((cercaspc p1 p2) (angoloms p1 p2 p3 sp));angolod
	      ("t" (angolon p1 p2 p3 sp))
	)
      )
    )
  (setq ang_prof (angle p2 p3))
)


(defun pannello_s (p1 p2 sp)
  (if (and hpans (/= hpans 0))
      (cond ((and (cercaspF p2 p1) (cercaspF p1 p2))
	     (pannellosd_f p1 p2 sp) (setq $__$ (cdr (assoc '5 (entget (entlast)))))(command"_elev" "" hpans) (setq zpanf zpan) (setq zpan zpans)
	     (pannellosd_f p1 p2 sp) (allega (entlast) "PSUP" $__$)(command"_elev" "" hpan) (setq zpan zpanf)(setq $__$ nil)
	    )
            ((cercaspF p2 p1)
	     (pannellos_f p1 p2 sp)(setq $__$ (cdr (assoc '5 (entget (entlast))))) (command"_elev" "" hpans)(setq zpanf zpan)(setq zpan zpans)
	     (pannellos_f p1 p2 sp)(allega (entlast) "PSUP" $__$)(command"_elev" "" hpan)(setq zpan zpanf) (setq $__$ nil)
	    )
	    ((cercaspF p1 p2)
	     (pannellod_f p1 p2 sp)(setq $__$ (cdr (assoc '5 (entget (entlast)))))(command"_elev" "" hpans)(setq zpanf zpan)(setq zpan zpans)
	     (pannellod_f p1 p2 sp)(allega (entlast) "PSUP" $__$)(command"_elev" "" hpan)(setq zpan zpanf)(setq $__$ nil)
	    )
	    ("t"
	     (pannellon_f p1 p2 sp)(setq $__$ (cdr (assoc '5 (entget (entlast)))))(command"_elev" "" hpans)(setq zpanf zpan)(setq zpan zpans)
	     (pannellon_f p1 p2 sp)(allega (entlast) "PSUP" $__$)(command"_elev" "" hpan)(setq zpan zpanf)(setq $__$ nil)
	    )
      )
      (if (= hpans 0)
	(cond
;;;	  ((and (cercaspf p2 p1) (cercaspf p1 p2)) (pannellosd_f p1 p2 sp))
;;;	      ((cercaspf p2 p1) (pannellos_f p1 p2 sp))
;;;	      ((cercaspf p1 p2) (pannellod_f p1 p2 sp))
	      ("t" (pannellon p1 p2 sp))
	)
	(if $na
	  (progn
;;;	    (alert "1")
          (cond
	    
;;;	    ((and (cercaspFx p2 p1) (cercaspFX p1 p2)) (pannellosd_F p1 p2 sp))
;;;      	      ((cercaspFX p2 p1) (pannellos_F p1 p2 sp))
;;;	      ((cercaspFX p1 p2) (pannellod_F p1 p2 sp))
;;;	      ((and (cercasp28x p2 p1) (cercasp28x p1 p2)) (pannellosd28 p1 p2 sp))
;;;	      ((cercasp28x p2 p1) (pannellos28 p1 p2 sp))
;;;	      ((cercasp28x p1 p2) (pannellod28 p1 p2 sp))
	      ((and (cercaspc p2 p1) (cercaspc p1 p2)) (pannellosd p1 p2 sp));pannellosd
	      ((cercaspc p2 p1) (pannellosd p1 p2 sp)(setq $na nil)(setq lnew_ang (cons p1 lnew_ang))) ;pannellosd
	      ((cercaspc p1 p2) (pannellod p1 p2 sp)) ;pannellod
	      ("t" (pannellod p1 p2 sp) (setq $na nil)(setq lnew_ang (cons p1 lnew_ang))) ;pannellod 
	  )
	    )
	  (progn
;;;	    (alert "0")
	  (cond
	    
;;;	    ((and (cercaspFx p2 p1) (cercaspFX p1 p2)) (pannellosd_F p1 p2 sp))
;;;      	      ((cercaspFX p2 p1) (pannellos_F p1 p2 sp))
;;;	      ((cercaspFX p1 p2) (pannellod_F p1 p2 sp))
;;;	      ((and (cercasp28x p2 p1) (cercasp28x p1 p2)) (pannellosd28 p1 p2 sp))
;;;	      ((cercasp28x p2 p1) (pannellos28 p1 p2 sp))
;;;	      ((cercasp28x p1 p2) (pannellod28 p1 p2 sp))
	      ((and (cercaspc p2 p1) (cercaspc p1 p2)) (pannellomd p1 p2 sp));pannellosd
	      ((cercaspc p2 p1) (pannellomd p1 p2 sp));prova
	      ((cercaspc p1 p2) (if (= sp 0) (pannello2d p1 p2 sp) (pannelloms p1 p2 sp)));pannellod;manina
	      ("t" (if (= sp 0) (pannellofd p1 p2 sp) (pannellon p1 p2 sp)))
	  )
	    )
	)
      )
   )
  (setq ang_prof (angle p1 p2))
)
(defun cercaspc (pt1 pt2)
  (setq dsr (cadr (assoc 'panSPCricerca parametri)))
  (setq spc nil)
  (foreach n (cdr (assoc 'BlocchipanSPC parametri))
    (SETQ N (CAR N))
;;;	(if (ssget "_f" (list (polar pt1 (angle pt1 pt2) dsr) (polar pt1 (angle pt2 pt1) dsr))
;;;		  (list (cons '0 "INSERT")
;;;			(cons '2 n)
;;;	          )
;;;            )
;;;	    (setq spc "T")
;;;	)
    
     (if (ssget "_C"  (LIST (- (CAR pt1) DSR) (- (CADR pt1) DSR));new
 		      (LIST (+ (CAR pt1) DSR) (+ (CADR pt1) DSR));new
		  (list (cons '0 "INSERT");new
			(cons '2 n);new
	          );new
         )    ;new
         (setq spc "T");new
     );new
    
   )
   spc
)
(DEFUN ALLEGA (ENT APP TIPO)
  (if (and ent app tipo)
    (progn
      (REGAPP APP)
      (SETQ ENTG (ENTGET ENT))
      (SETQ ENTN (APPEND ENTG (LIST (LIST '-3 (LIST APP (CONS 1000 TIPO))))))
      (ENTMOD ENTN)
      (ENTUPD ENT)
    )
    (alert "\nCompletamenti mancanti")
  )
)






(DEFUN C:LEGGI ()
  (setq ents (entsel))
  (SETQ TX1 (LEGGI (CAR ENTS) "PANFIN"))
  (if (null TX1) (setq TX1 "A"))
  (SETQ TX2 (LEGGI (CAR ENTS) "PANTIP"))
  (SETQ TX3 (LEGGI (CAR ENTS) "PANRES"))
  (setq tx4 (leggi (CAR ENTS) "BLOKIT"))
  (setq tx5 (leggi (CAR ENTS) "PSUP"))
  (setq tx6 (leggi (CAR ENTS) "BLOKIT_V"));30.03.04
  (setq tx7 (leggi (CAR ENTS) "PANRETRO"));30.03.04
  (setq tx8 (leggi (CAR ENTS) "PANLANA"));30.03.04
  (setq tx9 (leggi (CAR ENTS) "PANPIEG"));30.03.04
  (if (null tx6) (setq tx6 "Null"));30.03.04
  (if (null tx5) (setq tx5 "Null"))
  (if (null tx4) (setq tx4 "Null"))
  (if (null tx7) (setq tx7 "Null"));30.03.04
  (if (null tx8) (setq tx8 "Null"))
  (if (null tx9) (setq tx9 "Null"))
  (PROMPT (strcat "\nFinitura:" tx1 " Tipo:" tx2 "  Resis: " tx3 " Kit: " tx4 " Psup: " tx5 "  Kit Vincolato:" tx6));30.03.04
  (PROMPT (strcat "\nRETRO:" tx7 " LANA:" tx8 "  PIEGHE: " tx9));30.03.04
  
)

(DEFUN LEGGI (ENT APP)
  (IF (AND (ENTGET ENT)
	   (SETQ TMP (ENTGET ENT (LIST APP)))
	   (SETQ TMP (ASSOC -3 TMP))
      )
    (progn
	
	(CDR (CADR (CADR TMP)))
      
      )
  )
)


(DEFUN SELEZ (BASESEL)
 (SETQ ENTNE BASESEL)
 (SETQ GRSELx (SSADD))
 (WHILE (SETQ ENTNE (ENTNEXT ENTNE))
     (SETQ GRSEL (SSADD ENTNE GRSELx))
 )
 GRSELx
)
(defun c:quotacab ()
   (START)
   (setq distanza_quote_int 500)
   (setq grdl (ssget "x" (list '(0 . "DIMENSION") '(8 . "QUOTECABINA"))))
   (IF GRDL (COMMAND"_ERASE" GRDL ""))

   (command "_ucs" "_W" "_layer" "_m" "quoteCABINA" "") 
   (setq gr (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "TRACCIA"))))
;   (if (/= (sslength gr) 1) (progn (alert "Non c'e una sola Polilinea di contorno !!") (exit)))
 (SETQ NRQQ -1)
 (WHILE (SETQ ENT (SSNAME GR (SETQ NRQQ (+ 1 NRQQ))))
;   (setq ent (ssname gr 0))
   (setq lispt nil)
   (setq  xmin 1000000 ymin 1000000 xmax -1000000 ymax -1000000 lspt nil)
   (SETQ PINI NIL)
   (foreach n (entget ent) 
        (if (= '10 (car n)) 
            (progn
                 (IF (NULL PINI) (SETQ PINI (CDR N)))
                 (setq lispt (cons (setq pt (cdr n)) lispt))
                 (if (> xmin (car pt)) (setq xmin (car pt)))
                 (if (> ymin (cadr pt)) (setq ymin (cadr pt)))
                 (if (< xmax (car pt)) (setq xmax (car pt)))
                 (if (< ymax (cadr pt)) (setq ymax (cadr pt)))
             )
        )
   )
   (IF (/= '128 (CDR (ASSOC '70 (ENTGET ENT))))
       (SETQ LISPT (CONS PINI LISPT))
   )
   (SETQ NRQ 0)
   (FOREACH N (CDR LISPT)
      (command "_dim" "_RES" "CABINA" "_ALI" "_non" N "_non" (NTH NRQ LISPT) 
               "_non" (POLAR N (- (ANGLE N (NTH NRQ LISPT)) (/ PI 2)) DISTANZA_QUOTE_INT) "" "_EXIT")
      (SETQ NRQ (+ 1 NRQ))
   )
 )
  (setq grpan (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (setq nr -1)
  (while (setq ent (ssname grpan (setq nr (+ 1 nr))))
    (setq og ent)
    (setq lispt nil) (FOREACH N (ENTGET og) (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))))
    (setq lispt (reverse lispt))

    (if (= "pannelli-dritti" (cdr (assoc '8 (entget ent))))
      (setq p1 (car lispt) p2 (nth 1 lispt) p3 nil)
      (setq p1 (car lispt) p2 (nth 1 lispt)  p3 (nth 2 lispt))
      )
    (command "_dim1" "_RES" "pannelli_CABINA")
    ;(if (or (= (angle p1 p2) 10) (= (angle p1 p2)(/ pi 3)))(command "_dim1" "_RES" "pannelli_CABINA""_dim1"  "_ALI" "_non" p1 "_non" p2 "_non" (polar p1 (+ (/ pi 2) (angle p1 p2)) 250) "")
                            (command "_dim1" "_RES" "pannelli_CABINA""_dim1"  "_ALI" "_non" p2 "_non" p1 "_non" (polar p2 (+ (/ pi 2) (angle p2 p1)) 40) "")
;;;    )
    (if p3 ;(if (or (= (angle p2 p3) 10) (= (angle p2 p3)(/ pi 32)))
              (command "_dim1" "_RES" "pannelli_CABINA" "_dim1"  "_ALI" "_non" p2 "_non" p3 "_non" (polar p2 (+ (/ pi 2) (angle p3 p2)) 40) "")
;;;      (command "_dim1" "_RES" "pannelli_CABINA" "_dim1"  "_ALI" "_non" p3 "_non" p2 "_non" (polar p3 (+ (/ pi 2) (angle p3 p2)) 10) ""))
      )
    )
;;;   (command"_undo" "_end")
   (stop)
)
(defun c:quotacab1 ()
   (START)
   (setq distanza_quote_int 500)
   (setq grdl (ssget "x" (list '(0 . "DIMENSION") '(8 . "QUOTECABINA"))))
   (IF GRDL (COMMAND"_ERASE" GRDL ""))

   (command "_ucs" "_W"  "_layer" "_m" "quoteCABINA" "") 
   (setq gr (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "TRACCIA"))))
;   (if (/= (sslength gr) 1) (progn (alert "Non c'e una sola Polilinea di contorno !!") (exit)))
 (SETQ NRQQ -1)
 (WHILE (SETQ ENT (SSNAME GR (SETQ NRQQ (+ 1 NRQQ))))
;   (setq ent (ssname gr 0))
   (setq lispt nil)
   (setq  xmin 1000000 ymin 1000000 xmax -1000000 ymax -1000000 lspt nil)
   (SETQ PINI NIL)
   (foreach n (entget ent) 
        (if (= '10 (car n)) 
            (progn
                 (IF (NULL PINI) (SETQ PINI (CDR N)))
                 (setq lispt (cons (setq pt (cdr n)) lispt))
                 (if (> xmin (car pt)) (setq xmin (car pt)))
                 (if (> ymin (cadr pt)) (setq ymin (cadr pt)))
                 (if (< xmax (car pt)) (setq xmax (car pt)))
                 (if (< ymax (cadr pt)) (setq ymax (cadr pt)))
             )
        )
   )
   (IF (/= '128 (CDR (ASSOC '70 (ENTGET ENT))))
       (SETQ LISPT (CONS PINI LISPT))
   )
   (SETQ NRQ 0)
   (FOREACH N (CDR LISPT)
      (command "_dim" "_RES" "CABINA" "_ALI" "_non" N "_non" (NTH NRQ LISPT) 
               "_non" (POLAR N (- (ANGLE N (NTH NRQ LISPT)) (/ PI 2)) DISTANZA_QUOTE_INT) "" "_EXIT")
      (SETQ NRQ (+ 1 NRQ))
   )
 )
  (setq grpan (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (setq nr -1)
  (while (setq ent (ssname grpan (setq nr (+ 1 nr))))
    (setq og ent)
    (setq lispt nil) (FOREACH N (ENTGET og) (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))))
    (setq lispt (reverse lispt))

    (if (= "pannelli-dritti" (cdr (assoc '8 (entget ent))))
      (setq p1 (car lispt) p2 (nth 1 lispt) p3 nil)
      (setq p1 (car lispt) p2 (nth 1 lispt)  p3 (nth 2 lispt))
      )
    (command "_dim1" "_RES" "pannelli_CABINA")
    ;(if (or (= (angle p1 p2) 10) (= (angle p1 p2)(/ pi 3)))(command "_dim1" "_RES" "pannelli_CABINA""_dim1"  "_ALI" "_non" p1 "_non" p2 "_non" (polar p1 (+ (/ pi 2) (angle p1 p2)) 250) "")
                            (command "_dim1" "_RES" "pannelli_CABINA""_dim1"  "_ALI" "_non" p2 "_non" p1 "_non" (polar p2 (+ (/ pi 2) (angle p2 p1)) 250) "")
;;;    )
    (if p3 ;(if (or (= (angle p2 p3) 10) (= (angle p2 p3)(/ pi 32)))
              (command "_dim1" "_RES" "pannelli_CABINA" "_dim1"  "_ALI" "_non" p2 "_non" p3 "_non" (polar p2 (+ (/ pi 2) (angle p3 p2)) 250) "")
;;;      (command "_dim1" "_RES" "pannelli_CABINA" "_dim1"  "_ALI" "_non" p3 "_non" p2 "_non" (polar p3 (+ (/ pi 2) (angle p3 p2)) 10) ""))
      )
    )
;;;   (command"_undo" "_end")
   (stop)
)
(defun distbase_ca (testa assieme pt scala)
  (start)
;;;  (command"_undo" "_g")
  (setq listad nil)
  (setq pos 0)
  (setq rigedist nil)
  (setq lispan (carica "pannellil" modalita))
  (if (null rigedist)
    (progn
  (setq rigedist (carica "anag" ""))
    (setq file (open (findfile (strcat modalita "pannelli" ".txt")) "r"))
  (while (setq r (read-line file))
    (setq r (read (strcat "(" r ")")))
    (setq rigedist (cons (list (nth 0 r) (nth 4 r) (nth 2 r) (nth 1 r)) rigedist))
  )
  (close file)
;;;;-----  
;;;;---
  (setq file (open (findfile (strcat modalita "cabine" ".txt")) "r"))
  (while (setq r (read-line file))
    (setq r (read (strcat "(" r ")")))
    (setq rigedist (cons (list (nth 0 r) (nth 2 r) (nth 1 r)) rigedist))
  )
  (close file)
    (setq distpas dibpas)
  (if (null cabpas) (setq cabpas (setq distpas (carica_f "cabpan" modalita))))
  (foreach n cabpas
   (if (and (/= "" (cadr n)) (= assieme (car n)))
    (if (null (assoc (cadr n) rigedist))
      (progn (if (= "$" (substr (cadr n) (strlen (cadr n)) 1))
	       (if (null (assoc (substr (cadr n) 1 (- (strlen (cadr n))1)) rigedist))
		 (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
		 (setq listad
                   (cons
                     (list (substr (cadr n) 1 (- (strlen (cadr n))1)) (nth 2 (assoc (substr (cadr n) 1 (- (strlen (cadr n))1)) rigedist)) (nth 1 (assoc (substr (cadr n) 1 (- (strlen (cadr n))1)) rigedist)) (rtos (nth 2 n) 2 2))
                     listad
                   )
                  )
		 )
        (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
	)
	)
      (setq listad
         (cons
            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (nth 2 n) 2 2))
            listad
         )
      )
     )
   )
  )
;;;;-----  
;;;;---
    )
  )      
;;;
;;;;-----  
  (setvar"expert" 5)
  
  (if (= testa "si")
    (progn
      (COMMAND"_INSERT" "testatab-c" "_non" pt scala scala "0" assieme (nth 2 (assoc assieme rigedist)))
      (command"_-group" "_c" (substr assieme 2 2) "" (entlast) "")
    )
    (command"_-group" "_c" (substr assieme 2 2) "" "")
  )
  (setvar"expert" 1)
  (setq listam listad)
  (setq listad nil)
  (foreach n listam
    (if (assoc (car n) listad)
      (setq listad
	     (subst (list (nth 0 n) (nth 1 n) (nth 2 n) (rtos (+ (atof (nth 3 n)) (atof (nth 3 (assoc (car n) listad)))) 2 2))
		    (assoc (car n) listad)
		    listad
	     )
      )
      (setq listad (cons n listad))
    )
  )
  (setq listak listad listad nil)
  (setq lispas (carica "pannelli" modalita))
  (foreach n listak
    (if (and (setq a (assoc (nth 0 n) lispas)) (setq k (assoc (nth 1 a) lispan)))
      (progn
	(setq modo (substr (nth 1 n) 18 3))
	(setq txt (substr (nth 1 n) 22))
	(if (= "Angolo" (nth 1 k))
	  (setq listad (cons (list (nth 1 n) (nth 0 n) (nth 10 a) txt (strcat (rtos (nth 2 k) 2 0) "+" (rtos (nth 3 k) 2 0)) (rtos (nth 8 a) 2 0) (nth 6 a) (nth 7 a) (nth 3 a) "" (nth 2 n) (nth 3 n)) listad))
	      (setq listad (cons (list (nth 1 n) (nth 0 n) (nth 10 a) txt (rtos (nth 2 k) 2 0) (rtos (nth 8 a) 2 0) (nth 6 a) (nth 7 a) (nth 3 a) "" (nth 2 n) (nth 3 n)) listad))
)
	)
      (setq listad (cons (list (nth 1 n) (nth 0 n) "" "" "" "" "" "" "" "" (nth 2 n) (nth 3 n)) listad))
      )
      )
    (setq listad (reverse listad))
  (foreach n listad
      (COMMAND"_INSERT" "rigatab-c" "_non" pt scala scala "0")
      (cond ((= "MQ" (strcase (nth 10 n)))
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (nth 3 n) (nth 4 n) (nth 5 n) (nth 6 n) (nth 7 n) (nth 8 n) (nth 10 n) (rtos (/ (atof (nth 11 n)) 1000000)2 2))
	    )
	    ((= "ML" (strcase (nth 10 n)))
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (nth 3 n) (nth 4 n) (nth 5 n) (nth 6 n) (nth 7 n) (nth 8 n) (nth 10 n) (rtos (/ (atof (nth 11 n)) 1000)2 2))
	    )
	    ((= "KG" (strcase (nth 10 n)))
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (nth 3 n) (nth 4 n) (nth 5 n) (nth 6 n) (nth 7 n) (nth 8 n) (nth 10 n) (rtos (/ (atof (nth 11 n)) 1000)2 2))
	    )
	    ("t"
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (nth 3 n) (nth 4 n) (nth 5 n) (nth 6 n) (nth 7 n) (nth 8 n) (nth 10 n) (rtos (/ (atof (nth 11 n)) 1)2 0))
	    )
      )
      (if (= testa "si") (command"_-group" "_add" (substr assieme 2 2) (entlast) ""))
    (setq pt (list (car pt) (- (cadr pt) (* scala 100) 0)))
  )
  (setq gr (ssget "x" (list '(0 . "INSERT") '(2 . "POSPAN"))))
  (if gr (command"_erase" gr ""))

  (initget "Si No")
  
  (setq proseguo (Getkword "Proseguo con l'indicazione delle posizioni in pianta? [Si/No] <Si>"))
  (if (/= proseguo "No") (pospan listad) )
;;;  (command"_undo" "_e")
  (setq $pt$ pt)
)

(defun distbase_ca (testa assieme pt scala)
;;;  (start)
;;;  (command"_undo" "_g")
  (setq listad nil)
  (setq pos 0)
  (setq rigedist nil)
  (setq lispan (carica "pannellil" modalita))
  (if (null rigedist)
    (progn
  (setq rigedist (carica "anag" ""))
    (setq file (open (findfile (strcat modalita "pannelli" ".txt")) "r"))
  (while (setq r (read-line file))
    (setq r (read (strcat "(" r ")")))
    (setq rigedist (cons (list (nth 0 r) (nth 4 r) (nth 2 r) (nth 1 r)) rigedist))
  )
  (close file)
;;;;-----
    (setq file (open (findfile (strcat modalita "profili" ".txt")) "r"))
  (while (setq r (read-line file))
    (setq r (read (strcat "(" r ")")))
    (setq rigedist (cons (list (nth 0 r) "Nr" (nth 1 r) (nth 2 r)) rigedist))
  )
  (close file)
;;;;-----    
;;;;---
  (setq file (open (findfile (strcat modalita "cabine" ".txt")) "r"))
  (while (setq r (read-line file))
    (setq r (read (strcat "(" r ")")))
    (setq rigedist (cons (list (nth 0 r) (nth 2 r) (nth 1 r)) rigedist))
  )
  (close file)
    (setq distpas dibpas)
  (if (null cabpas) (setq cabpas (setq distpas (carica_f "cabpan" modalita))))
  (foreach n cabpas
   (if (and (/= "" (cadr n)) (= assieme (car n)))
    (if (null (assoc (cadr n) rigedist))
      (progn (if (= "$" (substr (cadr n) (strlen (cadr n)) 1))
	       (if (null (assoc (substr (cadr n) 1 (- (strlen (cadr n))1)) rigedist))
		 (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
		 (setq listad
                   (cons
                     (list (substr (cadr n) 1 (- (strlen (cadr n))1)) (nth 2 (assoc (substr (cadr n) 1 (- (strlen (cadr n))1)) rigedist)) (nth 1 (assoc (substr (cadr n) 1 (- (strlen (cadr n))1)) rigedist)) (rtos (nth 2 n) 2 2))
                     listad
                   )
                  )
		 )
        (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
	)
	)
      (setq listad
         (cons
            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (nth 2 n) 2 2))
            listad
         )
      )
     )
   )
  )
;;;;-----  
;;;;---
    )
  )      
;;;
;;;;-----  
  (setvar"expert" 5)
  
  (if (= testa "si")
    (progn
      (COMMAND"_INSERT" "testatab-c" "_non" pt scala scala "0" assieme (nth 2 (assoc assieme rigedist)))
      (command"_-group" "_c" (substr assieme 2 2) "" (entlast) "")
    )
    (command"_-group" "_c" (substr assieme 2 2) "" "")
  )
  (setvar"expert" 1)
  (setq listam listad)
  (setq listad nil)
  (foreach n listam
    (if (assoc (car n) listad)
      (setq listad
	     (subst (list (nth 0 n) (nth 1 n) (nth 2 n) (rtos (+ (atof (nth 3 n)) (atof (nth 3 (assoc (car n) listad)))) 2 2))
		    (assoc (car n) listad)
		    listad
	     )
      )
      (setq listad (cons n listad))
    )
  )
  (setq listak listad listad nil)
  (setq lispas (carica "pannelli" modalita))
  (foreach n listak
    (if (and (setq a (assoc (nth 0 n) lispas)) (setq k (assoc (nth 1 a) lispan)))
      (progn
	(if (vl-string-search "Bifacciale" (nth 1 n))
	  (progn
	    (setq modo (substr (nth 1 n) 24 3))
	    (setq txt (substr (nth 1 n) 28))
	    )
	  (progn
	    (setq modo (substr (nth 1 n) 26 3))
	    (setq txt (substr (nth 1 n) 30))
	    )
	  )
	(if (= "Angolo" (nth 1 k))
	  (setq listad (cons (list (nth 1 n) (nth 0 n) (nth 10 a) txt (strcat (rtos (nth 2 k) 2 0) "+" (rtos (nth 3 k) 2 0)) (rtos (nth 8 a) 2 0) (nth 6 a) (nth 7 a) (nth 3 a) "" (nth 2 n) (nth 3 n)) listad))
	      (setq listad (cons (list (nth 1 n) (nth 0 n) (nth 10 a) txt (rtos (nth 2 k) 2 0) (rtos (nth 8 a) 2 0) (nth 6 a) (nth 7 a) (nth 3 a) "" (nth 2 n) (nth 3 n)) listad))
)
	)
      (setq listad (cons (list (nth 1 n) (nth 0 n) "" "" "" "" "" "" "" "" (nth 2 n) (nth 3 n)) listad))
      )
      )
    (setq listad (reverse listad))
  (setq listad1 nil)
  (foreach n cabpas
        (if (and (= assieme (car n)) (nth 4 n))
	  (progn
	    (setq listad1 listad)
	    (setq listad nil)
	  (foreach b listad1
	    (if (and (= (nth 1 n) (nth 1 b)))
	      (setq listad (cons (list (strcat "Assieme " (nth 1 b)) (nth 1 b)
				       (nth 4 n) (nth 5 n) (nth 6 n) (nth 7 n) (nth 8 n) (nth 9 n) (nth 10 n) "?"
				       (nth 10 b)
				       (nth 11 b))
				 listad)
		    )
	      
	      (setq listad (cons b listad))
	      )
	    )
	  )
	  )
    )
  ;profili
  (setq listak listad listad nil)
  (setq profili (carica "profili" modalita))
  (foreach n listak
    (if (and (setq a (assoc (nth 1 n) profili)) )
      (progn
	  (setq listad (cons (list (nth 0 n) (nth 1 n)  "" "" "" (rtos (nth 4 a) 2 0) ""  (nth 3 a)"" "" (nth 10 n) (nth 11 n)) listad))
	)
      (setq listad (cons n listad))
      )
      )
    (setq listad (reverse listad))
  
  (foreach n listad
      (COMMAND"_INSERT" "rigatab-c" "_non" pt scala scala "0")
      (cond ((= "MQ" (strcase (nth 10 n)))
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (nth 3 n) (nth 4 n) (nth 5 n) (nth 6 n) (nth 7 n) (nth 8 n) (nth 10 n) (rtos (/ (atof (nth 11 n)) 1000000)2 2))
	    )
	    ((= "ML" (strcase (nth 10 n)))
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (nth 3 n) (nth 4 n) (nth 5 n) (nth 6 n) (nth 7 n) (nth 8 n) (nth 10 n) (rtos (/ (atof (nth 11 n)) 1000)2 2))
	    )
	    ((= "KG" (strcase (nth 10 n)))
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (nth 3 n) (nth 4 n) (nth 5 n) (nth 6 n) (nth 7 n) (nth 8 n) (nth 10 n) (rtos (/ (atof (nth 11 n)) 1000)2 2))
	    )
	    ("t"
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (nth 3 n) (nth 4 n) (nth 5 n) (nth 6 n) (nth 7 n) (nth 8 n) (nth 10 n) (rtos (/ (atof (nth 11 n)) 1)2 0))
	    )
      )
      (if (= testa "si") (command"_-group" "_add" (substr assieme 2 2) (entlast) ""))
    (setq pt (list (car pt) (- (cadr pt) (* scala 100) 0)))
  )
  (setq gr (ssget "x" (list '(0 . "INSERT") '(2 . "POSPAN"))))
  (if gr (command"_erase" gr ""))

  (initget "Si No")
  
  (setq proseguo (Getkword "Proseguo con l'indicazione delle posizioni in pianta? [Si/No] <Si>"))
  (if (/= proseguo "No") (pospan listad) )
;;;  (command"_undo" "_e")
  (setq $pt$ pt)
)
(defun pospan (listad)
  (setq gr (ssget "x" (list '(0 . "INSERT") '(2 . "POSPAN"))))
  (if gr (command"_erase" gr ""))
  (setq pos 0)
  (foreach n listad
    (setq pos (+ 1 pos))
    (if (= "PB-" (substr (nth 1 n) 1 3))
      (progn
	(setq nomeat (nth 1 n))
	(setq gr (ssget "x" (list '(0 . "INSERT") '(8 . "siglepan") '(2 . "codpas"))))
	(setq nr -1)
	(while (setq ent (ssname gr (setq nr (+ 1 nr))))
	   (if (= nomeat (cdr (assoc '1 (entget (entnext ent)))))
	     (command "_insert" "pospan" "_non" (cdr (assoc '10 (entget ent))) "1" "1" (angtos (cdr (assoc '50 (entget ent)))) (rtos pos 2 0))
	     )
	  )
	)
      )
    )
  (stop)
  )
(defun c:distbase ()
  (start)
  (setq assieme nil)
  (setq modo_dist nil)
  (while (null assieme)
    (setq cabin nil)
   (setq ents (nentsel "\nSelezionare codice o invio per nome DWG:"))
   (if ents
      (if (= "ATTRIB" (CDR (ASSOC '0 (ENTGET (CAR ENTS)))))
          (setq assieme (cdr (assoc '1 (entget (car ents)))))
          (progn (ALERT "Selezione non valida"))
      )
      (progn
          (setq cabin (getvar "dwgname"))
          (setq assieme (substr cabin 1 (- (strlen cabin) 4)))
	  (SETQ ASSIEME (SUBSTR ASSIEME 1 10))
      )
   )
  )
  (setq scala 1)
  (setq scala (cadr (assoc 'scaladistinta parametri)))
  (SETQ PT (GETPOINT"\nPunto di inserimento:"))
  (if cabin
    (progn 
  (setq gr (ssget "x"  (list '(8 . "traccia"))))
  (if gr (setq modo_dist "Cab"))
  )
    )
  (setq nbase (substr assieme 1 3))
  (cond ((= modo_dist "Cab") (distbase_ca "si" assieme pt scala))
	((= nbase "PM-") (distbase_pa "si" assieme pt scala))
	((= nbase "PN-") (distbase_pa "si" assieme pt scala))
	((= nbase "LM-") (distbase_pn "si" assieme pt scala))
	((= nbase "SV-") (distbase_sv "si" assieme pt scala))
	((= nbase "C") (distbase_ca "si" assieme pt scala))
	((= nbase "P") (distbase_ca "si" assieme pt scala))
;;;	((= nbase "CR") (distbase_ca "si" assieme pt scala))
;;;	((= nbase "MK") (distbase_ca "si" assieme pt scala))
	(t (distbase "si" assieme pt scala))
	)
  (stop)
)
(defun distbase_pn (testa assieme pt scala)
  (start)
;;;  (command"_undo" "_g")
  (setq listad nil)
  (setq pos 0)
  (setq rigedist nil)
  (if (null rigedist)
    (progn
  (setq rigedist (carica "anag" ""))
;---
;;;  (setq file (open (findfile (strcat modalita "corridoi" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 2 r) (nth 3 r)) rigedist))
;;;  )
;;;  (close file)
;;;;---
;;;  (setq file (open (findfile (strcat modalita "carrelli" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 1 r) (nth 2 r)) rigedist))
;;;  )
;;;  (close file)
;;;;---
;;;  (if (findfile (strcat modalita "$$pannelli" ".txt"))
;;;    (setq file (open (findfile (strcat modalita "$$pannelli" ".txt")) "r"))
;;;    (setq file (open (findfile (strcat modalita "pannelli" ".txt")) "r"))
;;;    )
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 4 r) (nth 2 r)) rigedist))
;;;  )
;;;  (close file)
;;;;-----  
;;;;---
  ;(setq file (open (findfile (strcat modalita "pannellil" ".txt")) "r"))
  (if (findfile (strcat modalita "$$pannellil" ".txt"))
    (setq file (open (findfile (strcat modalita "$$pannellil" ".txt")) "r"))
    (setq file (open (findfile (strcat modalita "pannellil" ".txt")) "r"))
    )
  (while (setq r (read-line file))
    (setq r (read (strcat "(" r ")")))
    (setq rigedist (cons (list (nth 0 r) (nth 12 r) (nth 1 r)) rigedist))
  )
  (close file)
;;;;-----  
;;;;---
;;;  (if (findfile (strcat modalita "$$kit" ".txt"))
;;;    (setq file (open (findfile (strcat modalita "$$kit" ".txt")) "r"))
;;;    (setq file (open (findfile (strcat modalita "kit" ".txt")) "r"))
;;;    )
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 5 r) (nth 1 r)) rigedist))
;;;  )
;;;  (close file)
;;;;-----  
;;;;---
;;;  (setq file (open (findfile (strcat modalita "cabine" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 2 r) (nth 1 r)) rigedist))
;;;  )
;;;  (close file)
;;;;-----  
;;;;---
;;;;;;  (setq file (open (findfile (strcat modalita "pannellis" ".txt")) "r"))
;;;;;;  (while (setq r (read-line file))
;;;;;;    (setq r (read (strcat "(" r ")")))
;;;;;;    (setq rigedist (cons (list (nth 0 r) (nth 6 r) (nth 1 r)) rigedist))
;;;;;;  )
;;;;;;  (close file)
;;;;-----  
;;;;---
;;;  (setq file (open (findfile (strcat modalita "sviluppi" ".txt")) "r"))
  (if (findfile (strcat modalita "$$sviluppi" ".txt"))
    (setq file (open (findfile (strcat modalita "$$sviluppi" ".txt")) "r"))
    (setq file (open (findfile (strcat modalita "sviluppi" ".txt")) "r"))
    )
  (while (setq r (read-line file))
    (setq r (read (strcat "(" r ")")))
    (setq rigedist (cons (list (nth 0 r) (nth 10 r) (nth 1 r)) rigedist))
  )
  (close file)
;;;;-----  
;;;;---
;;;  (setq file (open (findfile (strcat modalita "cesoiati" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 7 r) (nth 1 r)) rigedist))
;;;  )
;;;  (close file)
;;;;-----  
;;;;---
;;;  (setq file (open (findfile (strcat modalita "mater" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 7 r) (nth 1 r)) rigedist))
;;;  )
;;;  (close file)
;;;;-----  
;;;;---
;;;  (setq file (open (findfile (strcat modalita "tabrinf" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 4 r) (nth 1 r)) rigedist))
;;;  )
;;;  (close file)
;;;;-----  
;;;;---
;;;  (setq file (open (findfile (strcat modalita "travinf" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 4 r) (nth 1 r)) rigedist))
;;;  )
;;;  (close file)
;;;;-----  
;;;;---
;;;  (setq file (open (findfile (strcat modalita "travsup" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 4 r) (nth 1 r)) rigedist))
;;;  )
;;;  (close file)
;;;;-----  
;;;;---
;;;  (setq file (open (findfile (strcat modalita "rinfpiede" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 4 r) (nth 1 r)) rigedist))
;;;  )
;;;  (close file)
;;;;-----  
;;;;---
;;;  (setq file (open (findfile (strcat modalita "lanacer" ".txt")) "r"))
;;;  (while (setq r (read-line file))
;;;    (setq r (read (strcat "(" r ")")))
;;;    (setq rigedist (cons (list (nth 0 r) (nth 7 r) (nth 1 r)) rigedist))
;;;  )
;;;  (close file)
;;;    )
;;;  )      
;;;
;;;;-----  
;;;  (if (null kitpan) (setq kitpan (setq distkit (carica_f "kitpan" modalita))) (setq distkit kitpan))
;;;  (foreach n distkit
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (nth 3 n) 2 0))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
;;;;-----  
;;;  (if (null distsvi) (setq distsvi (carica_f "dibsvi" modalita)))
;;;  (foreach n distsvi
;;;   (if (and (/= "000000000" (cadr n)) (= assieme (car n)))
;;;    
;;;;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 4 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
;;;;-----  
;;;  (if (null distces) (setq distces (carica_f "dibces" modalita)))
;;;  (foreach n distces
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 5 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
;;;;-----  
;;;  (if (null distrin) (setq distrin (carica "dibrin" modalita)))
;;;  (foreach n distrin
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 2 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
;;;;-----  
;;;  (if (null distrin) (setq distrin (carica "dibrin" modalita)))
;;;  (foreach n distrin
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 2 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
;;;;-----  
;;;  (if (null distrinp) (setq distrinp (carica "dibrinp" modalita)))
;;;  (foreach n distrinp
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 2 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
;;;  ;-----  
;;;  (if (null distcor) (setq distcor (carica "dibcor" modalita)))
;;;  (foreach n distcor
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 2 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
;;;  ;-----  
;;;  (if (null distcar) (setq distcar (carica "dibcar" modalita)))
;;;  (foreach n distcar
;;;    (if (and (/= "" (caddr n)) (= assieme (car n)))
;;;     (if (null (assoc (caddr n) rigedist)) (alert (strcat "Codice " (caddr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (caddr n) (nth 1 (assoc (caddr n) rigedist)) (nth 2 (assoc (caddr n) rigedist)) (rtos (/ (nth 3 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
;;;  ;-----  
;;;  (if (null disttrs) (setq disttrs (carica "dibtrs" modalita)))
;;;  (foreach n disttrs
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 2 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
;;;  ;-----  
;;;  (if (null disttri) (setq disttri (carica "dibtri" modalita)))
;;;  (foreach n disttri
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 2 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
;;;
;;;;-----  9
;;;  (if (null distsvi) (setq distsvi (carica "dibldr" modalita)))
;;;  (foreach n distsvi
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 5 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
;;;;-----  
;;;  (if (null distlcr) (setq distlcr (carica "diblcr" modalita)))
;;;  (foreach n distlcr
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;     (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;       (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (/ (nth 5 n) 1) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;    )
;;;  )
;;;;-----  
  (if (null distpan) (setq distpan (carica_f "dibpan" modalita)))
  (foreach n distpan
   (if (and (/= "000000000" (cadr n)) (= assieme (car n)))
    (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
      (setq listad
         (cons
            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (nth 7 n) 2 0))
            listad
         )
      )
     )
   )
  )
;;;  (if (null distpas) (setq dibpas (setq distpas (carica_f "dibpas" modalita))))
;;;  (foreach n distpas
;;;   (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;    (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;      (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (nth 2 n) 2 2))
;;;            listad
;;;         )
;;;      )
;;;     )
;;;   )
;;;  )
;;;  (if (null distcab) (setq distcab (carica "cabkit" modalita)))
;;;  (foreach n distcab
;;;    (if (and (/= "" (cadr n)) (= assieme (car n)))
;;;      (if (null (assoc (cadr n) rigedist)) (alert (strcat "Codice " (cadr n) " non in anarafica !!!"))
;;;        (setq listad
;;;         (cons
;;;            (list (cadr n) (nth 2 (assoc (cadr n) rigedist)) (nth 1 (assoc (cadr n) rigedist)) (rtos (nth 2 n) 2 0))
;;;            listad
;;;         )
;;;        )
;;;      )
    )
  )
  (setvar"expert" 5)
  
  (if (= testa "si")
    (progn
      (COMMAND"_INSERT" "testatab" "_non" pt scala scala "0" assieme (nth 2 (assoc assieme rigedist)))
      (command"_-group" "_c" (substr assieme 2 2) "" (entlast) "")
    )
    (command"_-group" "_c" (substr assieme 2 2) "" "")
  )
  (setvar"expert" 1)
  (setq listam listad)
  (setq listad nil)
  (foreach n listam
    (if (assoc (car n) listad)
      (setq listad
	     (subst (list (nth 0 n) (nth 1 n) (nth 2 n) (rtos (+ (atof (nth 3 n)) (atof (nth 3 (assoc (car n) listad)))) 2 2))
		    (assoc (car n) listad)
		    listad
	     )
      )
      (setq listad (cons n listad))
    )
  )
  (foreach n listad
      (COMMAND"_INSERT" "rigatab" "_non" pt scala scala "0")
      (cond ((= "MQ" (strcase (nth 2 n)))
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (rtos (/ (atof (nth 3 n)) 1000000)2 2))
	    )
	    ((= "ML" (strcase (nth 2 n)))
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (rtos (/ (atof (nth 3 n)) 1000)2 2))
	    )
	    ((= "KG" (strcase (nth 2 n)))
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (rtos (/ (atof (nth 3 n)) 1000)2 2))
	    )
	    ("t"
             (command (rtos (setq pos (+ 1 pos)) 2 0) (nth 0 n) (nth 1 n) (nth 2 n) (rtos (/ (atof (nth 3 n)) 1)2 0))
	    )
      )
      (if (= testa "si") (command"_-group" "_add" (substr assieme 2 2) (entlast) ""))
    (setq pt (list (car pt) (- (cadr pt) (* scala 100) 0)))
  )
;;;  (command"_undo" "_e")
  (setq $pt$ pt)
  (stop)
)


(defun c:specchiacab ()
  (start)
;;;  (command"_undo" "_G")
  (setq modop_d nil)
  (command"_layer" "_ON" "traccia" "")
  (setq punto1 (getpoint "\nPunto Uno"))
  (setq punto2 (getpoint punto1 "\nPunto Due"))
  (setq oggetti (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "traccia"))))
  (setq nrs -1)
  (setq og_ nil)
  (while (and oggetti (setq og_ (ssname oggetti (setq nrs (+ 1 nrs)))))
    (command"_mirror" og_ "" "_non" punto1 "_non" punto2 "_n")
    (setq og (entlast))
    (setq lispt nil) (FOREACH N (ENTGET og) (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))))
    (if (or (= 128 (cdr (assoc '70 (entget og)))) (= 0 (cdr (assoc '70 (entget og))))) (setq ch nil) (setq ch t))
    (command"_layer" "_m" "traccia" "")
    (command"_pline")
    (foreach pt lispt
      (command"_non" pt)
      )
    (if ch (command"_close"))
    (entdel og)
  )
  (if oggetti (command"_erase" oggetti ""))
  (setq oggetti (ssget "x" (list '(0 . "INSERT") '(2 . "Porta*"))))
  (setq nrs -1)
  (while (and oggetti (setq og_ (ssname oggetti (setq nrs (+ 1 nrs)))))
    (command"_mirror" og_ "" "_non" punto1 "_non" punto2 "_n")
    (setq og (entlast))
    (setq pt (cdr (assoc '10 (entget og))))
    (setq ang (cdr (assoc '50 (entget og))))
    (command"_mirror" og "" "_non" pt  "_non" (polar pt (+ (/ pi 2) ang) 300) "_y")
  )
  (if oggetti (command"_erase" oggetti ""))

  (setq oggetti (ssget "x" (list '(0 . "INSERT") '(2 . "DIVIDI*"))))
  (setq nrs -1)
  (while (and oggetti (setq og_ (ssname oggetti (setq nrs (+ 1 nrs)))))
    (command"_mirror" og_ "" "_non" punto1 "_non" punto2 "_n")
    (setq og (entlast))
    (setq pt (cdr (assoc '10 (entget og))))
    (setq ang (cdr (assoc '50 (entget og))))
    (command"_mirror" og "" "_non" pt  "_non" (polar pt (+ (/ pi 2) ang) 300) "_y")
    (setq ent1 (entlast))
    (COMMAND"_ROTATE" ENT1 "" "_non" pt "180")
    (COND ((= "DIVIDISX" (cdr (assoc '2 (entget og))))
      (progn
	(setq ed (entget ent1))
	(setq ed (subst (cons 2 "DIVIDIDX") (assoc 2 ed) ed ))
	(entmod ed)
	)
      )
    ((= "DIVIDIDX" (cdr (assoc '2 (entget og))))
      (progn
	(setq ed (entget ent1))
	(setq ed (subst (cons 2 "DIVIDISX") (assoc 2 ed) ed ))
	(entmod ed)
	)
      )
	  )
  )

  (if oggetti (command"_erase" oggetti ""))

    (setq oggetti (ssget "x" (list '(0 . "INSERT") '(2 . "FORO*"))))
  (setq nrs -1)
  (while (and oggetti (setq og_ (ssname oggetti (setq nrs (+ 1 nrs)))))
    (command"_mirror" og_ "" "_non" punto1 "_non" punto2 "_n")
    (setq og (entlast))
    (setq pt (cdr (assoc '10 (entget og))))
    (setq ang (cdr (assoc '50 (entget og))))
    (command"_mirror" og "" "_non" pt  "_non" (polar pt (+ (/ pi 2) ang) 300) "_y")
    (setq ent1 (entlast))
;;;    (COMMAND"_ROTATE" ENT1 "" "_non" pt "180")
    
  )
  (if oggetti (command"_erase" oggetti ""))

  (setq oggetti (ssget "x" (list '(0 . "MLINE") '(8 . "rinforzi") '(2 . "RINFORZO-*"))))
  (setq nrs -1)
  (while (and oggetti (setq og_ (ssname oggetti (setq nrs (+ 1 nrs)))))
    (setq lispt nil) (FOREACH N (ENTGET og_) (IF (= '11 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))))

    (command"_mirror" og_ "" "_non" punto1 "_non" punto2 "_n")
    (setq ogX (entlast))
    (setq lispt nil) (FOREACH N (ENTGET ogX) (IF (= '11 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))))
    (setq pm (polar (nth 1 lispt) (angle (nth 1 lispt) (nth 0 lispt))(/ (distance (nth 0 lispt) (nth 1 lispt)) 2) ))
    (command "_mirror" ogx "" "_non" pm "_non" (polar pm  (- (angle (nth 1 lispt) (nth 0 lispt)) (/ pi 2)) 100) "_y")
    
  )
  (if oggetti (command"_erase" oggetti ""))


      (setq oggetti (ssget "x" (list '(0 . "INSERT") '(2 . "Rinforzo-*"))))
  (setq nrs -1)
  (while (and oggetti (setq og_ (ssname oggetti (setq nrs (+ 1 nrs)))))
    (command"_mirror" og_ "" "_non" punto1 "_non" punto2 "_n")
    (setq og (entlast))
    (setq pt (cdr (assoc '10 (entget og))))
    (setq ang (cdr (assoc '50 (entget og))))
    (command"_mirror" og "" "_non" pt  "_non" (polar pt (+ (/ pi 2) ang) 300) "_y")
    (setq ent1 (entlast))
;;;    (COMMAND"_ROTATE" ENT1 "" "_non" pt "180")
    
  )
  (if oggetti (command"_erase" oggetti ""))
A

  (setq oggetti (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (setq nrxk -1)
  (while (and oggetti (setq og_ (ssname oggetti (setq nrxk (+ 1 nrxk)))))
    (command "_mirror" og_ "" "_non" punto1 "_non" punto2 "_n")
    (setq og (entlast))
    (setq lispt nil) (FOREACH N (ENTGET og) (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))))
    (setq lispt (reverse lispt))
    (setq pm1 (nth 0 lispt))
    (setq pm2 (nth 1 lispt))
    (setq pm3 (nth 2 lispt))
    (setq modo (leggi og "PANTIP"))
    (setq fpan (leggi og "PANFIN"))
    (setq hpan (cdr (assoc '39 (entget og))))
    (setq zpan (cdr (assoc '38 (entget og))))
    (if (= "pannelli-dritti" (cdr (assoc '8 (entget og))))
      (cond ((= modo "STD") (SPPAN PM2 PM1 "STD" hpan fpan zpan) (allega entlastpan "PANTIP" "STD"))

	    ((= modo "SFD")  (SPPAN PM2 PM1 "SFS" hpan fpan zpan)(allega entlastpan "PANTIP" "SFS"))
	    ((= modo "SFS")  (SPPAN PM2 PM1 "SFD" hpan fpan zpan)(allega entlastpan "PANTIP" "SFD"))

	    ((= modo "SMD")  (SPPAN PM2 PM1 "SMS" hpan fpan zpan)(allega entlastpan "PANTIP" "SMS"))
	    ((= modo "SMS")  (SPPAN PM2 PM1 "SMD" hpan fpan zpan)(allega entlastpan "PANTIP" "SMD"))

    	    ((= modo "S2D")  (SPPAN PM2 PM1 "S2S" hpan fpan zpan)(allega entlastpan "PANTIP" "S2S"))
	    ((= modo "S2S")  (SPPAN PM2 PM1 "S2D" hpan fpan zpan)(allega entlastpan "PANTIP" "S2D"))

    	    ((= modo "S4D")  (SPPAN PM2 PM1 "S4S" hpan fpan zpan)(allega entlastpan "PANTIP" "S4S"))
	    ((= modo "S4S")  (SPPAN PM2 PM1 "S4D" hpan fpan zpan)(allega entlastpan "PANTIP" "S4D"))

	    ((= modo "S3D") (SPPAN PM2 PM1 "S3D" hpan fpan zpan)(allega entlastpan "PANTIP" "S3D"))
	    
	    ((= modo "MAN")
	      (SETQ VAL (LEGGI OG "SVILMAN"))
	      (SPPAN PM2 PM1 "MAN" hpan fpan zpan)(allega entlastpan "PANTIP" "MAN")
              (SETQ LVAL (READ VAL))
              (SETQ VAL (STRCAT "(" (RTOS (NTH 1 LVAL) 2 1) " " (RTOS (NTH 0 LVAL) 2 1) " " (RTOS (NTH 2 LVAL) 2 1) " " (RTOS (NTH 3 LVAL) 2 1) ")"))
              (ALLEGA ENTLASTPAN "SVILMAN" VAL))

	    ((= modo "MTD") (SPPAN PM2 PM1 "MTD" hpan fpan zpan) (allega entlastpan "PANTIP" "MTD"))

	    ((= modo "MFD")  (SPPAN PM2 PM1 "MFS" hpan fpan zpan)(allega entlastpan "PANTIP" "MFS"))
	    ((= modo "MFS")  (SPPAN PM2 PM1 "MFD" hpan fpan zpan)(allega entlastpan "PANTIP" "MFD"))

	    ((= modo "MMD")  (SPPAN PM2 PM1 "MMS" hpan fpan zpan)(allega entlastpan "PANTIP" "MMS"))
	    ((= modo "MMS")  (SPPAN PM2 PM1 "MMD" hpan fpan zpan)(allega entlastpan "PANTIP" "MMD"))

    	    ((= modo "M2D")  (SPPAN PM2 PM1 "M2S" hpan fpan zpan)(allega entlastpan "PANTIP" "M2S"))
	    ((= modo "M2S")  (SPPAN PM2 PM1 "M2D" hpan fpan zpan)(allega entlastpan "PANTIP" "M2D"))

    	    ((= modo "M4D")  (SPPAN PM2 PM1 "M4S" hpan fpan zpan)(allega entlastpan "PANTIP" "M4S"))
	    ((= modo "M4S")  (SPPAN PM2 PM1 "M4D" hpan fpan zpan)(allega entlastpan "PANTIP" "M4D"))

	    ((= modo "M3D") (SPPAN PM2 PM1 "M3D" hpan fpan zpan)(allega entlastpan "PANTIP" "M3D"))
	    
	    )
    
      (cond ((= modo "STD") (SPANG PM3 PM2 PM1 "STD" hpan fpan zpan)(allega entlastpan "PANTIP" "STD"))
	    ((= modo "SFD")  (SPANG PM3 PM2 PM1 "SFS" hpan fpan zpan)(allega entlastpan "PANTIP" "SFS"))
	    ((= modo "SFS")  (SPANG PM3 PM2 PM1 "SFD" hpan fpan zpan)(allega entlastpan "PANTIP" "SFD"))

	    ((= modo "SMD")  (SPANG PM3 PM2 PM1 "SMS" hpan fpan zpan)(allega entlastpan "PANTIP" "SMS"))
	    ((= modo "SMS")  (SPANG PM3 PM2 PM1 "SMD" hpan fpan zpan)(allega entlastpan "PANTIP" "SMD"))

	    ((= modo "S2D")  (SPANG PM3 PM2 PM1 "S2S" hpan fpan zpan)(allega entlastpan "PANTIP" "S2S"))
	    ((= modo "S2S")  (SPANG PM3 PM2 PM1 "S2D" hpan fpan zpan)(allega entlastpan "PANTIP" "S2D"))

	    ((= modo "S4D")  (SPANG PM3 PM2 PM1 "S4S" hpan fpan zpan)(allega entlastpan "PANTIP" "S4S"))
	    ((= modo "S4S")  (SPANG PM3 PM2 PM1 "S4D" hpan fpan zpan)(allega entlastpan "PANTIP" "S4D"))
	    ((= modo "S3D") (SPANG PM3 PM2 PM1 "S3D" hpan fpan zpan)(allega entlastpan "PANTIP" "S3D"))

	    ((= modo "MAN") (SPANG PM3 PM2 PM1 "MAN" hpan fpan zpan)(allega entlastpan "PANTIP" "MAN")
             (SETQ VAL (LEGGI OG "SVILMAN"))
	                   (SETQ LVAL (READ VAL))
              (SETQ VAL (STRCAT "(" (RTOS (NTH 1 LVAL) 2 1) " " (RTOS (NTH 0 LVAL) 2 1) " " (RTOS (NTH 2 LVAL) 2 1) " " (RTOS (NTH 3 LVAL) 2 1) ")"))
              (ALLEGA ENTLASTPAN "SVILMAN" VAL)

            )
((= modo "STD") (SPANG PM3 PM2 PM1 "STD" hpan fpan zpan)(allega entlastpan "PANTIP" "STD"))
	    ((= modo "MFD")  (SPANG PM3 PM2 PM1 "MFS" hpan fpan zpan)(allega entlastpan "PANTIP" "MFS"))
	    ((= modo "MFS")  (SPANG PM3 PM2 PM1 "MFD" hpan fpan zpan)(allega entlastpan "PANTIP" "MFD"))

	    ((= modo "MMD")  (SPANG PM3 PM2 PM1 "MMS" hpan fpan zpan)(allega entlastpan "PANTIP" "MMS"))
	    ((= modo "MMS")  (SPANG PM3 PM2 PM1 "MMD" hpan fpan zpan)(allega entlastpan "PANTIP" "MMD"))

	    ((= modo "M2D")  (SPANG PM3 PM2 PM1 "M2S" hpan fpan zpan)(allega entlastpan "PANTIP" "M2S"))
	    ((= modo "M2S")  (SPANG PM3 PM2 PM1 "M2D" hpan fpan zpan)(allega entlastpan "PANTIP" "M2D"))

	    ((= modo "M4D")  (SPANG PM3 PM2 PM1 "M4S" hpan fpan zpan)(allega entlastpan "PANTIP" "M4S"))
	    ((= modo "M4S")  (SPANG PM3 PM2 PM1 "M4D" hpan fpan zpan)(allega entlastpan "PANTIP" "M4D"))
	    ((= modo "M3D") (SPANG PM3 PM2 PM1 "M3D" hpan fpan zpan)(allega entlastpan "PANTIP" "M3D"))
	    )
  
      )

;;;    SPPAN (PM1 PM2 MODO hpan fpan zpan)
(aggpann entlastpan "No")
    (entdel og)
  )

    (if oggetti (command"_erase" oggetti ""))
  (c:elimina_sporco)
;;;(command"_undo" "_e")
  (stop)
  )


(DEFUN SPPAN (PM1 PM2 MODO hpan fpan zpan)
  (setq ang_prof (angle pm1 pm2))
  (command"_elev" zpan hpan)
  (setq handinf nil)
  (if (/= 0 zpan)
    (progn
      (setq entbs (car (entsel "\nQuesto pannello è ad una quota diversa da 0.0, \n
      Se si trova sopra ad un'altro pannello selezionarlo adesso:")))
      (setq handinf (cdr (assoc '5 (entget entbs))))
    )
  )
  (cond
    ((= modo "STD") (pannelloN pm1 pm2 27) (allega (entlast) "PANTIP" modo))
    ((= modo "SFD") (pannelloFD pm1 pm2 27)(allega (entlast) "PANTIP" modo))
    ((= modo "SFS") (pannelloFS pm1 pm2 27)(allega (entlast) "PANTIP" modo))
    ((= modo "SMD") (pannelloMD pm1 pm2 27)(allega (entlast) "PANTIP" modo))
    ((= modo "SMS") (pannelloMS pm1 pm2 27) (allega (entlast) "PANTIP" modo))
    ((= modo "S2D") (pannello2D pm1 pm2 27) (allega (entlast) "PANTIP" modo))
    ((= modo "S2S") (pannello2S pm1 pm2 27) (allega (entlast) "PANTIP" modo))

    ((= modo "S3D") (pannello3D pm1 pm2 27)(allega (entlast) "PANTIP" modo))
    ((= modo "S4D") (pannello4D pm1 pm2 27)(allega (entlast) "PANTIP" modo))
    ((= modo "S4S") (pannello4S pm1 pm2 27)(allega (entlast) "PANTIP" modo))

    ((= modo "MTD") (pannelloN pm1 pm2 27) (allega (entlast) "PANTIP" modo))
    ((= modo "MFD") (pannelloFD pm1 pm2 27)(allega (entlast) "PANTIP" modo))
    ((= modo "MFS") (pannelloFS pm1 pm2 27)(allega (entlast) "PANTIP" modo))
    ((= modo "MMD") (pannelloMD pm1 pm2 27)(allega (entlast) "PANTIP" modo))
    ((= modo "MMS") (pannelloMS pm1 pm2 27) (allega (entlast) "PANTIP" modo))
    ((= modo "M2D") (pannello2D pm1 pm2 27) (allega (entlast) "PANTIP" modo))
    ((= modo "M2S") (pannello2S pm1 pm2 27) (allega (entlast) "PANTIP" modo))

    ((= modo "M3D") (pannello3D pm1 pm2 27)(allega (entlast) "PANTIP" modo))
    ((= modo "M4D") (pannello4D pm1 pm2 27)(allega (entlast) "PANTIP" modo))
    ((= modo "M4S") (pannello4S pm1 pm2 27)(allega (entlast) "PANTIP" modo))
  )
;;;  (if (null (assoc '62 (entget (entlast))))
    (command"_chprop" (entlast) "" "_co" "6" "")
;;;  )
  (if handinf (allega (entlast) "PSUP" handinf))
)

(defun SPang (PM1 PM2 PM3 MODO hpan fpan zpan)
  (setq $lpan (cadr (assoc 'lungpannello parametri)))
  (setq $ap2$ nil)
  (setq modop_d modo)
  (command"_elev" zpan hpan)
  (cond
    ((= modo "STD") (angolon pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "SFD") (angoloFD pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "SFS") (angoloFS pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "SMD") (angoloMD pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "SMS") (angoloMS pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "S2D") (angolo2D pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "S2S") (angolo2S pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "S3D") (angolo3D pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "S4D") (angolo4D pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "S4S") (angolo4S pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))

    ((= modo "MTD") (angolon pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "MFD") (angoloFD pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "MFS") (angoloFS pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "MMD") (angoloMD pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "MMS") (angoloMS pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "M2D") (angolo2D pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "M2S") (angolo2S pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "M3D") (angolo3D pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "M4D") (angolo4D pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))
    ((= modo "M4S") (angolo4S pm1 pm2 pm3 27)(allega (entlast) "PANTIP" modo))

 )
;;;  (if (null (assoc '62 (entget (entlast))))
    (command"_chprop" entlastpan "" "_co" "6" "")
;;;  )
  
)

(defun quote0-90 ()
  (setq gr (ssget "x" (list '(0 . "DIMENSION") '(42 . 0))))
  (setq gr (ssget "X" '((0 . "DIMENSION") (-4 . "<=") (42 . 0.5))))
;;;  (-4 . "<AND")
;;;        (0 . "CIRCLE")
;;;        (40 . 1.0)
;;;      (-4 . "AND>")
  (if gr (command "_erase" gr ""))
  (setq gr (ssget "X" '((0 . "DIMENSION") (-4 . "<AND")
			                      (-4 . "<=") (42 . 1.58)
			                      (-4 . ">=") (42 . 1.57)
			                  (-4 . "AND>")
			)))
    (if gr (command "_erase" gr ""))

  
  )
(defun selezpar (cosa testo)
  (if (= 1 (length (cdr (assoc cosa parametri))))
    (setq selpar (cadr (assoc cosa parametri)))
    (progn
      (setq lista "")
      (foreach n (cdr (assoc cosa parametri))
	(setq lista (strcat n "/" lista))
      )
      (setq selpar (getstring (strcat "\Selezionare " testo " [" lista "] <" (cadr (assoc cosa parametri))"> :")))
      (if (= "" selpar) (setq selpar (cadr (assoc cosa parametri))))
    )
  )
  selpar
)

(defun c:scrivitipo ()
  (start)

  (setq Lista (ssget "x" (list '(0 . "insert") '(2 . "codpas")'(8 . "siglepan"))))
  (if Lista
    (progn
      (setq nr -1)
      (setq lsttipo nil)
      (while (setq ent (ssname Lista (setq nr (+ 1 nr))))
        (setq lsttipo (cons (List (cdr (assoc '1 (entget (entnext (entnext ent))))) (cdr (assoc '10 (entget ent))) (cdr (assoc '50 (entget ent)))) lsttipo))      
      )
    )
  )
  
  (setq gr (ssget "x" (list '(0 . "TEXT") '(8 . "TestoTipoPannello"))))
  (if gr (command"_erase" gr ""))
  (command "_layer" "_m" "TestoTipoPannello" "")
  (setq grpan (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (setq nr -1)
  (while (setq ent (ssname grpan (setq nr (+ 1 nr))))
    (setq og ent)
    (setq lispt nil) (FOREACH N (ENTGET og) (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))))
    (setq lispt (reverse lispt))
    (SETQ MODOP (LEGGI ENT "PANTIP"))
    (setq tttt (entget og))
    (setq ttt (cdr (assoc '5 tttt)))
    (setq lpospan (assoc ttt lsttipo))
    (setq pm (polar (nth 0 lispt) (+ 0 (angle (nth 0 lispt) (nth 1 lispt))) (/ (distance (nth 0 lispt)(nth 1 lispt)) 2)))
;;;    (command "_text" "_m" "_non" (polar pm (+ (/ pi 2) (angle (nth 0 lispt) (nth 1 lispt))) 400) "50" (angtos (angle (nth 0 lispt) (nth 1 lispt))) modop)
    (command "_text" "_m" "_non" (polar (nth 1 lpospan) (+ (/ pi 2) (nth 2 lpospan)) 70) "50" (angtos (nth 2 lpospan)) modop)
    )
    
(stop)
    )
(defun c:scrivitipo ()
  (start)

  (setq Lista (ssget "x" (list '(0 . "insert") '(2 . "codpas")'(8 . "siglepan"))))
  (if Lista
    (progn
      (setq nr -1)
      (setq lsttipo nil)
      (while (setq ent (ssname Lista (setq nr (+ 1 nr))))
        (setq lsttipo (cons (List (cdr (assoc '1 (entget (entnext (entnext ent))))) (cdr (assoc '10 (entget ent))) (cdr (assoc '50 (entget ent)))) lsttipo))      
      )
    )
  )
  
  (setq gr (ssget "x" (list '(0 . "TEXT") '(8 . "TestoTipoPannello"))))
  (if gr
    (command "_erase" gr "")
    )
  (command "_layer" "_m" "TestoTipoPannello" "")
  (setq grpan (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (setq nr -1)
  (while (setq ent (ssname grpan (setq nr (+ 1 nr))))
    (setq og ent)
    (setq lispt nil) (FOREACH N (ENTGET og) (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))))
    (setq lispt (reverse lispt))
    (SETQ MODOP (LEGGI ENT "PANTIP"))
    (SETQ rESIS (LEGGI ENT "PANRES"))
    (setq tttt (entget og))
    (setq ttt (cdr (assoc '5 tttt)))
    (setq lpospan (assoc ttt lsttipo))
    (setq pm (polar (nth 0 lispt) (+ 0 (angle (nth 0 lispt) (nth 1 lispt))) (/ (distance (nth 0 lispt)(nth 1 lispt)) 2)))
    (command "_text" "_m" "_non" (polar pm (+ (/ pi 2) (angle (nth 0 lispt) (nth 1 lispt))) 300) "50" (angtos (angle (nth 0 lispt) (nth 1 lispt))) (STRCAT modop "-" RESIS))
;;;    (command "_text" "_m" "_non" (polar (nth 1 lpospan) (+ (/ pi 2) (nth 2 lpospan)) 70) "50" (angtos (nth 2 lpospan)) (STRCAT modop "-" RESIS))
    )
    
(stop)
    )

(defun C:leggiblocco ()
    (setq gr (ssget (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
    (setq nr -1)
    (while (setq ent (ssname gr (setq nr (+ 1 nr))))
       (Print "Pannello:")
       (princ (leggi ent "BLOCCOPAN"))
       (Princ "    Lamiera:")
       (princ (leggi ent "BLOCCOLAM"))
        (Princ "   Sviluppo:")
      (princ (leggi ent "BLOCCOSVI"))
      )
  )
	      
(defun c:bloccopan ()
  
  (setq gr (ssget (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (initget "Pannello Lamiera Sviluppo")
    (setq cosa (getkword "\nCosa blocco? [Pannello/Lamiera/Sviluppo] <Pannello>:"))
    (if (null cosa) (setq cosa "Pannello"))
  (setq nr -1)
    (while (setq ent (ssname gr (setq nr (+ 1 nr))))
       (if (= cosa "Pannello") (allega ent "BLOCCOPAN" "Si"))
       (if (= cosa "Lamiera") (allega ent "BLOCCOLAM" "Si"))
       (if (= cosa "Sviluppo") (allega ent "BLOCCOSVI" "Si"))

      )

  )

(defun pannelloM (p1 p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-dritti" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" p2
	  "_non" (setq px (polar p2 (setq app2 (+ (angle p1 p2) ap2)) 27.8));piega1
	  "_NON" (setq p (POLAR P2 (+ (ANGLE P1 P2) ap2 pi) 1)) "_non" px ;lineetta
	  "_non" (setq px (polar px (angle p1 p2) 4.181));punta
	  "_non" (setq px (polar px (+ (angle p1 p2) 0.2618) 7.6126));vertice molla
	  "_non" (setq pxx (setq px (polar px (- (angle p1 p2) 0.2618) 4.5119)))
	  "_non" (setq pxx (setq px (polar px (- (angle p1 p2) 0.2618 (- 0 (/ pi 2))) 0.7)))
	  "_non" (setq pxx (setq px (polar px (- (angle p2 p1) 0.2618) 4.7)))
	  "_non" (setq pxx (setq px (polar px (+ (angle p2 p1) 0.2618) 7.8)))
	  "_non" (setq pxx (setq px (polar px (+ (angle p2 p1) 0) 4.7)))
	  "_non" (setq px (inters
			    (polar px (- app2 (* 2 (/ pi 2))) 26.6);torna
			 pxx
			 (polar p1 (+ (angle p1 p2) (/ pi 2)) 0.7)
			 (polar p2 (+ (angle p1 p2) (/ pi 2)) 0.7)
			 nil))
	  "_non" (setq px (inters (polar px (angle p2 p1) (- (distance p1 p2) 1.4))
			 px
			 (polar p1 (- (angle p2 p1) ap1 (/ pi 2)) 0.7)
			 (polar (polar p1 (- (angle p2 p1) ap1) 20.0) (- (angle p2 p1) ap1 (/ pi 2)) 0.7)
			 nil))
	  "_non" (setq px (polar px (setq app1 (- (angle p2 p1) ap1)) 26.4));1
	  	  "_non" (setq px (polar px (angle p2 p1) 4.7));2
	  "_non" (setq px (polar px (- (angle p2 p1) 0.2618) 7.8));3
	  "_non" (setq pxx (setq px (polar px (+ (angle p2 p1) 0.2618) 4.7)));4
	  "_non" (setq pxx (setq px (polar px (+ (angle p2 p1) 0.2618 (+ 0 (/ pi 2))) 0.7)));5
	  "_non" (setq pxx (setq px (polar px (+ (angle p1 p2) 0.2618) 4.5119)));6
	  "_non" (setq pxx (setq px (polar px (- (angle p1 p2) 0.2618) 7.6126)));7
	  "_non" (setq pxx (setq px (polar px (+ (angle p1 p2) 0) 4.181)))

;;;	  "_non" (setq px (polar px (angle p1 p2) 27.8));inizio incastro
;;;	  "_non" (setq pprofilo (setq px (polar px (- (angle p2 p1) (/ pi 2) ) 0.7)));punta incastro
;;;	  "_non" (setq pprofilo (setq px (polar px (+ (angle p2 p1) 0 )28.5)));punta
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
 (if modop_d
    (progn
        (allega (entlast) "PANTIP" modop_d)
	(if (or (= modop_d "MAN") (= modop_d "SPD") (= modop_d "MPD") (= modop_d "STD") (= modop_d "MTD")) (command"_insert" "profilo1" "_non" pprofilo 1 1 (angtos (angle p2 p1)))))
    (if (or (= (car p1_nua) "AN1") (= tipoangolo 0) (= (rtos ang_prof 2 4) (rtos (angle p1 p2) 2 4)))
      (PROGN
	(allega (entlast) "PANTIP" "STD")
	(command"_insert" "profilo1" "_non" pprofilo 1 1 (angtos (angle p2 p1)))
	)
	 (allega (entlast) "PANTIP" "SPC"))
    )
;;;    (if modop_d
;;;    (allega (entlast) "PANTIP" modop_d)
;;;
;;;    (if (or (= (car p1_nua) "AN1") (= tipoangolo 0) (= (rtos ang_prof 2 4) (rtos (angle p1 p2) 2 4))) (command"_insert" "profilo1" "_non" pprofilo 1 1 (angtos (angle p2 p1))))
;;;    )

;;;  (allega (entlast) "PANFIN" fpan)
;;;  (if (or prk (= (car p1_nua) "AN1") (= tipoangolo 0) (= (rtos ang_prof 2 4) (rtos (angle p1 p2) 2 4))) (allega (entlast) "PANTIP" "STD") (allega (entlast) "PANTIP" "SPC"))
;;;  (allega (entlast) "PANRES" "B0")
;;;  (if (or prk (= (car p1_nua) "AN1") (= tipoangolo 0) (= (rtos ang_prof 2 4) (rtos (angle p1 p2) 2 4))) (command"_insert" "profilo1" "_non" pprofilo 1 1 (angtos (angle p2 p1))))
 )

(defun angoloM (p1 p2 p3 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-Angoli" "")
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p2 p3)(setq ap2 (- $ap2$ (angle p3 p2) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2))
  (setq ap2 (/ pi 2))
  
	
  (setq p1d p1 p2d p2 p3d p3)
  (setq n1 (polar (POLAR P1 (ANGLE P1 P2) 0.7) (setq app1 (- (angle p2 p1) ap1)) 27.8))
  (setq n2 (polar n1 (+ app1 (/ pi 2)) 4.7))
  	 (setq N3 (polar N2 (- (angle p2 p1) 0.2618) 7.8));3

	  ;(setq N4 (polar N3 (+ (angle p2 p1) 0.2618 (* 3 (/ pi 2))) 0.7));5
	  (setq N4 (polar N3 (+ (angle p2 p1) 0.2618 ) 4.7));5
          (setq N5 (polar N4 (+ (angle p2 p1) 0.2618 (* 1 (/ pi 2))) 0.7))
	  (setq N6 (polar N5 (+ (angle p1 p2) 0.2618) 4.5119));6
	  (setq N7 (polar N6 (- (angle p1 p2) 0.2618) 7.6126));7
	  (setq N8 (polar N7 (+ (angle p1 p2) 0) 4.181))


  
;;;  (setq n3 (polar n2 (- app1 pi) 0.7))
;;;  (SETQ N4 (POLAR N3 (- app1 (/ pi 2) pi) 27.8))
;;;  
;;;  
;;;  
  (SETQ N9 
  (inters n1
	  (POLAR N1 (+ (ANGLE P1 P2) (/ PI 2)) 25.1)
	  (polar p1 (+ (angle p1 p2) (/ pi 2)) 0.7)
	  (polar p2 (+ (angle p1 p2) (/ pi 2)) 0.7)
	  nil))
  (if (null zpan) (setq zpan 0))
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" p2 "_non" p3
	  "_non" (setq pprofilo (setq px (polar p3 (setq app2 (+ (angle p2 p3) ap2)) 27.8)));piega1

	  "_NON" (POLAR P3 (+ (ANGLE P2 P3) ap2 pi) 1) "_non" px ;lineetta
	  
	  "_non" (setq px (polar px (angle p2 p3) 4.181));punta
	  "_non" (setq px (polar px (+ (angle p2 p3) 0.2618) 7.6126));vertice molla
	  "_non" (setq pxx (setq px (polar px (- (angle p2 p3) 0.2618) 4.5119)))
	  "_non" (setq pxx (setq px (polar px (- (angle p2 p3) 0.2618 (- 0 (/ pi 2))) 0.7)))
	  "_non" (setq pxx (setq px (polar px (- (angle p3 p2) 0.2618) 4.7)))
	  "_non" (setq pxx (setq px (polar px (+ (angle p3 p2) 0.2618) 7.8)))
	  "_non" (setq pxx (setq px (polar px (+ (angle p3 p2) 0) 4.7)))


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
	  "_non" n1
	  "_non" n2
	  "_non" n3
	  "_non" n4
	  "_non" n5
	  "_non" n6
	  "_non" n7
	  "_non" (setq pprofilo n8)
	  "_c"
	   )
  (allega (entlast) "PANFIN" fpan)
  (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (if modop_d
    (allega (entlast) "PANTIP" modop_d)
    (if (or (= (car p1_nua) "AN1") (= tipoangolo 0) (= (rtos ang_prof 2 4) (rtos (angle p1 p2) 2 4)))
      (progn
	
	(allega (entlast) "PANTIP" "STD")
	(command"_insert" "profilo1" "_non" pprofilo 1 1 (angtos (angle p2 p1)))
	)
	(allega (entlast) "PANTIP" "SPS"))
    )
    (if modop_d
      (progn
        (allega (entlast) "PANTIP" modop_d)
	(if (or (= modop_d "SPD")(= modop_d "MAN")(= modop_d "SPD")  (= modop_d "STD")) (command"_insert" "profilo1" "_non" pprofilo 1 1 (angtos (angle p2 p1)))))
    )

)

(defun c:gestparametri ()
  (setq origin (getfiled "Selezionare file Parametri da utilizzare" (substr (findfile "svilpan.txt") 1 (- (strlen (findfile "svilpan.txt"))11))  "txt" 8))
  
  (if (/= "PARAMETRI" (STRCASE (substr (vl-filename-base origin) 1 9))) (PROGN (ALERT "File non Valido !!!") (exit)))
  (setq nome (substr (vl-filename-base origin) 10))
  (setq filer (open origin "r"))
  (setq filew (open "c:\\dwdati\\parametri.txt" "w"))
  (while (setq r (read-line filer))
    (if (/= 'NomeParametri (car (read (strcat "(" r ")")))) (write-line r filew)))
  (write-line (strcat "NomeParametri" (chr 9) "\"" nome "\"") filew)
  (close filer)
  (close filew)
  (setq parametri (carica "Parametri" ""))
(setq modalita (cadr (assoc 'percregtmp parametri)))
(print "-----------------------------------------------")
(print "-----------------------------------------------")
(print "-----------------------------------------------")
(print (cadr (assoc 'NomeParametri parametri)))
(print "-----------------------------------------------")
(print "-----------------------------------------------")
(print "-----------------------------------------------")
(if (cadr (assoc 'NomeParametri parametri)) (grtext -1 (strcat "FILE CONFIG: " (cadr (assoc 'NomeParametri parametri))) 1))

  )
(setq parametri (carica "Parametri" ""))
(setq modalita (cadr (assoc 'percregtmp parametri)))
(print "-----------------------------------------------")
(print "-----------------------------------------------")
(print "-----------------------------------------------")
(print (cadr (assoc 'NomeParametri parametri)))
(print "-----------------------------------------------")
(print "-----------------------------------------------")
(print "-----------------------------------------------")
(if (cadr (assoc 'NomeParametri parametri)) (grtext -1 (strcat "FILE CONFIG: " (cadr (assoc 'NomeParametri parametri))) 1))

(defun c:gestparametri ()
  (setq *error* nil)
  (setq origin (getfiled "Selezionare file Parametri da utilizzare" (substr (findfile "svilpan.txt") 1 (- (strlen (findfile "svilpan.txt"))11)) "txt" 8))
  
  (if (/= "PARAMETRI" (STRCASE (substr (vl-filename-base origin) 1 9))) (PROGN (ALERT "File non Valido !!!") (exit)))
  (setq nome (substr (vl-filename-base origin) 10))
  (setq filer (open (strcat (substr (findfile "svilpan.txt") 1 (- (strlen (findfile "svilpan.txt"))11)) origin) "r"))
  (setq filew (open "c:\\dwdati\\parametri.txt" "w"))
  (while (setq r (read-line filer))
    (if (/= 'NomeParametri (car (read (strcat "(" r ")")))) (write-line r filew)))
  (write-line (strcat "NomeParametri" (chr 9) "\"" nome "\"") filew)
  (close filer)
  (close filew)
  (setq parametri (carica "Parametri" ""))
(setq modalita (cadr (assoc 'percregtmp parametri)))
(print "-----------------------------------------------")
(print "-----------------------------------------------")
(print "-----------------------------------------------")
(print (cadr (assoc 'NomeParametri parametri)))
(print "-----------------------------------------------")
(print "-----------------------------------------------")
(print "-----------------------------------------------")
(if (cadr (assoc 'NomeParametri parametri)) (grtext -1 (strcat "FILE CONFIG: " (cadr (assoc 'NomeParametri parametri))) 1))

  )

(defun c:salvaparametri ()
  (setq parametri (carica "Parametri" ""))
  (setq nome (cadr (assoc 'NomeParametri parametri)))
  (vl-file-delete (strcat  (substr (findfile "svilpan.txt") 1 (- (strlen (findfile "svilpan.txt"))11)) "parametri" nome ".txt"))
  (vl-file-copy "c:\\dwdati\\parametri.txt" (strcat  (substr (findfile "svilpan.txt") 1 (- (strlen (findfile "svilpan.txt"))11)) "parametri" nome ".txt"))
  (alert (strcat "Parametri salvati con il nome:  "(substr (findfile "svilpan.txt") 1 (- (strlen (findfile "svilpan.txt"))11)) "parametri" nome ".txt"))
  )

(defun c:mang ()
  (start)
  (setq $lpan (cadr (assoc 'lungpannello parametri)))
  (setq pm1 (getpoint "\nPrimo punto del pannello:"))
;;;  (initget "Dritto")
;;;  (setq $ap1$ (getangle "\nSelezionare direzione pannello precedente <[Dritto]> :" pm1))
;;;  (if (= "Dritto" $ap1$) (setq $ap1$ nil))
  (setq $ap1$ nil)
  (setq pm2 nil)
  (while (null pm2)
    (setq pm2 (getpoint pm1 "\nSecondo punto del pannello:"))
;;;    (if (> (distance pm1 pm2) $lpan) (progn (prompt "lunghezza primo segmento eccessiva!!!!")
;;;				       (setq pm2 nil)))
  )
  (setq pm3 (getpoint pm2 "\nSecondo punto del pannello:"))
;;;  (initget "Dritto")
;;;  (setq $ap2$ (getangle "\nSelezionare direzione pannello successivo <[Dritto]> :" pm3))
;;;  (if (= "Dritto" $ap2$) (setq $ap2$ nil))
  (setq $ap2$ nil)
  (if (< $lpan (distance pm1 pm2))
    (progn
      (prompt"\nLunghezza pannello eccesiva pannelli tagliato!!!!")
      (setq pm2 (polar pm1 (angle pm1 pm2) $lpan))
    )
  )
;;;  (setq modo (getstring"\nTipologia pannello [STD/SPD/SFD/SFC/SFS/SPC/SSC/SSS/SPS/SFT/SSD/SPD] <STD>:"))
  (if (null modool)(setq modool "STD"))
  (setq modo (getstring (strcat "\nTipologia pannello [STD/SFD/SFS/SMD/SMS/S2D/S2S/S3D/S4D/S4S] <" modool ">:")))
  (if (= "" modo) (setq modo modool) (setq modool modo))
  (setq modop_d modo)
  (setq hpan (getdist (strcat "\Altezza pannello [" (cadr (assoc 'Altezze parametri)) "]<" (rtos (cadr (assoc 'altezzapannello parametri)) 2 0) ">:")))
  (if (null hpan) (setq hpan (cadr (assoc 'altezzapannello parametri))))
  (setq fpan (getstring (strcat "\Finitura pannello [" (cadr (assoc 'Finiture parametri)) "]<"(cadr (assoc 'Finitura parametri)) ">:")))
  (if (= "" fpan) (setq fpan (cadr (assoc 'Finitura parametri))))
  (setq zpan (caddr pm1))
  (command"_elev" zpan hpan)
  (cond
    ((= modo "STD") (angoloN pm1 pm2 pm3 27))
    ((= modo "SFD") (angoloFD pm1 pm2 pm3 27))
    ((= modo "SFS") (angoloFS pm1 pm2 pm3 27))
    ((= modo "SMD") (angoloMD pm1 pm2 pm3 27))
    ((= modo "SMS") (angoloMS pm1 pm2 pm3 27))
    ((= modo "S2D") (angolo2D pm1 pm2 pm3 27))
    ((= modo "S2S") (angolo2S pm1 pm2 pm3 27))

    ((= modo "S3D") (angolo3D pm1 pm2 pm3 27))
;;;    ((= modo "S3S") (angolo3S pm1 pm2 pm3 27))
    ((= modo "S4D") (angolo4D pm1 pm2 pm3 27))
    ((= modo "S4S") (angolo4S pm1 pm2 pm3 27))
    
  )
    (IF (= MODO "MAN")
    (progn
      (if (= TIPOMB "Bifacciale")
	(progn
	  (setq sx (getreal"\nValore della somma pieghe a Sinitra :"))
	  (setq dx (getreal"\nValore della somma pieghe a Destra :"))
	  (setq a (getreal"\nValore della somma pieghe in Alto :"))
	  (setq b (getreal"\nValore della somma pieghe in Basso :"))
	  (setq rsx (getreal"\nValore della somma pieghe del panndello retro a Sinitra :"))
	  (setq rdx (getreal"\nValore della somma pieghe del panndello retro a Destra :"))
	  (setq ra (getreal"\nValore della somma pieghe del panndello retro in Alto :"))
	  (setq rb (getreal"\nValore della somma pieghe del panndello retro in Basso :"))
	  (setq SvilMan (strcat "(" (rtos sx 2 1) " " (rtos dx 2 1) " " (rtos a 2 1)  " " (rtos b 2 1) " " (rtos rsx 2 1) " " (rtos rdx 2 1) " " (rtos ra 2 1)  " " (rtos rb 2 1) ")"))
	  (allega (entlast) "SVILMAN" SvilMan)
	  )
	(progn
	  (setq sx (getreal"\nValore della somma pieghe a Sinitra :"))
	  (setq dx (getreal"\nValore della somma pieghe a Destra :"))
	  (setq a (getreal"\nValore della somma pieghe in Alto :"))
	  (setq b (getreal"\nValore della somma pieghe in Basso :"))
	  (setq SvilMan (strcat "(" (rtos sx 2 1) " " (rtos dx 2 1) " " (rtos a 2 1)  " " (rtos b 2 1) ")"))
	  (allega (entlast) "SVILMAN" SvilMan)
	  )
	)
      )
    )
  
  (allega (entlast) "PANPIEG" "(0 0)")
  (RETROpan (SETQ ENTP (ENTLAST)))
  (LANAPAN ENTP (SETQ ENTR (ENTLAST)))

;;;  (if (null (assoc '62 (entget (entlast))))
;;;    (command"_chprop" entlastpan "" "_co" "3" "")
;;;  )
  (stop)
)
(defun c:mpan ()
  (start)
  (setq pm1 (getpoint "\nPrimo punto del pannello:"))
  (initget "Dritto")
;;;  (setq $ap1$ (getangle "\nSelezionare direzione pannello precedente <[Dritto]> :" pm1))
;;;  (if (= "Dritto" $ap1$) (setq $ap1$ nil))
  (setq $ap1$ nil)
  (setq pm2 (getpoint pm1 "\nSecondo punto del pannello:"))
  (initget "Dritto")
;;;  (setq $ap2$ (getangle "\nSelezionare direzione pannello successivo <[Dritto]> :" pm2))
;;;  (if (= "Dritto" $ap2$) (setq $ap2$ nil))
  (setq $ap2$ nil)
  (setq $lpan (cadr (assoc 'lungpannello parametri)))
  (if (< $lpan (distance pm1 pm2))
    (progn
      (prompt"\nLunghezza pannello eccesiva pannelli tagliato!!!!")
      (setq pm2 (polar pm1 (angle pm1 pm2) $lpan))
    )
  )
  (if (null modool)(setq modool "STD"))
  (setq modo (getstring (strcat "\nTipologia pannello [STD/SFD/SFS/SMD/SMS/S2D/S2S/S3D/S4D/S4S] <" modool ">:")))
  (if (= "" modo) (setq modo modool) (setq modool modo))
  (setq modop_d modo)
  (setq hpan (getdist (strcat "\Altezza pannello [" (cadr (assoc 'Altezze parametri)) "]<" (rtos (cadr (assoc 'altezzapannello parametri)) 2 0) ">:")))
  (if (null hpan) (setq hpan (cadr (assoc 'altezzapannello parametri))))
  (setq fpan (getstring (strcat "\Finitura pannello [" (cadr (assoc 'Finiture parametri)) "]<"(cadr (assoc 'Finitura parametri)) ">:")))
  (if (= "" fpan) (setq fpan (cadr (assoc 'Finitura parametri))))
  (setq zpan (caddr pm1))
  (command"_elev" zpan hpan)
  (setq handinf nil)
  (if (/= 0 zpan)
    (progn
      (setq entbs (car (entsel "\nQuesto pannello è ad una quota diversa da 0.0, \n
      Se si trova sopra ad un'altro pannello selezionarlo adesso:")))
      (setq handinf (cdr (assoc '5 (entget entbs))))
    )
  )
  (cond
    ((= modo "STD") (pannelloN pm1 pm2 27))
    ((= modo "SFD") (pannelloFD pm1 pm2 27))
    ((= modo "SFS") (pannelloFS pm1 pm2 27))
    ((= modo "SMD") (pannelloMD pm1 pm2 27))
    ((= modo "SMS") (pannelloMS pm1 pm2 27))
    ((= modo "S2D") (pannello2D pm1 pm2 27))
    ((= modo "S2S") (pannello2S pm1 pm2 27))

    ((= modo "S3D") (pannello3D pm1 pm2 27))
    ((= modo "S4D") (pannello4D pm1 pm2 27))
    ((= modo "S4S") (pannello4S pm1 pm2 27))
    
  )
  
  (IF (= MODO "MAN")
    (progn
      (if (= TIPOMB "Bifacciale")
	(progn
	  (setq sx (getreal"\nValore della somma pieghe a Sinitra :"))
	  (setq dx (getreal"\nValore della somma pieghe a Destra :"))
	  (setq a (getreal"\nValore della somma pieghe in Alto :"))
	  (setq b (getreal"\nValore della somma pieghe in Basso :"))
	  (setq rsx (getreal"\nValore della somma pieghe del panndello retro a Sinitra :"))
	  (setq rdx (getreal"\nValore della somma pieghe del panndello retro a Destra :"))
	  (setq ra (getreal"\nValore della somma pieghe del panndello retro in Alto :"))
	  (setq rb (getreal"\nValore della somma pieghe del panndello retro in Basso :"))
	  (setq SvilMan (strcat "(" (rtos sx 2 1) " " (rtos dx 2 1) " " (rtos a 2 1)  " " (rtos b 2 1) " " (rtos rsx 2 1) " " (rtos rdx 2 1) " " (rtos ra 2 1)  " " (rtos rb 2 1) ")"))
	  (allega (entlast) "SVILMAN" SvilMan)
	  )
	(progn
	  (setq sx (getreal"\nValore della somma pieghe a Sinitra :"))
	  (setq dx (getreal"\nValore della somma pieghe a Destra :"))
	  (setq a (getreal"\nValore della somma pieghe in Alto :"))
	  (setq b (getreal"\nValore della somma pieghe in Basso :"))
	  (setq SvilMan (strcat "(" (rtos sx 2 1) " " (rtos dx 2 1) " " (rtos a 2 1)  " " (rtos b 2 1) ")"))
	  (allega (entlast) "SVILMAN" SvilMan)
	  )
	)
      )
    )
  
  (allega (entlast) "PANPIEG" "(0 0)")
  (RETROPAN (SETQ ENTP (ENTLAST)))
  (LANAPAN ENTP (SETQ ENTR (ENTLAST)))
;;;  (if (null (assoc '62 (entget (entlast))))
;;;    (command"_chprop" (entlast) "" "_co" "6" "")
;;;  )
  (if handinf (allega (entlast) "PSUP" handinf))
  
  (stop)
)
(defun pannelloN (p1 p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-dritti" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle p1 p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  "_non" (SETQ P (POLAR P (- (angle p1 p2) ap2) 6.0))
	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 p2) ap2)) 6.0));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "STD")
 )

(defun pannello3D (p1 p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-dritti" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle p1 p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  "_non" (SETQ P (POLAR P (- (angle p1 p2) ap2) 6.0))
	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 p2) ap2)) 6.0));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "S3D")
 )

(defun pannelloMS (p1 p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-dritti" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle p1 p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  "_non" (SETQ P (POLAR P (- (angle p1 p2) ap2) 6.0))
	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 p2) ap2)) 22.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "SMS")
 )
(defun pannelloMD (p1 p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-dritti" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle p1 p2) ap2)) 23));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  "_non" (SETQ P (POLAR P (- (angle p1 p2) ap2) 22.5))
	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 p2) ap2)) 6.0));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "SMD")
 )

(defun pannello4S (p1 p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-dritti" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle p1 p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  "_non" (SETQ P (POLAR P (- (angle p1 p2) ap2) 6.0))
	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 p2) ap2)) 22.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "S4S")
 )
(defun pannello4D (p1 p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-dritti" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle p1 p2) ap2)) 23));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  "_non" (SETQ P (POLAR P (- (angle p1 p2) ap2) 22.5))
	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 p2) ap2)) 6.0));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "S4D")
 )


(defun pannello2S (p1 p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-dritti" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle p1 p2) ap2)) 23));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  "_non" (SETQ P (POLAR P (- (angle p1 p2) ap2)22.5))
	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 p2) ap2)) 6.0));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "S2S")
 )
(defun pannello2D (p1 p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-dritti" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle p1 p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  "_non" (SETQ P (POLAR P (- (angle p1 p2) ap2) 6))
	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 p2) ap2)) 22.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "S2D")
 )

(defun pannellofd (p1 p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-dritti" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle p1 p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  "_non" (SETQ P (POLAR P (- (angle p1 p2) ap2) 6.0))
	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "SFD")
 )
(defun pannellofS (p1 p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-dritti" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle p1 p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  "_non" (SETQ P (POLAR P (- (angle p1 p2) ap2) 6.0))
	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "SFS")
 )
(defun c:elimina_sporco ()
  (setq nr 0)
  (setq grZ (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "pannelli-*"))))
  (setq lane_ok nil)
  (setq retro_ok nil)
  (WHILE (SETQ ENTZ (SSNAME GRZ NR))
    (IF (AND (ENTGET ENTZ) (SETQ TMP (ENTGET ENTZ (LIST "TLANA1"))) (SETQ TMP (ASSOC -3 TMP)))
    (progn
      (SETQ HAND (LEGGI ENTZ "TLANA1"))
      (if (= 'LIST (type (read hand)))
	  (setq lane_ok (cons (cadr (read hand)) lane_ok))
	  (setq lane_ok (cons hand laneok))
	)
      )
    )
    (IF (AND (ENTGET ENTZ) (SETQ TMP (ENTGET ENTZ (LIST "TLANA2"))) (SETQ TMP (ASSOC -3 TMP)))
    (progn
      (SETQ HAND (LEGGI ENTZ "TLANA2"))
      (if (= 'LIST (type (read hand)))
	  (setq lane_ok (cons (cadr (read hand)) lane_ok))
	  (setq lane_ok (cons hand laneok))
	)
      )
    )
    (IF (AND (ENTGET ENTZ) (SETQ TMP (ENTGET ENTZ (LIST "PANRETRO"))) (SETQ TMP (ASSOC -3 TMP)))
    (progn
      (SETQ HAND (LEGGI ENTZ "PANRETRO"))
      (if (= 'LIST (type (read hand)))
	  (setq retro_ok (cons (cadr (read hand)) retro_ok))
	  (setq retro_ok (cons hand retro_ok))
	)
      )
    )
    (SETQ NR (+ 1 NR))
  )
  (setq nr -1)
  (setq gr (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "pan-Retro"))))
  (WHILE (AND GR (SETQ ENTD (SSNAME GR (SETQ NR (+ 1 NR)))))
    (IF (NULL (MEMBER (CDR (ASSOC '5 (ENTGET ENTD))) RETRO_OK))
      (COMMAND "_ERASE" ENTD "")
      )
    )
  (setq nr -1)
  (setq gr (ssget "x" (list '(0 . "LWPOLYLINE") '(8 . "pan-lana"))))
  (WHILE (AND GR (SETQ ENTD (SSNAME GR (SETQ NR (+ 1 NR)))))
    (IF (NULL (MEMBER (CDR (ASSOC '5 (ENTGET ENTD))) LANE_OK))
      (COMMAND "_ERASE" ENTD "")
      )
    )

  )


(DEFUN lanaPAN (ENT entr)
  (IF (AND (ENTGET ENT)
	   (SETQ TMP (ENTGET ENT (LIST "TLANA1")))
	   (SETQ TMP (ASSOC -3 TMP))
      )
    (progn
      (SETQ HAND (LEGGI ENT "TLANA1"))
      (if (= 'LIST (type (read hand)))
	(if (= (cdr (assoc '5 (entget ent))) (car (read hand)))
	  (IF (HANDENT (cadr (read hand))) (COMMAND"_ERASE" (HANDENT (cadr (read hand))) ""))
	  
	  )
      
      (IF (HANDENT HAND) (COMMAND"_ERASE" (HANDENT HAND) ""))
      )
      )
    )
  (IF (AND (ENTGET ENT)
	   (SETQ TMP (ENTGET ENT (LIST "TLANA2")))
	   (SETQ TMP (ASSOC -3 TMP))
      )
    (progn
      (SETQ HAND (LEGGI ENT "TLANA2"))
      (if (= 'LIST (type (read hand)))
	(if (= (cdr (assoc '5 (entget ent))) (car (read hand)))
	  (IF (HANDENT (cadr (read hand))) (COMMAND"_ERASE" (HANDENT (cadr (read hand))) ""))
	  
	  )
      
      (IF (HANDENT HAND) (COMMAND"_ERASE" (HANDENT HAND) ""))
      )
      )
    )
    (SETQ POSPIEG (READ (LEGGI ENT "PANPIEG")))

 ; (SETQ POSPIEG '(100 100))
  ;;;  (SETQ POSPIEG '(0 0))
  (setq sr 0.3)
  (IF (OR (= MODO "S3D") (= MODO "S4D") (= MODO "S4S")) (setq sr 0.5))
  (SETQ MODO (LEGGI ENT "PANTIP"))
  (SETQ P1 (CDR (ASSOC '10 (ENTGET ENT))))
  (SETQ Px (CDR (ASSOC '10 (CDR (MEMBER (ASSOC '10 (ENTGET ENT)) (ENTGET ENT))))))
  ;;;  (SETQ Px (CDR (ASSOC '10 (CDR (MEMBER (ASSOC '10 (ENTGET ENT)) (ENTGET ENT))))))
  (setq lispt nil) (FOREACH N (ENTGET ent) (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))))
  (setq lispt (reverse lispt))
  (if (/= "pannelli-dritti" (cdr (assoc '8 (entget ent))))
    (progn
      (setq p1 (nth 0 lispt))
      (setq px (nth 1 lispt))
      (setq p2 (nth 2 lispt))
      (setq p1_ (polar p1 (+ (angle p1 px)(/ pi 2)) 0.5))
      (setq p2_ (polar p2 (+ (angle px p2)(/ pi 2)) 0.5))
      (setq px_ (inters p1_ (polar p1_ (angle p1 px) 100) p2_ (polar p2_ (angle p2 px) 100) nil))
      (setq px_1 px)
      (setq px_2 px)
      )
    (progn
      (setq p1 (nth 0 lispt))
      (setq px (nth 1 lispt))
      (setq p2 (nth 1 lispt))
      (setq p1_ (polar p1 (+ (angle p1 p2)(/ pi 2)) 0.5))
      (setq p2_ (polar p2 (+ (angle p1 p2)(/ pi 2)) 0.5))
      (setq px_1 p1)
      (setq px_2 p2)
      )
    )
  (COMMAND"_LAYER" "_N" "pan-lana" "")
  
  ;;;  (SETQ PR1 (POLAR P1 (ANGLE P1 Px) 1.0))
  ;;;  (SETQ PR1 (POLAR PR1 (+ (ANGLE P1 Px) (/ PI 2)) (- 25.0 0)))
  ;;;  (SETQ PR2 (POLAR P2 (ANGLE P2 Px) 1.0))
  ;;;  (SETQ PR2 (POLAR PR2 (- (ANGLE P2 Px) (/ PI 2)) (- 25.0 0)))
  ;;;  (setq prx (inters pr1 (polar pr1 (angle p1 px) 100) pr2 (polar pr2 (angle p2 px) 100) nil))
  ;;;  (setq pr2< (polar pr2 (+ (angle pr2 prx) (/ pi 4)) (* sr 1.4142)))
  ;;;  (setq pr1< (polar pr1 (- (angle pr1 prx) (/ pi 4)) (* sr 1.4142)))
  ;;;  (setq prx< (inters pr1< (polar pr1< (angle pr1 prx) 100) pr2< (polar pr2< (angle pr2 prx) 100) nil))
  ;;;  (setq pr1_ (polar pr1 (- (angle pr1 prx) (/ pi 2)) 10))
  ;;;  (setq pr2_ (polar pr2 (+ (angle pr2 prx) (/ pi 2)) 10))
  ;;;  (setq prx_ (inters pr1_ (polar pr1_ (angle pr1 prx) 100) pr2_ (polar pr2_ (angle pr2 prx) 100) nil))
  ;;;  (setq pr1_< (polar pr1 (- (angle pr1 prx) (/ pi 2)) (+ sr 10)))
  ;;;  (setq pr2_< (polar pr2 (+ (angle pr2 prx) (/ pi 2)) (+ sr 10)))
  ;;;  (setq prx_< (inters pr1_< (polar pr1_< (angle pr1 prx) 100) pr2_< (polar pr2_< (angle pr2 prx) 100) nil))
  (COND ((= MODO "STD")
	 (command "_pline"
		  "_non" (setq p (polar p1_ (angle p1_ px_2) 0.5) )
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 11.5))
		  "_non" (setq p (polar p (- (angle p1_ px_2) 0) 23.0))
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 2.0))
		  "_non" (setq p (polar p (- (angle px_2 p1_) 0) 22.2))
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 10.7))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA1" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA1" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 (command "_pline"
		  "_non" (setq p (polar p2_ (angle p2_ px_1) 0.5) )
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 11.5))
		  "_non" (setq p (polar p (- (angle p2_ px_1) 0) 23.0))
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 2.0))
		  "_non" (setq p (polar p (- (angle px_1 p2_) 0) 22.2))
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 10.7))
		  ""
		  )
	 
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA2" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA2" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 
	 )
	((= MODO "SFD")
	 (command "_pline"
		  "_non" (setq p (polar p1_ (angle p1_ px_2) 0.5) )
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 11.5))
		  "_non" (setq p (polar p (- (angle p1_ px_2) 0) 23.0))
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 2.0))
		  "_non" (setq p (polar p (- (angle px_2 p1_) 0) 22.2))
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 10.7))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA1" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA1" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 (command "_pline"
		  "_non" (setq p (polar p2_ (angle p2_ px_1) 0.5) )
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 24.2))
		  ""
		  )
	 
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA2" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA2" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 )
	((= MODO "SFS")
	 (command "_pline"
		  "_non" (setq p (polar p1_ (angle p1_ px_2) 0.5) )
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 24.2))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
	 ;(allega ent "TLANA1" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA1" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 (command "_pline"
		  "_non" (setq p (polar p2_ (angle p2_ px_1) 0.5) )
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 11.5))
		  "_non" (setq p (polar p (- (angle p2_ px_1) 0) 23.0))
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2))2.0))
		  "_non" (setq p (polar p (- (angle px_1 p2_) 0) 22.2))
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 10.7))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA2" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA2" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 )
	
	((= MODO "SMD")
	 (command "_pline"
		  "_non" (setq p (polar p1_ (angle p1_ px_2) 0.5) )
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 11.5))
		  "_non" (setq p (polar p (- (angle p1_ px_2) 0) 23.0))
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 2.0))
		  "_non" (setq p (polar p (- (angle px_2 p1_) 0) 22.2))
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 10.7))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA1" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA1" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 (command "_pline"
		  "_non" (setq p (polar p2_ (angle p2_ px_1) 0.8) )
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 24.2))
		  ""
		  )
	 
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
	 (allega ent "TLANA2" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
;	 (allega ent "TLANA2" (cdr (assoc '5 (entget (entlast)))))
	 
	 )
	((= MODO "SMS")
	 (command "_pline"
		  "_non" (setq p (polar p1_ (angle p1_ px_2) 0.8) )
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 24.2))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA1" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA1" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 (command "_pline"
		  "_non" (setq p (polar p2_ (angle p2_ px_1) 0.5) )
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 11.5))
		  "_non" (setq p (polar p (- (angle p2_ px_1) 0) 23.0))
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 2.0))
		  "_non" (setq p (polar p (- (angle px_1 p2_) 0) 22.2))
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 10.7))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA2" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA2" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 
	 
	 )
	
	((= MODO "S2D")
	 (command "_pline"
		  "_non" (setq p (polar p1_ (angle p1_ px_2) 0.8) )
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 24.2))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA1" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA1" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 (command "_pline"
		  "_non" (setq p (polar p2_ (angle p2_ px_1) 0.5) )
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 24.2))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
	 ;(allega ent "TLANA2" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA2" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 
	 )
	((= MODO "S2S")
	 (command "_pline"
		  "_non" (setq p (polar p1_ (angle p1_ px_2) 0.5) )
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 24.2))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA1" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA1" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 
	 (command "_pline"
		  "_non" (setq p (polar p2_ (angle p2_ px_1) 0.8) )
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 24.2))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA2" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA2" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 
	 
	 
	 )
	((= MODO "S3D")
	 (command "_pline"
		  "_non" (setq p (polar p1_ (angle p1_ px_2) 0.5) )
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 11.5))
		  "_non" (setq p (polar p (- (angle p1_ px_2) 0) 23.0))
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 2.0))
		  "_non" (setq p (polar p (- (angle px_2 p1_) 0) 23.0))
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 10.5))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA1" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA1" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 (command "_pline"
		  "_non" (setq p (polar p2_ (angle p2_ px_1) 0.5) )
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 11.5))
		  "_non" (setq p (polar p (- (angle p2_ px_1) 0) 23.0))
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 2.0))
		  "_non" (setq p (polar p (- (angle px_1 p2_) 0) 23.0))
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 10.5))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA2" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA2" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 
	 
	 )
	((= MODO "S4D")
	 (command "_pline"
		  "_non" (setq p (polar p1_ (angle p1_ px_2) 0.5) )
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 11.5))
		  "_non" (setq p (polar p (- (angle p1_ px_2) 0) 23.0))
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 2.0))
		  "_non" (setq p (polar p (- (angle px_2 p1_) 0) 22.0))
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 10.5))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA1" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA1" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 (command "_pline"
		  "_non" (setq p (polar p2_ (angle p2_ px_1) 1.0) )
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 24.0))
		  ""
		  )
	 
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;;;	 (allega ent "TLANA2" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA2" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 )
	((= MODO "S4S")
	 (command "_pline"
		  "_non" (setq p (polar p1_ (angle p1_ px_2) 1.0) )
		  "_non" (setq p (polar p (+ (angle p1_ px_2) (/ pi 2)) 24.0))
		  ""
		  )
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;	 (allega ent "TLANA1" (cdr (assoc '5 (entget (entlast)))))
         (allega ent "TLANA1" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))	 
	 
	 (command "_pline"
		  "_non" (setq p (polar p2_ (angle p2_ px_1) 0.5) )
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 11.5))
		  "_non" (setq p (polar p (- (angle p2_ px_1) 0) 23.0))
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 2.0))
		  "_non" (setq p (polar p (- (angle px_1 p2_) 0) 22.0))
		  "_non" (setq p (polar p (- (angle p2_ px_1) (/ pi 2)) 10.5))
		  ""
		  )
	 
	 (command"_chprop" (entlast) "" "_la" "pan-lana" "")
;;;	 (allega ent "TLANA2" (cdr (assoc '5 (entget (entlast)))))
	 (allega ent "TLANA2" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (cdr (assoc '5 (entget (entlast))))"\")"))
	 
	 
	 )
	
	
	
	
	)
  ;  (command"_chprop" (entlast) "" "_la" "pan-lana" "")
  )

(defun angoloN (p1 px p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-Angoli" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (setq p1< (polar p1 (+ (angle p1 px) (/ pi 4)) 0.707106))
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" px "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle px p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 Px) 0.5))
	  "_non" (setq p2< (SETQ P (POLAR P (- (angle px p2) ap2) 6.0)))
	  "_non" (inters p1< (polar p1< (angle p1 px) 100) p2<  (polar p2< (angle p2 px) 100) nil)
;	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (setq p p1<)
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 px) ap2)) 6.0));piega1
	  "_NON" (setq p (POLAR P (ANGLE Px P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "STD")
 )
(defun angolofd (p1 px p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-Angoli" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (setq p1< (polar p1 (+ (angle p1 px) (/ pi 4)) 0.707106))
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" px "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle px p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 Px) 0.5))
	  "_non" (setq p2< (SETQ P (POLAR P (- (angle px p2) ap2) 6.0)))
	  ;"_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (inters p1< (polar p1< (angle p1 px) 100) p2<  (polar p2< (angle p2 px) 100) nil)
	  "_non" (setq p p1<)
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 px) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE Px P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "SFD")
 )
(defun angolofS (p1 px p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-Angoli" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
  (setq p1< (polar p1 (+ (angle p1 px) (/ pi 4)) 0.707106))

  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" px "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle px p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 Px) 0.5))
	  "_non" (setq p2< (SETQ P (POLAR P (- (angle px p2) ap2) 6.0)))
;	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (inters p1< (polar p1< (angle p1 px) 100) p2<  (polar p2< (angle p2 px) 100) nil)
	  "_non" (setq p p1<)
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 px) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE Px P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "SFS")
 )

(defun angoloMD (p1 px p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-Angoli" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (setq p1< (polar p1 (+ (angle p1 px) (/ pi 4)) 0.707106))
  
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" px "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle px p2) ap2)) 23));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 Px) 0.5))
	  "_non" (setq p2< (SETQ P (POLAR P (- (angle px p2) ap2) 22.5)))
;	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (inters p1< (polar p1< (angle p1 px) 100) p2<  (polar p2< (angle p2 px) 100) nil)
	  "_non" (setq p p1<)
	  
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 px) ap2)) 6.0));piega1
	  "_NON" (setq p (POLAR P (ANGLE Px P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "SMD")
 )

(defun angoloMS (p1 px p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-Angoli" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
  (setq p1< (polar p1 (+ (angle p1 px) (/ pi 4)) 0.707106))
  
;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" px "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle px p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 Px) 0.5))
	  "_non" (setq p2< (SETQ P (POLAR P (- (angle px p2) ap2) 6.0)))
	  ;"_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (inters p1< (polar p1< (angle p1 px) 100) p2<  (polar p2< (angle p2 px) 100) nil)
	  "_non" (setq p p1<)
	  
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 px) ap2)) 22.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE Px P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "SMS")
 )

(defun angolo2S (p1 px p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-Angoli" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
  (setq p1< (polar p1 (+ (angle p1 px) (/ pi 4)) 0.707106))
  
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" px "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle px p2) ap2)) 23));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 Px) 0.5))
	  "_non" (setq p2< (SETQ P (POLAR P (- (angle px p2) ap2) 22.5)))
	  ;"_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (inters p1< (polar p1< (angle p1 px) 100) p2<  (polar p2< (angle p2 px) 100) nil)
	  "_non" (setq p p1<)
	  
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 px) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE Px P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "S2S")
 )

(defun angolo2D (p1 px p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-Angoli" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
    (setq p1< (polar p1 (+ (angle p1 px) (/ pi 4)) 0.707106))

;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" px "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle px p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 Px) 0.5))
	  "_non" (setq p2< (SETQ P (POLAR P (- (angle px p2) ap2) 6.0)))
;	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (inters p1< (polar p1< (angle p1 px) 100) p2<  (polar p2< (angle p2 px) 100) nil)
	  "_non" (setq p p1<)
	  
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 px) ap2)) 22.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE Px P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "S2D")
 )

(defun angolo3D (p1 px p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-Angoli" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
    (setq p1< (polar p1 (+ (angle p1 px) (/ pi 4)) 0.707106))
  
;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" px "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle px p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 Px) 0.5))
	  "_non" (setq p2< (SETQ P (POLAR P (- (angle px p2) ap2) 6.0)))
;	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (inters p1< (polar p1< (angle p1 px) 100) p2<  (polar p2< (angle p2 px) 100) nil)
	  "_non" (setq p p1<)
	  
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 px) ap2)) 6.0));piega1
	  "_NON" (setq p (POLAR P (ANGLE Px P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "S3D")
 )
(defun angolo4S (p1 px p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-Angoli" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
;;;  (setq p p1 p1 p2 p2 p)
    (setq p1< (polar p1 (+ (angle p1 px) (/ pi 4)) 0.707106))
  
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" px "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle px p2) ap2)) 6.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 Px) 0.5))
	  "_non" (setq p2< (SETQ P (POLAR P (- (angle px p2) ap2) 6.0)))
;	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (inters p1< (polar p1< (angle p1 px) 100) p2<  (polar p2< (angle p2 px) 100) nil)
	  "_non" (setq p p1<)
	  
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 px) ap2)) 22.5));piega1
	  "_NON" (setq p (POLAR P (ANGLE Px P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "S4S")
 )
(defun angolo4D (p1 px p2 sp)
  (COMMAND"_COLOR" "_BYLAYER" "_LAYER" "_M" "pannelli-Angoli" "")
  (if (null zpan) (setq zpan 0))
;;;  (if (null $ap1$) (cerca_pg_p1 p1 p2) (setq ap1 (- (angle p1 p2) $ap1$ (/ pi 2))))
  (setq $ap1$ nil)
;;;  (if (null $ap2$) (cerca_pg_p2 p1 p2) (setq ap2 (- $ap2$ (angle p2 p1) (* 1 (/ pi 2)))))
  (setq $ap2$ nil)
  (setq ap1 (/ pi 2)) (setq ap2 (/ pi 2))
    (setq p1< (polar p1 (+ (angle p1 px) (/ pi 4)) 0.707106))
  
;;;  (setq p p1 p1 p2 p2 p)
  (command"_pline" "_non" (list (car p1) (cadr p1) zpan) "_non" px "_non" p2
	  "_non" (setq p (polar p2 (setq app2 (+ (angle px p2) ap2)) 23));piega1
	  "_NON" (setq p (POLAR P (ANGLE P2 Px) 0.5))
	  "_non" (setq p2< (SETQ P (POLAR P (- (angle px p2) ap2) 22.5)))
;	  "_non" (SETQ P (POLAR P (angle p2 p1) (- (DISTANCE P1 P2) 1.0)))
	  "_non" (inters p1< (polar p1< (angle p1 px) 100) p2<  (polar p2< (angle p2 px) 100) nil)
	  "_non" (setq p p1<)
	  
	  "_non" (setq p (polar p (setq app2 (+ (angle p1 px) ap2)) 6.0));piega1
	  "_NON" (setq p (POLAR P (ANGLE Px P1) 0.5))
	  
  
	  
	  "_c"
  )
    (allega (entlast) "PANFIN" fpan)
   (allega (entlast) "PANRES" "B0")
  (setq entlastpan (entlast))
  (allega (entlast) "PANTIP" "S4D")
 )



(DEFUN RETROang (ENT)
  (SETQ POSPIEG '(60 100))
;;;  (SETQ POSPIEG '(0 0))
  (setq sr 0.3)
  (IF (OR (= MODO "S3D") (= MODO "S4D") (= MODO "S4S")) (setq sr 0.5))
  (SETQ MODO (LEGGI ENT "PANTIP"))
  (SETQ P1 (CDR (ASSOC '10 (ENTGET ENT))))
  (SETQ Px (CDR (ASSOC '10 (CDR (MEMBER (ASSOC '10 (ENTGET ENT)) (ENTGET ENT))))))
;;;  (SETQ Px (CDR (ASSOC '10 (CDR (MEMBER (ASSOC '10 (ENTGET ENT)) (ENTGET ENT))))))
  (setq lispt nil) (FOREACH N (ENTGET ent) (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))))
  (setq lispt (reverse lispt))
  (setq p1 (nth 0 lispt))
  (setq px (nth 1 lispt))
  (setq p2 (nth 2 lispt))
  
  (SETQ PR1 (POLAR P1 (ANGLE P1 Px) 1.0))
  (SETQ PR1 (POLAR PR1 (+ (ANGLE P1 Px) (/ PI 2)) (- 25.0 0)))
  (SETQ PR2 (POLAR P2 (ANGLE P2 Px) 1.0))
  (SETQ PR2 (POLAR PR2 (- (ANGLE P2 Px) (/ PI 2)) (- 25.0 0)))
  (setq prx (inters pr1 (polar pr1 (angle p1 px) 100) pr2 (polar pr2 (angle p2 px) 100) nil))
  (setq pr2< (polar pr2 (+ (angle pr2 prx) (/ pi 4)) (* sr 1.4142)))
  (setq pr1< (polar pr1 (- (angle pr1 prx) (/ pi 4)) (* sr 1.4142)))
  (setq prx< (inters pr1< (polar pr1< (angle pr1 prx) 100) pr2< (polar pr2< (angle pr2 prx) 100) nil))
  (setq pr1_ (polar pr1 (- (angle pr1 prx) (/ pi 2)) 10))
  (setq pr2_ (polar pr2 (+ (angle pr2 prx) (/ pi 2)) 10))
  (setq prx_ (inters pr1_ (polar pr1_ (angle pr1 prx) 100) pr2_ (polar pr2_ (angle pr2 prx) 100) nil))
  (setq pr1_< (polar pr1 (- (angle pr1 prx) (/ pi 2)) (+ sr 10)))
  (setq pr2_< (polar pr2 (+ (angle pr2 prx) (/ pi 2)) (+ sr 10)))
  (setq prx_< (inters pr1_< (polar pr1_< (angle pr1 prx) 100) pr2_< (polar pr2_< (angle pr2 prx) 100) nil))
  (COND ((= MODO "STD")
	 (COMMAND "_PLINE" "_NON" PR1)
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_)
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pp pf)
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
                  "_NON" PR2
		  "_NON" (SETQ P (POLAR PR2 (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.3)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.2)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (- (cadr pospieg) 0.42)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.42))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p pr1<)
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PRx) (/ PI 2)) 6.2))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PRx PR1) 0.3)))
		  

		  "_C")
	 )
((= MODO "SFD")
	 (COMMAND "_PLINE" "_NON" PR1)
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_)
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pp pf)
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
;;;		  "_NON" (SETQ P (POLAR P (- (ANGLE PRx PR2) 1.6057029) 20.0))
;;;		  "_NON" (SETQ P (POLAR P (- (- (ANGLE PRx PR2) 1.6057029)(/ PI 2)) 0.3))
;;;		  "_NON" (SETQ P (POLAR P (- (ANGLE PR2 PRx) 1.6057029 ) 19.69))
		    
                  "_NON" (setq p (polar PR2 (angle prx pr2) 26.3))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PRx PR2) 1.6057029) 20.0))
		  "_NON" (SETQ P (POLAR P (- (- (ANGLE PRx PR2) 1.6057029)(/ PI 2)) 0.3))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR2 PRx) 1.6057029 ) 19.69))
		  
;;;		  "_NON" (SETQ P (POLAR p (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
;;;		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.3)))
;;;		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.2))
		  )
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (- (+ 26.3 (cadr pospieg)) 0.42)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.42))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p pr1<)
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PRx) (/ PI 2)) 6.2))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PRx PR1) 0.3)))
		  

		  "_C")
	 )
	
((= MODO "SMD")
	 (COMMAND "_PLINE" "_NON" PR1)
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_)
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pp pf)
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
;;;		  "_NON" (SETQ P (POLAR P (- (ANGLE PRx PR2) 1.6057029) 20.0))
;;;		  "_NON" (SETQ P (POLAR P (- (- (ANGLE PRx PR2) 1.6057029)(/ PI 2)) 0.3))
;;;		  "_NON" (SETQ P (POLAR P (- (ANGLE PR2 PRx) 1.6057029 ) 19.69))
		    
                  "_NON" (setq p (polar PR2 (angle prx pr2) 0.5))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PRx PR2) (/ pi 2)) 20.0))
		  "_NON" (SETQ P (POLAR P (- (- (ANGLE PRx PR2) (/ pi 2))(/ PI 2)) 0.3))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR2 PRx) (/ pi 2) ) 19.7))
		  
;;;		  "_NON" (SETQ P (POLAR p (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
;;;		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.3)))
;;;		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.2))
		  )
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (- (+ 0 (cadr pospieg)) 0)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.42))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p pr1<)
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PRx) (/ PI 2)) 6.2))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PRx PR1) 0.3)))
		  

		  "_C")
	 )
	((= MODO "SMS")
	 (COMMAND "_PLINE" "_NON" (polar PR1 (angle prx pr1) 0.5))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_)
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pp pf)
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
                  "_NON" PR2
		  "_NON" (SETQ P (POLAR PR2 (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.3)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.2)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (- (cadr pospieg) 0.42)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.42))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p (polar pr1< (angle prx pr1) 0.5))
		  
		  "_NON" (SETQ P (POLAR P (+ (ANGLE PRx PR1) (/ pi 2) ) 20.0))
		  "_NON" (setq pp2 (SETQ P (POLAR P (+ (ANGLE PR1 PRx) (/ pi 2) (/ pi 2)) 0.3)))
		  

		  "_C")
	 )
	((= MODO "SFS")
	 (COMMAND "_PLINE" "_NON" (polar PR1 (angle prx pr1) 26.3))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_)
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pp pf)
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
                  "_NON" PR2
		  "_NON" (SETQ P (POLAR PR2 (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.3)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.2)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (- (cadr pospieg) 0.42)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.42))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p (polar pr1< (angle prx pr1) 26.3))
		  
		  "_NON" (SETQ P (POLAR P (+ (ANGLE PRx PR1) 1.6057029 ) 20.0))
		  "_NON" (setq pp2 (SETQ P (POLAR P (+ (ANGLE PR1 PRx) (/ pi 2) 1.6057029) 0.3)))
		  

		  "_C")
	 )
((= MODO "S2D")
	 (COMMAND "_PLINE" "_NON" (POLAR PR1 (ANGLE PRX PR1) 0.5))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_)
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pp pf)
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
;;;		  "_NON" (SETQ P (POLAR P (- (ANGLE PRx PR2) 1.6057029) 20.0))
;;;		  "_NON" (SETQ P (POLAR P (- (- (ANGLE PRx PR2) 1.6057029)(/ PI 2)) 0.3))
;;;		  "_NON" (SETQ P (POLAR P (- (ANGLE PR2 PRx) 1.6057029 ) 19.69))
		    
                  "_NON" (setq p (polar PR2 (angle prx pr2) 26.3))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PRx PR2) 1.6057029) 20.0))
		  "_NON" (SETQ P (POLAR P (- (- (ANGLE PRx PR2) 1.6057029)(/ PI 2)) 0.3))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR2 PRx) 1.6057029 ) 19.69))
		  
;;;		  "_NON" (SETQ P (POLAR p (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
;;;		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.3)))
;;;		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.2))
		  )
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (- (+ 26.3 (cadr pospieg)) 0.42)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.42))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p (POLAR pr1< (ANGLE PRX PR1) 0.5))
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PRx) (/ PI 2)) 19.7))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PRx PR1) 0.3)))
		  

		  "_C")
	 )	
	((= MODO "S2S")
	 (COMMAND "_PLINE" "_NON" (polar PR1 (angle prx pr1) 26.3))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_)
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pp pf)
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
                  "_NON" (SETQ P (POLAR PR2 (ANGLE PRX PR2) 0.5))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PRx PR2) (/ PI 2)) 20))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.3)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 19.7)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (- (cadr pospieg) 0.42)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.42))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p (polar pr1< (angle prx pr1) 26.3))
		  
		  "_NON" (SETQ P (POLAR P (+ (ANGLE PRx PR1) 1.6057029 ) 20.0))
		  "_NON" (setq pp2 (SETQ P (POLAR P (+ (ANGLE PR1 PRx) (/ pi 2) 1.6057029) 0.3)))
		  

		  "_C")
	 )
	((= MODO "S3D")
	 (COMMAND "_PLINE" "_NON" (POLAR PR1 (ANGLE PRX PR1) 1.0))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_)
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pp pf)
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
                  "_NON" (SETQ PP (POLAR PR2 (ANGLE PRX PR2) 1.0))
		  "_NON" (SETQ P (POLAR PP (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.5)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.0)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (+ (cadr pospieg) 0.29)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.41 0.30))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p (POLAR pr1< (ANGLE PRX PR1) 1.0))
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PRx) (/ PI 2)) 6.0))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PRx PR1) 0.5)))
		  

		  "_C")
	 )
	((= MODO "S4D")
	 (COMMAND "_PLINE" "_NON" (POLAR PR1 (ANGLE PRX PR1) 0))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_)
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pp pf)
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
                  "_NON" (SETQ PP (POLAR PR2 (ANGLE PRX PR2) 0.5))
		  "_NON" (SETQ P (POLAR PP (- (ANGLE PRx PR2) (/ PI 2)) 20.0))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.5)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 19.5)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (+ (cadr pospieg) -0.21)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.41 0.30))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p (POLAR pr1< (ANGLE PRX PR1) 0))
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PRx) (/ PI 2)) 6.0))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PRx PR1) 0.5)))
		  

		  "_C")
	 )
	
	((= MODO "S4S")
	 (COMMAND "_PLINE" "_NON" (POLAR PR1 (ANGLE PRX PR1) 0.5))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) )
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_)
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pp pf)
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
                  "_NON" (SETQ PP (POLAR PR2 (ANGLE PRX PR2) 0.0))
		  "_NON" (SETQ P (POLAR PP (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.5)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.0)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (+ (cadr pospieg) -1.0 0.29)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.41 0.30))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p (POLAR pr1< (ANGLE PRX PR1) 0.5))
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PRx) (/ PI 2)) 19.5))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PRx PR1) 0.5)))
		  

		  "_C")
	 )
	
	
	
	
	)
;  (ALLEGA ENT "PANRETRO" (CDR (ASSOC '5 (ENTGET (ENTLAST)))))
  (ALLEGA ENT "PANRETRO" (strcat "(\"" (cdr assoc '5 (entget ent)) "\" \"" (CDR (ASSOC '5 (ENTGET (ENTLAST)))) "\")"))
  
  (COMMAND"_LAYER" "_N" "pan-Retro" "")
  (command"_chprop" (entlast) "" "_la" "pan-Retro" "")
  )


(DEFUN RETROpan (ENT)
  (IF (AND (ENTGET ENT) (SETQ TMP (ENTGET ENT (LIST "PANRETRO"))) (SETQ TMP (ASSOC -3 TMP)))
    (progn
      (SETQ HAND (LEGGI ENT "PANRETRO"))
      (if (= 'LIST (type (read hand)))
	(if (= (cdr (assoc '5 (entget ent))) (car (read hand)))
	  (IF (HANDENT (cadr (read hand)))
	    (COMMAND"_ERASE" (HANDENT (cadr (read hand))) "")
	    )
	  )
	(if (handent hand)
	  (command"_erase" (handent hand) "")
	  )
	)
      )
    )
  (SETQ POSPIEG '(60 100))
  (SETQ POSPIEG (READ (LEGGI ENT "PANPIEG")))
  (setq sr 0.3)
  (IF (OR (= MODO "S3D") (= MODO "S4D") (= MODO "S4S"))
    (setq sr 0.5)
    )
  (SETQ MODO (LEGGI ENT "PANTIP"))
  (SETQ P1 (CDR (ASSOC '10 (ENTGET ENT))))
  (SETQ Px (CDR (ASSOC '10 (CDR (MEMBER (ASSOC '10 (ENTGET ENT)) (ENTGET ENT))))))
  (setq lispt nil) (FOREACH N (ENTGET ent) (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))))
  (setq lispt (reverse lispt))
  (setq p2 (nth 1 lispt))
  (setq grr (ssget "_f" (list (setq pk (polar p1 (angle p1 p2) (/ (distance p1 p2) 2))) (polar pk (+ (angle p1 p2) (/ pi 2)) 26)) (list '(0 . "LWPOLYLINE") '(8 . "pan-Retro"))))
  (if grr
    (command"_erase" grr "")
    )
  (if (/= "pannelli-dritti" (cdr (assoc '8 (entget ent))))
(progn  
  (SETQ Px (CDR (ASSOC '10 (CDR (MEMBER (ASSOC '10 (ENTGET ENT)) (ENTGET ENT))))))
  (setq lispt nil) (FOREACH N (ENTGET ent) (IF (= '10 (CAR N)) (SETQ LISPT (CONS (CDR N) LISPT))))
  (setq lispt (reverse lispt))
		  
  (setq p1 (nth 0 lispt))
  (setq px (nth 1 lispt))
  (setq p2 (nth 2 lispt))
  
  (SETQ PR1 (POLAR P1 (ANGLE P1 Px) 1.0))
  (SETQ PR1 (POLAR PR1 (+ (ANGLE P1 Px) (/ PI 2)) (- 25.0 0)))
  (SETQ PR2 (POLAR P2 (ANGLE P2 Px) 1.0))
  (SETQ PR2 (POLAR PR2 (- (ANGLE P2 Px) (/ PI 2)) (- 25.0 0)))
  (setq prx (inters pr1 (polar pr1 (angle p1 px) 100) pr2 (polar pr2 (angle p2 px) 100) nil))
  (setq pr2< (polar pr2 (+ (angle pr2 prx) (/ pi 4)) (* sr 1.4142)))
  (setq pr1< (polar pr1 (- (angle pr1 prx) (/ pi 4)) (* sr 1.4142)))
  (setq prx< (inters pr1< (polar pr1< (angle pr1 prx) 100) pr2< (polar pr2< (angle pr2 prx) 100) nil))
  (setq pr1_ (polar pr1 (- (angle pr1 prx) (/ pi 2)) 10))
  (setq pr2_ (polar pr2 (+ (angle pr2 prx) (/ pi 2)) 10))
  (setq prx_ (inters pr1_ (polar pr1_ (angle pr1 prx) 100) pr2_ (polar pr2_ (angle pr2 prx) 100) nil))
  (setq pr1_< (polar pr1 (- (angle pr1 prx) (/ pi 2)) (+ sr 10)))
  (setq pr2_< (polar pr2 (+ (angle pr2 prx) (/ pi 2)) (+ sr 10)))
  (setq prx_< (inters pr1_< (polar pr1_< (angle pr1 prx) 100) pr2_< (polar pr2_< (angle pr2 prx) 100) nil))
)
    (progn
  ;(setq *error* nil)
  (setq p1 (nth 0 lispt))
  (setq px (nth 1 lispt))
  (setq p2 (nth 1 lispt))
  (setq px (polar p1 (angle p1 p2) (/ (distance p1 p2) 2)))
  (SETQ PR1 (POLAR P1 (ANGLE P1 Px) 1.0))
  (SETQ PR1 (POLAR PR1 (+ (ANGLE P1 Px) (/ PI 2)) (- 25.0 0)))
  (SETQ PR2 (POLAR P2 (ANGLE P2 Px) 1.0))
  (SETQ PR2 (POLAR PR2 (- (ANGLE P2 Px) (/ PI 2)) (- 25.0 0)))
  ;(setq prx (inters pr1 (polar pr1 (angle p1 px) 100) pr2 (polar pr2 (angle p2 px) 100) nil))
  (setq prx (polar pr1 (angle pr1 pr2) (/ (distance pr1 pr2) 2)))
  
  (setq pr2< (polar pr2 (+ (angle pr2 prx) (/ pi 4)) (* sr 1.4142)))
  (setq pr1< (polar pr1 (- (angle pr1 prx) (/ pi 4)) (* sr 1.4142)))
  ;(setq prx< (inters pr1< (polar pr1< (angle pr1 prx) 100) pr2< (polar pr2< (angle pr2 prx) 100) nil))
  (setq prx< (polar pr1< (angle pr1< pr2<) (/ (distance pr1< pr2<) 2)))

  (setq pr1_ (polar pr1 (- (angle pr1 prx) (/ pi 2)) 10))
  (setq pr2_ (polar pr2 (+ (angle pr2 prx) (/ pi 2)) 10))
 ; (setq prx_ (inters pr1_ (polar pr1_ (angle pr1 prx) 100) pr2_ (polar pr2_ (angle pr2 prx) 100) nil))
    (setq prx_ (polar pr1_ (angle pr1_ pr2_) (/ (distance pr1_ pr2_) 2)))

  (setq pr1_< (polar pr1 (- (angle pr1 prx) (/ pi 2)) (+ sr 10)))
  (setq pr2_< (polar pr2 (+ (angle pr2 prx) (/ pi 2)) (+ sr 10)))
  ;(setq prx_< (inters pr1_< (polar pr1_< (angle pr1 prx) 100) pr2_< (polar pr2_< (angle pr2 prx) 100) nil))
      (setq prx_< (polar pr1_< (angle pr1_< pr2_<) (/ (distance pr1_< pr2_<) 2)))
)
    )
  (COND ((= MODO "STD")
	 (COMMAND "_PLINE" "_NON" (setq pquota1 PR1))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pquota2 (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) ))
			   "_non" (setq pquota3 (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421)))
			   "_non" (setq pquotaang (setq pp prx_))
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pquota6 (setq pp pf))
			   )
	   (command "_non" (setq pquota3 prx))
			   
	   )
	 
	          (command 
   		  ;"_non" prx
                  "_NON" (setq pquota7 PR2)
		  "_NON" (SETQ P (POLAR PR2 (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.3)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.2)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (- (cadr pospieg) 0.42)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.42))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p pr1<)
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PRx) (/ PI 2)) 6.2))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PRx PR1) 0.3)))
		  

		  "_C")
	 )
((= MODO "SFD")
	 (COMMAND "_PLINE" "_NON" (setq pquota1 PR1))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pquota2 (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) ))
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pquotaang (setq pp prx_))
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pquota6 (setq pp pf))
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
;;;		  "_NON" (SETQ P (POLAR P (- (ANGLE PRx PR2) 1.6057029) 20.0))
;;;		  "_NON" (SETQ P (POLAR P (- (- (ANGLE PRx PR2) 1.6057029)(/ PI 2)) 0.3))
;;;		  "_NON" (SETQ P (POLAR P (- (ANGLE PR2 PRx) 1.6057029 ) 19.69))
		    
                  "_NON" (setq pquota7 (setq p (polar PR2 (angle prx pr2) 26.3)))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PRx PR2) 1.6057029) 20.0))
		  "_NON" (SETQ P (POLAR P (- (- (ANGLE PRx PR2) 1.6057029)(/ PI 2)) 0.3))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR2 PRx) 1.6057029 ) 19.69))
		  
;;;		  "_NON" (SETQ P (POLAR p (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
;;;		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.3)))
;;;		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.2))
		  )
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (- (+ 26.3 (cadr pospieg)) 0.42)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.42))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p pr1<)
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PRx) (/ PI 2)) 6.2))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PRx PR1) 0.3)))
		  

		  "_C")
	 )
	
((= MODO "SMD")
	 (COMMAND "_PLINE" "_NON" (setq pquota1 PR1))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pquota2 (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) ))
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pquotaang (setq pp prx_))
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pquota6 (setq pp pf))
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
;;;		  "_NON" (SETQ P (POLAR P (- (ANGLE PRx PR2) 1.6057029) 20.0))
;;;		  "_NON" (SETQ P (POLAR P (- (- (ANGLE PRx PR2) 1.6057029)(/ PI 2)) 0.3))
;;;		  "_NON" (SETQ P (POLAR P (- (ANGLE PR2 PRx) 1.6057029 ) 19.69))
		    
                  "_NON" (setq pquota7 (setq p (polar PR2 (angle prx pr2) 0.5)))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PRx PR2) (/ pi 2)) 20.0))
		  "_NON" (SETQ P (POLAR P (- (- (ANGLE PRx PR2) (/ pi 2))(/ PI 2)) 0.3))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR2 PRx) (/ pi 2) ) 19.7))
		  
;;;		  "_NON" (SETQ P (POLAR p (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
;;;		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.3)))
;;;		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.2))
		  )
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (- (+ 0 (cadr pospieg)) 0)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.42))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p pr1<)
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PRx) (/ PI 2)) 6.2))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PRx PR1) 0.3)))
		  

		  "_C")
	 )
	((= MODO "SMS")
	 (COMMAND "_PLINE" "_NON" (setq pquota1 (polar PR1 (angle prx pr1) 0.5)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pquota2 (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) ))
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pquotaang (setq pp prx_))
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pquota6 (setq pp pf))
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
                  "_NON" (setq pquota7 PR2)
		  "_NON" (SETQ P (POLAR PR2 (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.3)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.2)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (- (cadr pospieg) 0.42)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.42))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p (polar pr1< (angle prx pr1) 0.5))
		  
		  "_NON" (SETQ P (POLAR P (+ (ANGLE PRx PR1) (/ pi 2) ) 20.0))
		  "_NON" (setq pp2 (SETQ P (POLAR P (+ (ANGLE PR1 PRx) (/ pi 2) (/ pi 2)) 0.3)))
		  

		  "_C")
	 )
	((= MODO "SFS")
	 (COMMAND "_PLINE" "_NON" (setq pquota1 (polar PR1 (angle prx pr1) 26.3)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pquota2 (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) ))
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pquotaang (setq pp prx_))
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pquota6 (setq pp pf))
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
                  "_NON" (setq pquota7 PR2)
		  "_NON" (SETQ P (POLAR PR2 (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.3)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.2)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (- (cadr pospieg) 0.42)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.42))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p (polar pr1< (angle prx pr1) 26.3))
		  
		  "_NON" (SETQ P (POLAR P (+ (ANGLE PRx PR1) 1.6057029 ) 20.0))
		  "_NON" (setq pp2 (SETQ P (POLAR P (+ (ANGLE PR1 PRx) (/ pi 2) 1.6057029) 0.3)))
		  

		  "_C")
	 )
((= MODO "S2D")
	 (COMMAND "_PLINE" "_NON" (setq pquota1 (POLAR PR1 (ANGLE PRX PR1) 0.5)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pquota2 (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) ))
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pquotaang (setq pp prx_))
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pquota6 (setq pp pf))
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
;;;		  "_NON" (SETQ P (POLAR P (- (ANGLE PRx PR2) 1.6057029) 20.0))
;;;		  "_NON" (SETQ P (POLAR P (- (- (ANGLE PRx PR2) 1.6057029)(/ PI 2)) 0.3))
;;;		  "_NON" (SETQ P (POLAR P (- (ANGLE PR2 PRx) 1.6057029 ) 19.69))
		    
                  "_NON" (setq pquota7 (setq p (polar PR2 (angle prx pr2) 26.3)))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PRx PR2) 1.6057029) 20.0))
		  "_NON" (SETQ P (POLAR P (- (- (ANGLE PRx PR2) 1.6057029)(/ PI 2)) 0.3))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR2 PRx) 1.6057029 ) 19.69))
		  
;;;		  "_NON" (SETQ P (POLAR p (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
;;;		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.3)))
;;;		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.2))
		  )
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (- (+ 26.3 (cadr pospieg)) 0.42)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.42))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p (POLAR pr1< (ANGLE PRX PR1) 0.5))
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PRx) (/ PI 2)) 19.7))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PRx PR1) 0.3)))
		  

		  "_C")
	 )	
	((= MODO "S2S")
	 (COMMAND "_PLINE" "_NON" (setq pquota1 (polar PR1 (angle prx pr1) 26.3)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pquota2 (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) ))
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pquotaang (setq pp prx_))
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pquota6 (setq pp pf))
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
                  "_NON" (setq pquota7 (SETQ P (POLAR PR2 (ANGLE PRX PR2) 0.5)))
		  "_NON" (SETQ P (POLAR P (- (ANGLE PRx PR2) (/ PI 2)) 20))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.3)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 19.7)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (- (cadr pospieg) 0.42)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.42))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p (polar pr1< (angle prx pr1) 26.3))
		  
		  "_NON" (SETQ P (POLAR P (+ (ANGLE PRx PR1) 1.6057029 ) 20.0))
		  "_NON" (setq pp2 (SETQ P (POLAR P (+ (ANGLE PR1 PRx) (/ pi 2) 1.6057029) 0.3)))
		  

		  "_C")
	 )
	((= MODO "S3D")
	 (COMMAND "_PLINE" "_NON" (setq pquota1 (POLAR PR1 (ANGLE PRX PR1) 1.0)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pquota2 (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) ))
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pquotaang (setq pp prx_))
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pquota6 (setq pp pf))
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
                  "_NON" (setq pquota7 (SETQ PP (POLAR PR2 (ANGLE PRX PR2) 1.0)))
		  "_NON" (SETQ P (POLAR PP (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.5)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.0)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (+ (cadr pospieg) 0.29)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.41 0.30))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p (POLAR pr1< (ANGLE PRX PR1) 1.0))
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PRx) (/ PI 2)) 6.0))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PRx PR1) 0.5)))
		  

		  "_C")
	 )
	((= MODO "S4D")
	 (COMMAND "_PLINE" "_NON" (setq pquota1 (POLAR PR1 (ANGLE PRX PR1) 0)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pquota2 (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) ))
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pquotaang (setq pp prx_))
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pquota6 (setq pp pf))
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
                  "_NON" (setq pquota7 (SETQ PP (POLAR PR2 (ANGLE PRX PR2) 0.5)))
		  "_NON" (SETQ P (POLAR PP (- (ANGLE PRx PR2) (/ PI 2)) 20.0))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.5)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 19.5)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (+ (cadr pospieg) -0.21)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.41 0.30))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p (POLAR pr1< (ANGLE PRX PR1) 0))
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PRx) (/ PI 2)) 6.0))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PRx PR1) 0.5)))
		  

		  "_C")
	 )
	
	((= MODO "S4S")
	 (COMMAND "_PLINE" "_NON" (setq pquota1 (POLAR PR1 (ANGLE PRX PR1) 0.5)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pquota2 (setq pp (polar pr1 (angle pr1 prx) (car pospieg)) ))
			   "_non" (setq pp (polar pp (- (angle pr1 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pquotaang (setq pp prx_))
			   "_non" (polar (setq pf (polar pr2 (angle pr2 prx) (cadr pospieg))) (+ (angle pr2 prx) (/ pi 4)) 14.1421)
			   "_non" (setq pquota6 (setq pp pf))
			   ;"_non" (setq pp (polar pp (angle prx pr2) (- (distance prx pr2) 20 (car pospieg) (cadr pospieg))))
			   )
	   (command "_non" prx)
			   
	   )
	 
	          (command 
   		;  "_non" prx
                  "_NON" (setq pquota7 (SETQ PP (POLAR PR2 (ANGLE PRX PR2) 0.0)))
		  "_NON" (SETQ P (POLAR PP (- (ANGLE PRx PR2) (/ PI 2)) 6.5))
		  "_NON" (setq pp1 (SETQ P (POLAR P (ANGLE P2 Px) 0.5)))
		  "_NON" (SETQ p (POLAR P (+ (ANGLE PRx PR2) (/ PI 2)) 6.0)))
	 (if (NOT (EQUAL '(0 0) POSPIEG))
	          (command "_non" (setq pp (polar p (angle pr2 prx) (+ (cadr pospieg) -1.0 0.29)) )
			   "_non" (setq pp (polar pp (+ (angle pr2 prx) (/ pi 4)) 14.1421))
			   "_non" (setq pp prx_<)
			   "_non" (setq pp (polar (setq pf (polar pr1< (angle pr1 prx) (- (car pospieg) 0.41 0.30))) (- (angle pr1 prx) (/ pi 4)) 14.1421))
	                   "_non" pf
			   )
	   (command "_non" prx<)
			   
	   )
	 (command
			   
			   
	   "_non" (setq p (POLAR pr1< (ANGLE PRX PR1) 0.5))
		  
		  "_NON" (SETQ P (POLAR P (- (ANGLE PR1 PRx) (/ PI 2)) 19.5))
		  "_NON" (setq pp2 (SETQ P (POLAR P (ANGLE PRx PR1) 0.5)))
		  

		  "_C")
	 )
	
	
	
	
	)
 ; (ALLEGA ENT "PANRETRO" (CDR (ASSOC '5 (ENTGET (ENTLAST)))))
  (ALLEGA ENT "PANRETRO" (strcat "(\"" (cdr (assoc '5 (entget ent))) "\" \"" (CDR (ASSOC '5 (ENTGET (ENTLAST)))) "\")"))
  (COMMAND"_LAYER" "_N" "pan-Retro" "")
  (command"_chprop" (entlast) "" "_la" "pan-Retro" "")
  (setq ent_qret ent)
  )
(DEFUN C:RETROPAN ()
  (SETQ ENT (CAR (ENTSEL)))
  (RETROPAN ENT)
  (LANAPAN ENT (ENTLAST))
  )


(defun c:tipob15 ()
  (setvar "pickstyle" 0)
  (setq gr (ssget (list '(8 . "pannelli-*"))))
  (setq nr 0)
  (while (and gr (setq ent (ssname gr nr)))
    (allega ent "PANRES" "B15")
    (setq nr (+ 1 nr))
  )
  (setvar "pickstyle" 1)
)
(defun c:tipob0 ()
  (setvar "pickstyle" 0)
  (setq gr (ssget (list '(8 . "pannelli-*"))))
  (setq nr 0)
  (while (and gr (setq ent (ssname gr nr)))
    (allega ent "PANRES" "B0")
    (setq nr (+ 1 nr))
  )
  (setvar "pickstyle" 1)
)