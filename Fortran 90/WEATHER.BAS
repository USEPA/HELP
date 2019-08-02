DECLARE SUB FILECHK (fdrive$, fpath$, filen$, found!)
DECLARE SUB LASTPAGE ()
DECLARE SUB CLIMFILES (d4edit$, D7EDIT$, D13EDIT$, D11EDIT$, FILEPATH$())
DECLARE SUB CLIMSAVE (FILEPATH$())
DECLARE SUB EVAPIND (SELECCT$, SELECST$, fromsub!)
DECLARE SUB GENCLIMATE ()
DECLARE SUB CLIMATE (SELECST$, SELECCT$, CLIM$)
DECLARE SUB ACELL (IROW!, ISTART!, NS!, TEXT$, ITYPE!)
DECLARE SUB BOX ()
DECLARE SUB EXOUT (ANSCODE!)
DECLARE SUB GETDIR (ext$, SELECT$, directory$)
DECLARE SUB HLPMSG (F2HELP$)
DECLARE SUB ITEMSELECT (FIRSTROW!, NUMINCOL!, TOTNUM!, states$(), SELECT$, ITEMNUM!)
DECLARE SUB menu ()
DECLARE SUB USERHELP ()

COMMON SHARED tempu$, precu$, solu$, UNITS$, UNIT$, UN1$, UN2$, UN3$, UN4$, UN5$
COMMON SHARED unev$, UNSL$, TOHELP%, NL%, CLIMATF$, irtrn%, PAGE%, CLIM
COMMON SHARED APAGE%, VPAGE%, HPAGE%, DIRECT$, SELECCT$, SELECST$
COMMON SHARED GROW1$, GROW2$, MAXLAI$, EVBARE$, EVFAIR$, EVEXCL$
COMMON SHARED evdepth$, LAI$, GR1$, GR2$, AREA$, NYEARS$, latitude$
COMMON SHARED HUMID1$, HUMID2$, HUMID3$, HUMID4$, WIND$, LATIT$, meansel
COMMON SHARED WSPEED$, BARE$, FAIR$, EXCL$, UNI$, ETCITY$, ETSTATE$, etunits$
COMMON SHARED defrain, deftemp, cmeth(), cunit(), ccity$(), cstate$(), ct$, st$, FILEPATH$()
COMMON SHARED SROWBOX, SCOLBOX, BOXWIDTH, EROWBOX, defp(), deft()

DIM deft(12), defp(12), TM$(12), RM$(12), cmeth(3), cunit(3, 10), ccity$(3), cstate$(3), FILEPATH$(4)

VIEW PRINT 1 TO 25
   APAGE% = 0: VPAGE% = 1: HPAGE% = 2
   SCREEN 0, , VPAGE%, VPAGE%
   KEY OFF: COLOR 15, 1: CLS
   'SHELL "CHKDSK"
'---------------------
' SET DEFAULT VALUES
'---------------------
   UNITS$ = "1"
   UN1$ = "    (IN)   ": UN2$ = "  (IN/SEC)   ": UN3$ = "   (FT)   "
   UN4$ = "  (IN/YR)   ": UN5$ = "    (#/ACRE)    "
   unev$ = "IN"
   tempu$ = "    (๘F)   ": precu$ = "  (IN)   ": solu$ = "  (LANGLEYS)  "
	 IF DIR$("*.TMP") <> "" THEN KILL "*.TMP"
20000    CLOSE
	 CALL CLIMFILES(d4edit$, D7EDIT$, D13EDIT$, D11EDIT$, FILEPATH$())
	 CLOSE
	 IF irtrn% = 1000 OR irtrn% = -1 THEN GOTO 70007
	 GOSUB 60000
	 IF irtrn% = 1 THEN GOTO 20002
20002    CLOSE
	 IF d4edit$ <> "" THEN GOSUB 40000
	 IF D7EDIT$ <> "" THEN GOSUB 45000
	 IF D13EDIT$ <> "" THEN GOSUB 47500
	 IF D11EDIT$ <> "" THEN GOSUB 50000
20025    CLOSE
	 CALL GENCLIMATE
	 CLOSE
	 IF irtrn% = 2 THEN GOTO 80008
	 IF irtrn% = 1000 THEN GOTO 70007
	 IF irtrn% = 1 OR irtrn% = -1 THEN GOTO 30003
30003    CLOSE
	 CALL CLIMATE(SELECST$, SELECCT$, CLIM$)
	 CLOSE
	 IF irtrn% = -1 OR irtrn% = 1 OR irtrn% = 100 THEN GOTO 20025
	 IF irtrn% = 2 THEN GOTO 80008
	 IF irtrn% = 1000 THEN GOTO 70007
80008    CLOSE
	 CALL LASTPAGE

	 IF irtrn% = -1 THEN GOTO 30003
	 IF irtrn% = 1 THEN GOTO 20025
	 IF irtrn% = 1000 THEN GOTO 70007
	 CLOSE
70007    IF DIR$("*.TMP") <> "" THEN KILL "*.TMP"
	 CLEAR
   APAGE% = 0: VPAGE% = 1: HPAGE% = 2
   SCREEN 0, , VPAGE%, VPAGE%
   KEY OFF: COLOR 15, 1: CLS
   APAGE% = 0: VPAGE% = 1: HPAGE% = 2
   SCREEN 0, , APAGE%, APAGE%
   KEY OFF: COLOR 15, 1: CLS
   APAGE% = 0: VPAGE% = 1: HPAGE% = 2
   SCREEN 0, , HPAGE%, HPAGE%
   KEY OFF: COLOR 15, 1: CLS
	 END
'------------------------------------
'READ D4EDIT$+".D4" FILE IF EDIT FILE
'HAS BEEN SELECTED
'------------------------------------
40000  a$ = DIR$(d4edit$)
       IF a$ <> "" THEN
	    OPEN d4edit$ FOR INPUT AS #1
	    LINE INPUT #1, a$: cmeth(1) = VAL(LTRIM$(RTRIM$(a$)))
	    LINE INPUT #1, a$: cunit(1, cmeth(1)) = VAL(LTRIM$(RTRIM$(a$)))
	    cunit(1, 3) = cunit(1, cmeth(1))
	    LINE INPUT #1, a$
	      ccity$(1) = LTRIM$(RTRIM$(LEFT$(a$, 20)))
	      cstate$(1) = LTRIM$(RTRIM$(MID$(a$, 21, 20)))
	    CLOSE #1
       END IF
   RETURN
'------------------------------------
'READ D7EDIT$ FILE IF EDIT FILE
'HAS BEEN SELECTED
'------------------------------------
45000  a$ = DIR$(D7EDIT$)
       IF a$ <> "" THEN
	    OPEN D7EDIT$ FOR INPUT AS #1
	    LINE INPUT #1, a$: cmeth(2) = VAL(LTRIM$(RTRIM$(a$)))
	    LINE INPUT #1, a$: cunit(2, cmeth(2)) = VAL(LTRIM$(RTRIM$(a$)))
	    cunit(2, 3) = cunit(2, cmeth(2))
	    LINE INPUT #1, a$
		 ccity$(2) = LTRIM$(RTRIM$(LEFT$(a$, 20)))
		 cstate$(2) = LTRIM$(RTRIM$(MID$(a$, 21, 20)))
	    CLOSE #1
       END IF
       RETURN
'---------------------------------------
'READ D13EDIT$+".D13" FILE IF EDIT FILE
'HAS BEEN SELECTED (read only header lines)
'---------------------------------------
47500  a$ = DIR$(D13EDIT$)
       IF a$ <> "" THEN
	    OPEN D13EDIT$ FOR INPUT AS #1
	    LINE INPUT #1, a$: cmeth(3) = VAL(LTRIM$(RTRIM$(a$)))
	    LINE INPUT #1, a$: cunit(3, cmeth(3)) = VAL(LTRIM$(RTRIM$(a$)))
	    cunit(3, 3) = cunit(3, cmeth(3))
	    LINE INPUT #1, a$
		 ccity$(3) = LTRIM$(RTRIM$(LEFT$(a$, 20)))
		 cstate$(3) = LTRIM$(RTRIM$(MID$(a$, 21, 20)))
	    CLOSE #1
       END IF
       RETURN
'------------------------------------
'READ D11EDIT$+".D11" FILE IF EDIT FILE
'HAS BEEN SELECTED
'------------------------------------
50000    a$ = DIR$(D11EDIT$)
	 IF a$ <> "" THEN
	    OPEN D11EDIT$ FOR INPUT AS #1
	    LINE INPUT #1, a$: etunits$ = LTRIM$(RTRIM$(LEFT$(a$, 2)))
	    LINE INPUT #1, a$
		ETCITY$ = LTRIM$(RTRIM$(LEFT$(a$, 20)))
		ETSTATE$ = LTRIM$(RTRIM$(MID$(a$, 21, 20)))
	    LINE INPUT #1, a$
		 LATIT$ = LTRIM$(RTRIM$(LEFT$(a$, 10)))
		 GR1$ = LTRIM$(RTRIM$(MID$(a$, 11, 4)))
		 GR2$ = LTRIM$(RTRIM$(MID$(a$, 15, 4)))
		 LAI$ = LTRIM$(RTRIM$(MID$(a$, 20, 7)))
		 evdepth$ = LTRIM$(RTRIM$(MID$(a$, 27, 7)))
		 WIND$ = LTRIM$(RTRIM$(MID$(a$, 34, 5)))
		 HUMID1$ = LTRIM$(RTRIM$(MID$(a$, 39, 5)))
		 HUMID2$ = LTRIM$(RTRIM$(MID$(a$, 44, 5)))
		 HUMID3$ = LTRIM$(RTRIM$(MID$(a$, 49, 5)))
		 HUMID4$ = LTRIM$(RTRIM$(MID$(a$, 54, 5)))
	 END IF
	RETURN
'------------------------------
'COPY EDIT FILES INTO TEMPORARY
'FILES WITH EXTENSIONS "TMP"
'------------------------------
60000   IF d4edit$ <> "" OR D7EDIT$ <> "" OR D13EDIT$ <> "" OR D11EDIT$ <> "" THEN
	   CLS
	   LOCATE 10, 35: PRINT "Please Wait..."
	ELSE
	   RETURN
	END IF
	IF d4edit$ <> "" THEN
	    OPEN d4edit$ FOR INPUT AS #1
	    LOCATE 12, 17: PRINT "COPYING PRECIPITATION DATA TO A WORKING FILE"
	    OPEN "DATA4.TMP" FOR OUTPUT AS #2
	    FOR I = 1 TO 3704
	      IF EOF(1) THEN CLOSE : GOTO 60500
	      LINE INPUT #1, l$
	      PRINT #2, l$
	    NEXT I
	END IF
60500   CLOSE
	IF D7EDIT$ <> "" THEN
	    OPEN D7EDIT$ FOR INPUT AS #1
	    LOCATE 13, 17: PRINT "COPYING TEMPERATURE DATA TO A WORKING FILE"
	    OPEN "DATA7.TMP" FOR OUTPUT AS #2
	    FOR I = 1 TO 3704
	      IF EOF(1) THEN CLOSE : GOTO 60510
	      LINE INPUT #1, l$
	      PRINT #2, l$
	    NEXT I
	END IF
60510   CLOSE
	IF D13EDIT$ <> "" THEN
	    OPEN D13EDIT$ FOR INPUT AS #1
	    LOCATE 14, 17: PRINT "COPYING SOLAR RADIATION DATA TO A WORKING FILE"
	    OPEN "DATA13.TMP" FOR OUTPUT AS #2
	    FOR I = 1 TO 3704
	      IF EOF(1) THEN CLOSE : GOTO 60520
	      LINE INPUT #1, l$
	      PRINT #2, l$
	    NEXT I
	END IF
60520  CLOSE
	IF D11EDIT$ <> "" THEN
	    OPEN D11EDIT$ FOR INPUT AS #1
	    LOCATE 15, 17: PRINT "COPYING EVAPOTRANSPIRATION DATA TO A WORKING FILE"
	    OPEN "DATA11.TMP" FOR OUTPUT AS #2
	    FOR I = 1 TO 3704
	      IF EOF(1) THEN GOTO 60530
	      LINE INPUT #1, l$
	      PRINT #2, l$
	    NEXT I
	END IF
60530   CLOSE
     RETURN
   END

SUB CLIMFILES (d4edit$, D7EDIT$, D13EDIT$, D11EDIT$, FILEPATH$())
   DIM DATAFILE$(4), locline(4), ext$(4), fileok(4)
'------------------------------------------------------------------
'THIS ROUTINE IS USED TO ENTER THE EDIT FILE NAMES OF THE WEATHER
'DATA FILES.  IT ALLOWS A SELECTION OF A FILE NAME FROM THE LIST
'FILES WITH THE SEPCIFIC EXTENSIONS.
'  D4            PRECIPITATION
'  D7            TEMPERATURE
'  D11           ET
'  D13           SOLAR RADIATION
'DEFINITION OF VARIABLEs..................................
'  DATAFILE$()   ARRAY OF FILE NAMES
'  FILEPATH$()   ARRAY FO PATH TO DATA FILE
'  NLINES        NUMBER OF LINES (FILES)
'  LOLINE()      ROW LOCATION OF ENTRY ON SCREEN
'  EXT$()        FILE EXTENSION FOR EACH WEATHER FILE
'  FILEOK()      = 0 IF FILE IS NOT GOOD, = 1 IF FILE IS OK
'----------------------------------------------------------------
   '------------------------------------
   'ASSIGN CURRENT DIRECTORY AS DEFAULT
   'FOR ALL FILE PATHS
   '------------------------------------
   BLANK$ = "                                                        "
   DEFDIR$ = CURDIR$
   DIRECT$ = CURDIR$
   nlines = 4
   ext$(1) = "D4"
   ext$(2) = "D7"
   ext$(3) = "D13"
   ext$(4) = "D11"
   FOR I = 1 TO nlines
      FILEPATH$(I) = DIRECT$
      fileok(I) = 0
      locline(I) = 6 + I
   NEXT I
7101 CLS
   irtrn% = 0
   ON ERROR RESUME NEXT
   '------------------------------------
   'DISPLAY HEADER LINES AND MENU LINES
   'DISPLAY WEATHER ITEM DESCRIPTION
   'DISPLAY PATH
   '------------------------------------
   COLOR 11, 6
   LOCATE 1, 1:  PRINT "                          WEATHER DATA  -  FILE EDITING                         ";
   COLOR 15, 1
   COLOR 4, 3
   LOCATE 25, 1: PRINT "  F1=Info   F2=Help   F3=Keys   F9=Quit   F10=End   PgUp=Return   PgDn=Proceed  ";
   'LOCATE 25, 1: PRINT "     F1=Info        F2=Help        F3=Keys       F9=Quit        PgDn=Proceed    ";
   COLOR 15, 1
   SROWBOX = 5: SCOLBOX = 3: BOXWIDTH = 75: EROWBOX = 11
   CALL BOX
   LOCATE 6, 26: PRINT "----------------DIRECTORY---------------": LOCATE 6, 68: PRINT "--FILE--"
   LOCATE 7, 5:  PRINT "PRECIPITATION"
   LOCATE 8, 5:  PRINT "TEMPERATURE"
   LOCATE 9, 5:  PRINT "SOLAR RADIATION"
   LOCATE 10, 5: PRINT "EVAPOTRANSPIRATION"
   LOCATE 16, 5: PRINT "Enter names for files that you wish to edit, or PgDn to create new data."
   F1HELP$ = "#CLIMEDIT"
   F2HELP$ = "#CLIMEDIT"
   GOSUB 78165
   FOR I = 1 TO nlines
     IF LTRIM$(RTRIM$(FILEPATH$(I))) = "" THEN FILEPATH$(I) = DEFDIR$
     LOCATE locline(I), 26: PRINT FILEPATH$(I)
     LOCATE locline(I), 68: PRINT DATAFILE$(I)
   NEXT I
   '-------------------------
   'BEGIN SCREEN INPUT
   '-------------------------
   I = 1
7140 CALL ACELL(locline(I), 26, 40, FILEPATH$(I), 1)
     LOCATE locline(nlines) + 2, 25: PRINT "                                                  "
     fileok(I) = 0
     GOSUB 73190
     FILEPATH$(I) = LTRIM$(RTRIM$(FILEPATH$(I)))
     IF LTRIM$(RTRIM$(FILEPATH$(I))) = "" THEN
       FILEPATH$(I) = DEFDIR$
       LOCATE locline(I), 26: PRINT FILEPATH$(I)
     END IF
     IF RIGHT$(FILEPATH$(I), 1) <> "\" THEN FILEPATH$(I) = FILEPATH$(I) + "\"
201  IF RIGHT$(FILEPATH$(I), 2) = "\\" THEN
       FILEPATH$(I) = LEFT$(FILEPATH$(I), LEN(FILEPATH$(I)) - 1)
       GOTO 201
     END IF
     '------------------------------------------------
     'IF PGDN IS PRESSED, CHECK THE EXISTENCE OF
     'ALL FILES ENTERED, AND IF THEY DO EXIST
     'PROCEED TO THE NEXT STAGE (READ DATA...)
     'ALSO IF NO FILE NAMES WERE ENTERED, PROCEED
     'WITH THE PROGRAM.
     '------------------------------------------------
     IF irtrn% = 1 OR irtrn% = 2 THEN
	FOR j = 1 TO nlines
	  ERR = 0
	  IF LTRIM$(RTRIM$(DATAFILE$(j))) = "" THEN fileok(j) = 1
	  COLOR 15, 1: LOCATE locline(I), 26: PRINT FILEPATH$(I)
	  I = j
	  IF LTRIM$(RTRIM$(FILEPATH$(I))) = "" THEN
	     FILEPATH$(I) = CURDIR$
	     LOCATE locline(I), 26: PRINT FILEPATH$(I)
	  END IF
	  GOSUB 83190
	  LOCATE locline(I), 26: PRINT FILEPATH$(I)
	  IF fileok(I) = 0 GOTO 7140
	NEXT j
	FOR j = 1 TO nlines
	   IF fileok(j) = 0 THEN GOTO 7140
	NEXT j
	GOSUB 5700: EXIT SUB
     END IF
     IF irtrn% = 1000 OR irtrn% = -1 THEN EXIT SUB
     IF irtrn% = 3 THEN
	IF I = 1 THEN
	  I = nlines
	ELSE
	  I = I - 1
	END IF
	GOTO 7140
     END IF
     IF irtrn% = 4 THEN
	IF I <> nlines THEN
	   I = I + 1
	ELSE
	   I = 1
	END IF
	GOTO 7140
     END IF
     IF irtrn% = 6 OR irtrn% = 5 THEN
	IF LTRIM$(RTRIM$(FILEPATH$(I))) = "" THEN
	   FILEPATH$(I) = CURDIR$
	   LOCATE locline(I), 26: PRINT FILEPATH$(I)
	END IF
	GOSUB 83190
	GOTO 7145
     END IF
     IF irtrn% = 7 THEN
	IF I = 1 THEN
	   I = nlines
	ELSE
	   I = I - 1
	END IF
	GOTO 7145
     END IF
     IF irtrn% = 23 THEN
	CALL GETDIR(ext$(I), SELECT$, FILEPATH$(I))
	IF irtrn% = 1000 OR irtrn% = -1 THEN EXIT SUB
	IF SELECT$ <> "" THEN
	   DATAFILE$(I) = SELECT$
	   COLOR 15, 1: LOCATE locline(I), 68: PRINT "        "
	   LOCATE locline(I), 68: PRINT DATAFILE$(I)
	   SELECT$ = ""
	END IF
	GOSUB 78165
     END IF
     GOTO 7140
7145 CALL ACELL(locline(I), 68, 8, DATAFILE$(I), 1)
     ERR = 0
     LOCATE locline(nlines) + 2, 25: PRINT "                                                   "
     GOSUB 73190
     DATAFILE$(I) = LTRIM$(RTRIM$(DATAFILE$(I)))
     IF irtrn% = 1 OR irtrn% = 2 THEN
	FOR j = 1 TO nlines
	  ERR = 0
	  IF LTRIM$(RTRIM$(DATAFILE$(j))) = "" THEN fileok(j) = 1
	  COLOR 15, 1: LOCATE locline(I), 68: PRINT DATAFILE$(I)
	  IF fileok(j) = 0 THEN
	    I = j
	    GOSUB 83190
	    LOCATE locline(I), 68: PRINT DATAFILE$(I)
	    IF fileok(I) = 0 GOTO 7145
	  END IF
	NEXT j
	FOR j = 1 TO nlines
	   IF fileok(j) = 0 THEN GOTO 7145
	NEXT j
	GOSUB 5700: EXIT SUB
     END IF
'''''''''''
'F9  (END)
'''''''''''
     IF irtrn% = 1000 OR irtrn% = -1 THEN EXIT SUB
'''''''''''
'UP ARROW
'''''''''''
     IF irtrn% = 3 THEN
       IF I = 1 THEN
	  I = nlines
       ELSE
	  I = I - 1
       END IF
       GOTO 7145
     END IF
''''''''''''
'DOWN ARROW
''''''''''''
     IF irtrn% = 4 THEN
	IF I <> nlines THEN
	   I = I + 1
	ELSE
	   I = 1
	END IF
	GOTO 7145
     END IF
'''''''''''
'SHIFT TAB
'''''''''''
     IF irtrn% = 7 THEN GOTO 7140
''''''''''''''''''''''''
'TAB OR CARRAIGE RETURN
''''''''''''''''''''''''
     IF irtrn% = 6 OR irtrn% = 5 THEN
	IF LTRIM$(RTRIM$(DATAFILE$(I))) <> "" THEN GOSUB 83190
	IF I = nlines THEN
	   I = 1
	ELSE
	   I = I + 1
	END IF
	GOTO 7140
     END IF
''''''''''''''''''''''''
'F4 -- GET LIST OF FILE
''''''''''''''''''''''''
     IF irtrn% = 23 THEN
	CALL GETDIR(ext$(I), SELECT$, FILEPATH$(I))
	IF irtrn% = 1000 OR irtrn% = -1 THEN EXIT SUB
	IF SELECT$ <> "" THEN
	  DATAFILE$(I) = LTRIM$(RTRIM$(SELECT$)): SELECT$ = ""
	  LOCATE locline(I), 68: PRINT DATAFILE$(I)
	END IF
	GOSUB 78165
     END IF
    GOTO 7145
  EXIT SUB
'----------------------------------
'SUBROUTINE TO PRINT DIRECTORY LINE
'----------------------------------
78165  COLOR 11, 6
       LOCATE 24, 1: PRINT "                     F4 = List Files on Specified Directory                     ";
     COLOR 15, 1
     RETURN
'------------------------------
'check if directory is valid
'------------------------------
83190  IF FILEPATH$(I) = CURDIR$ AND DATAFILE$(I) = "" THEN RETURN
       IF (irtrn% = 1 OR irtrn% = 2) AND DATAFILE$(I) = "" THEN RETURN
       A1 = INSTR(FILEPATH$(I), ":")
       IF A1 = 0 THEN
	  fdrive$ = LEFT$(CURDIR$, 1)
	  fpath$ = FILEPATH$(I)
       ELSE
	  fdrive$ = LEFT$(FILEPATH$(I), A1 - 1)
	  fpath$ = RIGHT$(FILEPATH$(I), LEN(FILEPATH$(I)) - A1)
       END IF
       IF fpath$ = "\" THEN fpath$ = ""
       filen$ = DATAFILE$(I)
       found = -1
       CALL FILECHK(fdrive$, fpath$, filen$ + "." + ext$(I), found)
       IF found = 0 THEN
	 LOCATE locline(nlines) + 2, 35: PRINT "Invalid Directory"; : FILEPATH$(I) = CURDIR$
	 LOCATE locline(I), 26: PRINT LEFT$(BLANK$, 40)
	 LOCATE locline(I), 26: PRINT FILEPATH$(I)
       ELSEIF found = 1 THEN
	 LOCATE locline(nlines) + 2, 35: PRINT "Error in Path"; : FILEPATH$(I) = CURDIR$
	 LOCATE locline(I), 26: PRINT LEFT$(BLANK$, 40)
	 LOCATE locline(I), 26: PRINT FILEPATH$(I)
       ELSEIF found = 2 THEN
	 IF LTRIM$(RTRIM$(DATAFILE$(I))) <> "" THEN LOCATE locline(nlines) + 2, 35: PRINT "BAD FILE NAME";
	 DATAFILE$(I) = "        ": LOCATE locline(I), 68: PRINT DATAFILE$(I)
       ELSEIF found = 3 THEN
	 IF LTRIM$(RTRIM$(DATAFILE$(I))) <> "" THEN LOCATE locline(nlines) + 2, 35: PRINT "FILE NOT FOUND";
	 DATAFILE$(I) = "        ": LOCATE locline(I), 68: PRINT DATAFILE$(I)
       ELSEIF found = 5 THEN
	 LOCATE locline(nlines) + 2, 35: PRINT "Invalid Drive"; : FILEPATH$(I) = CURDIR$
	 LOCATE locline(I), 26: PRINT LEFT$(BLANK$, 40)
	 LOCATE locline(I), 26: PRINT FILEPATH$(I)
       END IF
       IF found = 4 THEN
	 fileok(I) = 1
       ELSE
	 fileok(I) = 0
       END IF
RETURN
'''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO DEFINE D4EDIT$, D7EDIT$
'D13EDIT$, AND D11EDIT$
' FIRST IF FILE NAME IS A BLANK
' THEN SET THE FILEOK TO ZERO
'''''''''''''''''''''''''''''''''''''''
5700  FOR I = 1 TO 4
       IF LTRIM$(RTRIM$(DATAFILE$(I))) = "" THEN fileok(I) = 0
      NEXT I
      IF fileok(1) = 0 THEN
	d4edit$ = ""
      ELSE
	IF RIGHT$(FILEPATH$(1), 1) <> "\" THEN FILEPATH$(1) = FILEPATH$(1) + "\"
	d4edit$ = FILEPATH$(1) + DATAFILE$(1) + "." + ext$(1)
      END IF
      IF fileok(2) = 0 THEN
	D7EDIT$ = ""
      ELSE
	IF RIGHT$(FILEPATH$(2), 1) <> "\" THEN FILEPATH$(2) = FILEPATH$(2) + "\"
	D7EDIT$ = FILEPATH$(2) + DATAFILE$(2) + "." + ext$(2)
      END IF
      IF fileok(3) = 0 THEN
	D13EDIT$ = ""
      ELSE
	IF RIGHT$(FILEPATH$(3), 1) <> "\" THEN FILEPATH$(3) = FILEPATH$(3) + "\"
	D13EDIT$ = FILEPATH$(3) + DATAFILE$(3) + "." + ext$(3)
      END IF
      IF fileok(4) = 0 THEN
	D11EDIT$ = ""
      ELSE
	IF RIGHT$(FILEPATH$(4), 1) <> "\" THEN FILEPATH$(4) = FILEPATH$(4) + "\"
	D11EDIT$ = FILEPATH$(4) + DATAFILE$(4) + "." + ext$(4)
      END IF
RETURN
'----------------------------------------------
'SUBROUTINE TO CALL help MESSAGES FOR HELP
'----------------------------------------------
73190  IF irtrn% = 20 THEN CALL HLPMSG(F1HELP$)
       IF irtrn% = 21 THEN CALL HLPMSG(F2HELP$)
       IF irtrn% = 22 THEN CALL USERHELP
      RETURN
END SUB

SUB CLIMHELP2 (CLIM$, clfile$, cit$, sta$)
'--------------------------------------------------------
'CONVERSION FROM HELP2 CLIMATE FORMAT TO HELP3 FORMAT
'--------------------------------------------------------
   CLOSE
   clim1$ = "     PRECIPITATION     "
   clim2$ = "      TEMPERATURE      "
   clim3$ = "    SOLAR RADIATION    "
   IF CLIM$ = clim1$ THEN
      a$ = DIR$(clfile$)
      IF LEN(a$) = 0 THEN EXIT SUB
      OPEN clfile$ FOR INPUT AS #1
      OPEN "DATA4.TMP" FOR OUTPUT AS #2:
      IM% = 10
   ELSEIF CLIM$ = clim2$ THEN
      a$ = DIR$(clfile$)
      IF LEN(a$) = 0 THEN EXIT SUB
      OPEN clfile$ FOR INPUT AS #1
      OPEN "DATA7.TMP" FOR OUTPUT AS #2
      IM% = 5
   ELSEIF CLIM$ = clim3$ THEN
      a$ = DIR$(clfile$)
      IF LEN(a$) = 0 THEN EXIT SUB
      OPEN clfile$ FOR INPUT AS #1
      OPEN "DATA13.TMP" FOR OUTPUT AS #2
      IM% = 5
   END IF
'-------------------------------------------------
'READ YEARS FROM HELP2 FILE AND SAVE IN HELP 3 FORMAT
'-------------------------------------------------
'PRINT NEW HEADING LINES FIRST
      PRINT #2, " 8"
      PRINT #2, " 1"
      PRINT #2, USING "\                   \"; cit$; sta$
      PRINT #2,
      FOR I = 1 TO 100
	LOCATE 23, 2: PRINT "                                                                             ";
	LOCATE 23, 2: PRINT "Converting Year ";
	FOR II = 1 TO 37
	  IF EOF(1) THEN CLOSE : EXIT SUB
	  LINE INPUT #1, a$
	  IF LEN(a$) < 30 THEN CLOSE : EXIT SUB
	  IF CLIM$ = clim1$ THEN
	     b$ = RIGHT$(a$, (LEN(a$) - 5))
	     IF II = 1 THEN PRINT LEFT$(b$, 5);
	     PRINT #2, "     "; b$
	  ELSE
	     IF II = 1 THEN PRINT LEFT$(a$, 5);
	     PRINT #2, a$
	  END IF
	  PRINT ".";
	NEXT II
      NEXT I
      CLOSE
      EXIT SUB
END SUB

SUB EVAPIND (SELECCT$, SELECST$, fromsub)
'-----------------------------------------------------------------
'selecct$ : the selected city from the list
'selecst$ : the selected state from the list
'fromsub  = 1 means this subroutine is called from the ET data screen
'             and tape 2 data is read for guidance
'         = 2 means that this subroutine is called only for obtaining
'             the normal mean montly temperature.  Tape2 is read in
'             subroutine meantemp.
'-----------------------------------------------------------------
       DIM CITY$(52, 20), NCITIES(52), states$(52)
       DIM a$(52)
       COLOR 15, 1
       PCOPY VPAGE%, HPAGE%
       irtrn% = 0
       PCOPY VPAGE%, APAGE%
       CLS APAGE%
       SCREEN 0, , VPAGE%, VPAGE%
       HEADING$ = " SYNTHETIC TEMPERATURE AND SOLAR RADIATION "
'SYNTHETIC TEMPERATURE AND SOLAR RADIATION
      NSTATES = 52
      states$(1) = "ALABAMA": NCITIES(1) = 3
	CITY$(1, 1) = "BIRMINGHAM": CITY$(1, 2) = "MOBILE": CITY$(1, 3) = "MONTGOMERY"
      states$(2) = "ALASKA": NCITIES(2) = 3
	CITY$(2, 1) = "ANNETTE": CITY$(2, 2) = "BETHEL": CITY$(2, 3) = "FAIRBANKS"
      states$(3) = "ARIZONA": NCITIES(3) = 4
	CITY$(3, 1) = "FLAGSTAFF": CITY$(3, 2) = "PHOENIX": CITY$(3, 3) = "TUCSON": CITY$(3, 4) = "YUMA "
      states$(4) = "ARKANSAS": NCITIES(4) = 2
	CITY$(4, 1) = "FORT SMITH": CITY$(4, 2) = "LITTLE ROCK"
      states$(5) = "CALIFORNIA": NCITIES(5) = 10
	CITY$(5, 1) = "BAKERSFIELD": CITY$(5, 2) = "BLUE CANYON": CITY$(5, 3) = "EUREKA": CITY$(5, 4) = "LOS ANGELES": CITY$(5, 5) = "FRESNO"
	CITY$(5, 6) = "MT. SHASTA": CITY$(5, 7) = "SACRAMENTO": CITY$(5, 8) = "SAN DIEGO": CITY$(5, 9) = "SAN FRANCISCO": CITY$(5, 10) = "SANTA MARIA"
      states$(6) = "COLORADO": NCITIES(6) = 4
	CITY$(6, 1) = "COLORADO SPRINGS": CITY$(6, 2) = "DENVER":  CITY$(6, 3) = "GRAND JUNCTION":  CITY$(6, 4) = "PUEBLO"
      states$(7) = "CONNECTICUT": NCITIES(7) = 4
	CITY$(7, 1) = "BRIDGEPORT": CITY$(7, 2) = "HARTFORD": CITY$(7, 3) = "NEW HAVEN": CITY$(7, 4) = "WINDSOR LOCKS"
      states$(8) = "DELAWARE": NCITIES(8) = 1
	CITY$(8, 1) = "WILMINGTON"
      states$(9) = "DISTRICT OF COLUMBIA": NCITIES(9) = 1
	CITY$(9, 1) = "WASHINGTON"
      states$(10) = "FLORIDA": NCITIES(10) = 6
	CITY$(10, 1) = "JACKSONVILLE": CITY$(10, 2) = "MIAMI": CITY$(10, 3) = "ORLANDO": CITY$(10, 4) = "TALLAHASSEE": CITY$(10, 5) = "TAMPA": CITY$(10, 6) = "W. PALM BEACH"
      states$(11) = "GEORGIA": NCITIES(11) = 5
	CITY$(11, 1) = "ATLANTA": CITY$(11, 2) = "AUGUSTA": CITY$(11, 3) = "MACON": CITY$(11, 4) = "SAVANNAH": CITY$(11, 5) = "WATKINSVILLE"
      states$(12) = "HAWAII": NCITIES(12) = 1
	CITY$(12, 1) = "HONOLULU"
      states$(13) = "IDAHO": NCITIES(13) = 2
	CITY$(13, 1) = "BOISE": CITY$(13, 2) = "POCATELLO"
      states$(14) = "ILLINOIS": NCITIES(14) = 2
	CITY$(14, 1) = "CHICAGO": CITY$(14, 2) = "EAST ST. LOUIS"
      states$(15) = "INDIANA": NCITIES(15) = 3
	CITY$(15, 1) = "EVANSVILLE": CITY$(15, 2) = "FORT WAYNE":  CITY$(15, 3) = "INDIANAPOLIS"
      states$(16) = "IOWA ": NCITIES(16) = 2
	CITY$(16, 1) = "DES MOINES": CITY$(16, 2) = "DUBUQUE":
      states$(17) = "KANSAS": NCITIES(17) = 3
	CITY$(17, 1) = "DODGE CITY": CITY$(17, 2) = "TOPEKA": CITY$(17, 3) = "WICHITA"
      states$(18) = "KENTUCKY": NCITIES(18) = 3
	CITY$(18, 1) = "COVINGTON": CITY$(18, 2) = "LEXINGTON": CITY$(18, 3) = "LOUISVILLE"
      states$(19) = "LOUISIANA": NCITIES(19) = 4
	CITY$(19, 1) = "BATON ROUGE": CITY$(19, 2) = "LAKE CHARLES": CITY$(19, 3) = "NEW ORLEANS": CITY$(19, 4) = "SHREVEPORT"
      states$(20) = "MAINE": NCITIES(20) = 4
	CITY$(20, 1) = "AUGUSTA": CITY$(20, 2) = "BANGOR": CITY$(20, 3) = "CARIBOU": CITY$(20, 4) = "PORTLAND"
      states$(21) = "MARYLAND": NCITIES(21) = 1
	CITY$(21, 1) = "BALTIMORE"
      states$(22) = "MASSACHUSETTS": NCITIES(22) = 4
	CITY$(22, 1) = "BOSTON": CITY$(22, 2) = "NANTUCKET": CITY$(22, 3) = "PLAINFIELD": CITY$(22, 4) = "WORCHESTER"
      states$(23) = "MICHIGAN": NCITIES(23) = 4
	CITY$(23, 1) = "DETROIT": CITY$(23, 2) = "E. LANSING": CITY$(23, 3) = "GRAND RAPIDS": CITY$(23, 4) = "SAULT STE. MARIE"
      states$(24) = "MINNESOTA": NCITIES(24) = 3
	CITY$(24, 1) = "DULUTH": CITY$(24, 2) = "MINNEAPOLIS": CITY$(24, 3) = "ST. CLOUD"
      states$(25) = "MISSISSIPPI": NCITIES(25) = 2
	CITY$(25, 1) = "JACKSON": CITY$(25, 2) = "MERIDIAN"
      states$(26) = "MISSOURI": NCITIES(26) = 3
	CITY$(26, 1) = "COLUMBIA": CITY$(26, 2) = "KANSAS CITY": CITY$(26, 3) = "ST. LOUIS"
      states$(27) = "MONTANA": NCITIES(27) = 7
	CITY$(27, 1) = "BILLINGS": CITY$(27, 2) = "GLASGOW": CITY$(27, 3) = "GREAT FALLS": CITY$(27, 4) = "HAVRE": CITY$(27, 5) = "HELENA": CITY$(27, 6) = "KALISPELL": CITY$(27, 7) = "MILES CITY"
      states$(28) = "NEBRASKA": NCITIES(28) = 4
	CITY$(28, 1) = "GRAND ISLAND": CITY$(28, 2) = "NORTH PLATTE": CITY$(28, 3) = "OMAHA": CITY$(28, 4) = "SCOTTSBLUFF":
      states$(29) = "NEVADA": NCITIES(29) = 5
	CITY$(29, 1) = "ELKO ": CITY$(29, 2) = "ELY  ": CITY$(29, 3) = "LAS VEGAS": CITY$(29, 4) = "RENO ": CITY$(29, 5) = "WINNEMUCCA":
      states$(30) = "NEW HAMPSHIRE": NCITIES(30) = 3
	CITY$(30, 1) = "CONCORD": CITY$(30, 2) = "MT. WASHINGTON": CITY$(30, 3) = "NASHUA"
      states$(31) = "NEW JERSEY": NCITIES(31) = 3
	CITY$(31, 1) = "EDISON": CITY$(31, 2) = "NEWARK": CITY$(31, 3) = "SEABROOK"
      states$(32) = "NEW MEXICO": NCITIES(32) = 2
	CITY$(32, 1) = "ALBUQUERQUE": CITY$(32, 2) = "ROSWELL"
      states$(33) = "NEW YORK": NCITIES(33) = 7
	CITY$(33, 1) = "ALBANY": CITY$(33, 2) = "BUFFALO": CITY$(33, 3) = "CENTRAL PARK": CITY$(33, 4) = "ITHACA": CITY$(33, 5) = "NEW YORK": CITY$(33, 6) = "SCHENECTADY": CITY$(33, 7) = "SYRACUSE"
      states$(34) = "NORTH CAROLINA": NCITIES(34) = 4
	CITY$(34, 1) = "ASHEVILLE": CITY$(34, 2) = "CHARLOTTE": CITY$(34, 3) = "GREENSBORO": CITY$(34, 4) = "RALEIGH"
      states$(35) = "NORTH DAKOTA": NCITIES(35) = 2
	CITY$(35, 1) = "BISMARCK": CITY$(35, 2) = "WILLISTON":
      states$(36) = "OHIO ": NCITIES(36) = 5
	CITY$(36, 1) = "CINCINNATI": CITY$(36, 2) = "CLEVELAND": CITY$(36, 3) = "COLUMBUS": CITY$(36, 4) = "PUT-IN-BAY": CITY$(36, 5) = "TOLEDO"
      states$(37) = "OKLAHOMA": NCITIES(37) = 2
	CITY$(37, 1) = "OKLAHOMA CITY": CITY$(37, 2) = "TULSA"
      states$(38) = "OREGON": NCITIES(38) = 8
	CITY$(38, 1) = "ASTORIA": CITY$(38, 2) = "BURNS": CITY$(38, 3) = "MEACHAM": CITY$(38, 4) = "MEDFORD": CITY$(38, 5) = "PENDLETON"
	CITY$(38, 6) = "PORTLAND": CITY$(38, 7) = "SALEM": CITY$(38, 8) = "SEXTON SUMMIT"
      states$(39) = "PENNSYLVANIA": NCITIES(39) = 2
	CITY$(39, 1) = "PHILADELPHIA": CITY$(39, 2) = "PITTSBURGH"
      states$(40) = "RHODE ISLAND": NCITIES(40) = 1
	CITY$(40, 1) = "PROVIDENCE"
      states$(41) = "SOUTH CAROLINA": NCITIES(41) = 2
	CITY$(41, 1) = "CHARLESTON": CITY$(41, 2) = "COLUMBIA"
      states$(42) = "SOUTH DAKOTA": NCITIES(42) = 2
	CITY$(42, 1) = "HURON": CITY$(42, 2) = "RAPID CITY"
      states$(43) = "TENNESSEE": NCITIES(43) = 4
	CITY$(43, 1) = "CHATTANOOGA": CITY$(43, 2) = "KNOXVILLE": CITY$(43, 3) = "MEMPHIS": CITY$(43, 4) = "NASHVILLE"
      states$(44) = "TEXAS": NCITIES(44) = 13
	CITY$(44, 1) = "ABILENE":  CITY$(44, 2) = "AMARILLO":  CITY$(44, 3) = "AUSTIN":  CITY$(44, 4) = "BROWNSVILLE":  CITY$(44, 5) = "CORPUS CHRISTI":  CITY$(44, 6) = "DALLAS":  CITY$(44, 7) = "EL PASO"
	CITY$(44, 8) = "GALVESTON":  CITY$(44, 9) = "HOUSTON": CITY$(44, 10) = "MIDLAND": CITY$(44, 11) = "SAN ANTONIO": CITY$(44, 12) = "TEMPLE": CITY$(44, 13) = "WACO ":
      states$(45) = "UTAH ": NCITIES(45) = 3
	CITY$(45, 1) = "CEDAR CITY": CITY$(45, 2) = "MILFORD": CITY$(45, 3) = "SALT LAKE CITY"
      states$(46) = "VERMONT": NCITIES(46) = 3
	CITY$(46, 1) = "BURLINGTON": CITY$(46, 2) = "MONTPELIER": CITY$(46, 3) = "RUTLAND"
      states$(47) = "VIRGINIA": NCITIES(47) = 3
	CITY$(47, 1) = "LYNCHBURG": CITY$(47, 2) = "NORFOLK": CITY$(47, 2) = "RICHMOND"
      states$(48) = "WASHINGTON": NCITIES(48) = 7
	CITY$(48, 1) = "OLYMPIA": CITY$(48, 2) = "PULLMAN": CITY$(48, 3) = "SEATTLE": CITY$(48, 4) = "SPOKANE": CITY$(48, 5) = "STAMPEDE PASS": CITY$(48, 6) = "WALLA WALLA": CITY$(48, 7) = "YAKIMA"
      states$(49) = "WEST VIRGINIA": NCITIES(49) = 1
	CITY$(49, 1) = "CHARLESTON"
      states$(50) = "WISCONSIN": NCITIES(50) = 4
	CITY$(50, 1) = "GREEN BAY": CITY$(50, 2) = "LACROSSE": CITY$(50, 3) = "MADISON": CITY$(50, 4) = "MILWAUKEE"
      states$(51) = "WYOMING": NCITIES(51) = 2
	CITY$(51, 1) = "CHEYENNE": CITY$(51, 2) = "LANDER"
      states$(52) = "PUERTO RICO": NCITIES(52) = 1
	CITY$(52, 1) = "SAN JUAN"
      CLS
      SCREEN 0: COLOR 15, 1: CLS
      SLINE = 1
   LOCATE SLINE, 1
   PRINT "ษออออออออออออออออออ"; : PRINT HEADING$; : PRINT "อออออออออออออออออป"
   FOR I% = 1 TO NOPT + 16
   LOCATE SLINE + I%, 1: PRINT "บ": LOCATE SLINE + I%, 80: PRINT "บ"
   NEXT I%
   LOCATE 18, 1
   PRINT "ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
   FOR I% = 19 TO 22
   LOCATE I%, 1: PRINT "บ": LOCATE I%, 80: PRINT "บ"
   NEXT I%
   PRINT "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
	k = 2
	CALL ITEMSELECT(k, 13, NSTATES, states$(), SELECST$, ITEMNUM)
	IF irtrn% = 100 OR irtrn% = 22 THEN 9007
	TOTNUM = NCITIES(ITEMNUM)
	FOR I = 1 TO TOTNUM
	   a$(I) = CITY$(ITEMNUM, I)
	NEXT I
	FOR I = TOTNUM + 1 TO 20
	    a$(I) = " "
	NEXT I
	CALL ITEMSELECT(18, 4, TOTNUM, a$(), SELECCT$, ITEMNUM)
	IF irtrn% = 100 OR irtrn% = 22 THEN 9007
	IF SELECCT$ = "" THEN SELECST$ = "": GOTO 9007
	def$ = DIR$("TAPE2.NEW")
      IF LEN(def$) <> 0 THEN
	'------------------------------------
	'OPEN TAPE2 TO READ DEFAULT VALUES
	'------------------------------------
	CLOSE
	OPEN "TAPE2.NEW" FOR INPUT AS #1
	s1$ = LEFT$(SELECST$, 5): s2$ = LEFT$(SELECCT$, 5)
	LOCATE 24, 1: PRINT "         Reading Default Data From Tape2 File.  Please Wait...               ";
	FOR I = 1 TO 300
	   IF EOF(1) THEN END
	   LINE INPUT #1, l$
	   IF LEFT$(l$, 5) = s1$ AND MID$(l$, 7, 5) = s2$ THEN
	      LINE INPUT #1, l$
	      IF fromsub = 1 THEN LATIT$ = LTRIM$(RTRIM$(LEFT$(l$, 7)))
	      FOR j = 1 TO 4
		 LINE INPUT #1, l$
	      NEXT j
	    IF fromsub = 2 THEN
	       mult = 1: subt = 0
	       st% = 1
	       FOR k% = 1 TO 12
		  deft(k%) = VAL(MID$(l$, st%, 6))
		  IF unt = 2 THEN mult = .556: subt = .556 * 32
		 'unmmt$(k%) = LTRIM$(STR$(deft(k%) * mult - subt * mult))
		  st% = st% + 6
	       NEXT k%
	    END IF
	      LINE INPUT #1, l$
	      IF fromsub = 1 THEN
		GR1$ = LTRIM$(RTRIM$(LEFT$(l$, 4)))
		GR2$ = LTRIM$(RTRIM$(MID$(l$, 5, 4)))
		MAXLAI$ = LTRIM$(RTRIM$(MID$(l$, 9, 4)))
		BARE$ = LTRIM$(RTRIM$(MID$(l$, 13, 4)))
		FAIR$ = LTRIM$(RTRIM$(MID$(l$, 17, 4)))
		EXCL$ = LTRIM$(RTRIM$(MID$(l$, 21, 4)))
	      END IF
		FOR II = 1 TO 4
		   LINE INPUT #1, l$
		NEXT II
	      IF fromsub = 1 THEN
		WSPEED$ = LTRIM$(RTRIM$(LEFT$(l$, 5)))
		HUMID1$ = LTRIM$(RTRIM$(MID$(l$, 6, 5)))
		HUMID2$ = LTRIM$(RTRIM$(MID$(l$, 11, 5)))
		HUMID3$ = LTRIM$(RTRIM$(MID$(l$, 16, 5)))
		HUMID4$ = LTRIM$(RTRIM$(MID$(l$, 21, 5)))
	      END IF
		GOTO 9007
	    ELSE
	      FOR j = 1 TO 10
		 LINE INPUT #1, l$
	      NEXT j
	  END IF
	NEXT I
      ELSE
	COLOR 11, 6
	LOCATE 24, 1: PRINT "   File TAPE2 is not loaded on current directory.  Press any key to continue... ";
	COLOR 15, 1
9008    KBD$ = INKEY$
	IF KBD$ = "" THEN GOTO 9008
      END IF
9007  CLS : CLOSE 1: PCOPY HPAGE%, VPAGE%: ' IRTRN% = 0
      EXIT SUB
END SUB

SUB GENCLIMATE
'---------------------------------------------------------
'THIS ROUTINE IS USED TO ENTER EVAPO-TRANSPIRATION VALUES
'THE USER MAY ENTER VALUES MANUALLY AND MAY OBTAIN
'SOME DEFAULT VALUES FROM TAPE2.NEW.
'---------------------------------------------------------
   DIM LAYERHD$(18)
   COLOR 15, 1, 1: CLS
   SCREEN 0
101 F1HELP$ = "#ET"
   CLS : NLAYER = 18            'NUMBER OF OPTIONS IN THE LAYER MAIN MENU
   LAYERHD$(1) = "                             Units:      1 - CUSTOMARY"
   LAYERHD$(2) = "                                         2 - METRIC"
   LAYERHD$(3) = " "
   LAYERHD$(4) = "                      Nearby City :"
   LAYERHD$(5) = "                            State :"
   LAYERHD$(6) = "                         Latitude =             Negative for southern"
   LAYERHD$(7) = "                                                     hemisphere "
   LAYERHD$(8) = "           Evaporative zone depth ="
   LAYERHD$(9) = "          Maximum leaf area index ="
   LAYERHD$(10) = "         Growing season start day ="
   LAYERHD$(11) = "           Growing season end day ="
   LAYERHD$(12) = " "
   LAYERHD$(13) = "               Average wind speed ="
   LAYERHD$(14) = " "
   LAYERHD$(15) = "  First quarter relative humidity ="
   LAYERHD$(16) = " Second quarter relative humidity ="
   LAYERHD$(17) = "  Third quarter relative humidity ="
   LAYERHD$(18) = " Fourth quarter relative humidity ="
   stcol% = 37
   APAGE% = 0: VPAGE% = 1: HPAGE% = 2
   SCREEN , , VPAGE%, VPAGE%
   CLS
   UNIT1$ = "CUSTOMARY": UNIT2$ = "METRIC"
   IF etunits$ <> "2" THEN
     etunits$ = "1"
     unev$ = "IN"
     'tempu$ = "(๘F)": precu$ = "(IN)": solu$ = "(Langleys)"
     WINDU$ = "MPH"
   ELSE
     unev$ = "CM"
     'tempu$ = "(๘C)": precu$ = "(MM)": solu$ = "   MJ/Mý   "
     WINDU$ = "KPH"
   END IF
'-------------------------------------------------------
'DISPLAY SCREEN WITH APPROPRIATE ENTRY DESCRIPTIONS AND
'APPROPRIATE ENTRIES PREVIOUSLY DEFINED OR READ FROM A
'D11 FILE.
'-------------------------------------------------------
   LOCATE 1, 1: COLOR 11, 6
		 PRINT "                     WEATHER DATA - EVAPOTRANSPIRATION DATA                     "
   LOCATE 24, 1: PRINT "           F5 = City Selection for Default Evapotranspiration Data              ";
   CALL menu
   FOR I% = 1 TO NLAYER
     LOCATE 4 + I%, 1: PRINT LAYERHD$(I%)
   NEXT I%
     LOCATE 5, 37: PRINT etunits$
     LOCATE 8, stcol%: PRINT USING "\                    \"; LTRIM$(RTRIM$(ETCITY$))
     LOCATE 9, stcol%: PRINT USING "\                    \"; LTRIM$(RTRIM$(ETSTATE$))
     LOCATE 10, stcol%: PRINT USING "\        \"; LTRIM$(RTRIM$(LATIT$))
     LOCATE 12, stcol%: PRINT USING "\   \"; LTRIM$(RTRIM$(evdepth$))
     LOCATE 13, stcol%: PRINT USING "\   \"; LTRIM$(RTRIM$(LAI$))
     LOCATE 14, stcol%: PRINT USING "\   \"; LTRIM$(RTRIM$(GR1$))
     LOCATE 15, stcol%: PRINT USING "\   \"; LTRIM$(RTRIM$(GR2$))
     LOCATE 17, stcol%: PRINT USING "\   \"; LTRIM$(RTRIM$(WIND$))
     LOCATE 19, stcol%: PRINT USING "\   \"; LTRIM$(RTRIM$(HUMID1$))
     LOCATE 20, stcol%: PRINT USING "\   \"; LTRIM$(RTRIM$(HUMID2$))
     LOCATE 21, stcol%: PRINT USING "\   \"; LTRIM$(RTRIM$(HUMID3$))
     LOCATE 22, stcol%: PRINT USING "\   \"; LTRIM$(RTRIM$(HUMID4$))
     IF VAL(etunits$) = 1 OR VAL(etunits$) = 0 THEN COLOR 1, 15: LOCATE 5, stcol% + 9: PRINT UNIT1$
     IF VAL(etunits$) = 2 THEN COLOR 1, 15: LOCATE 6, stcol% + 9: PRINT UNIT2$
     COLOR 15, 1
     LOCATE 12, 43: PRINT unev$
     LOCATE 17, 43: PRINT WINDU$
     LOCATE 19, 43: PRINT "%"
     LOCATE 20, 43: PRINT "%"
     LOCATE 21, 43: PRINT "%"
     LOCATE 22, 43: PRINT "%"
     OLDCITY$ = ETCITY$: OLDSTATE$ = ETSTATE$
     LOCATE 8, stcol%: PRINT USING "\                    \"; ETCITY$
     LOCATE 9, stcol%: PRINT USING "\                    \"; ETSTATE$
'------------------
'BEGIN SCREEN ENTRY
'------------------
141  oldunits$ = LEFT$(etunits$, 1)
     F2HELP$ = "#UNITS"
     CALL ACELL(5, 37, 1, etunits$, 2)
     etunits$ = LTRIM$(RTRIM$(etunits$))
     GOSUB 1190
'---------
'F9  (END)
'---------
     IF irtrn% = 1000 THEN GOSUB 1195
'---------------------------------------------
'DISPLAY AND DEFINE PROPER UNITS ON THE SCREEN
'---------------------------------------------
     IF etunits$ <> "2" THEN
	etunits$ = "1"
	LOCATE 5, stcol%: PRINT etunits$
	unev$ = "IN"
	'tempu$ = "(๘F)": precu$ = "(IN)": solu$ = "(Langleys)"
	WINDU$ = "MPH"
	COLOR 15, 1: LOCATE 6, stcol% + 9: PRINT UNIT2$
	COLOR 1, 15: LOCATE 5, stcol% + 9: PRINT UNIT1$
     ELSE
	unev$ = "CM"
	'tempu$ = "(๘C)": precu$ = "(CM)": solu$ = "  (MJ/Mý)  "
	WINDU$ = "KPH"
	COLOR 15, 1: LOCATE 5, stcol% + 9: PRINT UNIT1$
	COLOR 1, 15: LOCATE 6, stcol% + 9: PRINT UNIT2$
     END IF
     IF etunits$ <> oldunits$ THEN GOSUB 11910
     IF irtrn% = 1 THEN EXIT SUB: GOTO 101
     IF irtrn% = -1 OR irtrn% = 2 THEN EXIT SUB
     IF irtrn% >= 3 AND irtrn% <= 5 THEN
     '   IF ETUNITS$ = "1" THEN
     '      COLOR 15, 1: LOCATE 6, STCOL% + 9: PRINT UNIT2$
     '      COLOR 1, 15: LOCATE 5, STCOL% + 9: PRINT UNIT1$
     '   ELSE
     '      COLOR 15, 1: LOCATE 5, STCOL% + 9: PRINT UNIT1$
     '      COLOR 1, 15: LOCATE 6, STCOL% + 9: PRINT UNIT2$
     '   END IF
'--------
'PGUP
'--------
      IF irtrn% = 3 THEN GOTO 270
     END IF
'-------------------
'F3 (HELP ON KEYS)
'-------------------
      IF irtrn% = 40 THEN GOSUB 191
'-------------------
'CARRAIGE RETURN
'-------------------
      IF irtrn% > 5 THEN GOTO 141
'-------------------------
'CITY AND STATE SELECTION
'-------------------------
165  OLDCITY$ = ETCITY$
     CALL ACELL(8, 37, 20, ETCITY$, 1)
     F2HELP$ = "#ETLOCATION"
     GOSUB 1190
     IF irtrn% = 1000 THEN GOTO 1195
     IF LEFT$(OLDCITY$, 4) <> LEFT$(ETCITY$, 4) THEN
	EVBARE$ = " "
	LOCATE 10, 48: PRINT "                                "
	LOCATE 11, 48: PRINT "                                "
	LOCATE 12, 48: PRINT "                                "
	LOCATE 13, 48: PRINT "                                "
     END IF
     IF irtrn% = 1 THEN EXIT SUB: GOTO 101
     IF irtrn% = -1 THEN EXIT SUB
     IF irtrn% = 2 THEN EXIT SUB
     IF irtrn% = 3 GOTO 141
     IF irtrn% = 40 THEN GOSUB 191
     IF irtrn% > 5 OR irtrn% < 1 THEN GOTO 165
185  F2HELP$ = "#ETLOCATION"
     OLDSTATE$ = ETSTATE$
     CALL ACELL(9, 37, 20, ETSTATE$, 1)
     GOSUB 1190
     IF irtrn% = 1000 THEN GOTO 1195
     IF LEFT$(OLDSTATE$, 4) <> LEFT$(ETSTATE$, 4) THEN
	EVBARE$ = " "
	LOCATE 10, 48: PRINT "                                "
	LOCATE 11, 48: PRINT "                                "
	LOCATE 12, 48: PRINT "                                "
	LOCATE 13, 48: PRINT "                                "
     END IF
      IF irtrn% = 1 THEN EXIT SUB: GOTO 101
      IF irtrn% = -1 THEN EXIT SUB
      IF irtrn% = 2 THEN EXIT SUB
      IF irtrn% = 3 GOTO 165
      IF irtrn% = 40 THEN GOSUB 191
      IF irtrn% > 5 OR irtrn% < 1 THEN GOTO 185
1855  F2HELP$ = "#ETLOCATION"
      CALL ACELL(10, 37, 10, LATIT$, 2)
      GOSUB 1190
      IF irtrn% = 1000 THEN GOSUB 1195
      IF irtrn% = 1 THEN EXIT SUB: GOTO 101
      IF irtrn% = -1 THEN EXIT SUB
      IF irtrn% = 2 THEN EXIT SUB
      IF irtrn% = 3 GOTO 185
      IF irtrn% = 40 THEN GOSUB 191
      IF irtrn% > 5 OR irtrn% < 1 THEN GOTO 1855
186   F2HELP$ = "#ZONEDEPTH"
      CALL ACELL(12, 37, 5, evdepth$, 2)
      GOSUB 1190
      IF irtrn% = 1000 THEN GOSUB 1195
      IF irtrn% = 1 THEN EXIT SUB: GOTO 101
      IF irtrn% = -1 THEN EXIT SUB
      IF irtrn% = 2 THEN EXIT SUB
      IF irtrn% = 3 GOTO 1855
      IF irtrn% = 40 THEN GOSUB 191
      IF irtrn% > 5 OR irtrn% < 1 THEN GOTO 186
187   F2HELP$ = "#LAI"
      CALL ACELL(13, 37, 5, LAI$, 2)
      GOSUB 1190
      IF irtrn% = 1000 THEN GOSUB 1195
      IF VAL(EVBARE$) <> 0 THEN
       IF VAL(LAI$) > VAL(MAXLAI$) THEN
	 COLOR 4, 3: LOCATE 13, 48: PRINT "                                "
	 LOCATE 13, 48: PRINT "   MAX. LAI FOR CITY IS  ";
	 PRINT USING "\  \"; MAXLAI$;
	 COLOR 15, 1
       END IF
     END IF
     IF irtrn% = 1 THEN EXIT SUB: GOTO 101
     IF irtrn% = -1 THEN EXIT SUB
     IF irtrn% = 2 THEN EXIT SUB
     IF irtrn% = 3 GOTO 186
     IF irtrn% = 4 OR irtrn% = 5 THEN GOTO 188
     IF irtrn% = 40 THEN GOSUB 191
     IF irtrn% > 5 OR irtrn% < 1 THEN GOTO 187
     GOTO 141
188  F2HELP$ = "#SEASON"
     CALL ACELL(14, 37, 5, GR1$, 2)
     GOSUB 1190
     IF irtrn% = 1000 THEN GOSUB 1195
     IF irtrn% = 1 THEN EXIT SUB: GOTO 101
     IF irtrn% = -1 THEN EXIT SUB
     IF irtrn% = 2 THEN EXIT SUB
     IF irtrn% = 3 GOTO 187
     IF irtrn% = 4 OR irtrn% = 5 THEN GOTO 189
     IF irtrn% = 40 THEN GOSUB 191
     IF irtrn% > 5 OR irtrn% < 1 THEN GOTO 188
189  F2HELP$ = "#SEASON"
     CALL ACELL(15, 37, 5, GR2$, 2)
     GOSUB 1190
     IF irtrn% = 1000 THEN GOSUB 1195
     IF irtrn% = 1 THEN EXIT SUB: GOTO 101
     IF irtrn% = -1 THEN EXIT SUB
     IF irtrn% = 2 THEN EXIT SUB
     IF irtrn% = 3 GOTO 188
     IF irtrn% = 4 OR irtrn% = 5 THEN GOTO 200
     IF irtrn% = 40 THEN GOSUB 191
     GOTO 189
200  F2HELP$ = "#WIND"
     CALL ACELL(17, 37, 5, WIND$, 2)
     GOSUB 1190
     IF irtrn% = 1000 THEN GOSUB 1195
     IF irtrn% = 1 THEN EXIT SUB: GOTO 101
     IF irtrn% = -1 THEN EXIT SUB
     IF irtrn% = 2 THEN EXIT SUB
     IF irtrn% = 3 GOTO 189
     IF irtrn% = 4 OR irtrn% = 5 THEN GOTO 210
     IF irtrn% = 40 THEN GOSUB 191
     GOTO 200
210  F2HELP$ = "#HUMIDITY"
     CALL ACELL(19, 37, 5, HUMID1$, 2)
     IF VAL(HUMID1$) > 100 THEN HUMID1$ = ""
     GOSUB 1190
     IF irtrn% = 1000 THEN GOSUB 1195
     IF irtrn% = 1 THEN EXIT SUB: GOTO 101
     IF irtrn% = -1 THEN EXIT SUB
     IF irtrn% = 2 THEN EXIT SUB
     IF irtrn% = 3 GOTO 200
     IF irtrn% = 4 OR irtrn% = 5 THEN GOTO 230
     IF irtrn% = 40 THEN GOSUB 191
     GOTO 210
230  F2HELP$ = "#HUMIDITY"
     CALL ACELL(20, 37, 5, HUMID2$, 2)
     IF VAL(HUMID2$) > 100 THEN HUMID2$ = ""
     GOSUB 1190
     IF irtrn% = 1000 THEN GOSUB 1195
     IF irtrn% = 1 THEN EXIT SUB: GOTO 101
     IF irtrn% = -1 OR irtrn% = 2 THEN EXIT SUB
     IF irtrn% = 3 GOTO 210
     IF irtrn% = 4 OR irtrn% = 5 THEN GOTO 260
     IF irtrn% = 40 THEN GOSUB 191
     GOTO 230
260  F2HELP$ = "#HUMIDITY"
     CALL ACELL(21, 37, 5, HUMID3$, 2)
     IF VAL(HUMID3$) > 100 THEN HUMID3$ = ""
     GOSUB 1190
     IF irtrn% = 1000 THEN GOSUB 1195
     IF irtrn% = 1 OR irtrn% = -1 OR irtrn% = 2 THEN EXIT SUB
     IF irtrn% = 3 GOTO 230
     IF irtrn% = 4 OR irtrn% = 5 THEN GOTO 270
     IF irtrn% = 40 THEN GOSUB 191
     GOTO 260
270  F2HELP$ = "#HUMIDITY"
     CALL ACELL(22, 37, 5, HUMID4$, 2)
     IF VAL(HUMID4$) > 100 THEN HUMID4$ = ""
     GOSUB 1190
     IF irtrn% = 1000 THEN GOSUB 1195
     IF irtrn% = 1 OR irtrn% = -1 OR irtrn% = 2 THEN EXIT SUB
     IF irtrn% = 3 GOTO 260
     IF irtrn% = 4 OR irtrn% = 5 THEN GOTO 141
     IF irtrn% = 40 THEN GOSUB 191
     GOTO 270
'------------------------------------
'SELECT CITY AND GET DATA FROM TAPE2
'DISPLAY VALUES ON SCREEN FOR SELECTED
'CITY
'------------------------------------
191  CALL EVAPIND(ETCITY$, ETSTATE$, 1)
     IF LEN(ETCITY$) = 0 THEN RETURN
     LOCATE 8, 37: PRINT USING "\                    \"; ETCITY$
     LOCATE 9, 37: PRINT USING "\                    \"; ETSTATE$
     IF OLDCITY$ <> LTRIM$(RTRIM$(ETCITY$)) OR OLDSTATE$ <> LTRIM$(RTRIM$(ETSTATE$)) THEN
       evdepth$ = "     ": LAI$ = "     "
       LOCATE 12, stcol%: PRINT evdepth$
       LOCATE 13, stcol%: PRINT LAI$
       LOCATE 13, 48: PRINT "                                "
     END IF
     OLDCITY$ = ETCITY$
     OLDSTATE$ = ETSTATE$
     GOSUB 195
    RETURN
'---------------------------------------------------------------
'SUBROUTINE TO ISSUE GUIDANCE FOR EVAP. ZONE DEPTH, WIND AND HUMIDITY
'---------------------------------------------------------------
195  IF VAL(BARE$) = 0 THEN RETURN
     COLOR 11, 6
     LOCATE 10, 48: PRINT "                                "
     LOCATE 11, 48: PRINT "                                "
     LOCATE 12, 48: PRINT "                                "
     IF etunits$ = "1" THEN
       LOCATE 10, 48: PRINT "   EVAPORATIVE ZONE DEPTH (IN)  "
       EVBARE$ = BARE$: EVFAIR$ = FAIR$: EVEXCL$ = EXCL$: WIND$ = WSPEED$
     END IF
     IF etunits$ = "2" THEN
	LOCATE 10, 48: PRINT "   EVAPORATIVE ZONE DEPTH (CM)  "
	EVBARE$ = STR$(INT(VAL(BARE$) * 2.54))
	EVFAIR$ = STR$(INT(VAL(FAIR$) * 2.54))
	EVEXCL$ = STR$(INT(VAL(EXCL$) * 2.54))
	WIND$ = STR$(INT(VAL(WSPEED$) * 1.6))
     END IF
     WIND$ = LTRIM$(RTRIM$(WIND$))
     LOCATE 11, 48: PRINT "     BARE   FAIR   EXCELLENT    "
     LOCATE 12, 54: PRINT USING "\   \"; EVBARE$;
     LOCATE 12, 61: PRINT USING "\   \"; EVFAIR$;
     LOCATE 12, 70: PRINT USING "\   \"; EVEXCL$;
     COLOR 15, 1
     LOCATE 10, 37: PRINT USING "\        \"; LATIT$
     LOCATE 14, 37: PRINT USING "\ \"; GR1$;
     LOCATE 15, 37: PRINT USING "\ \"; GR2$;
     LOCATE 17, 37: PRINT USING "\   \"; WIND$;
     LOCATE 19, 37: PRINT USING "\    \"; HUMID1$
     LOCATE 20, 37: PRINT USING "\    \"; HUMID2$
     LOCATE 21, 37: PRINT USING "\    \"; HUMID3$
     LOCATE 22, 37: PRINT USING "\    \"; HUMID4$
     COLOR 15, 1
     RETURN
'------------------------------------
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
'------------------------------------
1195 CALL EXOUT(ANSCODE)
     IF ANSCODE = 0 THEN
	GOTO 101
     ELSE
	EXIT SUB
     END IF
RETURN
'-----------------------------------------------
'SUBROUTINE TO CHANGE ETDEPTH AND WIND SPEED UNITS
'-----------------------------------------------
11910 COLOR 15, 1
      IF VAL(etunits$) = 1 AND VAL(oldunits$) = 2 THEN
	  evd = VAL(evdepth$) / 2.54
	  evdepth$ = STR$(evd)
	  wsp = VAL(WIND$) / 1.6
	  WIND$ = STR$(wsp)
	  evdepth$ = LTRIM$(RTRIM$(evdepth$))
	  WIND$ = LTRIM$(RTRIM$(WIND$))
	  LOCATE 12, stcol%: PRINT USING "\   \"; evdepth$
	  LOCATE 17, stcol%: PRINT USING "\   \"; WIND$
       ELSEIF VAL(etunits$) = 2 AND VAL(oldunits$) = 1 THEN
	  evdepth$ = STR$(VAL(evdepth$) * 2.54)
	  WIND$ = STR$(VAL(WIND$) * 1.6)
	  evdepth$ = LTRIM$(RTRIM$(evdepth$))
	  WIND$ = LTRIM$(RTRIM$(WIND$))
	  LOCATE 12, stcol%: PRINT USING "\   \"; evdepth$
	  LOCATE 17, stcol%: PRINT USING "\   \"; WIND$
       END IF
	COLOR 15, 1: LOCATE 12, 43: PRINT unev$: LOCATE 17, 43: PRINT WINDU$
	RETURN
'----------------------------------------------
'SUBROUTINE TO CALL MANUAL MESSAGES FOR HELP
'----------------------------------------------
1190  IF irtrn% = 20 THEN CALL HLPMSG(F1HELP$)
       IF irtrn% = 21 THEN CALL HLPMSG(F2HELP$)
       IF irtrn% = 22 THEN CALL USERHELP
      RETURN
    EXIT SUB
END SUB

