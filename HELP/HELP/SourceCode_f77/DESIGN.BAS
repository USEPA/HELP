DECLARE SUB filechk (fdrive$, FPATH$, filen$, FOUND!)
DECLARE SUB SVFILE (SVPATH$)
DECLARE SUB CURVENUM ()
DECLARE SUB D10FILES ()
DECLARE SUB LASTPAGE ()
DECLARE SUB CHKLNDFL (ERREXIST)
DECLARE SUB CHKUSOIL (SOILSVD)
DECLARE SUB DESCHK (NLYR, ERREXIST)
DECLARE SUB LAYER ()
DECLARE SUB LAYERSHEET ()
DECLARE SUB LYRCHK (NLYR, ERREXIST)
DECLARE SUB ACELL (IROW!, ISTART!, NS!, TEXT$, ITYPE!)
DECLARE SUB BOX ()
DECLARE SUB EXOUT (ANSCODE!)
DECLARE SUB GETDIR (ext$, SELECT$, directory$)
DECLARE SUB HLPMSG (f2help$)
DECLARE SUB menu ()
DECLARE SUB RCELL (IROW!, ISTART!, NS!, TXT$)
DECLARE SUB USERHELP ()
   COMMON SHARED D10EDIT$, D11EDIT$, D11SAVE$, d10save$, D10PATH$
   COMMON SHARED hsoil$(), PROP$(), US$(), CW(), UNMOIST$
   COMMON SHARED UNITS$, UNIT$, UN1$, UN2$, UN3$, UN4$, UN5$, UNAREA$, UNSL$
   COMMON SHARED TOHELP%, NLAYER, NL%, TITLE$, IRTRN%
   COMMON SHARED moist$, SNOW$, PAGE%, soiln%
   COMMON SHARED APAGE%, VPAGE%, HPAGE%, DIRECT$
   COMMON SHARED BLANKS$, AREA$
   COMMON SHARED SELCN$, CN$, slope$, slength$, soiltex$, VEGET$, prcnt$
   COMMON SHARED CNII, UNI$, SVPATH$
   COMMON SHARED SROWBOX, SCOLBOX, BOXWIDTH, EROWBOX, LATIT$

20 DIM MDSCRP$(10), PROP$(21, 10, 3), hsoil$(50, 4)
   DIM US$(100), DEFT(12), DEFP(12), CW(3, 7), TM$(12), RM$(12), SYNTH$(12)
   DIM CMETH(3), CUNIT(3), CCITY$(3), CSTATE$(3)
   COLOR 15, 1
   VIEW PRINT 1 TO 25
   APAGE% = 0: VPAGE% = 1: HPAGE% = 2
   SCREEN 0, , VPAGE%, VPAGE%
   GOSUB 8000
   KEY OFF: COLOR 15, 1: CLS
''''''''''''''''''''''
' SET DEFAULT VALUES
''''''''''''''''''''''
   UNITS$ = "1"
   UN1$ = "    (IN)   ": UN2$ = "  (IN/SEC)   ": UN3$ = "   (FT)   "
   UN4$ = "   (IN/YR)  ": UN5$ = "    (#/ACRE)    ": UNAREA$ = "ACRES  "
   UNEV$ = "IN"
   TEMPU$ = "    (๘F)   ": PRECU$ = "  (IN)   ": SOLU$ = "  (LANGLEYS)  "
   BLANKS$ = "                                                                             "
   prcnt$ = "100"
   AREA$ = "1.0"
2000   CALL D10FILES
	CLOSE
	 IF IRTRN% = 1 OR IRTRN% = 2 THEN
	    IF LTRIM$(RTRIM$(D10EDIT$)) <> "" THEN GOSUB 6500
	    IF VAL(UNITS$) = 1 OR VAL(UNITS$) = 0 THEN
		UNITS$ = "1"
		UN1$ = "    (IN)   ": UN2$ = "  (IN/SEC)   ": UN3$ = "   (FT)   "
		UN4$ = "   (IN/YR)  ": UN5$ = "    (#/ACRE)    ": UNAREA$ = "ACRES    "
		UNEV$ = "IN": UNSL$ = "FT": UNMOIST$ = "IN"
	    ELSEIF VAL(UNITS$) = 2 THEN
		UN1$ = "    (CM)   ": UN2$ = "  (CM/SEC)   ": UN3$ = "   (M)    "
		UN4$ = "   (MM/YR)  ": UN5$ = "  (#/HECTARE)   ": UNAREA$ = "HECTARES"
		UNEV$ = "CM": UNSL$ = "M": UNMOIST$ = "MM"
	    END IF
	    GOTO 3000
	 END IF
	 IF IRTRN% = 1000 OR IRTRN% = -1 THEN
   SCREEN 0, , VPAGE%, VPAGE%
   CLS
   SCREEN 0, , HPAGE%, HPAGE%
   CLS
   SCREEN 0, , APAGE%, APAGE%
   CLS
	    CLEAR
	    END
	 END IF
3000   CALL LAYER
	 CLOSE
	 IF IRTRN% = 1 THEN
	   PAGE% = 1
	   GOTO 4000
	 END IF
	 IF IRTRN% = -1 THEN GOTO 5000
	 IF IRTRN% = 2 THEN GOTO 6000
	 IF IRTRN% = 1000 THEN
   SCREEN 0, , VPAGE%, VPAGE%
   CLS
   SCREEN 0, , HPAGE%, HPAGE%
   CLS
   SCREEN 0, , APAGE%, APAGE%
   CLS
	    CLEAR
	    END
	 END IF
4000     CALL LAYERSHEET
	 CLOSE
	 IF IRTRN% = 1 THEN GOTO 5000
	 IF IRTRN% = -1 THEN GOTO 3000
	 IF IRTRN% = 2 THEN GOTO 6000
	 IF IRTRN% = 1000 THEN
   SCREEN 0, , VPAGE%, VPAGE%
   CLS
   SCREEN 0, , HPAGE%, HPAGE%
   CLS
   SCREEN 0, , APAGE%, APAGE%
   CLS
	    CLEAR
	    END
	 END IF
5000   CALL CURVENUM
	 CLOSE
	 IF IRTRN% = 1 THEN GOTO 3000
	 IF IRTRN% = -1 THEN
	     PAGE% = 3: GOTO 4000
	 END IF
	 IF IRTRN% = 2 THEN GOTO 6000
	 IF IRTRN% = 1000 THEN
   SCREEN 0, , VPAGE%, VPAGE%
   CLS
   SCREEN 0, , HPAGE%, HPAGE%
   CLS
   SCREEN 0, , APAGE%, APAGE%
   CLS
	    CLEAR
	    END
	 END IF
6000   CALL LASTPAGE
	 CLOSE
	 IF IRTRN% = 1 THEN GOTO 3000
	 IF IRTRN% = -1 THEN GOTO 5000
	 IF IRTRN% = 2 THEN GOTO 3000
7000   SCREEN 0, , VPAGE%, VPAGE%
       CLS
   SCREEN 0, , HPAGE%, HPAGE%
   CLS
   SCREEN 0, , APAGE%, APAGE%
   CLS
       CLEAR
       END
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' SUBROUTINE TO READ SOIL AND DESIGN DATA FROM D10EDIT FILE
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
6500   CLOSE
       OPEN D10PATH$ + D10EDIT$ + ".D10" FOR INPUT AS #1
       IF EOF(1) THEN GOTO 6540
       LINE INPUT #1, a$
	 TITLE$ = LEFT$(a$, 60)
       IF EOF(1) THEN GOTO 6540
       LINE INPUT #1, a$
	 UNITS$ = LTRIM$(RTRIM$(LEFT$(a$, 2)))
	 MOIS$ = LTRIM$(RTRIM$(MID$(a$, 3, 2)))
	 IF MOIS$ = "1" THEN moist$ = "Y"
	 IF MOIS$ = "0" THEN moist$ = "N"
	 SNOW$ = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 5, 10)))))
	 AREA$ = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 15, 10)))))
	 prcnt$ = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 25, 6)))))
	 SELCN$ = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 31, 2)))))
	IF EOF(1) THEN GOTO 6540
	LINE INPUT #1, a$
	 IF VAL(SELCN$) = 1 THEN
	    CN$ = LTRIM$(RTRIM$(STR$(VAL(LEFT$(a$, 7)))))
	 ELSEIF VAL(SELCN$) = 2 THEN
	    CN$ = LTRIM$(RTRIM$(STR$(VAL(LEFT$(a$, 7)))))
	    slope$ = LTRIM$(STR$(VAL(RTRIM$(MID$(a$, 8, 7)))))
	    slength$ = LTRIM$(STR$(VAL(RTRIM$(MID$(a$, 15, 7)))))
	    CNII = VAL(LTRIM$(STR$(VAL(RTRIM$(MID$(a$, 22, 7))))))
	 ELSEIF VAL(SELCN$) = 3 THEN
	    slope$ = LTRIM$(RTRIM$(STR$(VAL(LEFT$(a$, 7)))))
	    slength$ = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 8, 7)))))
	    soiltex$ = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 15, 3)))))
	    VEGET$ = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 18, 3)))))
	    CNII = VAL(LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 21, 7))))))
	 END IF
       FOR i = 1 TO 20
	 IF EOF(1) THEN RETURN
	 LINE INPUT #1, a$
	   PROP$(i, 1, 1) = LTRIM$(RTRIM$(STR$(VAL(LEFT$(a$, 2)))))
	   PROP$(i, 1, 2) = PROP$(i, 1, 1)
	   PROP$(i, 1, 3) = PROP$(i, 1, 1)
	   PROP$(i, 2, 1) = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 3, 7)))))
	   PROP$(i, 3, 1) = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 10, 4)))))
	   PROP$(i, 4, 1) = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 14, 6)))))
	   PROP$(i, 5, 1) = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 20, 6)))))
	   PROP$(i, 6, 1) = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 26, 6)))))
	   PROP$(i, 7, 1) = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 32, 6)))))
	   PROP$(i, 2, 2) = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 38, 16)))))
	 IF EOF(1) THEN RETURN
	 LINE INPUT #1, a$
	   PROP$(i, 3, 2) = LTRIM$(RTRIM$(STR$(VAL(LEFT$(a$, 7)))))
	   PROP$(i, 4, 2) = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 8, 6)))))
	   PROP$(i, 5, 2) = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 14, 6)))))
	   PROP$(i, 6, 2) = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 20, 3)))))
	   PROP$(i, 7, 2) = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 23, 13)))))
	   PROP$(i, 2, 3) = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 36, 7)))))
	   PROP$(i, 3, 3) = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 43, 7)))))
	   PROP$(i, 4, 3) = LTRIM$(RTRIM$(STR$(VAL(MID$(a$, 50, 2)))))
	   PROP$(i, 5, 3) = LTRIM$(RTRIM$(MID$(a$, 52, 14)))
	NEXT i
6540 RETURN
''''''''''''''''''''''''''
'subroutine to define
'DEFAULT SOIL TEXTURES
''''''''''''''''''''''''''
8000
  hsoil$(1, 1) = ".417": hsoil$(1, 2) = ".045": hsoil$(1, 3) = ".018": hsoil$(1, 4) = "1.0E-02"
  hsoil$(2, 1) = ".437": hsoil$(2, 2) = ".062": hsoil$(2, 3) = ".024": hsoil$(2, 4) = "5.8E-03"
  hsoil$(3, 1) = ".457": hsoil$(3, 2) = ".083": hsoil$(3, 3) = ".033": hsoil$(3, 4) = "3.1E-03"
  hsoil$(4, 1) = ".437": hsoil$(4, 2) = ".105": hsoil$(4, 3) = ".047": hsoil$(4, 4) = "1.7E-03"
  hsoil$(5, 1) = ".457": hsoil$(5, 2) = ".131": hsoil$(5, 3) = ".058": hsoil$(5, 4) = "1.0E-03"
  hsoil$(6, 1) = ".453": hsoil$(6, 2) = ".190": hsoil$(6, 3) = ".085": hsoil$(6, 4) = "7.2E-04"
  hsoil$(7, 1) = ".473": hsoil$(7, 2) = ".222": hsoil$(7, 3) = ".104": hsoil$(7, 4) = "5.2E-04"
  hsoil$(8, 1) = ".463": hsoil$(8, 2) = ".232": hsoil$(8, 3) = ".116": hsoil$(8, 4) = "3.7E-04"
  hsoil$(9, 1) = ".501": hsoil$(9, 2) = ".284": hsoil$(9, 3) = ".135": hsoil$(9, 4) = "1.9E-04"
  hsoil$(10, 1) = ".398": hsoil$(10, 2) = ".244": hsoil$(10, 3) = ".136": hsoil$(10, 4) = "1.2E-04"
  hsoil$(11, 1) = ".464": hsoil$(11, 2) = ".310": hsoil$(11, 3) = ".187": hsoil$(11, 4) = "6.4E-05"
  hsoil$(12, 1) = ".471": hsoil$(12, 2) = ".342": hsoil$(12, 3) = ".210": hsoil$(12, 4) = "4.2E-05"
  hsoil$(13, 1) = ".430": hsoil$(13, 2) = ".321": hsoil$(13, 3) = ".221": hsoil$(13, 4) = "3.3E-05"
  hsoil$(14, 1) = ".479": hsoil$(14, 2) = ".371": hsoil$(14, 3) = ".251": hsoil$(14, 4) = "2.5E-05"
  hsoil$(15, 1) = ".475": hsoil$(15, 2) = ".378": hsoil$(15, 3) = ".265": hsoil$(15, 4) = "1.7E-05"
  hsoil$(16, 1) = ".427": hsoil$(16, 2) = ".418": hsoil$(16, 3) = ".367": hsoil$(16, 4) = "1.0E-07"
  hsoil$(17, 1) = ".750": hsoil$(17, 2) = ".747": hsoil$(17, 3) = ".400": hsoil$(17, 4) = "3.0E-09"
  hsoil$(18, 1) = ".671": hsoil$(18, 2) = ".292": hsoil$(18, 3) = ".077": hsoil$(18, 4) = "1.0E-03"
  hsoil$(19, 1) = ".168": hsoil$(19, 2) = ".073": hsoil$(19, 3) = ".019": hsoil$(19, 4) = "1.0E-03"
  hsoil$(20, 1) = ".850": hsoil$(20, 2) = ".010": hsoil$(20, 3) = ".005": hsoil$(20, 4) = "1.0E+01"
  hsoil$(21, 1) = ".397": hsoil$(21, 2) = ".032": hsoil$(21, 3) = ".013": hsoil$(21, 4) = "3.0E-01"
  hsoil$(22, 1) = ".419": hsoil$(22, 2) = ".307": hsoil$(22, 3) = ".180": hsoil$(22, 4) = "1.9E-05"
  hsoil$(23, 1) = ".461": hsoil$(23, 2) = ".360": hsoil$(23, 3) = ".203": hsoil$(23, 4) = "9.0E-06"
  hsoil$(24, 1) = ".365": hsoil$(24, 2) = ".305": hsoil$(24, 3) = ".202": hsoil$(24, 4) = "2.7E-06"
  hsoil$(25, 1) = ".437": hsoil$(25, 2) = ".373": hsoil$(25, 3) = ".266": hsoil$(25, 4) = "3.6E-06"
  hsoil$(26, 1) = ".445": hsoil$(26, 2) = ".393": hsoil$(26, 3) = ".277": hsoil$(26, 4) = "1.9E-06"
  hsoil$(27, 1) = ".400": hsoil$(27, 2) = ".366": hsoil$(27, 3) = ".288": hsoil$(27, 4) = "7.8E-07"
  hsoil$(28, 1) = ".452": hsoil$(28, 2) = ".411": hsoil$(28, 3) = ".311": hsoil$(28, 4) = "1.2E-06"
  hsoil$(29, 1) = ".451": hsoil$(29, 2) = ".419": hsoil$(29, 3) = ".332": hsoil$(29, 4) = "6.8E-07"
  hsoil$(30, 1) = ".541": hsoil$(30, 2) = ".187": hsoil$(30, 3) = ".047": hsoil$(30, 4) = "5.0E-05"
  hsoil$(31, 1) = ".578": hsoil$(31, 2) = ".076": hsoil$(31, 3) = ".025": hsoil$(31, 4) = "4.1E-03"
  hsoil$(32, 1) = ".450": hsoil$(32, 2) = ".116": hsoil$(32, 3) = ".049": hsoil$(32, 4) = "1.0E-02"
  hsoil$(33, 1) = ".375": hsoil$(33, 2) = ".055": hsoil$(33, 3) = ".020": hsoil$(33, 4) = "4.1E-02"
  hsoil$(34, 1) = ".850": hsoil$(34, 2) = ".010": hsoil$(34, 3) = ".005": hsoil$(34, 4) = "3.3E+01"
  hsoil$(35, 1) = "    ": hsoil$(35, 2) = "    ": hsoil$(35, 3) = "    ": hsoil$(35, 4) = "2.0E-13"
  hsoil$(36, 1) = "    ": hsoil$(36, 2) = "    ": hsoil$(36, 3) = "    ": hsoil$(36, 4) = "4.0E-13"
  hsoil$(37, 1) = "    ": hsoil$(37, 2) = "    ": hsoil$(37, 3) = "    ": hsoil$(37, 4) = "2.0E-11"
  hsoil$(38, 1) = "    ": hsoil$(38, 2) = "    ": hsoil$(38, 3) = "    ": hsoil$(38, 4) = "1.0E-12"
  hsoil$(39, 1) = "    ": hsoil$(39, 2) = "    ": hsoil$(39, 3) = "    ": hsoil$(39, 4) = "4.0E-12"
  hsoil$(40, 1) = "    ": hsoil$(40, 2) = "    ": hsoil$(40, 3) = "    ": hsoil$(40, 4) = "3.0E-12"
  hsoil$(41, 1) = "    ": hsoil$(41, 2) = "    ": hsoil$(41, 3) = "    ": hsoil$(41, 4) = "2.0E-12"
  hsoil$(42, 1) = "    ": hsoil$(42, 2) = "    ": hsoil$(42, 3) = "    ": hsoil$(42, 4) = "3.0E-12"
  CLOSE
  RETURN
  SCREEN 0, , VPAGE%, VPAGE%
  CLS
   SCREEN 0, , HPAGE%, HPAGE%
   CLS
   SCREEN 0, , APAGE%, APAGE%
   CLS
  CLEAR
END

SUB CURVENUM
    DIM MIR(30)
    MIR(1) = .5: MIR(2) = .4: MIR(3) = .39: MIR(4) = .38: MIR(5) = .34
    MIR(6) = .3: MIR(7) = .25: MIR(8) = .2: MIR(9) = .17: MIR(10) = .11
    MIR(11) = .09: MIR(12) = .07: MIR(13) = .06: MIR(14) = .02: MIR(15) = .01
    MIR(16) = .002: MIR(17) = .001: MIR(18) = .4: MIR(19) = .4: MIR(21) = .54
220 CLS
   F1HELP$ = "#CNSELECT"
   COLOR 11, 6
   LOCATE 1, 1:   PRINT "             SOIL AND DESIGN DATA - RUNOFF CURVE NUMBER INFORMATION             "
   COLOR 15, 1
   LOCATE 3, 2:   PRINT "    ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
   LOCATE 4, 2:   PRINT "    บ            Select Method For Computing Runoff Curve Number        บ"
   LOCATE 5, 2:   PRINT "    บ                                                                   บ"
   LOCATE 6, 2:   PRINT "    บ                 User Specified Curve Number                       บ"
   LOCATE 7, 2:   PRINT "    บ                 Modified User's Curve Number                      บ"
   LOCATE 8, 2:   PRINT "    บ                 HELP Model Computed Curve Number                  บ"
   LOCATE 9, 2:   PRINT "    ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
   LOCATE 11, 2:  PRINT "ษอออออออออออออออออออออป ษออออออออออออออออออออออออป ษออออออออออออออออออออออออป"
   LOCATE 12, 2:  PRINT "บ  USER SPECIFIED CN  บ บ   MODIFIED USER'S CN   บ บ     HELP MODEL CN      บ"
   LOCATE 13, 2:  PRINT "ฬอออออออออออออออออออออน ฬออออออออออออออออออออออออน ฬออออออออออออออออออออออออน"
   LOCATE 14, 2:  PRINT "บ                     บ บ                        บ บ                        บ"
   LOCATE 15, 2:  PRINT "บ SCS AMCII CN=       บ บ SCS AMCII CN=          บ บ        Slope=        % บ"
   LOCATE 16, 2:  PRINT "บ                     บ บ                        บ บ Slope Length=          บ"
   LOCATE 17, 2:  PRINT "บ                     บ บ        Slope=        % บ บ Soil Texture=          บ"
   LOCATE 18, 2:  PRINT "บ                     บ บ Slope Length=          บ บ   Vegetation=          บ"
   LOCATE 19, 2:  PRINT "ศอออออออออออออออออออออผ ศออออออออออออออออออออออออผ ศออออออออออออออออออออออออผ"
   LOCATE 21, 2:  PRINT "             ษอออออออออออออออออออออออออออออออออออออออออออออออออป"
   LOCATE 22, 2:  PRINT "             บ Program will use a runoff curve number of       บ"
   LOCATE 23, 2:  PRINT "             ศอออออออออออออออออออออออออออออออออออออออออออออออออผ"
   COLOR 4, 3
   LOCATE 25, 1: PRINT "  F1=Info   F2=Help   F3=Keys   F9=Quit   F10=End   PgUp=Cycle   PgDn=Proceed   ";
   COLOR 15, 1
   'CALL menu
   LOCATE 15, 18: PRINT CN$
   LOCATE 15, 42: PRINT CN$
   LOCATE 17, 42: PRINT slope$
   LOCATE 18, 42: PRINT slength$
   LOCATE 15, 69: PRINT slope$
   LOCATE 16, 69: PRINT slength$
   IF CNII <> 0 THEN LOCATE 22, 59: PRINT USING "###.#"; CNII
   IF VAL(soiltex$) = 0 THEN
     IF VAL(PROP$(1, 3, 1)) <= 30 THEN
       soiltex$ = PROP$(1, 3, 1)
     END IF
   END IF
   LOCATE 17, 69: PRINT soiltex$
   LOCATE 18, 69: PRINT VEGET$
   LOCATE 18, 49: PRINT UNSL$: LOCATE 16, 76: PRINT UNSL$
   SELCN1$ = "User Specified Curve Number"
   SELCN2$ = "Modified User's Curve Number"
   SELCN3$ = "HELP Model Computed Curve Number"
   CN1$ = "  USER SPECIFIED CN  "
   CN2$ = "   MODIFIED USER'S CN   "
   CN3$ = "     HELP MODEL CN      "
   IF SELCN$ = "1" THEN
       LOCATE 6, 22: PRINT CHR$(26)
       CNII = VAL(CN$): LOCATE 22, 59: PRINT USING "###.#"; CNII
       GOTO 233
   END IF
   IF SELCN$ = "2" THEN
       LOCATE 7, 22: PRINT CHR$(26)
       GOTO 235
   END IF
   IF SELCN$ = "3" THEN
       LOCATE 8, 22: PRINT CHR$(26)
       GOTO 240
   END IF
'USER SEPCIFIED CURVE NUMBER
233  XLEN = LEN(SELCN1$)
     LOCATE 12, 3: COLOR 1, 15: PRINT CN1$
     GOSUB 23322
     f2help$ = "#CNUSER"
     CALL RCELL(6, 24, XLEN, SELCN1$)
     GOSUB 23190
     IF IRTRN% = 3 OR IRTRN% = 7 THEN
	 LOCATE 12, 3: COLOR 15, 1: PRINT CN1$
	 LOCATE 12, 54: COLOR 1, 15: PRINT CN3$
	 GOTO 240
     END IF
     IF IRTRN% = 4 OR IRTRN% = 6 THEN
	LOCATE 12, 3: COLOR 15, 1: PRINT CN1$
	LOCATE 12, 27: COLOR 1, 15: PRINT CN2$
	GOTO 235
     END IF
     IF IRTRN% = 1000 THEN GOSUB 2196
     IF IRTRN% = 2 OR IRTRN% = 1 OR IRTRN% = -1 THEN EXIT SUB
2331 IF IRTRN% = 5 THEN
       GOSUB 23311
       SELCN$ = "1"
       LOCATE 6, 24: COLOR 1, 15: PRINT SELCN1$
       COLOR 15, 1: LOCATE 12, 27: PRINT CN2$: LOCATE 12, 54: PRINT CN3$
       COLOR 1, 15: LOCATE 12, 3: PRINT CN1$
234    f2help$ = "#CNAMCII"
       CALL ACELL(15, 18, 5, CN$, 2)
       IF VAL(CN$) > 100 OR VAL(CN$) < 0 THEN CN$ = "     ": LOCATE 15, 18: PRINT CN$
       GOSUB 23190
       IF IRTRN% = 5 OR IRTRN% = 100 OR IRTRN% = 2 OR IRTRN% = 1 OR IRTRN% = -1 THEN
	 CNII = VAL(CN$)
	 LOCATE 22, 59: PRINT USING "###.#"; CNII
	 LOCATE 7, 22: PRINT " "
	 LOCATE 8, 22: PRINT " "
	 LOCATE 6, 22: PRINT CHR$(26)
	 IF IRTRN% = 2 OR IRTRN% = 1 OR IRTRN% = -1 THEN EXIT SUB
	 GOTO 233
       END IF
       IF IRTRN% = 6 THEN
	LOCATE 6, 24: COLOR 15, 1: PRINT SELCN1$
	LOCATE 7, 24: COLOR 1, 15: PRINT SELCN2$
	IRTRN% = 5: GOTO 2351
       END IF
       IF IRTRN% = 7 THEN
	  LOCATE 6, 24: COLOR 15, 1: PRINT SELCN1$
	  LOCATE 8, 24: COLOR 1, 15: PRINT SELCN3$
	  IRTRN% = 5: GOTO 2401
       END IF
       IF IRTRN% = 1000 THEN GOSUB 2196
       IF IRTRN% = 2 OR IRTRN% = 1 OR IRTRN% = -1 THEN EXIT SUB
       GOTO 234
     END IF
     GOTO 233
'MODIFY USER'S CURVE NUMBER
235 f2help$ = "#CNMODIFY"
    LOCATE 12, 27: COLOR 1, 15: PRINT CN2$
    XLEN = LEN(SELCN2$)
    GOSUB 23322
    CALL RCELL(7, 24, XLEN, SELCN2$)
    GOSUB 23190
    IF IRTRN% = 1000 THEN GOSUB 2196
    IF IRTRN% = 3 OR IRTRN% = 7 THEN
       LOCATE 12, 27: COLOR 15, 1: PRINT CN2$
       LOCATE 12, 3: COLOR 1, 15: PRINT CN1$
       GOTO 233
    END IF
    IF IRTRN% = 4 OR IRTRN% = 6 THEN
	LOCATE 12, 27: COLOR 15, 1: PRINT CN2$
	LOCATE 12, 54: COLOR 1, 15: PRINT CN3$
       GOTO 240
    END IF
    IF IRTRN% = 2 OR IRTRN% = 1 OR IRTRN% = -1 THEN EXIT SUB
2351 IF IRTRN% = 5 THEN
       GOSUB 23311
       SELCN$ = "2"
       LOCATE 7, 24: COLOR 1, 15: PRINT SELCN2$
       COLOR 15, 1:  LOCATE 12, 3: PRINT CN1$: LOCATE 12, 54: PRINT CN3$
       COLOR 1, 15: LOCATE 12, 27: PRINT CN2$
236  f2help$ = "#CNAMCII"
     CALL ACELL(15, 42, 5, CN$, 2)
     IF VAL(CN$) = 0 OR VAL(CN$) > 100 THEN CN$ = "     ": LOCATE 15, 42: PRINT CN$: CNII = 0: LOCATE 22, 59: PRINT USING "###.#"; CNII
     GOSUB 23190
       IF IRTRN% = 1000 THEN GOSUB 2196
       IF IRTRN% = 2 OR IRTRN% = 1 OR IRTRN% = -1 THEN GOSUB 24433: EXIT SUB
       IF IRTRN% = 3 THEN GOTO 238
       IF IRTRN% = 5 OR IRTRN% = 4 THEN GOTO 237
       IF IRTRN% = 100 THEN GOSUB 24433: GOTO 235
       IF IRTRN% = 6 THEN
	   LOCATE 7, 24: COLOR 15, 1: PRINT SELCN2$
	   LOCATE 8, 24: COLOR 1, 15: PRINT SELCN3$
	   IRTRN% = 5
	   GOTO 2401
       END IF
       IF IRTRN% = 7 THEN
	   LOCATE 7, 24: COLOR 15, 1: PRINT SELCN2$
	   LOCATE 6, 24: COLOR 1, 15: PRINT SELCN1$
	   IRTRN% = 5
	   GOTO 2331
       END IF
       GOTO 236
237   f2help$ = "#CNSLOPE"
      CALL ACELL(17, 42, 6, slope$, 2)
      IF VAL(slope$) = 0 THEN slope$ = "     ": LOCATE 17, 42: PRINT slope$: CNII = 0: LOCATE 22, 59: PRINT USING "###.#"; CNII
      GOSUB 23190
      'XVAL$ = SLOPE$: XWIDE = 6: XR = 20: XC = 42: GOSUB 2260: SLOPE$ = XVAL$
	  IF IRTRN% = 1000 THEN GOSUB 2196
	  IF IRTRN% = 2 OR IRTRN% = 1 OR IRTRN% = -1 THEN GOSUB 24433: EXIT SUB
	  IF IRTRN% = 3 THEN GOTO 236
	  IF (IRTRN% = 5) OR (IRTRN% = 4) THEN GOTO 238
	  IF IRTRN% = 100 GOTO 235
	  IF IRTRN% = 6 THEN
	     LOCATE 7, 24: COLOR 15, 1: PRINT SELCN2$
	     LOCATE 8, 24: COLOR 1, 15: PRINT SELCN3$
	     IRTRN% = 5: GOTO 2401
	  END IF
	  IF IRTRN% = 7 THEN
	     LOCATE 7, 24: COLOR 15, 1: PRINT SELCN2$
	     LOCATE 6, 24: COLOR 1, 15: PRINT SELCN1$
	     IRTRN% = 5: GOTO 2331
	  END IF
	  GOTO 237
238  f2help$ = "#CNSLOPE"
     CALL ACELL(18, 42, 6, slength$, 2)
     IF VAL(slength$) = 0 THEN slength$ = "      ": LOCATE 18, 42: PRINT slength$: CNII = 0: LOCATE 22, 59: PRINT USING "###.#"; CNII
     GOSUB 23190
     IF IRTRN% = 1000 THEN GOSUB 2196
     IF IRTRN% = 2 OR IRTRN% = 1 OR IRTRN% = -1 THEN GOSUB 24433: EXIT SUB
	IF IRTRN% = 3 THEN GOTO 237
	IF (IRTRN% = 5) THEN
	   GOSUB 24433
	   LOCATE 6, 22: PRINT " "
	   LOCATE 8, 22: PRINT " "
	   LOCATE 7, 22: PRINT CHR$(26)
	   GOTO 235
	END IF
	IF (IRTRN% = 4) THEN GOTO 236
	IF IRTRN% = 100 THEN GOTO 235
	IF IRTRN% = 6 THEN
	     LOCATE 7, 24: COLOR 15, 1: PRINT SELCN2$
	     LOCATE 8, 24: COLOR 1, 15: PRINT SELCN3$
	     IRTRN% = 5: GOTO 2401
	END IF
	IF IRTRN% = 7 THEN
	     LOCATE 7, 24: COLOR 15, 1: PRINT SELCN2$
	     LOCATE 6, 24: COLOR 1, 15: PRINT SELCN1$
	     IRTRN% = 5: GOTO 2331
	END IF
	GOTO 238
     END IF
     GOTO 235
'''''''''''''''''''''''''''''
'HELP COMPUTED CURVE NUMBER
'''''''''''''''''''''''''''''
240  f2help$ = "#CNHELP"
     LOCATE 12, 54: COLOR 1, 15: PRINT CN3$
     XLEN = LEN(SELCN3$)
     GOSUB 23322
     CALL RCELL(8, 24, XLEN, SELCN3$)
     GOSUB 23190
     IF IRTRN% = 1000 THEN GOSUB 2196
     IF IRTRN% = 2 OR IRTRN% = 1 OR IRTRN% = -1 THEN EXIT SUB
     IF IRTRN% = 3 OR IRTRN% = 7 THEN
	LOCATE 12, 54: COLOR 15, 1: PRINT CN3$
	LOCATE 12, 27: COLOR 1, 15: PRINT CN2$
	GOTO 235
     END IF
     IF IRTRN% = 4 OR IRTRN% = 6 THEN
	 LOCATE 12, 54: COLOR 15, 1: PRINT CN3$
	 LOCATE 12, 3: COLOR 1, 15: PRINT CN1$
	 GOTO 233
     END IF
2401 IF IRTRN% = 5 THEN
       GOSUB 23311
       SELCN$ = "3"
       LOCATE 8, 24: COLOR 1, 15: PRINT SELCN3$
       COLOR 15, 1: LOCATE 12, 3: PRINT CN1$: LOCATE 12, 27: PRINT CN2$
       COLOR 1, 15: LOCATE 12, 54: PRINT CN3$
241    f2help$ = "#CNSLOPE"
       CALL ACELL(15, 69, 6, slope$, 2)
       IF VAL(slope$) = 0 THEN slope$ = "      ": LOCATE 15, 69: PRINT slope$: CNII = 0: LOCATE 22, 59: PRINT USING "###.#"; CNII
       GOSUB 23190
       IF IRTRN% = 1000 THEN GOSUB 2196
       IF IRTRN% = 2 OR IRTRN% = 1 OR IRTRN% = -1 THEN GOSUB 25533:  EXIT SUB
       IF (IRTRN% = 5) OR (IRTRN% = 4) THEN GOTO 242
       IF IRTRN% = 3 THEN GOTO 244
       IF IRTRN% = 100 THEN GOTO 240
       IF IRTRN% = 6 THEN
	    LOCATE 8, 24: COLOR 15, 1: PRINT SELCN3$
	    LOCATE 6, 24: COLOR 1, 15: PRINT SELCN1$
	    IRTRN% = 5: GOTO 2331
       END IF
       IF IRTRN% = 7 THEN
	     LOCATE 8, 24: COLOR 15, 1: PRINT SELCN3$
	     LOCATE 7, 24: COLOR 1, 15: PRINT SELCN2$
	   IRTRN% = 5: GOTO 2351
       END IF
       GOTO 241
242    f2help$ = "#CNSLOPE"
       CALL ACELL(16, 69, 6, slength$, 2)
       IF VAL(slength$) = 0 THEN slength$ = "      ": LOCATE 16, 69: PRINT slength$: CNII = 0: LOCATE 22, 59: PRINT USING "###.#"; CNII
       GOSUB 23190
       IF IRTRN% = 1000 THEN GOSUB 2196
       IF IRTRN% = 2 OR IRTRN% = 1 OR IRTRN% = -1 THEN GOSUB 25533:  EXIT SUB
       IF (IRTRN% = 5) OR (IRTRN% = 4) THEN GOTO 243
       IF IRTRN% = 3 THEN GOTO 241
       IF IRTRN% = 100 THEN GOTO 240
       IF IRTRN% = 6 THEN
	   LOCATE 8, 24: COLOR 15, 1: PRINT SELCN3$
	   LOCATE 6, 24: COLOR 1, 15: PRINT SELCN1$
	   IRTRN% = 5: GOTO 2331
       END IF
       IF IRTRN% = 7 THEN
	   LOCATE 8, 24: COLOR 15, 1: PRINT SELCN3$
	   LOCATE 7, 24: COLOR 1, 15: PRINT SELCN2$
	   IRTRN% = 5: GOTO 2351
       END IF
       GOTO 242
243    f2help$ = "#CNTEXTURE"
       CALL ACELL(17, 69, 2, soiltex$, 2)
       IF (VAL(soiltex$) > 0 AND VAL(soiltex$) <= 19) OR (VAL(soiltex$) > 20 AND VAL(soiltex$) < 34) THEN GOTO 203040
       soiltex$ = "  ": LOCATE 17, 69: PRINT soiltex$
       CNII = 0: LOCATE 22, 59: PRINT USING "###.#"; CNII
203040 GOSUB 23190
       IF IRTRN% = 1000 THEN GOSUB 2196
       IF IRTRN% = 2 OR IRTRN% = 1 OR IRTRN% = -1 THEN GOSUB 25533:  EXIT SUB
       IF IRTRN% = 5 OR IRTRN% = 4 GOTO 244
       IF IRTRN% = 100 THEN GOTO 240
       IF IRTRN% = 3 THEN GOTO 242
       IF IRTRN% = 6 THEN
	  LOCATE 8, 24: COLOR 15, 1: PRINT SELCN3$
	  LOCATE 6, 24: COLOR 1, 15: PRINT SELCN1$
	  IRTRN% = 5: GOTO 2331
       END IF
       IF IRTRN% = 7 THEN
	  LOCATE 8, 24: COLOR 15, 1: PRINT SELCN3$
	  LOCATE 7, 24: COLOR 1, 15: PRINT SELCN2$
	  IRTRN% = 5: GOTO 2351
       END IF
       GOTO 243
244    f2help$ = "#CNVEGETATION"
       CALL ACELL(18, 69, 1, VEGET$, 2)
       IF VAL(VEGET$) < 1 OR VAL(VEGET$) > 5 THEN VEGET$ = " ": LOCATE 18, 69: PRINT VEGET$: CNII = 0: LOCATE 22, 59: PRINT USING "###.#"; CNII
       GOSUB 23190
       IF IRTRN% = 1000 THEN GOSUB 2196
       IF IRTRN% = 2 OR IRTRN% = 1 OR IRTRN% = -1 THEN GOSUB 25533: EXIT SUB
       IF IRTRN% = 5 THEN
	   LOCATE 6, 22: PRINT " "
	   LOCATE 7, 22: PRINT " "
	   LOCATE 8, 22: PRINT CHR$(26)
	   GOSUB 25533
	   GOTO 240
       END IF
       IF IRTRN% = 4 THEN GOTO 241
       IF IRTRN% = 3 THEN GOTO 243
       IF IRTRN% = 100 THEN GOTO 240
       IF IRTRN% = 6 THEN
	  LOCATE 8, 24: COLOR 15, 1: PRINT SELCN3$
	  LOCATE 6, 24: COLOR 1, 15: PRINT SELCN1$
	  IRTRN% = 5: GOTO 2331
       END IF
       IF IRTRN% = 7 THEN
	  LOCATE 8, 24: COLOR 15, 1: PRINT SELCN3$
	  LOCATE 7, 24: COLOR 1, 15: PRINT SELCN2$
	  IRTRN% = 5: GOTO 2351
       END IF
       GOTO 244
     END IF
     GOTO 240
297  EXIT SUB
23311 COLOR 11, 6
      LOCATE 24, 1: PRINT "  Enter value then press Enter              Esc = Return to method selection   ";
      COLOR 15, 1
      RETURN
23322 COLOR 11, 6
      LOCATE 24, 1: PRINT "  Cursor    and press Enter to select method for computing runoff curve number  ";
      LOCATE 24, 10: PRINT CHR$(24); CHR$(25);
      COLOR 15, 1
      RETURN
'''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO COMPUTE USER MODIFIED CURVE NUMBER
'''''''''''''''''''''''''''''''''''''''''''''''''
24433 IF VAL(slope$) = 0 OR VAL(slength$) = 0 OR VAL(CN$) = 0 THEN RETURN
      IF UNITS$ = "1" THEN
	LMODIF = VAL(slength$) / 500
      ELSE
	LMODIF = VAL(slength$) * 3.280833 / 500     'CONVERT METERS INTO FEET
      END IF
      SMODIF = VAL(slope$) / 4
      CNII0 = VAL(CN$)
      CNII = 100 - ((100 - CNII0) * (LMODIF ^ 2 / SMODIF) ^ (CNII0 ^ (-.82)))
      LOCATE 22, 59: PRINT USING "###.#"; CNII
      RETURN
'''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO COMPUTE HELP CURVE NUMBER AND
'MODIFY IT BASED ON SLOPE AND SLOPE LENGTH
'''''''''''''''''''''''''''''''''''''''''''''''''
25533 IF VAL(slope$) = 0 OR VAL(slength$) = 0 OR VAL(VEGET$) > 5 OR VAL(VEGET$) < 1 THEN RETURN
      IF VAL(soiltex$) <= 33 AND VAL(soiltex$) > 0 THEN
      IF VAL(soiltex$) = 20 THEN RETURN
	IF VEGET$ = "1" THEN                      'BARE GROUND
	  C0 = 96.77: C1 = -20.8: C2 = -54.94
	ELSEIF VEGET$ = "2" THEN
	  C0 = 93.51: C1 = -24.85: C2 = -71.92    'POOR GRASS
	ELSEIF VEGET$ = "3" THEN
	  C0 = 90.09: C1 = -23.73: C2 = -158.4    'FAIR GRASS
	ELSEIF VEGET$ = "4" THEN
	  C0 = 86.72: C1 = -43.38: C2 = -151.2    'GOOD GRASS
	ELSEIF VEGET$ = "5" THEN
	  C0 = 83.83: C1 = -26.91: C2 = -229.4    'EXCELLENT GRASS
	END IF
	IF (VAL(soiltex$) <= 21 AND VAL(soiltex$) > 0) THEN
	  ISOIL = VAL(soiltex$)
	  CNII0 = C0 + C1 * MIR(ISOIL) + C2 * MIR(ISOIL) ^ 2
	END IF
	IF VAL(soiltex$) <= 33 AND VAL(soiltex$) > 21 THEN
	  KCON = VAL(PROP$(1, 2, 2))
	  MIRCOPM = -14669.292# - 14859.874# * KCON + 14669.263# * EXP(KCON) + 16.968947# * SQR(KCON)
	  CNII0 = C0 + C1 * MIRCOMP + C2 * MIRCOMP ^ 2
	END IF
	IF UNITS$ = "1" THEN
	  LMODIF = VAL(slength$) / 500
	ELSE
	  LMODIF = VAL(slength$) * 3.280833 / 500     'CONVERT METERS INTO FEET
	END IF
	SMODIF = VAL(slope$) / 4
	CNII = 100 - ((100 - CNII0) * (LMODIF ^ 2 / SMODIF) ^ (CNII0 ^ (-.82)))
	LOCATE 22, 59: PRINT USING "###.#"; CNII
      END IF
      RETURN
'''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO CALL MANUAL MESSAGES FOR HELP
'''''''''''''''''''''''''''''''''''''''''''''''''
23190  IF IRTRN% = 22 THEN CALL USERHELP
       IF IRTRN% = 21 THEN CALL HLPMSG(f2help$)
       IF IRTRN% = 20 THEN CALL HLPMSG(F1HELP$)
      RETURN
''''''''''''''''''''''''''''''''''''''''
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
''''''''''''''''''''''''''''''''''''''''
2196 CALL EXOUT(ANSCODE)
     IF ANSCODE = 0 THEN
	GOTO 220
     ELSE
	EXIT SUB
     END IF
RETURN
END SUB

SUB D10FILES
7101 CLS
     ON LOCAL ERROR RESUME NEXT
     F1HELP$ = "#D10EDIT"
     defdir$ = CURDIR$
     D10PATH$ = CURDIR$
     ext$ = "D10"
     COLOR 11, 6
     LOCATE 1, 1:  PRINT "                     SOIL AND DESIGN DATA - FILE EDITING                        "
     COLOR 15, 1
     SROWBOX = 5: SCOLBOX = 2: BOXWIDTH = 78: EROWBOX = 8
     CALL BOX
     LOCATE 6, 28: PRINT "---------------DIRECTORY----------------  --FILE--"
     LOCATE 7, 3:  PRINT " SOIL AND DESIGN DATA"
     LOCATE 16, 7: PRINT "Enter name for file that you wish to edit, or PgDn to create new data."
     CALL menu
     GOSUB 78165
     LOCATE 7, 70: PRINT D10EDIT$
7140 CALL ACELL(7, 28, 40, D10PATH$, 1)
     LOCATE 9, 20: FOR III = 1 TO 55: PRINT " "; : NEXT III
     f2help$ = "#PATH"
     GOSUB 73290
     D10PATH$ = LTRIM$(RTRIM$(D10PATH$))
     IF D10PATH$ = "" THEN D10PATH$ = CURDIR$
     IF RIGHT$(D10PATH$, 1) <> "\" THEN D10PATH$ = D10PATH$ + "\"
101  IF RIGHT$(D10PATH$, 2) = "\\" THEN
       D10PATH$ = LEFT$(D10PATH$, LEN(D10PATH$) - 1)
       GOTO 101
     END IF
     IF IRTRN% = 5 OR IRTRN% = 6 OR IRTRN% = 7 THEN
	GOSUB 83190
	GOTO 7145
     END IF
     IF IRTRN% = 1 OR IRTRN% = 2 THEN
       IF LTRIM$(RTRIM$(D10EDIT$)) = "" THEN
	  GOSUB 340
	  EXIT SUB
       ELSE
	  GOSUB 83190
	  IF FOUND = 4 THEN EXIT SUB
       END IF
      END IF
      IF IRTRN% = 1000 OR IRTRN% = -1 THEN EXIT SUB
      IF IRTRN% = 23 THEN
	 IF ERR <> 0 THEN GOTO 7140
	 CALL GETDIR(ext$, SELECT$, D10PATH$)
	 'IF IRTRN% = 1000 THEN EXIT SUB
	 IF SELECT$ <> "" THEN D10EDIT$ = SELECT$: SELECT$ = ""
	 LOCATE 7, 70: PRINT "        ": LOCATE 7, 70: PRINT D10EDIT$
	 GOSUB 78165
      END IF
      GOTO 7140
7145 CALL ACELL(7, 70, 8, D10EDIT$, 1)
     LOCATE 9, 20: FOR III = 1 TO 55: PRINT " "; : NEXT III
     f2help$ = "#FILE"
     GOSUB 73290
     D10EDIT$ = LTRIM$(RTRIM$(D10EDIT$))
     IF IRTRN% = 1 OR IRTRN% = 2 THEN
       IF LTRIM$(RTRIM$(D10EDIT$)) = "" THEN
	  GOSUB 340
	  EXIT SUB
       ELSE
	  GOSUB 83190
	  IF FOUND = 4 THEN EXIT SUB
       END IF
     END IF
     IF IRTRN% = 6 OR IRTRN% = 5 OR IRTRN% = 7 THEN
	IF D10EDIT$ = "" THEN
	   GOTO 7140
	ELSE
	   GOSUB 83190
	END IF
	GOTO 7140
     END IF
     IF IRTRN% = 1000 OR IRTRN% = -1 THEN EXIT SUB
     IF IRTRN% = 23 THEN
	IF ERR <> 0 THEN GOTO 7145
	CALL GETDIR(ext$, SELECT$, D10PATH$)
	'IF IRTRN% = 1000 THEN EXIT SUB
	IF SELECT$ <> "" THEN D10EDIT$ = SELECT$: SELECT$ = ""
	LOCATE 7, 70: PRINT "        ": LOCATE 7, 70: PRINT D10EDIT$
	GOSUB 78165
      END IF
     GOTO 7145
EXIT SUB
'-----------------------------------
'SUBROUTINE TO PRINT DIRECTORY LINE
'-----------------------------------
78165  COLOR 11, 6
       LOCATE 24, 1: PRINT "                   F4 = List Files on Specified Directory                       ";
       'LOCATE 24, 15: PRINT DIRECTORY$;
       COLOR 15, 1
     RETURN
'------------------------------
'check if directory is valid
'------------------------------
83190 IF D10PATH$ = CURDIR$ AND D10EDIT$ = "" THEN RETURN
      IF IRTRN% = 1 AND D10EDIT$ = "" THEN RETURN
      A1 = INSTR(D10PATH$, ":")
       IF A1 = 0 THEN
	  fdrive$ = LEFT$(CURDIR$, 1)
	  FPATH$ = D10PATH$
       ELSE
	  fdrive$ = LEFT$(D10PATH$, A1 - 1)
	  FPATH$ = RIGHT$(D10PATH$, LEN(D10PATH$) - A1)
       END IF
       IF FPATH$ = "\" THEN FPATH$ = ""
       filen$ = D10EDIT$
       FOUND = -1
       CALL filechk(fdrive$, FPATH$, filen$ + "." + ext$, FOUND)
       IF FOUND = 0 THEN
	 LOCATE 9, 35: PRINT "Invalid Directory"; : D10PATH$ = CURDIR$
	 LOCATE 7, 28: PRINT LEFT$(BLANK$, 40)
	 LOCATE 7, 28: PRINT D10PATH$
       ELSEIF FOUND = 1 THEN
	 LOCATE 9, 35: PRINT "Error in Path"; : D10PATH$ = CURDIR$
	 LOCATE 7, 28: PRINT LEFT$(BLANK$, 40)
	 LOCATE 7, 28: PRINT D10PATH$
       ELSEIF FOUND = 2 THEN
	 IF LTRIM$(RTRIM$(D10EDIT$)) <> "" THEN LOCATE 9, 35: PRINT "Bad File Name";
	 D10EDIT$ = "        ": LOCATE 7, 70: PRINT D10EDIT$
       ELSEIF FOUND = 3 THEN
	 IF LTRIM$(RTRIM$(D10EDIT$)) <> "" THEN LOCATE 9, 35: PRINT "File Not Found";
	 D10EDIT$ = "        ": LOCATE 7, 70: PRINT D10EDIT$
       ELSEIF FOUND = 5 THEN
	 LOCATE 9, 35: PRINT "Invalid Drive"; : D10PATH$ = CURDIR$
	 LOCATE 7, 28: PRINT LEFT$(BLANK$, 40)
	 LOCATE 7, 28: PRINT D10PATH$
       END IF
RETURN
'--------------------------
'SUBROUTINE TO SELECT UNITS
'--------------------------
340  LOCATE 16, 6: FOR i = 1 TO 73: PRINT " "; : NEXT i
     SROWBOX = 13: SCOLBOX = 34: BOXWIDTH = 13: EROWBOX = 16
     CALL BOX
     LOCATE 13, 38: PRINT "UNITS"
     LOCATE 14, 36: PRINT "CUSTOMARY"
     LOCATE 15, 36: PRINT "METRIC"
     UNIT1$ = "CUSTOMARY": UNIT2$ = "METRIC"
     RO = 14
     UNTS$ = UNIT1$
     XLEN = LEN(UNTS$)
160  f2help$ = "#UNITS"
     CALL RCELL(RO, 36, 1, UNTS$)
     GOSUB 73290
     IF RO = 14 THEN       'UNITS$ = 1
	UN1$ = "   (IN)    ": UN2$ = "  (IN/SEC)   ": UN3$ = "   (FT)   "
	UN4$ = "   (IN/YR)  ": UN5$ = "    (#/ACRE)    ": UNAREA$ = "ACRES    "
	UNEV$ = "IN": UNSL$ = "FT": UNMOIST$ = "IN"
     ELSE
	UN1$ = "   (CM)    ": UN2$ = "  (CM/SEC)   ": UN3$ = "   (M)    "
	UN4$ = "   (MM/YR)  ": UN5$ = "  (#/HECTARE)   ": UNAREA$ = "HECTARES"
	UNEV$ = "CM": UNSL$ = "M": UNMOIST$ = "MM"
     END IF
     IF IRTRN% = 5 THEN
	IF RO = 14 THEN
	   UNITS$ = "1"
	ELSE
	   UNITS$ = "2"
	END IF
	EXIT SUB
     END IF
     IF IRTRN% = 3 OR IRTRN% = 4 THEN
	 IF RO = 14 THEN
	    RO = 15
	    UNTS$ = UNIT2$
	 ELSE
	    RO = 14
	    UNTS$ = UNIT1$
	 END IF
      END IF
      IF IRTRN% = 1 OR IRTRN% = 2 THEN RETURN
      IF IRTRN% = 1000 THEN EXIT SUB
      IF IRTRN% = 100 THEN GOTO 7101
      GOTO 160
RETURN
'-----------------------------------------
'SUBROUTINE TO CALL help MESSAGES
'-----------------------------------------
73290  IF IRTRN% = 22 THEN CALL USERHELP
       IF IRTRN% = 21 THEN CALL HLPMSG(f2help$)
       IF IRTRN% = 20 THEN CALL HLPMSG(F1HELP$)
       RETURN
END SUB

SUB LASTPAGE
DIM SOILNAME$(20), DIFFSOIL%(20), SVHEAD$(10), LYRLIST(20), CHKOK%(5)
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'THIS ROUTINE CHECKS THE SOIL AND DESIGN DATA FOR
'POSSIBLE VIOLATION OF SOIL OR FML PROPERTIES
'AND FOR POSSIBLE VIOLATION OF LAYER ARRANGEMENT.
'THE PROGRAM DISPLAYS ERROR MESSAGES OEN LAYER AT A TIME,
'THEN DISPLAYS ERROR MESSAGES RELATED TO LAYER ARRANGEMENT.
'
'     NERR = NUMBER OF ERRORS IN A GIVEN LAYER
' DESERR() = DESIGN ERROR NUMBER
'ERRMSG$() = ERROR MESSAGE TO BE DISPLAYED
'     NLYR = NUMBER OF NON-ZERO LAYERS
'LYRLIST ()= ARRAY CONTAINING THE LINE NUMBER OF LAYERS
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
FOR i = 1 TO 5: CHKOK%(i) = 2: NEXT i
95100 CLS
   F1HELP$ = "#D10VERIFY"
   COLOR 11, 6
   LOCATE 1, 1:  PRINT "                 SOIL AND DESIGN DATA - VERIFICATION AND SAVING                 ";
   COLOR 15, 1
   COLOR 4, 3
   LOCATE 25, 1: PRINT " F1=Info   F2=Help   F3=Keys   F9=Quit   F10=End   PgUp=Return to Input Screens ";
   COLOR 15, 1
   COLOR 11, 6
   LOCATE 24, 1: PRINT "                    TO SELECT: Cursor    Then Enter or PgDn                     ";
   LOCATE 24, 39: PRINT CHR$(24); CHR$(25);
   COLOR 15, 1
   SROWBOX = 7: SCOLBOX = 10: BOXWIDTH = 61: EROWBOX = 13
   CALL BOX
   SVHEAD$(1) = "Verify Landfill General Design Data"
   SVHEAD$(2) = "Verify Soil Layer/Geomembrane Properties"
   SVHEAD$(3) = "Verify Layer Arrangement"
   SVHEAD$(4) = "Review/Save User-Defined Soil Textures"
   SVHEAD$(5) = "Save Soil and Design Data"
   FOR i = 1 TO 5
      LOCATE i + 7, 19: PRINT SVHEAD$(i);
      IF CHKOK%(i) = 1 THEN LOCATE i + 7, 65: PRINT "BAD"
      IF CHKOK%(i) = 0 THEN LOCATE i + 7, 65: PRINT "OK"
   NEXT i
   IF OPTSEL = 1 THEN GOTO 9510
   IF OPTSEL = 2 THEN GOTO 9512
   IF OPTSEL = 3 THEN GOTO 9514
   IF OPTSEL = 4 THEN GOTO 9516
   IF OPTSEL = 5 THEN GOTO 9518
   GOTO 9518
9510 f2help$ = "#VLANDFILL"
     XLEN = LEN(SVHEAD$(1))
     CALL RCELL(8, 19, XLEN, SVHEAD$(1))
     GOSUB 93190
     IF IRTRN% = 2 GOTO 9618
     IF IRTRN% = 3 GOTO 9518
     IF IRTRN% = 4 THEN GOTO 9512
     IF IRTRN% = 5 OR IRTRN% = 1 THEN
       GOSUB 97500
       ERREXIST = 0: CHKOK%(1) = 0
       CALL CHKLNDFL(ERREXIST)
       OPTSEL = 1
       IF ERREXIST = 1 THEN CHKOK%(1) = 1
       IF IRTRN% = 1000 THEN GOSUB 92196
       GOTO 95100
     END IF
     IF IRTRN% = 1 OR IRTRN% = -1 THEN EXIT SUB
     IF IRTRN% = 1000 THEN GOSUB 92196
     GOTO 9510
9512 f2help$ = "#VSOILDATA"
     XLEN = LEN(SVHEAD$(2))
     CALL RCELL(9, 19, XLEN, SVHEAD$(2))
     GOSUB 93190
     IF IRTRN% = 2 GOTO 9618
     IF IRTRN% = 3 GOTO 9510
     IF IRTRN% = 4 THEN GOTO 9514
     IF IRTRN% = 5 OR IRTRN% = 1 THEN
       GOSUB 97500
       ERREXIST = 0: CHKOK%(2) = 0
       CALL DESCHK(NLYR, ERREXIST)
       OPTSEL = 2
       IF ERREXIST = 1 THEN CHKOK%(2) = 1
       IF IRTRN% = 1000 THEN GOSUB 92196
       GOTO 95100
     END IF
     IF IRTRN% = 1 OR IRTRN% = -1 THEN EXIT SUB
     IF IRTRN% = 1000 THEN GOSUB 92196
     GOTO 9512
9514 f2help$ = "#VLAYERS"
     XLEN = LEN(SVHEAD$(3))
     CALL RCELL(10, 19, XLEN, SVHEAD$(3))
     GOSUB 93190
     IF IRTRN% = 2 GOTO 9618
     IF IRTRN% = 3 GOTO 9512
     IF IRTRN% = 4 THEN GOTO 9516
     IF IRTRN% = 5 OR IRTRN% = 1 THEN
       GOSUB 97500
       ERREXIST = 0: CHKOK%(3) = 0
       CALL LYRCHK(NLYR, ERREXIST)
       OPTSEL = 3
       IF ERREXIST = 1 THEN CHKOK%(3) = 1
       IF IRTRN% = 1000 THEN GOSUB 92196
       GOTO 95100
     END IF
     IF IRTRN% = 1 OR IRTRN% = -1 THEN EXIT SUB
     IF IRTRN% = 1000 THEN GOSUB 92196
     GOTO 9514
9516 f2help$ = "#USERSOIL"
     XLEN = LEN(SVHEAD$(4))
     CALL RCELL(11, 19, XLEN, SVHEAD$(4))
     GOSUB 93190
     IF IRTRN% = 2 GOTO 9618
     IF IRTRN% = 3 GOTO 9514
     IF IRTRN% = 4 THEN GOTO 9518
     IF IRTRN% = 5 OR IRTRN% = 1 THEN
       GOSUB 97500
       SOILSVD = 0
       CALL CHKUSOIL(SOILSVD)
       OPTSEL = 4
       IF IRTRN% = 1000 THEN GOSUB 92196
       IF SOILSVD = 1 THEN CHKOK%(4) = 1
       IF SOILSVD = 0 THEN CHKOK%(4) = 2
       GOTO 95100
     END IF
     IF IRTRN% = 1 OR IRTRN% = -1 THEN EXIT SUB
     IF IRTRN% = 1000 THEN GOSUB 92196
     GOTO 9516
9518 f2help$ = "#D10VERIFY"
     XLEN = LEN(SVHEAD$(5))
     CALL RCELL(12, 19, XLEN, SVHEAD$(5))
     GOSUB 93190
9618 IF IRTRN% = 2 THEN IRTRN% = 5
     IF IRTRN% = 3 GOTO 9516
     IF IRTRN% = 4 THEN GOTO 9510
     IF IRTRN% = 1000 THEN GOSUB 92196
     IF IRTRN% = 5 OR IRTRN% = 1 THEN
	OPTSEL = 5
	GOSUB 97500
	ERREXIST = 0: CHKOK%(1) = 0
	CALL CHKLNDFL(ERREXIST)
	IF ERREXIST = 1 THEN CHKOK%(1) = 1
	IF IRTRN% = 100 THEN GOTO 95100
	IF IRTRN% = 1000 THEN GOSUB 92196
	GOSUB 97500
	ERREXIST = 0: CHKOK%(2) = 0
	CALL DESCHK(NLYR, ERREXIST)
	IF ERREXIST = 1 THEN CHKOK%(2) = 1
	IF IRTRN% = 100 THEN GOTO 95100
	IF IRTRN% = 1000 THEN GOSUB 92196
	GOSUB 97500
	ERREXIST = 0: CHKOK%(3) = 0
	CALL LYRCHK(NLYR, ERREXIST)
	IF ERREXIST = 1 THEN CHKOK%(3) = 1
	IF IRTRN% = 100 THEN GOTO 95100
	IF IRTRN% = 1000 THEN GOSUB 92196
	GOSUB 90095
	OPTSEL = 5
	GOTO 95100
     END IF
     IF IRTRN% = 1 OR IRTRN% = -1 THEN EXIT SUB
     GOTO 9518
EXIT SUB
'''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO SAVE SOIL AND DESIGN
'DATA INTO THE SPECIFIED D10SAVE
'FILE.
'''''''''''''''''''''''''''''''''''''
90095 CALL SVFILE(FILESV$)
      IF IRTRN% = 1000 THEN EXIT SUB
      IF IRTRN% = -1 THEN IRTRN% = 0: RETURN
      IF RIGHT$(SVPATH$, 1) <> "\" THEN SVPATH$ = SVPATH$ + "\"
      FILESV$ = LTRIM$(RTRIM$(SVPATH$)) + LTRIM$(RTRIM$(d10save$)) + ".D10"
      CLOSE
      OPEN FILESV$ FOR OUTPUT AS #1
'LINE 1
      PRINT #1, TITLE$
'LINE 2
      PRINT #1, USING "##"; VAL(UNITS$);
      IF UCASE$(moist$) = "Y" THEN PRINT #1, USING "\\"; " 1";
      IF UCASE$(moist$) = "N" THEN PRINT #1, USING "\\"; " 0"; : SNOW$ = ""
      rjust$ = SPACE$(10): RSET rjust$ = LTRIM$(RTRIM$(SNOW$)): SNOW$ = rjust$
      PRINT #1, USING "\        \"; SNOW$;
      SNOW$ = LTRIM$(RTRIM$(SNOW$))
      rjust$ = SPACE$(10): RSET rjust$ = LTRIM$(RTRIM$(AREA$)): AREA$ = rjust$
      PRINT #1, USING "\        \"; AREA$;
      AREA$ = LTRIM$(RTRIM$(AREA$))
      rjust$ = SPACE$(6):  RSET rjust$ = LTRIM$(RTRIM$(prcnt$)): prcnt$ = rjust$
      PRINT #1, USING "\    \"; prcnt$;
      prcnt$ = LTRIM$(RTRIM$(prcnt$))
      IF VAL(SELCN$) = 0 THEN SELCN$ = "1": CN$ = " "
      PRINT #1, USING "##"; VAL(SELCN$)
      rjust$ = SPACE$(7): RSET rjust$ = LTRIM$(RTRIM$(CN$)): CN$ = rjust$
      IF VAL(SELCN$) = 1 THEN
	 PRINT #1, USING "\     \"; CN$
	 CN$ = LTRIM$(RTRIM$(CN$))
      END IF
      IF VAL(SELCN$) = 2 THEN
	 PRINT #1, USING "\     \"; CN$;
	 CN$ = LTRIM$(RTRIM$(CN$))
	 rjust$ = SPACE$(7): RSET rjust$ = LTRIM$(RTRIM$(slope$)): slope$ = rjust$
	 PRINT #1, USING "\     \"; slope$;
	 slope$ = LTRIM$(RTRIM$(slope$))
	 rjust$ = SPACE$(7): RSET rjust$ = LTRIM$(RTRIM$(slength$)): slength$ = rjust$
	 PRINT #1, USING "\     \"; slength$;
	 slength$ = LTRIM$(RTRIM$(slength$))
	 PRINT #1, USING "####.#"; CNII
	'TMPCN$ = STR$(CNII)
	'DECPLACE = INSTR(TMPCN$, ".") + 1
	'TMPCN$ = LEFT$(TMPCN$, DECPLACE)
	'rjust$ = SPACE$(7): RSET rjust$ = LTRIM$(RTRIM$(TMPCN$)): TMPCN$ = rjust$
	'PRINT #1, USING "\     \"; TMPCN$
      END IF
      IF VAL(SELCN$) = 3 THEN
	 rjust$ = SPACE$(7): RSET rjust$ = LTRIM$(RTRIM$(slope$)): slope$ = rjust$
	 PRINT #1, USING "\     \"; slope$;
	 slope$ = LTRIM$(RTRIM$(slope$))
	 rjust$ = SPACE$(7): RSET rjust$ = LTRIM$(RTRIM$(slength$)): slength$ = rjust$
	 PRINT #1, USING "\     \"; slength$;
	 slength$ = LTRIM$(RTRIM$(slength$))
	 PRINT #1, USING "###"; VAL(soiltex$);
	 PRINT #1, USING "###"; VAL(VEGET$);
	 PRINT #1, USING "####.#"; CNII
	'TMPCN$ = STR$(CNII)
	'DECPLACE = INSTR(TMPCN$, ".") + 1
	'TMPCN$ = LEFT$(TMPCN$, DECPLACE)
	'rjust$ = SPACE$(7): RSET rjust$ = LTRIM$(RTRIM$(TMPCN$)): TMPCN$ = rjust$
	'PRINT #1, USING "\     \"; TMPCN$
       END IF
'PRINT LAYER DATA
'LINES OF LAYER SHEET THAT DO NOT HAVE VALUES ARE NOT SAVED
    FOR i = 1 TO 20
	 IF VAL(PROP$(i, 1, 1)) <> 0 THEN GOTO 93222
       GOTO 94222
'LAYER LINES
93222
     PRINT #1, USING "##"; VAL(PROP$(i, 1, 1));
     rjust$ = SPACE$(7): RSET rjust$ = LTRIM$(RTRIM$(PROP$(i, 2, 1))): PROP$(i, 2, 1) = rjust$
     PRINT #1, USING "\     \"; PROP$(i, 2, 1);
     PROP$(i, 2, 1) = LTRIM$(RTRIM$(PROP$(i, 2, 1)))
     PRINT #1, USING "####"; VAL(PROP$(i, 3, 1));
     FOR k = 4 TO 7
       rjust$ = SPACE$(6): RSET rjust$ = LTRIM$(RTRIM$(PROP$(i, k, 1))): PROP$(i, k, 1) = rjust$
       PRINT #1, USING "\    \"; PROP$(i, k, 1);
       PROP$(i, k, 1) = LTRIM$(RTRIM$(PROP$(i, k, 1)))
     NEXT k
     rjust$ = SPACE$(16): RSET rjust$ = LTRIM$(RTRIM$(PROP$(i, 2, 2))): PROP$(i, 2, 2) = rjust$
     PRINT #1, USING "\              \"; PROP$(i, 2, 2)
     PROP$(i, 2, 2) = LTRIM$(RTRIM$(PROP$(i, 2, 2)))
     k = 3
     rjust$ = SPACE$(7): RSET rjust$ = LTRIM$(RTRIM$(PROP$(i, k, 2))): PROP$(i, k, 2) = rjust$
     PRINT #1, USING "\     \"; PROP$(i, k, 2);
     PROP$(i, k, 2) = LTRIM$(RTRIM$(PROP$(i, k, 2)))
     FOR k = 4 TO 5
       rjust$ = SPACE$(6): RSET rjust$ = LTRIM$(RTRIM$(PROP$(i, k, 2))): PROP$(i, k, 2) = rjust$
       PRINT #1, USING "\    \"; PROP$(i, k, 2);
       PROP$(i, k, 2) = LTRIM$(RTRIM$(PROP$(i, k, 2)))
     NEXT k
     PRINT #1, USING "###"; VAL(PROP$(i, 6, 2));
     k = 7
     rjust$ = SPACE$(13): RSET rjust$ = LTRIM$(RTRIM$(PROP$(i, k, 2))): PROP$(i, k, 2) = rjust$
     PRINT #1, USING "\           \"; PROP$(i, k, 2);
     PROP$(i, k, 2) = LTRIM$(RTRIM$(PROP$(i, k, 2)))
     FOR k = 2 TO 3
       rjust$ = SPACE$(7): RSET rjust$ = LTRIM$(RTRIM$(PROP$(i, k, 3))): PROP$(i, k, 3) = rjust$
       PRINT #1, USING "\     \"; PROP$(i, k, 3);
       PROP$(i, k, 3) = LTRIM$(RTRIM$(PROP$(i, k, 3)))
     NEXT k
     PRINT #1, USING "##"; VAL(PROP$(i, 4, 3));
     k = 5
     rjust$ = SPACE$(14): RSET rjust$ = LTRIM$(RTRIM$(PROP$(i, k, 3))): PROP$(i, k, 3) = rjust$
     PRINT #1, USING "\            \"; PROP$(i, k, 3)
     PROP$(i, k, 3) = LTRIM$(RTRIM$(PROP$(i, k, 3)))
94222 NEXT i
      CLOSE #1
      IRTRN% = 1000
      EXIT SUB
RETURN
''''''''''''''''''''''''''''''''''''''''''''''''
'ROUTINE TO TEST IF SOIL AND DESIGN DATA EXISTS
''''''''''''''''''''''''''''''''''''''''''''''''
97500  NLYR = 0
       FOR i = 1 TO 20
	  FOR k = 1 TO 3
	    FOR J = 1 TO 7
	      IF VAL(PROP$(i, J, k)) <> 0 THEN
		 NLYR = NLYR + 1
		 LYRLIST(NLYR) = i
		 GOTO 97600
	      END IF
	  NEXT J
	  NEXT k
97600  NEXT i
       IF NLYR = 0 THEN
	  LOCATE 20, 20: BEEP: PRINT "NO SOIL AND DESIGN DATA WAS ENTERED"
	  LOCATE 21, 20: PRINT "PRESS ANY KEY TO CONTINUE..."
96001     IF INKEY$ = "" THEN GOTO 96001
	  IRTRN% = 1
	  EXIT SUB
       ELSE
	  '''''''''''''''''''''''''''''''''''''
	  'COMPRESS LAYERS BY ELIMINATING BLANK
	  'LINES IN THE THE LAYER SHEET
	  ''''''''''''''''''''''''''''''''''''''
	  FOR i = 1 TO 20
	    IF LYRLIST(i) = 0 THEN
		GOTO 96005
	    ELSE
		II = LYRLIST(i)
	    END IF
	    FOR k = 1 TO 3
	      FOR J = 1 TO 7
		PROP$(i, J, k) = PROP$(II, J, k)
	      NEXT J
	    NEXT k
	  NEXT i
       END IF
96005  FOR i = NLYR + 1 TO 20
	  FOR k = 1 TO 3
	    FOR J = 1 TO 7
	      PROP$(i, J, k) = ""
	    NEXT J
	  NEXT k
       NEXT i
RETURN
''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO CALL MANUAL MESSAGES FOR HELP
'''''''''''''''''''''''''''''''''''''''''''''''''
93190  IF IRTRN% = 22 THEN CALL USERHELP
       IF IRTRN% = 21 THEN CALL HLPMSG(f2help$)
       IF IRTRN% = 20 THEN CALL HLPMSG(F1HELP$)
      RETURN
'''''''''''''''''''''''''''''''''''
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
'''''''''''''''''''''''''''''''''''
92196 CALL EXOUT(ANSCODE)
     IF ANSCODE = 0 THEN
	GOTO 95100
     ELSE
	EXIT SUB
     END IF
RETURN
''''''''''''''''''''''''''''''''''''''
'DETERMINE WHETHER THERE IS A PERIOD
'IN THE VALUE OF EACH REAL VARIABLE
'IF NOT, PUT ONE.
''''''''''''''''''''''''''''''''''''''
93196 IF INSTR(TXT1$, PERIOD$) THEN
	'THERE IS A PERIOD IN number
      ELSE
	TXT1$ = LTRIM$(RTRIM$(TXT1$)) + "."
      END IF
RETURN
END SUB

SUB SOIL (LL%, SOILNUM$)
 DIM S$(50)
 TIT1$ = "          SOIL TEXTURE                          FIELD      WILTING    SAT. HYD. "
 TIT2$ = " ===============================    POROSITY   CAPACITY     POINT   CONDUCTIVITY"
 TIT3$ = " HELP     USDA     USCS             (VOL/VOL)  (VOL/VOL)  (VOL/VOL)   (CM/SEC)  "
 S$(1) = "   1       CoS      SP                "
 S$(2) = "   2        S       SW                "
 S$(3) = "   3       FS       SW                "
 S$(4) = "   4       LS       SM                "
 S$(5) = "   5       LFS      SM                "
 S$(6) = "   6       SL       SM                "
 S$(7) = "   7       FSL      SM                "
 S$(8) = "   8        L       ML                "
 S$(9) = "   9       SiL      ML                "
 S$(10) = "  10       SCL      SC                "
 S$(11) = "  11       CL       CL                "
 S$(12) = "  12      SiCL      CL                "
 S$(13) = "  13       SC       CH                "
 S$(14) = "  14       SiC      CH                "
 S$(15) = "  15        C       CH                "
 S$(16) = "  16  Barrier Soil                    "
 S$(17) = "  17  Bentonite Mat (0.6 cm)          "
 S$(18) = "  18  Municipal Waste (900 pcy)       "
 S$(19) = "  19  Municipal Waste w/ Channeling   "
 S$(20) = "  20  Drainage Net (0.5 cm)           "
 S$(21) = "  21  Gravel                          "
 S$(22) = "  22        L     ML (Compacted)      "
 S$(23) = "  23       SiL    ML (Compacted)      "
 S$(24) = "  24       SCL    SC (Compacted)      "
 S$(25) = "  25       CL     CL (Compacted)      "
 S$(26) = "  26      SiCL    CL (Compacted)      "
 S$(27) = "  27       SC     CH (Compacted)      "
 S$(28) = "  28       SiC    CH (Compacted)      "
 S$(29) = "  29        C     CH (Compacted)      "
 S$(30) = "  30  Comp. Coal-Burning Fly Ash      "
 S$(31) = "  31  Comp. Coal-Burning Bottom Ash   "
 S$(32) = "  32  Comp. Mun. Incinerator Fly Ash  "
 S$(33) = "  33  Comp. Fine Copper Slag          "
 S$(34) = "  34  Drainage Net (0.6 cm)           "
 S$(35) = "  35  High Density Polyethylene (HDPE)"
 S$(36) = "  36  Low Density Polyethylene (LDPE) "
 S$(37) = "  37  Polyvinyl Chloride (PVC)        "
 S$(38) = "  38  Butyl Rubber                    "
 S$(39) = "  39  Chlorinated Polythelene (CPE)   "
 S$(40) = "  40  Hypalon or Chlorosulfonated Polyethelene (CSPE) "
 S$(41) = "  41  Ethylene-Propylene Diene Monomer (EPDM)"
 S$(42) = "  42  Neoprene                               "
 FOR i = 1 TO 42
   IF i = 35 OR i = 36 OR i = 37 OR i = 38 OR i = 39 THEN
      S$(i) = S$(i) + hsoil$(i, 1) + SPACE$(6) + hsoil$(i, 2) + SPACE$(6) + hsoil$(i, 3) + SPACE$(9) + hsoil$(i, 4) + "  "
   ELSEIF i = 40 THEN
      S$(i) = S$(i) + hsoil$(i, 1) + hsoil$(i, 2) + hsoil$(i, 3) + SPACE$(5) + hsoil$(i, 4) + "  "
  'ELSEIF i = 40 THEN
  '    S$(i) = S$(i) + hsoil$(i, 1) + hsoil$(i, 2) + hsoil$(i, 3) + SPACE$(21) + hsoil$(i, 4) + "  "
   ELSEIF i > 40 THEN
       S$(i) = S$(i) + hsoil$(i, 1) + hsoil$(i, 2) + hsoil$(i, 3) + SPACE$(14) + hsoil$(i, 4) + "  "
  ' ELSEIF i >= 30 AND i <= 32 THEN
  '    S$(i) = S$(i) + "0" + hsoil$(i, 1) + SPACE$(6) + "0" + hsoil$(i, 2) + SPACE$(6) + "0" + hsoil$(i, 3) + SPACE$(7) + hsoil$(i, 4) + " "
   ELSE
      S$(i) = S$(i) + "0" + hsoil$(i, 1) + SPACE$(6) + "0" + hsoil$(i, 2) + SPACE$(6) + "0" + hsoil$(i, 3) + SPACE$(6) + hsoil$(i, 4) + "  "
   END IF
 NEXT i
 COLOR 15, 1
     PCOPY VPAGE%, HPAGE%
     IRTRN% = 0
     PCOPY VPAGE%, APAGE%
     CLS APAGE%
     SCREEN 0, , VPAGE%, VPAGE%
     COLOR 11, 6
     LOCATE 25, 1: PRINT "   TO SELECT: Cursor    then ENTER        PgDn/PgUp = More       Esc = RETURN   ";
     LOCATE 25, 22: PRINT CHR$(24); CHR$(25);
     COLOR 15, 1: LOCATE 1, 1
     PAGEMAX = 2
     INI% = 1: IFIN% = 21
     i = 0: N = 0: PAGE = 1
10202  LOCATE 1, 1: PRINT TIT1$: PRINT TIT2$: PRINT TIT3$
       FOR i = 4 TO 24
       LOCATE i, 1: PRINT "                                                                                ";
       NEXT i
       II = 4
       FOR i% = INI% TO IFIN%
	 LOCATE II, 1: PRINT S$(i%);
	 II = II + 1
       NEXT i%
      RO = 4: L% = INI%
10222 XLEN = LEN(S$(L%))
      CALL RCELL(RO, 1, XLEN, S$(L%))
'
'page down
'
      IF IRTRN% = 1 THEN
	PAGE = PAGE + 1
	IF PAGE > PAGEMAX THEN PAGE = 1
	IF PAGE = 1 THEN INI% = 1: IFIN% = 21: GOTO 10202
	IF PAGE = 2 THEN INI% = 22: IFIN% = 42: GOTO 10202
	'IF PAGE = 3 THEN INI% = 41: IFIN% = 44: GOTO 10202
      END IF
'
'page up
'
	IF IRTRN% = -1 THEN
	  PAGE = PAGE - 1
	  IF PAGE < 1 THEN PAGE = 2
	  IF PAGE = 1 THEN INI% = 1: IFIN% = 21: GOTO 10202
	  IF PAGE = 2 THEN INI% = 22: IFIN% = 42: GOTO 10202
	 'IF PAGE = 3 THEN INI% = 41: IFIN% = 44: GOTO 10202
	END IF
'
'exit
'
	IF IRTRN% = 100 OR IRTRN% = 22 THEN GOTO 10098
'
'cursor up
'
	IF IRTRN% = 3 THEN
	  IF RO = 4 THEN
	     RO = IFIN% - INI% + 4: L% = IFIN%
	  ELSE
	    RO = RO - 1: L% = L% - 1
	  END IF
	    GOTO 10222
	END IF
'
'cursor down
'
	IF IRTRN% = 4 THEN
	  IF L% < IFIN% THEN
	       L% = L% + 1
	       RO = RO + 1
	  ELSE
	      RO = 4: L% = INI%
	  END IF
	  GOTO 10222
	END IF
'
'make selection
'
       IF IRTRN% = 5 THEN : GOTO 10097
       GOTO 10222
10097 SOILNUM$ = STR$(L%)
10098 CLS : PCOPY HPAGE%, VPAGE%
EXIT SUB
END SUB

SUB USERSOIL (LL%, SOILNUM$)
'''''''''''''''''''''''''''''''''''''''''''''
'PROCEEDURE TO DISPLAY USER SOIL TEXTURES
'AND TO ALLOW SELECTION OF A TEXTURE FOR
'USE IN THE LANDFILL PROFILE
'SOILN% = NUMBER OF SOIL TEXTURES IN THE FILE
'''''''''''''''''''''''''''''''''''''''''''''
       COLOR 15, 1
       PCOPY VPAGE%, HPAGE%
       IRTRN% = 0
       PCOPY VPAGE%, APAGE%
       CLS APAGE%
       SCREEN 0, , VPAGE%, VPAGE%
       U$ = DIR$("SOIL.USR")
       IF LEN(U$) <> 0 THEN
	  CLOSE
	  OPEN "SOIL.USR" FOR INPUT AS #1
	  soiln% = 0
	  FOR i = 1 TO 100
	    IF EOF(1) THEN GOTO 80091
	    LINE INPUT #1, US$(i)
	    soiln% = soiln% + 1
	  NEXT i
80091  CLOSE 1
       IF soiln% > 100 THEN soiln% = 100
       END IF
    IF soiln% = 0 THEN
	LOCATE 10, 20: PRINT "USER SOIL TEXTURE FILE DOES NOT EXIST."
	LOCATE 11, 20: PRINT "USER SOIL TEXTURES WITH A NUMBER 42 OR "
	LOCATE 12, 20: PRINT "GREATER WILL BE SAVED TO A FILE."
	LOCATE 13, 20: PRINT "SOIL TEXTURES THAT HAVE A NUMBER OF"
	LOCATE 14, 20: PRINT "ZERO WILL NOT BE SAVED."
	LOCATE 16, 20: PRINT "PRESS ANY KEY TO CONTINUE"
11011   KBD$ = INKEY$: IF KBD$ = "" THEN GOTO 11011
	GOTO 11098
    END IF
 TIT1$ = "      SOIL TEXTURE                           FIELD      WILTING      SAT. HYD. "
 TIT2$ = " =======================         POROSITY   CAPACITY     POINT     CONDUCTIVITY"
 TIT3$ = " USER  ------NAME-------         (VOL/VOL)  (VOL/VOL)  (VOL/VOL)     (CM/SEC) "
       COLOR 11, 6
       LOCATE 25, 1: PRINT "   TO SELECT: Cursor    then Enter        PgDn/PgUp = More       Esc = RETURN   ";
       LOCATE 25, 22: PRINT CHR$(24); CHR$(25);
       COLOR 15, 1: LOCATE 1, 1
       IFIN% = 20
       IF soiln% <= 20 THEN PAGEMAX = 1: IFIN% = soiln%
       IF soiln% > 20 AND soiln% <= 40 THEN PAGEMAX = 2
       IF soiln% > 40 AND soiln% <= 60 THEN PAGEMAX = 3
       IF soiln% > 60 AND soiln% <= 80 THEN PAGEMAX = 4
       IF soiln% > 80 THEN PAGEMAX = 5
       INI% = 1
       i = 0: N = 0: PAGE = 1
11202  LOCATE 1, 1: PRINT TIT1$: PRINT TIT2$: PRINT TIT3$
       FOR i = 4 TO 23
	LOCATE i, 1: PRINT "                                                                             "
       NEXT i
	II = 4
	FOR i% = INI% TO IFIN%
	  LOCATE II, 1: PRINT US$(i%)
	  II = II + 1
       NEXT i%
      RO = 4: L% = INI%
11222 XLEN = LEN(US$(L%))
      CALL RCELL(RO, 1, 79, US$(L%))
'
'page down
'
      IF IRTRN% = 1 THEN
	PAGE = PAGE + 1
	IF PAGE > PAGEMAX THEN PAGE = 1
	IF PAGE = 1 AND soiln% <= 20 THEN INI% = 1: IFIN% = soiln%: GOTO 11202
	IF PAGE = 1 AND soiln% > 20 THEN INI% = 1: IFIN% = 20: GOTO 11202
	IF PAGEMAX >= 2 THEN
	  IF PAGE = 2 AND soiln% <= 40 THEN INI% = 21: IFIN% = soiln%: GOTO 11202
	  IF PAGE = 2 AND soiln% > 40 THEN INI% = 21: IFIN% = 40: GOTO 11202
	END IF
	IF PAGEMAX >= 3 THEN
	  IF PAGE = 3 AND soiln% <= 60 THEN INI% = 41: IFIN% = soiln%: GOTO 11202
	  IF PAGE = 3 AND soiln% > 60 THEN INI% = 41: IFIN% = 60: GOTO 11202
	END IF
	IF PAGEMAX >= 4 THEN
	  IF PAGE = 4 AND soiln% <= 80 THEN INI% = 61: IFIN% = soiln%: GOTO 11202
	  IF PAGE = 4 AND soiln% > 80 THEN INI% = 61: IFIN% = 80: GOTO 11202
	END IF
	IF PAGE = 5 AND soiln% <= 100 THEN INI% = 81: IFIN% = soiln%: GOTO 11202
	IF PAGE = 5 AND soiln% = 100 THEN INI% = 81: IFIN% = 100: GOTO 11202
	END IF
'
'page up
'
	IF IRTRN% = -1 THEN
	PAGE = PAGE - 1
	IF PAGE < 1 THEN PAGE = PAGEMAX
	IF PAGE = 1 AND soiln% <= 20 THEN INI% = 1: IFIN% = soiln%: GOTO 11202
	IF PAGE = 1 AND soiln% > 20 THEN INI% = 1: IFIN% = 20: GOTO 11202
	IF PAGEMAX >= 2 THEN
	  IF PAGE = 2 AND soiln% <= 40 THEN INI% = 21: IFIN% = soiln%: GOTO 11202
	  IF PAGE = 2 AND soiln% > 40 THEN INI% = 21: IFIN% = 40: GOTO 11202
	END IF
	IF PAGEMAX >= 3 THEN
	  IF PAGE = 3 AND soiln% <= 60 THEN INI% = 41: IFIN% = soiln%: GOTO 11202
	  IF PAGE = 3 AND soiln% > 60 THEN INI% = 41: IFIN% = 60: GOTO 11202
	END IF
	IF PAGEMAX >= 4 THEN
	  IF PAGE = 4 AND soiln% <= 80 THEN INI% = 61: IFIN% = soiln%: GOTO 11202
	  IF PAGE = 4 AND soiln% > 80 THEN INI% = 61: IFIN% = 80: GOTO 11202
	END IF
	IF PAGE = 5 AND soiln% <= 100 THEN INI% = 81: IFIN% = soiln%: GOTO 11202
	IF PAGE = 5 AND soiln% = 100 THEN INI% = 81: IFIN% = 100: GOTO 11202
	END IF
      IF IRTRN% = 100 OR IRTRN% = 23 THEN GOTO 11098
      IF IRTRN% = 3 THEN
	 IF RO = 4 THEN
	    RO = IFIN% - INI% + 4: L% = IFIN%
	 ELSE
	    RO = RO - 1: L% = L% - 1
	 END IF
	    GOTO 11222
       END IF
       IF IRTRN% = 4 THEN
	  IF L% < IFIN% THEN
	       L% = L% + 1
	       RO = RO + 1
	    ELSE
	      RO = 4: L% = INI%
	    END IF
	END IF
       IF IRTRN% = 5 THEN : GOTO 11097
       GOTO 11222
11097   L% = VAL(LEFT$(US$(L%), 5))
	SOILNUM$ = STR$(L%)
11098 CLS : PCOPY HPAGE%, VPAGE%
    EXIT SUB
END SUB

