DECLARE SUB filechk (FDRIVE$, FPATH$, FILEN$, FOUND!)
DECLARE SUB BOX ()
DECLARE SUB GETDIR (EXT$, SELECT$, directory$)
DECLARE SUB SOIL (LL%, SOILNUM$)
DECLARE SUB USERSOIL (LL%, SOILNUM$)
DECLARE SUB ACELL (IROW!, ISTART!, NS!, TEXT$, ITYPE!)
DECLARE SUB EXOUT (ANSCODE!)
DECLARE SUB HLPMSG (f2help$)
DECLARE SUB menu ()
DECLARE SUB RCELL (IROW!, ISTART!, NS!, TXT$)
DECLARE SUB USERHELP ()

   COMMON SHARED D10EDIT$, D11EDIT$, D11SAVE$, D10SAVE$, D10PATH$
   COMMON SHARED HSOIL$(), prop$(), US$(), CW(), UNMOIST$
   COMMON SHARED UNITS$, UNIT$, UN1$, UN2$, UN3$, UN4$, UN5$, UNAREA$, UNSL$
   COMMON SHARED TOHELP%, NLAYER, NL%, TITLE$, IRTRN%
   COMMON SHARED moist$, snow$, page%, SOILN%
   COMMON SHARED APAGE%, VPAGE%, HPAGE%, DIRECT$
   COMMON SHARED BLANKS$, AREA$
   COMMON SHARED SELCN$, CN$, SLOPE$, SLENGTH$, SOILTEX$, VEGET$, PRCNT$
   COMMON SHARED CNII, UNI$, SVPATH$
   COMMON SHARED SROWBOX, SCOLBOX, BOXWIDTH, EROWBOX, LATIT$

SUB CHKLNDFL (ERREXIST)
DIM ERRMSG$(14), DESERR(14)
''''''''''''''''''''''''''''''''''''''''''''''''''''
' SUBROUTINE TO CHECK GENRAL LANDFILL INFORMATION
' INCLUDING AREA, PERCENT AREA WHERE RUNOFF IS POSSIBLE
' AND CURVE NUMBER INFORMATION
'
'ERRNUM  = ERROR NUMBER... LAST VALUE INDICATES
'             TOTAL NUMBER OF ERRORS
'  DESERR() = ERRO NUMBER
'  ERRMSG() = ERROR MESSAGE TO BE PRINTED
''''''''''''''''''''''''''''''''''''''''''''''''''''
'
'DEFINE ERROR MESSAGES
''''''
ERRMSG$(1) = "LANDFILL AREA MUST BE ENTERED AS NON ZERO."
ERRMSG$(2) = "AMOUNT OF WATER OR SNOW ON LANDFILL SURFACE IS NOT SPECIFIED."
ERRMSG$(3) = "***WARNING***  EITHER ENTER VALUE OR ALLOW 'HELP' TO INITIALIZE"
ERRMSG$(4) = "               MOISTURE CONTENT."
ERRMSG$(5) = "CURVE NUMBER MUST BE BETWEEN 0 AND 100."
ERRMSG$(6) = "SURFACE SLOPE MUST BE GREATER THAN 0.5 PERCENT."
ERRMSG$(7) = "SURFACE SLOPE LENGTH MUST BE GREATER THAN 0."
ERRMSG$(8) = "THE SOIL TEXTURE OF THE TOP LAYER MUST BE FROM 1 - 19 OR 21 - 33."
ERRMSG$(9) = "VEGETATIVE COVER MUST HAVE A VALUE BETWEEN 1 AND 5."
ERRMSG$(10) = "A CURVE NUMBER METHOD MUST BE SELECTED."
ERRMSG$(11) = "***WARNING***  THE SELECTED SOIL TEXTURE FOR CURVE NUMBER CALCULATIONS"
ERRMSG$(12) = "               IS NOT THE SAME AS THE SOIL TEXTURE OF LAYER 1."

''''''
   ERRNUM = 0
   IF VAL(AREA$) = 0 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 1
   IF UCASE$(moist$) = "Y" THEN
       IF VAL(snow$) < 0 THEN
         ERRNUM = ERRNUM + 1
         DESERR(ERRNUM) = 2
         ERRNUM = ERRNUM + 1
         DESERR(ERRNUM) = 3
         ERRNUM = ERRNUM + 1
         DESERR(ERRNUM) = 4
       END IF
   END IF
   IF VAL(SELCN$) = 1 THEN
      IF CN$ = "     " OR VAL(CN$) > 100 OR VAL(CN$) < 0 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 5
   ELSEIF VAL(SELCN$) = 2 THEN
      IF CN$ = "     " OR VAL(CN$) > 100 OR VAL(CN$) < 0 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 5
      IF VAL(SLOPE$) <= .5 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 6
      IF VAL(SLENGTH$) <= 0 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 7
   ELSEIF VAL(SELCN$) = 3 THEN
      IF VAL(SLOPE$) <= .5 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 6
      IF VAL(SLENGTH$) <= 0 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 7
      IF (VAL(SOILTEX$) > 0 AND VAL(SOILTEX$) < 20) OR (VAL(SOILTEX$) > 21 AND VAL(SOILTEX$) < 34) THEN
        'DATA IS OK
      ELSE
         ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 8
      END IF
      IF VAL(SOILTEX$) <> VAL(prop$(1, 3, 1)) THEN
         ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 11
         ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 12
      END IF
      IF VAL(VEGET$) < 1 OR VAL(VEGET$) > 5 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 9
   ELSEIF VAL(SELCN$) = 0 THEN
      ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 10
   END IF
''''''''''''''
'DRAW A BOX
''''''''''''''
  IF ERRNUM <> 0 THEN
    ERREXIST = 1
    CLS
    COLOR 3, 4
    LOCATE 1, 1: PRINT STRING$(80, 219);
    FOR I% = 2 TO 24
       LOCATE I%, 1: PRINT CHR$(219);
       LOCATE I%, 80: PRINT CHR$(219);
    NEXT
    COLOR 4, 3
    LOCATE 1, 32: PRINT " GENERAL DESIGN DATA"
    LOCATE 25, 1: PRINT "     F10, PgDn or Enter = Proceed      PgUp or Esc = Return      F9 = Quit      ";
    COLOR 15, 1
    JJ = 0
    FOR J = 1 TO ERRNUM
     IF DESERR(J) = 3 OR DESERR(J) = 4 OR DESERR(J) = 12 THEN
       LOCATE J + 4, 7: PRINT ERRMSG$(DESERR(J));
     ELSE
       JJ = JJ + 1
       LOCATE J + 4, 3: PRINT USING "##"; JJ; : PRINT ". "; : PRINT ERRMSG$(DESERR(J));
     END IF
    NEXT J
91860  KSTRG$ = "": KSTRG$ = INKEY$: IF KSTRG$ = "" THEN GOTO 91860
       NL = ASC(MID$(KSTRG$, 1))
       IF LEN(KSTRG$) = 2 THEN NH = ASC(MID$(KSTRG$, 2))
       IF (NL = 0 AND NH = 81) OR (NL = 0 AND NH = 68) OR ASC(KSTRG$) = 13 THEN EXIT SUB
       IF (NL = 0 AND NH = 73) OR NL = 27 THEN IRTRN% = 100: EXIT SUB
       IF (NL = 0 AND NH = 67) THEN IRTRN% = 1000: EXIT SUB
       GOTO 91860
    END IF
    EXIT SUB
END SUB

SUB DESCHK (NLYR, ERREXIST)
DIM ERRMSG$(20), VOID(20), DESERR(20), FULL(20)
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO CHECK LAYER PROPERTY VIOLATIONS                  '
'THIS SUBROUTINE TEST THE VALUES ENTERED FOR EACH LAYER AND     '
'VERIFIES THEIR NUMERICAL VALIDITY....  IT ALSO TESTS THE       '
'THE NUMERICAL VALUES WITH RESPECT TO EACH OTHER                '
'                                                               '
'   ERRNUM  = ERROR NUMBER... LAST VALUE INDICATES              '
'             TOTAL NUMBER OF ERRORS                            '
'  DESERR() = ERRO NUMBER                                       '
'  ERRMSG() = ERROR MESSAGE TO BE PRINTED                       '
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
       ERRMSG$(1) = "LAYER TYPE MUST BE 1, 2, 3, OR 4."
       ERRMSG$(2) = "LAYER THICKNESS MUST BE GREATER THAN ZERO."
       ERRMSG$(3) = "POROSITY MUST BE BETWEEN 0 AND 1."
       ERRMSG$(4) = "FIELD CAPACITY MUST BE BETWEEN 0 AND 1."
       ERRMSG$(5) = "WILTING POINT MUST BE BETWEEN 0 AND 1."
       ERRMSG$(6) = "INITIAL MOISTURE CONTENT MUST BE BETWEEN 0 AND 1."
       ERRMSG$(7) = "HYDRAULIC CONDUCTIVITY MUST BE GREATER THAN ZERO."
       ERRMSG$(8) = "DRAINAGE LENGTH MUST BE GREATER THAN ZERO."
       ERRMSG$(9) = "DRAIN SLOPE MUST BE GREATER THAN OR EQUAL TO ZERO."
       ERRMSG$(10) = "LEACHATE MAY ONLY BE RECIRCULATED TO LAYERS OF TYPE 1 OR 2."
       ERRMSG$(11) = "SUBSURFACE INFLOW IS ALLOWED ONLY INTO LAYERS OF TYPE 1 OR 2."
       ERRMSG$(12) = "PLACEMENT QUALITY MUST BE BETWEEN 1 AND 6."
       ERRMSG$(13) = "GEOTEXTILE TRANSMISSIVTY MUST BE GREATER THAN ZERO."
       ERRMSG$(14) = "FIELD CAPACITY MUST BE GREATER THAN WILTING POINT."
       ERRMSG$(15) = "POROSITY MUST BE GREATER THAN FIELD CAPACITY."
       ERRMSG$(16) = "MUST SPECIFY LAYER TO WHICH LEACHATE IS TO BE RECIRCULATED (TYPE 1 OR 2)."
       ERRMSG$(17) = "MOISTURE CONTENT MUST BE LESS THAN OR EQUAL TO POROSITY."
       ERRMSG$(18) = "MOISTURE CONTENT MUST BE GREATER THAN OR EQUAL TO WILTING POINT."
       ERRMSG$(19) = "WARNING!!   DRAIN SLOPE IS EQUAL TO ZERO."
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'CALL DESIGN TEST ROUTINE TO CHECK SOIL AND DESIGN DATA
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''
       l% = 1
       NLYR = 0
       FOR I = 1 TO 20
          kount = 0
          FOR K = 1 TO 3
            FOR J = 1 TO 7
              IF VAL(prop$(I, J, K)) <> 0 THEN
                 NLYR = NLYR + 1
                 kount = 1
                 FULL(kount) = NLYR
                GOTO 9500
              END IF
            NEXT J
          NEXT K
9500      IF kount = 0 THEN
               VOID(LYR) = I
          END IF
       NEXT I
       l% = 1
       FOR I = 1 TO 20
         DESERR(I) = 0
       NEXT I
96000  '''''''''''''''''
       ERRNUM = 0
       IF VAL(prop$(l%, 1, 1)) > 4 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 1
       IF VAL(prop$(l%, 1, 1)) < 1 THEN
         FOR P = 1 TO 3
          FOR J = 1 TO 7
            IF VAL(prop$(l%, J, P)) <> 0 THEN
                ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 1
                GOTO 96780
            END IF
          NEXT J
         NEXT P
        END IF
96780  IF VAL(prop$(l%, 2, 1)) = 0 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 2
       IF VAL(prop$(l%, 3, 1)) = 0 OR VAL(prop$(l%, 3, 1)) > 42 THEN
          IF VAL(prop$(l%, 1, 1)) <> 4 AND VAL(prop$(l%, 1, 1)) <> 3 THEN
            IF VAL(prop$(l%, 4, 1)) <= 0 OR VAL(prop$(l%, 4, 1)) > 1 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 3
            IF VAL(prop$(l%, 5, 1)) <= 0 OR VAL(prop$(l%, 5, 1)) > 1 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 4
            IF VAL(prop$(l%, 6, 1)) <= 0 OR VAL(prop$(l%, 6, 1)) > 1 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 5
            IF VAL(prop$(l%, 2, 2)) <= 0 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 7
          END IF
       END IF
       '''''''''''''''''''''''''''''''''''''''''''
       'CHECK INITIAL MOISTURE CONTENT
       '''''''''''''''''''''''''''''''''''''''''''
       IF VAL(prop$(l%, 1, 1)) = 1 OR VAL(prop$(l%, 1, 1)) = 2 THEN
         IF moist$ = "Y" THEN
            IF VAL(prop$(l%, 7, 1)) = 0 OR VAL(prop$(l%, 7, 1)) > 1 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 6
            IF VAL(prop$(l%, 7, 1)) > VAL(prop$(l%, 4, 1)) THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 17
            IF VAL(prop$(l%, 7, 1)) < VAL(prop$(l%, 6, 1)) THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 18
         END IF
       END IF
       '''''''''''''''''''''''''''''''''''''''''''
       'CHECK DRAIN SLOPE AND LENGTH
       '''''''''''''''''''''''''''''''''''''''''''
       IF VAL(prop$(l%, 1, 1)) = 2 AND (VAL(prop$(l% + 1, 1, 1)) = 3 OR VAL(prop$(l% + 1, 1, 1)) = 4) THEN
          IF VAL(prop$(l%, 3, 2)) = 0 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 8
          IF VAL(prop$(l%, 4, 2)) = 0 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 19
          IF VAL(prop$(l%, 4, 2)) < 0 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 9
       END IF
       ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
       'CHECK RELATION BETWEEN POROSITY, WILTING POINT AND FIELD CAPACITY
       ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
       IF VAL(prop$(l%, 1, 1)) <> 4 THEN
          IF VAL(prop$(l%, 4, 1)) > VAL(prop$(l%, 5, 1)) THEN
             IF VAL(prop$(l%, 5, 1)) > VAL(prop$(l%, 6, 1)) THEN
             ELSE
               ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 14
             END IF
          ELSE
             ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 15
             IF VAL(prop$(l%, 5, 1)) < VAL(prop$(l%, 6, 1)) THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 14
          END IF
       END IF
       ''''''''''''''''''''''''''''''''''''''''''''''''''
       'CHECK LEACHATE RECIRCULATION
       ''''''''''''''''''''''''''''''''''''''''''''''''''
       IF VAL(prop$(l%, 5, 2)) <> 0 THEN
          IF VAL(prop$(l%, 6, 2)) = 0 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 16
          leachto = VAL(prop$(l%, 6, 2))
          IF VAL(prop$(leachto, 1, 1)) = 3 OR VAL(prop$(leachto, 1, 1)) = 4 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 10
       END IF
       ''''''''''''''''''''''''''''''''''''''''''''''''''
       'CHECK SUBSURFACE INFLOW
       ''''''''''''''''''''''''''''''''''''''''''''''''''
       'commented to allow subsurface inflow into barrier liners
       ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'       IF VAL(PROP$(L%, 7, 2)) <> 0 THEN
'         IF VAL(PROP$(L%, 1, 1)) = 3 OR VAL(PROP$(L%, 1, 1)) = 4 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 11
'      END IF
       '''''''''''''''''''''''''''''''''''''''''''
       'CHECK FML DATA
       '''''''''''''''''''''''''''''''''''''''''''
       IF VAL(prop$(l%, 1, 1)) = 4 THEN
         IF VAL(prop$(l%, 2, 3)) <> 0 OR VAL(prop$(l%, 3, 3)) <> 0 THEN
            IF VAL(prop$(l%, 4, 3)) > 6 OR VAL(prop$(l%, 4, 3)) < 1 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 12
         END IF
       END IF
       IF VAL(prop$(l%, 4, 3)) = 6 THEN
          IF VAL(prop$(l%, 5, 3)) = 0 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 13
    END IF
''''''''''''''''''''''''''''''''''''
'DISPLAY ERROR FOR EACH LAYER
''''''''''''''''''''''''''''''''''''
    IF ERRNUM <> 0 THEN
       ERREXIST = 1
       CLS
       ''''''''''''''
       'DRAW A BOX
       ''''''''''''''
       COLOR 3, 4
       LOCATE 1, 1: PRINT STRING$(80, 219);
       FOR I% = 2 TO 24
           LOCATE I%, 1: PRINT CHR$(219);
           LOCATE I%, 80: PRINT CHR$(219);
       NEXT
       COLOR 4, 3
       LOCATE 1, 21: PRINT "SOIL/GEOMEMBRANE PROPERTY VERIFICATION"
       LOCATE 25, 1: PRINT "     F10, PgDn or Enter = Proceed      PgUp or Esc = Return      F9 = Quit      ";
       COLOR 15, 1
       COLOR 1, 15: LOCATE 3, 3: PRINT "LAYER "; : PRINT USING "##"; l%; : COLOR 15, 1
       FOR J = 1 TO ERRNUM
          LOCATE J + 4, 3: PRINT USING "##"; J; : PRINT ". "; : PRINT ERRMSG$(DESERR(J));
       NEXT J
       IRTRN% = 0
9660   KSTRG$ = "": KSTRG$ = INKEY$: IF KSTRG$ = "" GOTO 9660
       NL = ASC(MID$(KSTRG$, 1))
       IF LEN(KSTRG$) = 2 THEN NH = ASC(MID$(KSTRG$, 2))
       IF (NL = 0 AND NH = 81) OR (NL = 0 AND NH = 68) OR (ASC(KSTRG$) = 13) THEN
          IF l% = NLYR THEN
             EXIT SUB  'l% = 1: GOTO 96000
          ELSE
             l% = l% + 1: GOTO 96000
          END IF
       END IF
       IF (ASC(KSTRG$) = 13) THEN GOTO 97000
       IF NL = 0 AND NH = 67 THEN IRTRN% = 1000: EXIT SUB
       IF (NL = 0 AND NH = 73) OR NL = 27 THEN IRTRN% = 100: EXIT SUB
       GOTO 9660
    END IF
97000  IF l% = NLYR THEN
          EXIT SUB
       ELSE
          l% = l% + 1
          GOTO 96000
       END IF
'       GOTO 9660
'EXIT SUB
''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO CALL MANUAL MESSAGES FOR HELP
'''''''''''''''''''''''''''''''''''''''''''''''''
'97190  IF IRTRN% = 21 THEN CALL HLPMSG(f2help$)
'       IF IRTRN% = 20 THEN CALL USERHELP
'      RETURN
      END SUB

SUB LAYER
   DIM LAYERHD$(14)
   CLS : NLAYER = 13          'NUMBER OF OPTIONS IN THE LAYER MAIN MENU
   LAYERHD$(1) = "                      Project Title "
   LAYERHD$(2) = " "
   LAYERHD$(3) = " "
   LAYERHD$(4) = " "
   LAYERHD$(5) = " "
   LAYERHD$(6) = "                                        Landfill area ="
   LAYERHD$(7) = " "
   LAYERHD$(8) = "             Percent of area where runoff is possible = "
   LAYERHD$(9) = ""
   LAYERHD$(10) = "Do you want to specify initial moisture storage? (Y/N)  N"
   LAYERHD$(11) = " (If No, the program will initialize moisture "
   LAYERHD$(12) = "   contents to approximately steady-state.)"
  LAYERHD$(13) = "                   Amount of water or snow on surface = "
  APAGE% = 0: VPAGE% = 1: HPAGE% = 2
   SCREEN , , VPAGE%, VPAGE%
120 CLS
     f1help$ = "#LANDFILL"
     MOISTN$ = "NO": MOISTY$ = "YES"
     IF moist$ = "" THEN moist$ = "N"
     LOCATE 1, 1
     COLOR 11, 6
     PRINT "              SOIL AND DESIGN DATA - LANDFILL GENERAL INFORMATION               "
     COLOR 4, 3
     LOCATE 25, 1: PRINT "  F1=Info   F2=Help   F3=Keys   F9=Quit   F10=End   PgUp=Cycle   PgDn=Proceed   ";
     COLOR 15, 1
     'CALL menu
     LOCATE 3, 13: PRINT LAYERHD$(1)
     FOR I% = 2 TO NLAYER - 1
       LOCATE 3 + I%, 5: PRINT LAYERHD$(I%)
     NEXT I%
     SROWBOX = 4: SCOLBOX = 11: BOXWIDTH = 62: EROWBOX = 6
     CALL BOX
     IF moist$ = "Y" THEN LOCATE 17, 5: PRINT LAYERHD$(NLAYER)
     LOCATE 5, 12: PRINT TITLE$
     LOCATE 9, 61: PRINT AREA$
     LOCATE 11, 61: PRINT PRCNT$
     LOCATE 11, 67: PRINT "%"
     LOCATE 13, 61: PRINT moist$
     IF moist$ = "Y" THEN
        LOCATE 17, 61: PRINT snow$
        LOCATE 17, 67: PRINT UNMOIST$
     END IF
     COLOR 15, 1:  LOCATE 9, 67: PRINT UNAREA$
140  f2help$ = "#TITLE"
     CALL ACELL(5, 12, 60, TITLE$, 1)
     GOSUB 1191
     IF IRTRN% = 1000 THEN GOSUB 1196
     IF IRTRN% = 1 OR IRTRN% = -1 OR IRTRN% = 2 THEN EXIT SUB
     IF IRTRN% = 3 GOTO 175
     IF IRTRN% = 5 OR IRTRN% = 4 THEN GOTO 166
     GOTO 140
166  CALL ACELL(9, 61, 5, AREA$, 2)
     f2help$ = "#AREA"
     GOSUB 1191
     IF IRTRN% = 1000 THEN GOSUB 1196
     IF IRTRN% = 1 OR IRTRN% = -1 OR IRTRN% = 2 THEN EXIT SUB
     IF IRTRN% = 3 GOTO 140
     IF IRTRN% = 4 OR IRTRN% = 5 THEN GOTO 167
     GOTO 166
167  CALL ACELL(11, 61, 5, PRCNT$, 2)
     IF VAL(PRCNT$) > 100 THEN PRCNT$ = "100  ": LOCATE 11, 61: PRINT PRCNT$
     IF VAL(PRCNT$) < 0 THEN PRCNT$ = "0    ": LOCATE 11, 61: PRINT PRCNT$
     f2help$ = "#RUNPERCENT"
     GOSUB 1191
     IF IRTRN% = 1000 THEN GOSUB 1196
     IF IRTRN% = 1 OR IRTRN% = -1 OR IRTRN% = 2 THEN EXIT SUB
     IF IRTRN% = 3 GOTO 166
     IF IRTRN% = 4 OR IRTRN% = 5 THEN GOTO 170
     GOTO 167
170  CALL ACELL(13, 61, 1, moist$, 1)
     f2help$ = "#INITIALSM"
     moist$ = UCASE$(moist$): LOCATE 13, 61: PRINT moist$
     GOSUB 1191
     IF moist$ = "N" THEN
       LOCATE 17, 10: PRINT "                                                               "
     END IF
     IF IRTRN% = 1000 THEN GOSUB 1196
     IF IRTRN% = 1 OR IRTRN% = -1 OR IRTRN% = 2 THEN EXIT SUB
     IF moist$ = "Y" THEN
        LOCATE 17, 5: PRINT LAYERHD$(NLAYER)
        LOCATE 17, 67: PRINT UNMOIST$
     END IF
     IF UCASE$(LTRIM$(RTRIM$(moist$))) <> "Y" AND UCASE$(LTRIM$(RTRIM$(moist$))) <> "N" THEN
        moist$ = "N"
        LOCATE 13, 61: PRINT moist$
     END IF
     IF IRTRN% = 3 GOTO 167
     IF IRTRN% = 4 OR IRTRN% = 5 THEN GOTO 175
     GOTO 170
175  f2help$ = "#INITIALSM"
     IF (moist$ = "Y") AND (IRTRN% = 4 OR IRTRN% = 5) THEN
1175      CALL ACELL(17, 61, 5, snow$, 2)
       GOSUB 1191
       IF IRTRN% = 1000 THEN GOSUB 1196
       IF IRTRN% = 1 OR IRTRN% = -1 OR IRTRN% = 2 THEN EXIT SUB
       IF IRTRN% = 3 THEN GOTO 170
       IF IRTRN% = 4 OR IRTRN% = 5 THEN GOTO 140
       GOTO 1175
     ELSEIF (moist$ = "Y") AND (IRTRN% = 3) THEN
2175   CALL ACELL(17, 61, 5, snow$, 2)
       GOSUB 1191
       IF IRTRN% = 1000 THEN GOSUB 1196
       IF IRTRN% = 1 OR IRTRN% = -1 OR IRTRN% = 2 THEN EXIT SUB
       IF IRTRN% = 3 GOTO 170
       IF IRTRN% = 4 OR IRTRN% = 5 THEN GOTO 140
       GOTO 2175
     ELSEIF IRTRN% = 4 OR IRTRN% = 5 THEN moist$ = "N": GOTO 140
     ELSEIF IRTRN% = 3 THEN moist$ = "N": GOTO 170
     END IF
197  EXIT SUB
'''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO CALL MANUAL MESSAGES FOR HELP
'''''''''''''''''''''''''''''''''''''''''''''''''
1191   IF IRTRN% = 22 THEN CALL USERHELP
       IF IRTRN% = 21 THEN CALL HLPMSG(f2help$)
       IF IRTRN% = 20 THEN CALL HLPMSG(f1help$)
      RETURN
''''''''''''''''''''''''''''''''''''''''
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
''''''''''''''''''''''''''''''''''''''''
1196 CALL EXOUT(ANSCODE)
     IF ANSCODE = 0 THEN
        GOTO 120
     ELSE
        EXIT SUB
     END IF
RETURN
END SUB

SUB LAYERSHEET
    DIM start(3, 7), row(50), COLHD$(3, 3, 7), COLHEAD$(3, 7)
    DIM NCOL(3), PRO$(20, 7), ST(3, 7)
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''',
'L% = LAYER #                                                   '
'J% = COLUMN # OF A LAYER PROPERTY                              '
'NL% = NUMBER OF LAYERS                                         '
'INI% = STARTING NUMBER OF LAYER   (=1)                         '
'ROW() = ROW NUMBER WHERE LAYER L% IS DISPLAYED                 '
'START() = STARTING COLUMN FOR PROPERTY J%                      '
'NCOL() = NUMBER OF COLUMNS IN A GIVEN PAGE                     '
'PAGE% = PAGE # (THERE ARE THREE PAGES)                         '
'COLHD(I,J,K) = ROW I: COLUMN J, PAGE K ...HEADINGS FOR PAGES   '
',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
    BLANKS$ = "                                                                             "
    NL% = 20
    NCOL(1) = 7
    COLHD$(1, 1, 1) = "    LAYER    ": COLHD$(1, 1, 2) = "   LAYER   ": COLHD$(1, 1, 3) = "  SOIL   ": COLHD$(1, 1, 4) = "  TOTAL    ": COLHD$(1, 1, 5) = "   FIELD    ": COLHD$(1, 1, 6) = "  WILTING  ": COLHD$(1, 1, 7) = " INITIAL  "
    COLHD$(1, 2, 1) = "    TYPE     ": COLHD$(1, 2, 2) = " THICKNESS ": COLHD$(1, 2, 3) = " TEXTURE ": COLHD$(1, 2, 4) = " POROSITY  ": COLHD$(1, 2, 5) = "  CAPACITY  ": COLHD$(1, 2, 6) = "   POINT   ": COLHD$(1, 2, 7) = " MOISTURE "
    COLHD$(1, 3, 1) = "             ": COLHD$(1, 3, 2) = UN1$: COLHD$(1, 3, 3) = "   NO.   ": COLHD$(1, 3, 4) = " (VOL/VOL) ": COLHD$(1, 3, 5) = "  (VOL/VOL) ": COLHD$(1, 3, 6) = " (VOL/VOL) ": COLHD$(1, 3, 7) = " (VOL/VOL)"
    start(1, 1) = 9: start(1, 2) = 20: start(1, 3) = 31: start(1, 4) = 40: start(1, 5) = 52: start(1, 6) = 63: start(1, 7) = 74
    CW(1, 1) = 1: CW(1, 2) = 6: CW(1, 3) = 3: CW(1, 4) = 5: CW(1, 5) = 5: CW(1, 6) = 5: CW(1, 7) = 5
    NCOL(2) = 7
    COLHD$(2, 1, 1) = "   LAYER   ": COLHD$(2, 1, 2) = "   SAT. HYD.   ": COLHD$(2, 1, 3) = " DRAINAGE ": COLHD$(2, 1, 4) = " DRAIN ": COLHD$(2, 1, 5) = "  LEACHATE ": COLHD$(2, 1, 6) = "  RECIRC.  ": COLHD$(2, 1, 7) = " SUBSURFACE "
    COLHD$(2, 2, 1) = "   TYPE    ": COLHD$(2, 2, 2) = " CONDUCTIVITY  ": COLHD$(2, 2, 3) = "  LENGTH  ": COLHD$(2, 2, 4) = " SLOPE ": COLHD$(2, 2, 5) = "   RECIRC. ": COLHD$(2, 2, 6) = " TO LAYER  ": COLHD$(2, 2, 7) = "   INFLOW   "
    COLHD$(2, 3, 1) = "           ": COLHD$(2, 3, 2) = "   (CM/SEC)    ": COLHD$(2, 3, 3) = UN3$: COLHD$(2, 3, 4) = "  (%)  ": COLHD$(2, 3, 5) = "     (%)   ": COLHD$(2, 3, 6) = "    (#)    ": COLHD$(2, 3, 7) = UN4$
    start(2, 1) = 9: start(2, 2) = 15: start(2, 3) = 32: start(2, 4) = 41: start(2, 5) = 51: start(2, 6) = 62: start(2, 7) = 69:
    CW(2, 1) = 1: CW(2, 2) = 15: CW(2, 3) = 6: CW(2, 4) = 5: CW(2, 5) = 5: CW(2, 6) = 2: CW(2, 7) = 12
    NCOL(3) = 5
    COLHD$(3, 1, 1) = "   LAYER     ": COLHD$(3, 1, 2) = "  GEOMEMBRANE   ": COLHD$(3, 1, 3) = "  GEOMEMBRANE  ": COLHD$(3, 1, 4) = "  GEOMEMBRANE ": COLHD$(3, 1, 5) = "    GEOTEXTILE     "
    COLHD$(3, 2, 1) = "   TYPE      ": COLHD$(3, 2, 2) = "PINHOLE DENSITY ": COLHD$(3, 2, 3) = "INSTAL. DEFECTS": COLHD$(3, 2, 4) = "   PLACEMENT  ": COLHD$(3, 2, 5) = "  TRANSMISSIVITY   "
    COLHD$(3, 3, 1) = "             ": COLHD$(3, 3, 2) = UN5$:               COLHD$(3, 3, 3) = UN5$: COLHD$(3, 3, 4) = "    QUALITY   ": COLHD$(3, 3, 5) = "     (CMý/SEC)     "
    start(3, 1) = 9: start(3, 2) = 22: start(3, 3) = 38: start(3, 4) = 55: start(3, 5) = 65
    CW(3, 1) = 1: CW(3, 2) = 6: CW(3, 3) = 6: CW(3, 4) = 1: CW(3, 5) = 13
    SCREEN 0: CLS
    LOCATE 12, 15: PRINT "Preparing Landfill Layer Data Sheets.  Please Wait."
    ST(1, 1) = 4: ST(2, 1) = 4: ST(3, 1) = 4
    FOR P% = 1 TO 3
      FOR J% = 2 TO NCOL(P%)
        ST(P%, J%) = ST(P%, J% - 1) + LEN(COLHD$(P%, 1, J% - 1))
      NEXT J%
    NEXT P%
    FOR l% = 1 TO 20
      FOR P% = 1 TO 3
         FOR J = 1 TO NCOL(P%)
           IF VAL(prop$(l%, J, P%)) = 0 THEN
              prop$(l%, J, P%) = LEFT$(BLANKS$, CW(P%, J))
           END IF
         NEXT J
      NEXT P%
    NEXT l%
    INI% = 1:  l% = 1: J% = 1
520 FOR J = 1 TO NCOL(page%)
      FOR K = 1 TO 3
        COLHEAD$(K, J) = COLHD$(page%, K, J)
      NEXT K
    NEXT J
    FOR ro = 1 TO 20
      row(ro) = 3 + ro
    NEXT ro
    COLOR 15, 1: CLS
    COLOR 11, 6
    LOCATE 24, 1: PRINT " Alt A=Add Above    Alt B=Add Below    Alt C=Copy    Alt D=Delete    Alt M=Move ";
    COLOR 4, 3
    LOCATE 25, 1: PRINT "F1=Info  F2=Help  F3=Keys  F6/F7=Default/User Soils  F9=Quit  F10=End  PgUp PgDn";
    COLOR 15, 1
    INI% = 1
    COLOR 0, 3
      FOR I = 1 TO 23
         LOCATE I, 1: PRINT "   "
      NEXT I
      FOR I% = 1 TO NCOL(page%)
          LOCATE 1, ST(page%, I%): PRINT COLHEAD$(1, I%)
          LOCATE 2, ST(page%, I%): PRINT COLHEAD$(2, I%)
          LOCATE 3, ST(page%, I%): PRINT COLHEAD$(3, I%)
      NEXT I%
      FOR I% = INI% TO NL%
         A$ = RIGHT$(STR$(I%), 2)
         LOCATE I% + 3, 1: PRINT USING "\ \"; A$
      NEXT I%
      COLOR 15, 1
      IFROM% = 1: ITO% = NL%
530  IPATH = 0
     FOR I = IFROM% TO ITO%
        FOR J = 1 TO NCOL(page%)
          LOCATE row(I), start(page%, J)
          IF J = 1 THEN
            PRINT prop$(I, J, 1)
          ELSE
            PRINT prop$(I, J, page%)
          END IF
        NEXT J
      NEXT I
1020  IF page% = 2 AND J% = 1 THEN J% = 2
      IF page% = 3 AND J% = 1 THEN J% = 2
      COLOR 15, 1
      LOCATE 1, ST(page%, J%): PRINT COLHEAD$(1, J%)
      LOCATE 2, ST(page%, J%): PRINT COLHEAD$(2, J%)
  '   LOCATE 3, ST(PAGE%, J%): PRINT COLHEAD$(3, J%)
      LOCATE row(l%), 1: A$ = RIGHT$(STR$(l%), 2): PRINT USING "\ \"; A$
  '   GOSUB 12160
      IF LTRIM$(RTRIM$(prop$(l%, J%, page%))) = "" THEN prop$(l%, J%, page%) = LEFT$(BLANKS$, CW(page%, J%))
      CALL ACELL(row(l%), start(page%, J%), CW(page%, J%), prop$(l%, J%, page%), 2)
      '------------------------------------------
      'Check whether layer number is 1 through 4
      '------------------------------------------
      IF J% = 1 THEN
         IF VAL(prop$(l%, J%, page%)) < 1 OR VAL(prop$(l%, J%, page%)) > 4 THEN
           prop$(l%, J%, page%) = " "
           LOCATE row(l%), start(page%, J%): PRINT prop$(l%, J%, page%)
         END IF
      END IF
      '-------------------------------------
      'Check whether porosity, wilting Point
      'and field capacity are less than 1
      '-------------------------------------
      IF page% = 1 AND J% > 3 THEN
         IF VAL(prop$(l%, J%, page%)) >= 1 OR VAL(prop$(l%, J%, page%)) <= 0 THEN
            prop$(l%, J%, page%) = " "
            LOCATE row(l%), start(page%, J%): PRINT prop$(l%, J%, page%)
         END IF
      END IF
      '------------------------------------
      'Check whether drainage slope is
      'greater than 100 or less that 0
      '------------------------------------
      IF page% = 2 AND (J% = 4 OR J% = 5) THEN
         IF VAL(prop$(l%, J%, page%)) > 100 OR VAL(prop$(l%, J%, page%)) <= 0 THEN
            prop$(l%, J%, page%) = "     "
            LOCATE row(l%), start(page%, J%): PRINT prop$(l%, J%, page%)
         END IF
      END IF
      '---------------------------------------------
      'Check whether layer to receive recirculation
      'has a valid nubmer from 1 to 20
      '------------------------------------
      IF page% = 2 AND J% = 6 THEN
         IF VAL(prop$(l%, J%, page%)) > 20 OR VAL(prop$(l%, J%, page%)) < 1 THEN
            prop$(l%, J%, page%) = "  "
            LOCATE row(l%), start(page%, J%): PRINT prop$(l%, J%, page%)
         END IF
      END IF
      '---------------------------------------------------
      'Check whether FML placement quality is from 1 to 6
      '---------------------------------------------------
      IF page% = 3 AND J% = 4 THEN
         IF VAL(prop$(l%, J%, page%)) > 6 OR VAL(prop$(l%, J%, page%)) < 1 THEN
            prop$(l%, J%, page%) = " "
            LOCATE row(l%), start(page%, J%): PRINT prop$(l%, J%, page%)
         END IF
      END IF
      f1help$ = "#LAYERS"
      IF page% = 1 THEN
         IF J% = 1 THEN f2help$ = "#LAYERTYPE"
         IF J% = 2 THEN f2help$ = "#THICKNESS"
         IF J% = 3 THEN f2help$ = "#TEXTURE"
         IF J% >= 4 AND J% <= 7 THEN f2help$ = "#RETENTION"
'         IF J% = 7 THEN f2help$ = "#SOILMOISTURE"
      ELSEIF page% = 2 THEN
         IF J% = 1 THEN f2help$ = "#LAYERTYPE"
         IF J% = 2 THEN f2help$ = "#CONDUCTIVITY"
         IF J% = 3 OR J% = 4 THEN f2help$ = "#DRAIN"
         IF J% = 5 OR J% = 6 THEN f2help$ = "#RECIRCULATION"
         IF J% = 7 THEN f2help$ = "#INFLOW"
      ELSEIF page% = 3 THEN
         IF J% = 1 THEN f2help$ = "#LAYERTYPE"
         IF J% = 2 THEN f2help$ = "#DEFECTS"
         IF J% = 3 THEN f2help$ = "#DEFECTS"
         IF J% = 4 THEN f2help$ = "#PLACEMENT"
         IF J% > 4 THEN f2help$ = "#TRANSMISSIVITY"
      END IF
      GOSUB 15190
      IF IRTRN% = 1000 THEN GOSUB 15196
      GOSUB 12160
      COLOR 1, 1: LOCATE row(l%), start(page%, J%): PRINT LEFT$(BLANKS$, CW(page%, J%));
      COLOR 15, 1: LOCATE row(l%), start(page%, J%): PRINT prop$(l%, J%, page%)
      COLOR 0, 3
      LOCATE 1, ST(page%, J%): PRINT COLHEAD$(1, J%)
      LOCATE 2, ST(page%, J%): PRINT COLHEAD$(2, J%)
   '  LOCATE 3, ST(PAGE%, J%): PRINT COLHEAD$(3, J%)
      LOCATE row(l%), 1: A$ = RIGHT$(STR$(l%), 2): PRINT USING "\ \"; A$
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'SELECT SOIL TYPE FROM DEFAULT SOIL TEXTURE LIST
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
     IF IRTRN% = 41 THEN
       SOILNUM$ = ""
       CALL SOIL(l%, SOILNUM$)
       IF SOILNUM$ = "" THEN GOTO 513
       prop$(l%, 3, 1) = LTRIM$(SOILNUM$)
       IF LEN(prop$(l%, 3, 1)) = 1 THEN prop$(l%, 3, 1) = prop$(l%, 3, 1) + "  "
       IF LEN(prop$(l%, 3, 1)) = 2 THEN prop$(l%, 3, 1) = prop$(l%, 3, 1) + " "
       GOSUB 12150
513    COLOR 15, 1: GOTO 520
    END IF
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'SELECT SOIL TEXTURES FROM USER LIST
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    IF IRTRN% = 42 THEN
       SOILNUM$ = ""
       CALL USERSOIL(l%, SOILNUM$)
       IF SOILNUM$ = "" THEN GOTO 514
       prop$(l%, 3, 1) = LTRIM$(SOILNUM$)
       IF LEN(prop$(l%, 3, 1)) = 1 THEN prop$(l%, 3, 1) = prop$(l%, 3, 1) + "  "
       IF LEN(prop$(l%, 3, 1)) = 2 THEN prop$(l%, 3, 1) = prop$(l%, 3, 1) + " "
       GOSUB 12150
514    COLOR 15, 1: GOTO 520
    END IF
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'  CHANGE SOIL TEXTURE NUMBER TO ZERO IF DEFAULT VALUES
'  ARE MODIFIED.
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    xv = VAL(prop$(l%, 3, 1))
    IF xv > 0 AND xv < 43 THEN
      IF page% = 1 THEN
        IF J% = 4 AND VAL(prop$(l%, 4, 1)) <> VAL(HSOIL$(xv, 1)) THEN GOSUB 12170
        IF J% = 5 AND VAL(prop$(l%, 5, 1)) <> VAL(HSOIL$(xv, 2)) THEN GOSUB 12170
        IF J% = 6 AND VAL(prop$(l%, 6, 1)) <> VAL(HSOIL$(xv, 3)) THEN GOSUB 12170
      END IF
      IF page% = 2 THEN
        IF J% = 2 AND VAL(prop$(l%, 2, 2)) <> VAL(HSOIL$(xv, 4)) THEN GOSUB 12170
      END IF
    END IF
    IF xv > 42 THEN
     FOR ISOIL% = 1 TO SOILN%
       IF VAL(LEFT$(US$(ISOIL%), 5)) = xv THEN
         IF page% = 1 THEN
           IF J% = 4 AND VAL(prop$(l%, 4, 1)) <> VAL(MID$(US$(ISOIL%), 35, 5)) THEN GOSUB 12170
           IF J% = 5 AND VAL(prop$(l%, 5, 1)) <> VAL(MID$(US$(ISOIL%), 46, 5)) THEN GOSUB 12170
           IF J% = 6 AND VAL(prop$(l%, 6, 1)) <> VAL(MID$(US$(ISOIL%), 56, 6)) THEN GOSUB 12170
         END IF
         IF page% = 2 THEN
           IF J% = 2 AND VAL(prop$(l%, 2, 2)) <> VAL(MID$(US$(ISOIL%), 64, 14)) THEN GOSUB 12170
         END IF
       END IF
     NEXT ISOIL%
     '''''''''''''''''''''''''''''''''''''''''''
     'SOIL IS NEITHER DEFAULT NOR USER DEFINED
     'THEREFORE COMPARE WITH SOIL TEXTURES ON
     'THE SCREEN, AND CHECK VALUES
     '''''''''''''''''''''''''''''''''''''''''''
     xv = VAL(prop$(l%, 3, 1))
     FOR I = 1 TO 20
       IF VAL(prop$(I, 3, 1)) = xv THEN
         IF page% = 1 THEN
           IF J% = 4 AND VAL(prop$(l%, 4, 1)) <> VAL(prop$(I, 4, 1)) THEN GOSUB 12170
           IF J% = 5 AND VAL(prop$(l%, 5, 1)) <> VAL(prop$(I, 5, 1)) THEN GOSUB 12170
           IF J% = 6 AND VAL(prop$(l%, 6, 1)) <> VAL(prop$(I, 6, 1)) THEN GOSUB 12170
         END IF
         IF page% = 2 THEN
           IF J% = 2 AND VAL(prop$(l%, 2, 2)) <> VAL(prop$(I, 2, 2)) THEN GOSUB 12170
         END IF
       END IF
     NEXT I
    END IF
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'CHANGE PAGES FORWARD (PG DN)
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
21912 IF IRTRN% = 1 THEN
        IF page% = 1 AND J% = 3 THEN GOSUB 12150
        IF page% < 3 THEN
          page% = page% + 1
          J% = 1: 'l% = 1
          GOTO 520
        ELSE
          COLOR 15, 1
          EXIT SUB
          GOTO 520
        END IF
    END IF
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'CHANGE PAGES BACKWARDS (PG UP)
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
    IF IRTRN% = -1 THEN
       IF page% = 1 AND J% = 3 THEN GOSUB 12150
       IF page% > 1 THEN
         page% = page% - 1
         J% = 1: 'L% = 1
         GOTO 520
       ELSE COLOR 15, 1: EXIT SUB
       END IF
    END IF
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'INSERT A LAYER ABOVE CURRENT LAYER
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''
      IF IRTRN% = 30 THEN GOSUB 12130
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'INSERT A LAYER BELOW CURRENT LAYER
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''
      IF IRTRN% = 31 THEN GOSUB 12120
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'COPY A LAYER   (COPY/REPLACE   COPY ABOVE   COPY BELOW) '
',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,'
        IF IRTRN% = 32 THEN
        COLOR 11, 6
        LOCATE 24, 1: PRINT " COPY LAYER:    Cursor then: Alt B=Below Layer   Alt A=Above Layer   Esc=Cancel";
        LOCATE 24, 14: PRINT USING "##"; l%;
        COLOR 15, 1
        CL% = l%
10200  IF LTRIM$(RTRIM$(prop$(l%, J%, page%))) = "" THEN prop$(l%, J%, page%) = LEFT$(BLANKS$, CW(page%, J%))
       CALL RCELL(row(l%), start(page%, J%), CW(page%, J%), prop$(l%, J%, page%))
'
'SCROLL DOWN TO SELECT A LAYER LOCATION FOR COPYING
'
        IF IRTRN% = 1000 THEN GOSUB 15196
        COLOR 0, 3
        IF IRTRN% = 10 THEN GOTO 1020
        IF IRTRN% = 4 THEN
          IF l% < NL% THEN
            l% = l% + 1
          ELSE
            l% = 1
          END IF
          GOTO 10200
        END IF
'
'SCROLL UP
'
       IF IRTRN% = 3 THEN
           IF l% <> 1 THEN
              l% = l% - 1
           ELSE
              l% = NL%
           END IF
            GOTO 10200
       END IF
       IF IRTRN% = 100 THEN
          GOTO 1020
       END IF
'
'COPY TO A NEW LAYER ABOVE CURRENT LAYER
'
      IF IRTRN% = 30 THEN
          IPATH = 1   'MEANS COPY ABOVE (CODE TO USE IN ALT A ROUTINE)
        FOR P = 1 TO 3
          FOR J = 1 TO NCOL(P)
            PRO$(J, P) = prop$(CL%, J, P)
          NEXT J
        NEXT P
        ML% = l%
        GOSUB 12130
        FOR P = 1 TO 3
         FOR J = 1 TO NCOL(P)
           prop$(ML%, J, P) = PRO$(J, P)
         NEXT J
        NEXT P
        IFROM% = 1: ITO% = NL%: INI% = 1
        GOSUB 12140
        GOTO 520
        END IF
'
'COPY LAYER TO BELOW CURRENT LAYER
'
      IF IRTRN% = 31 THEN
        IPATH = 1 ' MEANS COPY BELOW, CODE TO USE IN ALT B ROUTINE
        FOR P = 1 TO 3
          FOR J = 1 TO NCOL(P)
             PRO$(J, P) = prop$(CL%, J, P)
          NEXT J
        NEXT P
        ML% = l%
        GOSUB 12120
        FOR J = 1 TO 8
          prop$(ML% + 1, J, 1) = PRO$(J, 1)
          prop$(ML% + 1, J, 2) = PRO$(J, 2)
          prop$(ML% + 1, J, 3) = PRO$(J, 3)
        NEXT J
        INI% = 1
        IFROM% = 1: ITO% = NL%
        GOSUB 12140
        GOTO 520
     END IF
     END IF
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'MOVE A LAYER TO ANOTHER LOCATION (DEL + INS A OR INS B)
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
      IF IRTRN% = 33 THEN
        COLOR 11, 6
        LOCATE 24, 1: PRINT " MOVE LAYER:    Cursor then: Alt B=Below Layer   Alt A=Above Layer   Esc=Cancel";
        LOCATE 24, 14: PRINT USING "##"; l%;
        COLOR 15, 1
        CL% = l%
10201   XLEN = LEN(COLHD$(page%, 1, J%)) - 6
        CALL RCELL(row(l%), start(page%, J%), CW(page%, J%), prop$(l%, J%, page%))
'
'SCROLL DOWN
'
        IF IRTRN% = 1000 THEN GOSUB 15196
        IF IRTRN% = 10 THEN GOTO 1020
        IF IRTRN% = 4 THEN
          IF l% < NL% THEN
            l% = l% + 1
          ELSE
            l% = 1
          END IF
        GOTO 10201
        END IF
'
'SCROLL UP
'
       IF IRTRN% = 3 THEN
           IF l% <> 1 THEN
              l% = l% - 1
           ELSE
              l% = NL%
           END IF
           GOTO 10201
       END IF
       IF IRTRN% = 100 THEN
           GOTO 1020
       END IF
'
'MOVE LAYER TO ABOVE THIS LAYER (alt C + alt A)
'
      IF IRTRN% = 30 THEN
        IPATH = 1                 'CODE TO USE IN ALT A AND DELETE SUBROUTINES
        FOR P = 1 TO 3
          FOR J = 1 TO NCOL(P)
            PRO$(J, P) = prop$(CL%, J, P)
          NEXT J
        NEXT P
        ML% = l%
        l% = CL%
        GOSUB 12110
        IF CL% >= ML% THEN l% = ML%: GOSUB 12130
        IF CL% < ML% THEN l% = ML% - 1: GOSUB 12130: ML% = l%
        FOR P = 1 TO 3
          FOR J = 1 TO NCOL(P)
             prop$(ML%, J, P) = PRO$(J, P)
          NEXT J
        NEXT P
        IFROM% = 1: ITO% = NL%
        COLOR 15, 1
        GOSUB 12140
        GOTO 520
     END IF
'
'MOVE LAYER TO BELOW THIS LAYER (alt M + alt B)
'
      IF IRTRN% = 31 THEN
        FOR P = 1 TO 3
          FOR J = 1 TO NCOL(P)
            PRO$(J, P) = prop$(CL%, J, P)
          NEXT J
        NEXT P
        IPATH = 1
        ML% = l%
        l% = CL%
        GOSUB 12110
        IF CL% >= ML% THEN l% = ML%: GOSUB 12120: NLR% = l% + 1
        IF CL% < ML% THEN l% = ML%: GOSUB 12130: NLR% = l%
        FOR P = 1 TO 3
           FOR J = 1 TO NCOL(P)
              prop$(NLR%, J, P) = PRO$(J, P)
           NEXT J
        NEXT P
        IFROM% = 1: ITO% = NL%
        COLOR 15, 1
        GOSUB 12140
        GOTO 520
     END IF
     END IF
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' CALL DELETE SUBROUTINE
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
1115  IF IRTRN% = 13 THEN
         IF page% = 1 AND J% = 3 THEN GOSUB 12150
         GOSUB 12110
      END IF
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'EXIT
'''''''''''''''''''''''''''''''''''''''''''''''''''''''
      IF IRTRN% = 2 THEN
         IF page% = 1 AND J% = 3 THEN GOSUB 12150
         GOTO 1120
      END IF
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'MOVE DOWN
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
     IF IRTRN% = 4 THEN
     IF page% = 1 AND J% = 3 THEN GOSUB 12150
      IF l% < NL% THEN
          l% = l% + 1
          GOTO 1020
        ELSE
          l% = 1
        END IF
     END IF
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'MOVE UP
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  IF IRTRN% = 3 THEN
  IF page% = 1 AND J% = 3 THEN GOSUB 12150
      IF l% <> 1 THEN
        l% = l% - 1
        GOTO 1020
      ELSE
        l% = NL%
        GOTO 1020
      END IF
  END IF
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'MOVE RIGHT
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   IF (IRTRN% = 6) OR (IRTRN% = 5) THEN
    IF page% = 1 AND J% = 3 THEN GOSUB 12150
       IF J% < NCOL(page%) THEN
        J% = J% + 1
        GOTO 1020
      ELSEIF J% = NCOL(page%) AND l% < NL% THEN
        l% = l% + 1
        J% = 1
        GOTO 1020
      ELSEIF J% = NCOL(page%) THEN
        l% = 1: J% = 1: GOTO 1020
      END IF
   END IF
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'MOVE LEFT
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   IF IRTRN% = 7 THEN
      IF page% = 1 AND J% = 3 THEN GOSUB 12150
      stco% = 2         'this is the starting column for pages 2 and 3
      IF page% = 1 THEN stco% = 1
      IF J% > stco% THEN
         J% = J% - 1
         GOTO 1020
      ELSEIF J% = stco% AND l% > 1 THEN
         J% = NCOL(page%)
         l% = l% - 1
         GOTO 1020
      ELSEIF J% = stco% THEN
         l% = NL%: J% = NCOL(page%): GOTO 1020
      END IF
   END IF
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'MOVE HOME
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
      IF IRTRN% = 8 THEN
       IF page% = 1 AND J% = 3 THEN GOSUB 12150
        l% = 1
        J% = 1: GOTO 1020
      END IF
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'MOVE END
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
      IF IRTRN% = 9 THEN
      IF page% = 1 AND J% = 3 THEN GOSUB 12150
        l% = NL%
        J% = NCOL(page%): GOTO 1020
      END IF
      GOTO 1020
1120 COLOR 15, 1: EXIT SUB
     EXIT SUB
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'DELETE A LAYER
',,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
12110  FOR perase = 1 TO 3
         FOR jerase = 1 TO 7
            prop$(21, jerase, perase) = ""
         NEXT jerase
       NEXT perase
       K% = l%
       FOR P% = 1 TO 3
         FOR J = 1 TO 7
           prop$(l%, J, P%) = LEFT$(BLANKS$, CW(P%, J))
         NEXT J
       NEXT P%
       FOR I% = l% TO NL%
         FOR K = 1 TO 3
            FOR J = 1 TO NCOL(K)
               prop$(I%, J, K) = prop$(I% + 1, J, K)
            NEXT J
         NEXT K
           K% = K% + 1
         NEXT I%
         INI% = 1
         IFROM% = 1: ITO% = NL%
         COLOR 15, 1
         IF IPATH = 1 THEN RETURN
         GOTO 520
        RETURN
'''''''''''''''''''''''''''''''''''
'ADD A LAYER BELOW CURRENT LAYER
''''''''''''''''''''''''''''''''''
12120  K% = NL%
        FOR I% = l% + 1 TO NL%
         FOR P = 1 TO 3
            FOR J = 1 TO NCOL(P)
              prop$(K%, J, P) = prop$(K% - 1, J, P)
            NEXT J
          NEXT P
          K% = K% - 1
        NEXT I%
       FOR K = 1 TO 3
        FOR J = 1 TO NCOL(page%)
          prop$(l% + 1, J, K) = LEFT$(BLANKS$, CW(K, J))
        NEXT J
       NEXT K
        INI% = l% + 1
        IFROM% = l% + 1: ITO% = NL%
        COLOR 15, 1
        IF IPATH = 1 THEN RETURN
        GOTO 520
        RETURN
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'INSERT A LAYER ABOVE CURRENT LAYER
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
12130  K% = NL%
        FOR I% = l% TO NL%
          FOR P = 1 TO 3
            FOR J = 1 TO NCOL(P)
                prop$(K%, J, P) = prop$(K% - 1, J, P)
            NEXT J
          NEXT P
          K% = K% - 1
        NEXT I%
        'blank the line
        FOR K = 1 TO 3
          FOR J = 1 TO NCOL(page%)
            prop$(l%, J, K) = LEFT$(BLANKS$, CW(K, J))
          NEXT J
        NEXT K
        INI% = 1
        IFROM% = 1: ITO% = NL%
        COLOR 15, 1
        IF IPATH = 1 THEN RETURN
        GOTO 520
        RETURN
12140   COLOR 11, 6
        LOCATE 24, 1: PRINT " Alt A=Add Above    Alt B=Add Below    Alt C=Copy    Alt D=Delete    Alt M=Move ";
        COLOR 15, 1
        RETURN
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' SUBROUTINE TO CHECK WHETHER SOIL TEXTURE DATA IS AVAILABLE
' IN DEFAULT OR USER DEFINED SOIL TEXTURES
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
12150 IF VAL(prop$(l%, 3, 1)) <= 42 AND VAL(prop$(l%, 3, 1)) > 0 THEN
         xval = VAL(prop$(l%, 3, 1))
         prop$(l%, 4, 1) = HSOIL$(xval, 1) + "   "
         prop$(l%, 5, 1) = HSOIL$(xval, 2) + "   "
         prop$(l%, 6, 1) = HSOIL$(xval, 3) + "   "
         prop$(l%, 2, 2) = HSOIL$(xval, 4) + "   "
         COLOR 15, 1
         FOR J = 1 TO NCOL(page%)
            LEFTJUSTIFY$ = SPACE$(LEN(COLHD$(page%, 1, J)) - 6)
            LSET LEFTJUSTIFY$ = prop$(l%, J, page%)
            LOCATE row(l%), start(page%, J): PRINT prop$(l%, J, page%)
            COLOR 15, 1
         NEXT J
         COLOR 1, 15
      ELSE
      ''''''''''''''''''''''''''''''''''''''''''''
      'CHECK IF SOIL TEXTURE IS USER DEFINED
      ''''''''''''''''''''''''''''''''''''''''''''
      IF VAL(prop$(l%, 3, 1)) > 42 THEN
         CLOSE
         U$ = DIR$("SOIL.USR")
         IF LEN(U$) <> 0 THEN
          OPEN "SOIL.USR" FOR INPUT AS #1
          SOILN% = 0
          FOR I = 1 TO 100
            IF EOF(1) THEN GOTO 80092
            LINE INPUT #1, US$(I)
            SOILN% = SOILN% + 1
          NEXT I
80092   CLOSE 1
         xval = VAL(prop$(l%, 3, 1))
         FOR I = 1 TO SOILN%
            IF xval <> VAL(LEFT$(US$(I), 5)) THEN GOTO 91125
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''
            'THEREFORE SOIL TEXTURE IS USER DEFINED
            'GET VALUES AND PRINT IN PROPER LOCATION
            'ON SCREEN
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''
            prop$(l%, 4, 1) = LTRIM$(RTRIM$(MID$(US$(I), 35, 5))) + "   "
            prop$(l%, 5, 1) = LTRIM$(RTRIM$(MID$(US$(I), 46, 5))) + "   "
            prop$(l%, 6, 1) = LTRIM$(RTRIM$(MID$(US$(I), 56, 6))) + "   "
            prop$(l%, 2, 2) = LTRIM$(RTRIM$(MID$(US$(I), 64, 14))) + "   "
            COLOR 15, 1
            FOR J = 1 TO NCOL(page%)
              LEFTJUSTIFY$ = SPACE$(LEN(COLHD$(page%, 1, J)) - 6)
              LSET LEFTJUSTIFY$ = prop$(l%, J, page%)
              IF VAL(prop$(l%, 4, 1)) = 0 THEN prop$(l%, 4, 1) = "     "
              IF VAL(prop$(l%, 5, 1)) = 0 THEN prop$(l%, 5, 1) = "     "
              IF VAL(prop$(l%, 6, 1)) = 0 THEN prop$(l%, 6, 1) = "     "
              LOCATE row(l%), start(page%, J): PRINT prop$(l%, J, page%)
              COLOR 15, 1
              foundit = 1
            NEXT J
            COLOR 1, 15
91125    NEXT I
       END IF
       END IF
      ''''''''''''''''''''''''''''''''''''''''''''''''''''''''
      'SOIL TEXTURE IS NIETHER DEFAULT NOR USER DEFINED.
      'HENCE, IT IS A NEW USER DEFINED SOIL TEXTURE.
      'CHECK IF THIS SOIL TEXTURE HAS ALREADY BEEN ENTERED
      'ON THE SCREEN.  IF SO, PRINT THE SAME VALUES AS THE
      'PREVIOUSLY ENTERED TEXTURE.
      'IF NOT, CLEAR ALL SOIL PROPERTIES DATA
      ''''''''''''''''''''''''''''''''''''''''''''''''''''''''
      IF foundit = 1 THEN foundit = 0: RETURN
      xval = VAL(prop$(l%, 3, 1))
      IF xval > 42 THEN
      FOR I = 1 TO 20
        IF VAL(prop$(I, 3, 1)) = xval THEN
            prop$(l%, 4, 1) = prop$(I, 4, 1)
            prop$(l%, 5, 1) = prop$(I, 5, 1)
            prop$(l%, 6, 1) = prop$(I, 6, 1)
            prop$(l%, 2, 2) = prop$(I, 2, 2)
            COLOR 15, 1
            FOR J = 1 TO NCOL(page%)
              LEFTJUSTIFY$ = SPACE$(LEN(COLHD$(page%, 1, J)) - 6)
              LSET LEFTJUSTIFY$ = prop$(l%, J, page%)
              IF VAL(prop$(l%, 4, 1)) = 0 THEN prop$(l%, 4, 1) = "     "
              IF VAL(prop$(l%, 5, 1)) = 0 THEN prop$(l%, 5, 1) = "     "
              IF VAL(prop$(l%, 6, 1)) = 0 THEN prop$(l%, 6, 1) = "     "
              LOCATE row(l%), start(page%, J): PRINT prop$(l%, J, page%)
              COLOR 15, 1
            NEXT J
        ELSE
          ' PROP$(L%, 4, 1) = "      "
          ' PROP$(L%, 5, 1) = "      "
          ' PROP$(L%, 6, 1) = "      "
          ' PROP$(L%, 7, 1) = "      "
          ' PROP$(L%, 2, 2) = "            "
        END IF
      NEXT I
      END IF
      RETURN
    END IF
    RETURN
'SUBROUTINE TO LEFT JUSTIFY ALL VALUES
12160  LEFTJUSTIFY$ = SPACE$(CW(page%, J%))
        IF prop$(l%, J%, page%) = LEFT$(BLANKS$, CW(page%, J%)) THEN RETURN
        prop$(l%, J%, page%) = LTRIM$(prop$(l%, J%, page%))
        LSET LEFTJUSTIFY$ = prop$(l%, J%, page%)
        prop$(l%, J%, page%) = LEFT$(prop$(l%, J%, page%), CW(page%, J%))
        RETURN
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' SUBROUTINE TO CHANGE SOIL TEXTURE TO ZERO IF DEFAULT
' PARAMETERS ARE CHANGED BY THE USER.
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
12170  prop$(l%, 3, 1) = "0  "
       IF page% = 1 THEN
       COLOR 15, 1
          LOCATE row(l%), start(page%, 3): PRINT prop$(l%, 3, 1)
       COLOR 1, 15
       END IF
       RETURN
''''''''''''''''''''''''''''''''''''''''
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
''''''''''''''''''''''''''''''''''''''''
15196 CALL EXOUT(ANSCODE)
     IF ANSCODE = 0 THEN
        GOTO 520
     ELSE
        EXIT SUB
     END IF
RETURN

''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO CALL MANUAL MESSAGES FOR HELP
''''''''''''''''''''''''''''''''''''''''''''''''
15190  IF IRTRN% = 22 THEN CALL USERHELP
       IF IRTRN% = 21 THEN CALL HLPMSG(f2help$)
       IF IRTRN% = 20 THEN CALL HLPMSG(f1help$)
       RETURN
END SUB

SUB LYRCHK (NLYR, ERREXIST)
DIM ERRMSG$(50), DESERR(50)
''''''''''''''''''''''''''''''''''''''''''''''''''''
' SUBROUTINE TO CHECK LAYER ARRANGEMENT VIOLATIONS
'   ERRNUM  = ERROR NUMBER... LAST VALUE INDICATES
'             TOTAL NUMBER OF ERRORS
'  DESERR() = ERRO NUMBER
'  ERRMSG() = ERROR MESSAGE TO BE PRINTED
''''''''''''''''''''''''''''''''''''''''''''''''''''
'
'DEFINE ERROR MESSAGES
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
ERRMSG$(1) = "A SOIL LINER MAY NOT BE PLACED ON TOP OF THE LANDFILL."
ERRMSG$(2) = "A FLEXIBLE MEMBRANE LINER MAY NOT BE PLACED ON TOP OF THE LANDFILL."
ERRMSG$(3) = "A FLEXIBLE MEMBRANE LINER MAY NOT BE THE BOTTOMMOST LAYER."
ERRMSG$(4) = "A LATERAL DRAINAGE LAYER MAY NOT BE THE BOTTOMMOST LAYER IN THE LANDFILL."
ERRMSG$(5) = "A LATERAL DRAINAGE LAYER CANNOT BE ON TOP OF A VERTICAL PERCOLATION LAYER."
ERRMSG$(6) = "A SOIL LINER MAY NOT BE PLACED ON TOP OF ANOTHER SOIL LINER."
ERRMSG$(7) = "A FLEXIBLE MEMBRANE LINER MAY NOT BE PLACED BETWEEN TWO SOIL LINERS."
ERRMSG$(8) = "A FML MAY NOT BE PLACED ON TOP OF ANOTHER FML."
ERRMSG$(9) = "A SOIL LINER MAY NOT BE PLACED BETWEEN TWO FLEXIBLE MEMBRANE LINERS."
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

       ERRNUM = 0
'NO TYPE 3 OR 4 ON TOP
       IF VAL(prop$(1, 1, 1)) = 3 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 1
       IF VAL(prop$(1, 1, 1)) = 4 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 2
'NO TYPE 2 OR 4 ON BOTTOM
       IF VAL(prop$(NLYR, 1, 1)) = 2 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 4
       'IF VAL(prop$(NLYR, 1, 1)) = 4 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 3
'NO 2/1
       FOR I = 1 TO NLYR - 1
          IF VAL(prop$(I, 1, 1)) = 2 THEN
             IF VAL(prop$(I + 1, 1, 1)) = 1 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 5
          END IF
       NEXT I
'NO 3/3
       FOR I = 1 TO NLYR - 1
          IF VAL(prop$(I, 1, 1)) = 3 THEN
             IF VAL(prop$(I + 1, 1, 1)) = 3 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 6
          END IF
       NEXT I
'NO 3/4/3
       FOR I = 1 TO NLYR - 2
          IF VAL(prop$(I, 1, 1)) = 3 THEN
             IF VAL(prop$(I + 1, 1, 1)) = 4 THEN
                IF VAL(prop$(I + 2, 1, 1)) = 3 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 7
             END IF
          END IF
       NEXT I
'NO 4/4
       FOR I = 2 TO NLYR - 1
          IF VAL(prop$(I, 1, 1)) = 4 THEN
             IF VAL(prop$(I + 1, 1, 1)) = 4 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 8
          END IF
       NEXT I
'NO 4/3/4
       FOR I = 2 TO NLYR - 2
          IF VAL(prop$(I, 1, 1)) = 4 THEN
             IF VAL(prop$(I + 1, 1, 1)) = 3 THEN
                IF VAL(prop$(I + 2, 1, 1)) = 4 THEN ERRNUM = ERRNUM + 1: DESERR(ERRNUM) = 9
             END IF
          END IF
       NEXT I
'''''''''''''''''''''''''''''''''''''
'DISPLAY ERROR MESSAGES FOR LAYER
'ARRANGEMENT VIOLATIONS
'''''''''''''''''''''''''''''''''''''
    IF ERRNUM <> 0 THEN
       ERREXIST = 1
       CLS
       ''''''''''''''
       'DRAW A BOX
       ''''''''''''''
       COLOR 3, 4
       LOCATE 1, 1: PRINT STRING$(80, 219);
       FOR I% = 2 TO 24
           LOCATE I%, 1: PRINT CHR$(219);
           LOCATE I%, 80: PRINT CHR$(219);
       NEXT
       COLOR 4, 3
       LOCATE 1, 25: PRINT "LAYER ARRANGEMENT VERIFICATION"
       LOCATE 25, 1: PRINT "     F10, PgDn or Enter = Proceed      PgUp or Esc = Return      F9 = Quit      ";
       COLOR 15, 1
       FOR J = 1 TO ERRNUM
          LOCATE J + 2, 2: PRINT USING "##"; J; : PRINT ". "; : PRINT ERRMSG$(DESERR(J));
       NEXT J
9860   KSTRG$ = "": KSTRG$ = INKEY$: IF KSTRG$ = "" GOTO 9860
       NL = ASC(MID$(KSTRG$, 1))
       IF LEN(KSTRG$) = 2 THEN NH = ASC(MID$(KSTRG$, 2))
       IF (NL = 0 AND NH = 81) OR (NL = 0 AND NH = 68) OR ASC(KSTRG$) = 13 THEN EXIT SUB
       IF (NL = 0 AND NH = 73) OR NL = 27 THEN EXIT SUB
       IF (NL = 0 AND NH = 67) THEN IRTRN% = 1000: EXIT SUB
       GOTO 9860
    END IF
  EXIT SUB
END SUB

SUB SVFILE (FILESV$)
   ON LOCAL ERROR RESUME NEXT
   f1help$ = "#D10SAVE"
   D10SAVE$ = "DATA10"
   defdir$ = CURDIR$
7101 CLS
  'SVPATH$ = CURDIR$
   SVPATH$ = D10PATH$
   EXT$ = "D10"
   COLOR 11, 6
   LOCATE 1, 1:  PRINT "                       SOIL AND DESIGN DATA - FILE SAVING                       "
   COLOR 15, 1
   SROWBOX = 4: SCOLBOX = 2: BOXWIDTH = 78: EROWBOX = 8
   CALL BOX
   LOCATE 5, 22: PRINT "   Save"
   LOCATE 6, 22: PRINT "   Y/N ------------------DIRECTORY-------------  --FILE--"
   LOCATE 7, 3:  PRINT " SOIL AND DESIGN DATA"
   COLOR 4, 3
   LOCATE 25, 1: PRINT "  F1=Info   F2=Help   F3=Keys   F9=Quit   F10=End   PgUp=Cycle   PgDn=Proceed   ";
   COLOR 15, 1
   GOSUB 78165
   'CALL menu
   ANS$ = "Y"
   LOCATE 7, 26: PRINT "Y"
   LOCATE 7, 29: PRINT SVPATH$
   LOCATE 7, 71: PRINT D10SAVE$
7130 CALL ACELL(7, 26, 1, ANS$, 3)
   f2help$ = "#SAVETAG"
     LOCATE 13, 20: FOR III = 1 TO 55: PRINT " "; : NEXT III
     ANS$ = UCASE$(ANS$)
     IF ANS$ <> "Y" OR ANS$ <> "N" THEN
        ANS$ = "Y": LOCATE 7, 26: PRINT ANS$;
     END IF
     IF IRTRN% = 51 THEN ANS$ = "Y": LOCATE 7, 26: PRINT ANS$; : GOTO 7140
     IF IRTRN% = 50 THEN ANS$ = "N": LOCATE 7, 26: PRINT ANS$; : GOTO 7140
     LOCATE 13, 20: FOR III = 1 TO 55: PRINT " "; : NEXT III
     GOSUB 73290
     IF IRTRN% = 6 THEN GOTO 7140
     IF IRTRN% = 7 THEN GOTO 7145
     IF IRTRN% = 5 THEN
       IF ANS$ <> "Y" OR ANS$ <> "N" THEN
           ANS$ = "Y"
           GOTO 7140
       END IF
     END IF
     IF IRTRN% = 1 OR IRTRN% = 2 THEN
        IF ANS$ = "N" THEN
           GOSUB 2196
        ELSE
           GOSUB 6000
           IF FOUND = 3 THEN EXIT SUB
        END IF
     END IF
     IF IRTRN% = -1 THEN EXIT SUB
     IF IRTRN% = 1000 THEN GOSUB 2196
     IF IRTRN% = 23 THEN
        CALL GETDIR(EXT$, SELECT$, SVPATH$)
        IF IRTRN% = 1000 THEN EXIT SUB
        IF SELECT$ <> "" THEN D10SAVE$ = SELECT$: SELECT$ = ""
        LOCATE 7, 71: PRINT "        ": LOCATE 7, 71: PRINT D10SAVE$
        GOSUB 78165
     END IF
     GOTO 7130
7140 CALL ACELL(7, 29, 40, SVPATH$, 1)
     LOCATE 13, 20: FOR III = 1 TO 55: PRINT " "; : NEXT III
     f2help$ = "#PATHS"
     GOSUB 73290
     SVPATH$ = LTRIM$(RTRIM$(SVPATH$))
     IF SVPATH$ = "" THEN SVPATH$ = CURDIR$
     IF RIGHT$(SVPATH$, 1) <> "\" THEN SVPATH$ = SVPATH$ + "\"
201  IF RIGHT$(SVPATH$, 2) = "\\" THEN
       SVPATH$ = LEFT$(SVPATH$, LEN(SVPATH$) - 1)
       GOTO 201
     END IF
     IF IRTRN% = 5 OR IRTRN% = 6 OR IRTRN% = 7 THEN
       GOSUB 6000
       IF IRTRN% = 6 OR IRTRN% = 5 THEN
          GOTO 7145
       ELSE
          GOTO 7130
       END IF
     END IF
     IF IRTRN% = 1 OR IRTRN% = 2 THEN
        IF ANS$ = "N" THEN
           GOSUB 2196
        ELSE
           GOSUB 6000
           IF FOUND = 3 THEN EXIT SUB
        END IF
     END IF
     IF IRTRN% = -1 THEN EXIT SUB
     IF IRTRN% = 1000 THEN GOSUB 2196
     IF IRTRN% = 23 THEN
       IF ERR <> 0 THEN GOTO 7140
       CALL GETDIR(EXT$, SELECT$, SVPATH$)
       IF IRTRN% = 1000 THEN EXIT SUB
       IF SELECT$ <> "" THEN D10SAVE$ = SELECT$: SELECT$ = ""
       LOCATE 7, 71: PRINT "        ": LOCATE 7, 71: PRINT D10SAVE$
       GOSUB 78165
     END IF
     GOTO 7140
7145 CALL ACELL(7, 71, 8, D10SAVE$, 1)
     LOCATE 13, 20: FOR III = 1 TO 55: PRINT " "; : NEXT III
     f2help$ = "#FILES"
     GOSUB 73290
     D10SAVE$ = LTRIM$(RTRIM$(D10SAVE$))
     IF IRTRN% = 6 OR IRTRN% = 7 OR IRTRN% = 5 THEN
         GOSUB 6000
         IF IRTRN% = 5 OR IRTRN% = 6 THEN GOTO 7130
         GOTO 7140
     END IF
     IF IRTRN% = 1 OR IRTRN% = 2 THEN
        IF ANS$ = "N" THEN
           GOSUB 2196
        ELSE
           GOSUB 6000
           'LOCATE 20, 1: PRINT FOUND
           IF FOUND = 3 THEN EXIT SUB
        END IF
     END IF
     IF IRTRN% = -1 THEN EXIT SUB
     IF IRTRN% = 1000 THEN GOSUB 2196
     IF IRTRN% = 23 THEN
        IF ERR <> 0 THEN GOTO 7145
        CALL GETDIR(EXT$, SELECT$, SVPATH$)
        IF IRTRN% = 1000 THEN EXIT SUB
        IF SELECT$ <> "" THEN D10SAVE$ = SELECT$: SELECT$ = ""
        LOCATE 7, 71: PRINT "        ": LOCATE 7, 71: PRINT D10SAVE$
        GOSUB 78165
      END IF
     GOTO 7145
EXIT SUB
'------------------------------------
'SUBROUTINE TO PRINT DIRECTORY LINE
'------------------------------------
78165  COLOR 11, 6
        LOCATE 24, 1: PRINT "                   F4 = List Files on Specified Directory                       ";
        'LOCATE 24, 15: PRINT DIRECTORY$;
        COLOR 15, 1
      RETURN
'-------------------------------------------------
'CHECK WHETHER DIRECTORIES, PATHS, AND FILES NAMES
'ARE VALID AND WHETHER SPECIFIED FILES EXIST.....
'ISSUE WARNINGS....
'-------------------------------------------------
6000  LOCATE 10, 26: 'PRINT "Verifying Directory, Path, and File Name..."
      LOCATE 9, 26: PRINT "                                               "
      LOCATE 10, 26: PRINT "                                               "
      IF ANS$ = "Y" THEN
           A1 = INSTR(SVPATH$, ":")
           IF A1 = 0 THEN
              FDRIVE$ = LEFT$(CURDIR$, 1)
              FPATH$ = SVPATH$
           ELSE
              FDRIVE$ = LEFT$(SVPATH$, A1 - 1)
              FPATH$ = RIGHT$(SVPATH$, LEN(SVPATH$) - A1)
           END IF
           IF FPATH$ = "\" THEN FPATH$ = ""
           FILEN$ = D10SAVE$
           FOUND = -1
           CALL filechk(FDRIVE$, FPATH$, FILEN$ + "." + EXT$, FOUND)
           IF FOUND = 3 THEN RETURN
           LOCATE 10, 26: PRINT "                                               "
           IF FOUND = 0 THEN
             LOCATE 10, 26: PRINT "Invalid Directory": SVPATH$ = CURDIR$
             LOCATE 7, 29: PRINT LEFT$(BLANKS$, 40)
             LOCATE 7, 29: PRINT SVPATH$
             RETURN
           END IF
           IF FOUND = 1 THEN
             LOCATE 10, 26: PRINT "Error in Path": SVPATH$ = CURDIR$
             LOCATE 7, 29: PRINT LEFT$(BLANKS$, 40)
             LOCATE 7, 29: PRINT SVPATH$
             RETURN
           END IF
           IF FOUND = 2 THEN
             LOCATE 10, 26: PRINT "Bad File Name"
             D10SAVE$ = "        "
             LOCATE 7, 71: PRINT D10SAVE$
             RETURN
           END IF
           IF FOUND = 5 THEN
             LOCATE 10, 26: PRINT "Invalid Drive": SVPATH$ = CURDIR$
             LOCATE 7, 29: PRINT LEFT$(BLANKS$, 40)
             LOCATE 7, 29: PRINT SVPATH$
             RETURN
           END IF
           IF FOUND = 4 THEN
              IF IRTRN% = 1 OR IRTRN% = 2 THEN
                LOCATE 9, 26
                PRINT SVPATH$ + " "; D10SAVE$ + "."; EXT$
                LOCATE 10, 26
                PRINT "File Already Exists.  Overwrite ? Y/N "
                ANSER$ = "Y"
6010            CALL ACELL(10, 65, 1, ANSER$, 1)
                IF UCASE$(ANSER$) <> "N" THEN ANSER$ = "Y"
                LOCATE 10, 65: PRINT UCASE$(ANSER$)
                IF IRTRN% = 5 THEN
                  IF ANSER$ = "Y" THEN
                    ANS$ = "Y"
                    FILESV$ = FDRIVE$ + ":" + FPATH$ + "\" + FILEN$ + "." + "D10"
                    FOUND = 3
                    RETURN
                  ELSE
                    ANS$ = "N"
                     RETURN
                  END IF
                END IF
                IF IRTRN% = 100 OR IRTRN% = 1000 THEN RETURN
                GOTO 6010
             ELSE
                LOCATE 10, 26
                PRINT "File Already Exists"
             END IF
             RETURN
         END IF
END IF
'ALL FILES SPECIFIED ARE OK.
RETURN
'------------------------------------
'SUBROUTINE TO ALLOW THE USER TO EXIT
'------------------------------------
2196 CALL EXOUT(ANSCODE)
     IF ANSCODE = 0 THEN
        GOTO 7101
     ELSE
        IRTRN% = 1000
        EXIT SUB
     END IF
RETURN
'-----------------------------------------
'SUBROUTINE TO CALL help MESSAGES FOR HELP
'-----------------------------------------
73290  IF IRTRN% = 22 THEN CALL USERHELP
       IF IRTRN% = 21 THEN CALL HLPMSG(f2help$)
       IF IRTRN% = 20 THEN CALL HLPMSG(f1help$)
       RETURN
END SUB

