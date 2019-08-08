DECLARE SUB EVAPIND (SELECCT$, SELECST$, fromsub!)
DECLARE SUB CLIMSAVE (FILEPATH$())
DECLARE SUB BOX ()
DECLARE SUB HLPMSG (F2HELP$)
DECLARE SUB USERHELP ()
DECLARE SUB CHECKET (errexist!)
DECLARE SUB ACELL (IROW!, ISTART!, NS!, TEXT$, ITYPE!)
DECLARE SUB RCELL (IROW!, ISTART!, NS!, TXT$)
DECLARE SUB EXOUT (ANSCODE!)
COMMON SHARED tempu$, precu$, solu$, UNITS$, UNIT$, UN1$, UN2$, UN3$, UN4$, UN5$
COMMON SHARED unev$, UNSL$, TOHELP%, NL%, CLIMATF$, IRTRN%, PAGE%, clim
COMMON SHARED APAGE%, VPAGE%, HPAGE%, direct$, SELECCT$, SELECST$
COMMON SHARED GROW1$, GROW2$, MAXLAI$, EVBARE$, EVFAIR$, EVEXCL$
COMMON SHARED evdepth$, LAI$, GR1$, GR2$, AREA$, NYEARS$, latitude$
COMMON SHARED HUMID1$, HUMID2$, HUMID3$, HUMID4$, WIND$, LATIT$, meansel
COMMON SHARED WSPEED$, BARE$, FAIR$, EXCL$, UNI$, ETCITY$, ETSTATE$, etunits$
COMMON SHARED defrain, deftemp, cmeth(), CUNIT(), ccity$(), cstate$(), ct$, st$, FILEPATH$()
COMMON SHARED SROWBOX, SCOLBOX, BOXWIDTH, EROWBOX, defp(), deft()

11000 SUB ACELL (IROW, ISTART, NS, TEXT$, ITYPE)
    SCREEN , , VPAGE%, VPAGE%
    BLANKS$ = "                                                                          "
    IRTRN% = 0
'''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Subroutine to allow input of filenames and paths    '
' into a reverse video cell of prescribed dimensions. '
'''''''''''''''''''''''''''''''''''''''''''''''''''''''
'  *************************************************************************
'  * NS     = Cell width                                                   *
'  * ITYPE  = Flag for type of variable                                    *
'  *      0 = Numeric (But accepts E - for use in conductivity)            *
'  *      1 = Alphanumeric                                                 *
'  *      2 = Positive numeric only                                        *
'  *      3 = ALPHANUMERIC BUT ONLY FOR YES OR NO ANSWERS                  *
'  * IPOS   = Cursor location.  IPOS î [1,NS]                              *
'  * NLEN   = Length of variable (used to place a decimal point if needed) *
'  *************************************************************************
'
    PERIOD$ = "."
    IF ITYPE = 1 OR ITYPE = 3 THEN GOTO 11001
11001 TXT$ = LEFT$(TEXT$, NS)
      TXT$ = TXT$ + SPACE$(NS - LEN(TXT$))
'
'  Locate cursor at first position of cell and write reverse video cell
'
      COLOR 1, 15
      LOCATE IROW, ISTART
      PRINT TXT$;
      ITAB = ISTART - 1
      icol = ISTART
      IPOS = 1
'
'  Locate the cursor at the current position and capture a new key
'
11010 LOCATE IROW, icol, 1
11015 KSTRG$ = ""
      KSTRG$ = INKEY$: IF KSTRG$ = "" GOTO 11015
      NL = ASC(MID$(KSTRG$, 1))
      IF LEN(KSTRG$) = 2 THEN NH = ASC(MID$(KSTRG$, 2))
      IF ITYPE <> 1 AND ITYPE <> 3 THEN
        IF icol <> ISTART THEN
          IF NL = 46 AND INSTR(TXT$, PERIOD$) THEN GOTO 11010
        END IF
      END IF
'
'Alphanumeric input
'
      IF ITYPE = 1 OR ITYPE = 3 THEN GOTO 11017
' if the number should be positive (i.e. ITYPE=2), no negative sign
' is allowed but if the number may be negative (ITYPE=0) then the
' negative sign is allowed only as the first character in the cell
   'IF ITYPE = 2 THEN
   '     IF NL = 45 GOTO 11010
   '   ELSEIF NL = 45 AND IPOS <> 1 THEN GOTO 11010
   '   END IF
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'                               ' VALID KEYS '
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' PgDn                    (IRTRN%=1)     PgUp               (IRTRN%=-1)
' UP ARROW                (IRTRN%=3)     DOWN ARROW         (IRTRN%=4)
' CARRAIGE RETURN         (IRTRN%=5)     TAB (MOVE RIGHT)   (IRTRN%=6)
' SHIFT TAB (MOVE LEFT)   (IRTRN%=7)     HOME               (IRTRN%=8)
' END                     (IRTRN%=9)     Alt A insert above (IRTRN%=30)
' Alt B insert below      (IRTRN%=31)    Alt C copy         (IRTRN%=32)
' Alt M move              (IRTRN%=33)    Alt D delete       (IRTRN%=13)
' F1 INFORMATION          (IRTRN%=20)    F2 HELP            (IRTRN%=21)
' F3 KEYS                 (IRTRN%=22)    F4 LIST OF FILES   (IRTRN%=23)
' F5 LOCATION             (IRTRN%=40)    F6 DEFAULT SOIL TEXTURES  (IRTRN%=41)
' F7 USER SOIL TEXTURES   (IRTRN%=42)    F9 QUIT            (IRTRN%=1000)
' F10                     (IRTRN%=2)     ESC                (IRTRN%=100)
'"Y" OR "y"               (IRTRN%=51)    "N" OR "n"         (IRTRN%=50)
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
11017 IF NL = 0 AND NH = 81 THEN IRTRN% = 1
   IF NL = 0 AND NH = 73 THEN IRTRN% = -1
   IF NL = 0 AND NH = 68 THEN IRTRN% = 2
   IF NL = 0 AND NH = 72 THEN IRTRN% = 3
   IF NL = 0 AND NH = 80 THEN IRTRN% = 4
   IF NL = 13 THEN IRTRN% = 5
   IF NL = 9 THEN IRTRN% = 6
   IF NL = 0 AND NH = 15 THEN IRTRN% = 7
   IF NL = 0 AND NH = 71 THEN IRTRN% = 8:
   IF NL = 0 AND NH = 79 THEN IRTRN% = 9:
   IF NL = 0 AND NH = 59 THEN IRTRN% = 20  ': COLOR 15, 0: GOSUB 11022: EXIT SUB
   IF NL = 0 AND NH = 60 THEN IRTRN% = 21  ': COLOR 15, 0: GOSUB 11022: EXIT SUB
   IF NL = 0 AND NH = 61 THEN IRTRN% = 22  ': COLOR 15, 1: LOCATE IROW, ISTART, 0: GOSUB 11022: PRINT TXT$; : EXIT SUB
   IF NL = 0 AND NH = 62 THEN IRTRN% = 23  ': COLOR 15, 1: LOCATE IROW, ISTART, 0: GOSUB 11022: PRINT TXT$; : EXIT SUB
   IF NL = 0 AND NH = 67 THEN IRTRN% = 1000 ': COLOR 15, 1: GOSUB 11022: EXIT SUB
   IF NL = 0 AND NH = 30 THEN IRTRN% = 30
   IF NL = 0 AND NH = 48 THEN IRTRN% = 31
   IF NL = 0 AND NH = 46 THEN IRTRN% = 32
   IF NL = 0 AND NH = 50 THEN IRTRN% = 33
   IF NL = 0 AND NH = 32 THEN IRTRN% = 13: COLOR 15, 1: GOSUB 11022: EXIT SUB
   IF NL = 27 THEN IRTRN% = 100
   IF ITYPE = 3 THEN
      IF NL = 78 OR NL = 110 THEN IRTRN% = 50: LOCATE IROW, ISTART, 0: COLOR 15, 1: GOSUB 11022: PRINT TXT$; : EXIT SUB
      IF NL = 89 OR NL = 121 THEN IRTRN% = 51: LOCATE IROW, ISTART, 0: COLOR 15, 1: GOSUB 11022: PRINT TXT$; : EXIT SUB
   END IF
   IF NL = 0 AND NH = 63 THEN IRTRN% = 40: COLOR 15, 1: LOCATE IROW, ISTART, 0: GOSUB 11022: PRINT TXT$; : EXIT SUB
   IF NL = 0 AND NH = 64 THEN IRTRN% = 41: COLOR 15, 1: LOCATE IROW, ISTART, 0: GOSUB 11022: PRINT TXT$; : EXIT SUB
   IF NL = 0 AND NH = 65 THEN IRTRN% = 42: COLOR 15, 1: LOCATE IROW, ISTART, 0: GOSUB 11022: PRINT TXT$; : EXIT SUB
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'  Alphanumeric and numeric keys are activated in the appropriate   '
'  location.  No special characters are allowed.                    '
'  and record in TXT$                                               '
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
 IF ((ITYPE = 0) AND (NL > 47 AND NL < 58) OR (NL = 46) OR (NL = 45)) OR ((ITYPE = 1 OR ITYPE = 3) AND (NL > 31 AND NL < 126) OR ((ITYPE = 2) AND ((NL > 47 AND NL < 58) OR (NL = 46) OR (NL = 69) OR (NL = 101) OR (NL = 45)))) THEN
   NL = ASC(LEFT$(KSTRG$, 1))
   PRINT CHR$(NL);
   MID$(TXT$, IPOS) = CHR$(NL)
'
'erase entire cell if typing in first position
'
   IF IPOS = 1 THEN
      FOR I = 2 TO LEN(TXT$)
         MID$(TXT$, I) = " ": LOCATE IROW, ISTART + I - 1: PRINT " ";
      NEXT I
   END IF
   IF (IPOS < NS) THEN
     IPOS = IPOS + 1
     icol = icol + 1
   END IF
'
' backspace - make destructive (unless cursor is in 1st position)
' and shift rest of string left one character, add space on right
'
       ELSEIF (NL = 8) THEN
       IF (IPOS = 1) THEN
          BEEP
       ELSE
          IPOS = IPOS - 1
          icol = icol - 1
          FOR I = IPOS TO NS - 1
             LOCATE IROW, ITAB + I
             PRINT MID$(TXT$, I + 1, 1);
             MID$(TXT$, I) = MID$(TXT$, I + 1, 1)
          NEXT I
          LOCATE IROW, ITAB + NS
          PRINT " ";
          MID$(TXT$, NS) = " "
       END IF
'
'  ... SPACE BAR - blanks character and shifts rest of string right one
'  ... character
'
     ELSEIF (NL = 32) AND (ITYPE = 1 OR ITYPE = 3) THEN
      IF (IPOS = NS) THEN
'        beep
        ELSE
        IPOS = IPOS + 1
        icol = icol + 1
        FOR I = NS TO IPOS STEP -1
         LOCATE IROW, ITAB + I
           PRINT MID$(TXT$, I - 1, 1);
           MID$(TXT$, I) = MID$(TXT$, I - 1, 1)
         NEXT I
         LOCATE IROW, icol - 1
         PRINT " ";
         MID$(TXT$, IPOS - 1) = " "
      END IF
'
'  ... Delete - remove character at current cursor location and
'  ... shift rest of string left one character
'
       ELSEIF (NL = 0 AND NH = 83) THEN
          FOR I = IPOS TO NS - 1
            LOCATE IROW, ITAB + I
            PRINT MID$(TXT$, I + 1, 1);
            MID$(TXT$, I) = MID$(TXT$, I + 1, 1)
          NEXT I
         LOCATE IROW, ITAB + NS
         PRINT " ";
         MID$(TXT$, NS) = " "
'
'  ... Insert - add a space at current cursor location and shift
'  ... rest of string to the right
'
       ELSEIF (NL = 0 AND NH = 82) THEN
         FOR I = NS TO IPOS + 1 STEP -1
            LOCATE IROW, ITAB + I
            PRINT MID$(TXT$, I - 1, 1);
            MID$(TXT$, I) = MID$(TXT$, I - 1, 1)
          NEXT I
          LOCATE IROW, icol
          PRINT " ";
          MID$(TXT$, IPOS) = " "
'
'  ... RIGHT ARROW - move cursor but don't change characters - wrap
'  ... around to other end if needed
'
        ELSEIF (NL = 0 AND NH = 77) THEN
        IF (IPOS = NS) THEN
           IPOS = 1
           icol = ISTART
        ELSE
           IPOS = IPOS + 1
           icol = icol + 1
        END IF
'
' ... LEFT ARROW - move cursor but don't change characters - wrap
' ... around to other end if needed
'
        ELSEIF (NL = 0 AND NH = 75) THEN
        IF (IPOS = 1) THEN
           IPOS = NS
           icol = ITAB + NS
        ELSE
           IPOS = IPOS - 1
           icol = icol - 1
        END IF
'
' ... RETURN - end editing, replace cell with normal video,
' ... and return with new TEXT
'
      ELSEIF (IRTRN% < 10 AND IRTRN% > 0) OR (IRTRN% > 19 AND IRTRN% < 34) OR (IRTRN% = -1) OR IRTRN% = 100 OR IRTRN% = 1000 THEN
11021 COLOR 15, 1
      TEXT$ = LEFT$(TXT$, NS)
      LOCATE IROW, ISTART, 0
      GOSUB 11022
      TEXT$ = TXT$
      PRINT TXT$;
      EXIT SUB
    END IF
'Go back, locate cursor in new location, and capture another key
       GOTO 11010
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'subroutine to remove spaces (IF THEY EXIST) from numbers
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
11022  LEFTJUSTIFY$ = SPACE$(NS)
       TXT$ = LTRIM$(TXT$)
       LSET LEFTJUSTIFY$ = TXT$
       TXT$ = LEFT$(TXT$, NS)
       LOCATE IROW, ISTART, 0
       PRINT LEFT$(BLANKS$, NS);
       LOCATE IROW, ISTART, 0
       RETURN
END SUB

SUB CHECKET (errexist)
DIM ERRMSG$(13), ETERR(20)
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO CHECK EVAPOTRANSPIRATION DATA                    '
'THIS ROUTINE CHECKS THE VALUE ENTERED FOR EACH ET DATA         '
'                                                               '
'   ERRNUM  = ERROR NUMBER... LAST VALUE INDICATES              '
'             TOTAL NUMBER OF ERRORS                            '
'   ETERR() = ERROR NUMBER                                      '
'  ERRMSG() = ERROR MESSAGE TO BE PRINTED                       '
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
errnum = 0
ERRMSG$(1) = "WARNING... CITY NOT DEFINED."
ERRMSG$(2) = "WARNING... STATE NOT DEFINED."
ERRMSG$(3) = "LOCATION LATITUDE MUST BE SPECIFIED."
ERRMSG$(4) = "EVAPORATIVE ZONE DEPTH MUST BE SPECIFIED."
ERRMSG$(5) = "LEAF AREA INDEX MUST BE SPECIFIED."
ERRMSG$(6) = "START DAY OF GROWING SEASON MUST BE SPECIFIED."
ERRMSG$(7) = "END DAY OF GROWING SEASON MUST BE SPECIFIED."
ERRMSG$(8) = "AVERAGE WIND SPEED MUST BE SPECIFIED."
ERRMSG$(9) = "FIRST QUARTER RELATIVE HUMIDITY MUST BE SPECIFIED."
ERRMSG$(10) = "SECOND QUARTER RELATIVE HUMIDITY MUST BE SPECIFIED."
ERRMSG$(11) = "THIRD QUARTER RELATIVE HUMIDITY MUST BE SPECIFIED."
ERRMSG$(12) = "FOURTH QUARTER RELATIVE HUMIDITY MUST BE SPECIFIED."
ERRMSG$(13) = "EVAPORATIVE ZONE DEPTH MUST BE GREATER THAN ZERO (PREFERABLY, > 1)."
IF LTRIM$(RTRIM$(ETCITY$)) = "" THEN errnum = errnum + 1:  ETERR(errnum) = 1
IF LTRIM$(RTRIM$(ETSTATE$)) = "" THEN errnum = errnum + 1: ETERR(errnum) = 2
IF LTRIM$(RTRIM$(LATIT$)) = "" THEN errnum = errnum + 1:   ETERR(errnum) = 3
IF LTRIM$(RTRIM$(evdepth$)) = "" THEN errnum = errnum + 1: ETERR(errnum) = 4
IF VAL(LTRIM$(RTRIM$(evdepth$))) <= 0! THEN errnum = errnum + 1: ETERR(errnum) = 13
IF LTRIM$(RTRIM$(LAI$)) = "" THEN errnum = errnum + 1:     ETERR(errnum) = 5
IF LTRIM$(RTRIM$(GR1$)) = "" THEN errnum = errnum + 1:     ETERR(errnum) = 6
IF LTRIM$(RTRIM$(GR2$)) = "" THEN errnum = errnum + 1:     ETERR(errnum) = 7
IF LTRIM$(RTRIM$(WIND$)) = "" THEN errnum = errnum + 1:    ETERR(errnum) = 8
IF LTRIM$(RTRIM$(HUMID1$)) = "" THEN errnum = errnum + 1:  ETERR(errnum) = 9
IF LTRIM$(RTRIM$(HUMID2$)) = "" THEN errnum = errnum + 1:  ETERR(errnum) = 10
IF LTRIM$(RTRIM$(HUMID3$)) = "" THEN errnum = errnum + 1:  ETERR(errnum) = 11
IF LTRIM$(RTRIM$(HUMID4$)) = "" THEN errnum = errnum + 1:  ETERR(errnum) = 12
''''''''''''''
'DRAW A BOX
''''''''''''''
  IF errnum <> 0 THEN
    errexist = 1
    CLS
    COLOR 3, 4
    LOCATE 1, 1: PRINT STRING$(80, 219);
    FOR I% = 2 TO 24
       LOCATE I%, 1: PRINT CHR$(219);
       LOCATE I%, 80: PRINT CHR$(219);
    NEXT
    COLOR 4, 3
    LOCATE 1, 30: PRINT " EVAPOTRANSPIRATION DATA"
    LOCATE 25, 1: PRINT "      F9 = Quit         F10, PgDn or Enter = Proceed          Esc = Return      ";
    COLOR 15, 1
    JJ = 0
    FOR J = 1 TO errnum
       JJ = JJ + 1
       LOCATE J + 4, 3: PRINT USING "##"; JJ; : PRINT ". "; : PRINT ERRMSG$(ETERR(J));
    NEXT J
91860  KSTRG$ = "": KSTRG$ = INKEY$: IF KSTRG$ = "" THEN GOTO 91860
       NL = ASC(MID$(KSTRG$, 1))
       IF LEN(KSTRG$) = 2 THEN NH = ASC(MID$(KSTRG$, 2))
       IF (NL = 0 AND NH = 81) OR (NL = 0 AND NH = 68) OR ASC(KSTRG$) = 13 THEN EXIT SUB
       IF NL = 27 THEN IRTRN% = 100: EXIT SUB
       IF (NL = 0 AND NH = 73) THEN IRTRN% = 100: EXIT SUB
       IF (NL = 0 AND NH = 67) THEN IRTRN% = 1000: EXIT SUB
       GOTO 91860
   END IF
EXIT SUB
''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO CALL MANUAL MESSAGES FOR HELP
'''''''''''''''''''''''''''''''''''''''''''''''''
97190  IF IRTRN% = 21 THEN CALL HLPMSG(F2HELP$)
       IF IRTRN% = 20 THEN CALL USERHELP
      RETURN
END SUB

SUB GETDIR (EXT$, SELECT$, DIRECTRY$)
     DIM F$(120), ICLN%(6), BEG%(120), LENG%(120), TMP$(120)
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' This subroutine gets and displays the directory of files   '
' *.D4, *.D7, or *.D13                                       '
' and allows the user to                                     '
'     Select a file                                          '
'     change directory/path                                  '
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
     director$ = LTRIM$(RTRIM$(DIRECTRY$))
     IF RIGHT$(director$, 1) <> "\" THEN director$ = director$ + "\"
     PCOPY VPAGE%, HPAGE%
     IRTRN% = 0
     PCOPY VPAGE%, APAGE%
     CLS APAGE%
8200 SCREEN 0, , VPAGE%, VPAGE%
     COLOR 4, 3
     LOCATE 25, 1: PRINT "   To Select: Cursor      Then Enter                          Esc = RETURN      ";
     LOCATE 25, 22: PRINT CHR$(27); CHR$(24); CHR$(25); CHR$(26);
     COLOR 15, 1
     GOSUB 99111
     LOCATE 1, 1:  PRINT "                            *.    DATA FILES                                    "
     LOCATE 1, 31: PRINT USING "\ \"; EXT$;
     COLOR 15, 1
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Get data from current directory and display it in the form '
' of columns.  Each column will have NUMINCOL files          '
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
8103 A$ = DIR$(director$ + "*." + EXT$)
     IF LEN(A$) = 0 THEN
       LOCATE 10, 15: PRINT "           DATA FILES DO NOT EXIST"
     'LOCATE 12, 15: PRINT "            Press Esc to continue"
8105   INK$ = INKEY$: IF INK$ = "" THEN GOTO 8105
       NL = ASC(MID$(INK$, 1))
       IF NL = 27 THEN GOTO 19197
       IF LEN(INK$) = 2 THEN NH = ASC(MID$(INK$, 2))
       GOTO 8105
     ELSE
         F$(1) = A$
         LENGTH = LEN(F$(1))
         F$(1) = LEFT$(F$(1), LENGTH - LEN(EXT$) - 1)
         FILECOUNT = 1
         FOR I = 2 TO 120
           F$(I) = DIR$
           IF LEN(F$(I)) = 0 THEN GOTO 8400
           LENGTH = LEN(F$(I))
           F$(I) = LEFT$(F$(I), LENGTH - LEN(EXT$) - 1)
           FILECOUNT = FILECOUNT + 1
         NEXT I
       END IF
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' This protion of the routine lists all available data files '
' and allows the user select a file to retrun to the         '
' calling routine, delete a file, change directory or drive  '
' K = First row number for listing tabulated values          '
' NUMINCOL= Numner of values per column                      '
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
8400    FOR J = 1 TO FILECOUNT
           FOR I = 1 TO FILECOUNT - 1
              IF F$(I) > F$(I + 1) THEN
                 TMP$(I) = F$(I)
                 F$(I) = F$(I + 1)
                 F$(I + 1) = TMP$(I)
              END IF
           NEXT I
        NEXT J
        FIRSTROW = 2
        NUMINCOL = 20
        TOTNUM = FILECOUNT
        IROW = 0: icol = 0
          k = FIRSTROW
          FOR I = 1 TO NUMINCOL
            k = k + 1
            BEG%(I) = k: LENG%(I) = LEN(F$(I))
            LOCATE k, 6: PRINT F$(I)
         NEXT I
         k = FIRSTROW
         FOR I = NUMINCOL + 1 TO NUMINCOL * 2
            k = k + 1
            BEG%(I) = k: LENG%(I) = LEN(F$(I))
            LOCATE k, 18: PRINT F$(I)
         NEXT I
         k = FIRSTROW
         FOR I = NUMINCOL * 2 + 1 TO NUMINCOL * 3
            k = k + 1
            BEG%(I) = k: LENG%(I) = LEN(F$(I))
            LOCATE k, 30: PRINT F$(I)
         NEXT I
         k = FIRSTROW
         FOR I = NUMINCOL * 3 + 1 TO NUMINCOL * 4
            k = k + 1
            BEG%(I) = k: LENG%(I) = LEN(F$(I))
            LOCATE k, 42: PRINT F$(I)
         NEXT I
         k = FIRSTROW
         FOR I = NUMINCOL * 4 + 1 TO NUMINCOL * 5
            k = k + 1
            BEG%(I) = k: LENG%(I) = LEN(F$(I))
            LOCATE k, 54: PRINT F$(I)
         NEXT I
         k = FIRSTROW
         FOR I = NUMINCOL * 5 + 1 TO NUMINCOL * 6
            k = k + 1
            BEG%(I) = k: LENG%(I) = LEN(F$(I))
            LOCATE k, 66: PRINT F$(I)
         NEXT I
         ICLN%(1) = 6: ICLN%(2) = 18: ICLN%(3) = 30: ICLN%(4) = 42
         ICLN%(5) = 54: ICLN%(6) = 66
19099    I = 1: IROW = FIRSTROW + 1: KK = 1
19901    k = IROW - FIRSTROW + NUMINCOL * (I - 1)
         icol = ICLN%(I)
         IF k > TOTNUM THEN
           k = KK
           IROW = OLDROW
           icol = OLDCOL
         ELSE
           KK = k
           OLDROW = IROW
           OLDCOL = icol
         END IF
         xleng = LENG%(k)

19902   CALL RCELL(IROW, icol, xleng, F$(k))

'ESC
         IF IRTRN% = 100 THEN GOTO 19197
'MOVE UP
         IF IRTRN% = 3 THEN
         IF (IRTRN% = 3) AND (IROW = FIRSTROW + 1) THEN
           IF FIX(TOTNUM / NUMINCOL) * NUMINCOL >= I * NUMINCOL THEN
              IROW = NUMINCOL + FIRSTROW
              GOTO 19901
           ELSE
              IROW = FIRSTROW + TOTNUM - FIX(TOTNUM / NUMINCOL) * NUMINCOL
              GOTO 19901
           END IF
         END IF
         IF (IRTRN% = 3) AND (IROW > 3) THEN IROW = IROW - 1
         IF (IRTRN% = 4) AND (IROW = NUMINCOL + 2) THEN
           IROW = FIRSTROW + 1
           GOTO 19901
         END IF
         GOTO 19901
         END IF
'MOVE DOWN THE COLUMN
       IF (IRTRN% = 4) AND (IROW <= NUMINCOL + FIRSTROW) THEN
          IF FIX(TOTNUM / NUMINCOL) * NUMINCOL < I * NUMINCOL THEN
            IF IROW + 1 <= FIRSTROW + TOTNUM - FIX(TOTNUM / NUMINCOL) * NUMINCOL THEN
              IROW = IROW + 1
           ELSE
              IROW = FIRSTROW + 1
           END IF
         ELSE
           IF IROW < FIRSTROW + NUMINCOL THEN
              IROW = IROW + 1
           ELSE
              IROW = FIRSTROW + 1
           END IF
         END IF
         GOTO 19901
       END IF
'MOVE RIGHT
        IF IRTRN% = 6 THEN
        IF (IRTRN% = 6) AND (I = 6) THEN
          I = 1
          GOTO 19901
      END IF
      IF (IRTRN% = 6) AND (I < 6) THEN
         IF k + NUMINCOL > TOTNUM THEN
             I = 1
         ELSE
             I = I + 1
         END IF
      END IF
      GOTO 19901
      END IF
'MOVE LEFT
      IF (IRTRN% = 7) AND (I = 1) THEN
          IF IROW - FIRSTROW + NUMINCOL <= TOTNUM THEN I = 2
          IF IROW - FIRSTROW + NUMINCOL * 2 <= TOTNUM THEN I = 3
          IF IROW - FIRSTROW + NUMINCOL * 3 <= TOTNUM THEN I = 4
          IF IROW - FIRSTROW + NUMINCOL * 4 <= TOTNUM THEN I = 5
          IF IROW - FIRSTROW + NUMINCOL * 5 <= TOTNUM THEN I = 6
          GOTO 19901
      END IF
      IF (IRTRN% = 7) AND (I > 1) THEN I = I - 1: GOTO 19901
'SELECT HIGHLIGHTED ITEM
      IF IRTRN% = 5 THEN
          SELECT$ = F$(k)
          ITEMNUM = k
          GOTO 19197
      END IF
      GOTO 19902
19197 CLS : PCOPY HPAGE%, VPAGE%
  EXIT SUB
'''''''''''''''''''
'PRINT DIRECTORY
'''''''''''''''''''
99111   COLOR 11, 6
        LOCATE 24, 1: PRINT "   Directory:                                                                   ";
        LOCATE 24, 15: PRINT director$;
        RETURN
        EXIT SUB
END SUB

SUB LASTPAGE
DIM SVHEAD$(2), CHKOK%(2)
'----------------------------------------------------
'THIS ROUTINE CHECKS THE EVAPOTRANSPIRATION DATA FOR
'CORRECTNESS AND COMPLETENESS
'THE PROGRAM DISPLAYS ERROR MESSAGES
'  ETERR() = DESIGN ERROR NUMBER
'ERRMSG$() = ERROR MESSAGE TO BE DISPLAYED
'----------------------------------------------------
ON ERROR RESUME NEXT
FOR I = 1 TO 2: CHKOK%(I) = 2: NEXT I
95100 COLOR 15, 1
   CLS
   F1HELP$ = "#D11VERIFY"
   COLOR 15, 1
        errexist = 0: CHKOK%(1) = 0
        CALL CHECKET(errexist)
        IF errexist = 1 THEN CHKOK%(1) = 1
        IF IRTRN% = 100 THEN : IRTRN% = 1: EXIT SUB
        IF IRTRN% = 1000 THEN GOTO 5500
        A4$ = DIR$("DATA4.TMP")
        A7$ = DIR$("DATA7.TMP")
        A13$ = DIR$("DATA13.TMP")
        IF A4$ = "" OR A7$ = "" OR A13$ = "" THEN
           CLS
           COLOR 3, 4
           LOCATE 1, 1: PRINT STRING$(80, 219);
           FOR I% = 2 TO 24
              LOCATE I%, 1: PRINT CHR$(219);
              LOCATE I%, 80: PRINT CHR$(219);
           NEXT
           COLOR 4, 3
           LOCATE 1, 32: PRINT " WEATHER DATA FILES"
           LOCATE 25, 1: PRINT "      F9 = Quit         F10, PgDn or Enter = Proceed          Esc = Return      ";
           COLOR 15, 1
           LIN = 5
           IF A4$ = "" THEN
              LOCATE LIN, 5: PRINT "PRECIPITATION DATA IS NOT AVAILABLE.";
              LIN = LIN + 1
           END IF
           IF A7$ = "" THEN
              LOCATE LIN, 5: PRINT "TEMPERATURE DATA IS NOT AVAILABLE.";
              LIN = LIN + 1
           END IF
           IF A13$ = "" THEN
              LOCATE LIN, 5: PRINT "SOLAR RADIATION DATA IS NOT AVAILABLE.";
           END IF
71860      KSTRG$ = "": KSTRG$ = INKEY$: IF KSTRG$ = "" THEN GOTO 71860
           NL = ASC(MID$(KSTRG$, 1))
           IF LEN(KSTRG$) = 2 THEN NH = ASC(MID$(KSTRG$, 2))
           IF (NL = 0 AND NH = 81) OR (NL = 0 AND NH = 68) OR ASC(KSTRG$) = 13 THEN GOTO 72860
           IF (NL = 0 AND NH = 73) THEN GOTO 95100
           IF NL = 27 THEN IRTRN% = 100: GOTO 95100 'EXIT SUB
           IF (NL = 0 AND NH = 67) THEN GOSUB 5500
           GOTO 71860
        END IF
72860 CALL CLIMSAVE(FILEPATH$())
EXIT SUB
'----------------------------------
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
'----------------------------------
5500 CALL EXOUT(ANSCODE)
     IF ANSCODE = 0 THEN
       GOTO 95100
     ELSE
       EXIT SUB
     END IF
RETURN
'----------------------------------------------
'SUBROUTINE TO CALL help MESSAGES FOR HELP
'----------------------------------------------
73190  IF IRTRN% = 20 THEN CALL HLPMSG(F1HELP$)
       IF IRTRN% = 21 THEN CALL HLPMSG(F2HELP$)
       IF IRTRN% = 22 THEN CALL USERHELP
      RETURN
END SUB

SUB meantemp (cit$, sta$, unt)
DIM SYNTH$(12), ave(12, 100), month(12), sum(12, 100), va$(31), NMMT$(12)
'This subroutine is used to enter normal monthly mean                  '
'temperatures.  The routine reads in normal mean monthly               '
'temps. (unmmt) if available in the data7.tmp file, averages           '
'the data (nmmt) and displays values. The "unmmt" values               '
'displayed under the "user" column, and the "nmmt" values              '
'are displayed under the "Data Averaged" column.                       '
'The user may select normal mean monthly temperatures from              '
'the available list of cities (from tape2) THEN                         '
'values for the selected city ARE DIDPLAYED in the "user" column        '
'  UNMMT   = User defined normal mean monthly temperatures              '
'  NMMT    = Data averaged monthly temperatures                         '
'  MNTEMP  = contains the 12 values to be saved.  May be either         '
'          from tape2, user defined, or data averaged.                  '
'  meansel --- is used as a flag to declare whether a selection         '
'          has been made.  The usefulness of this flag is to            '
'          determine what values to show under the user column.         '
'          = 0    no mean value was selected                            '
'          = 1    mean values were selected                             '
'FHELP = 0, 1 IS FLAG FOR HELP: = 1 IF RETURNING FROM HELP              '
'IRET = 1,2,3 IS A FLAG FOR THE LOCATION FROM WHICH F1 OR F2 ARE PRESSED'
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  IF DIR$("data7.tmp") = "" THEN EXIT SUB
  FHELP = 0
''ADD F1 AND F2 HELP MESSAGE (F1 AND F2 ARE THE SAME HERE
  YES$ = "YES": NO$ = "NO"
  COLOR 15, 1
  CLS
  LOCATE 12, 12: PRINT "Computing Normal Mean Monthly Temperatures... Please Wait."
  GOSUB 23095
70002  CLS
  COLOR 11, 6
  LOCATE 24, 1: PRINT "       F5 = Select City for Default Normal Mean Monthly Temperatures            ";
  COLOR 4, 3
  LOCATE 25, 1: PRINT "     F1=Info     F2=Help     F9=Quit    F10=End    PgDn=End     Esc=Cancel      ";
  COLOR 11, 6
  LOCATE 1, 1:  PRINT "                         NORMAL MEAN MONTHLY TEMPERATURES                       "
  COLOR 15, 1
    IF ct$ = "" THEN
      ct$ = cit$: st$ = sta$
    END IF
7333 LOCATE 3, 30:  PRINT "  City:": LOCATE 3, 38: PRINT USING "\                    \"; cit$
     LOCATE 4, 30:  PRINT " State:"; : LOCATE 4, 38: PRINT USING "\                    \"; sta$
     LOCATE 5, 14: PRINT "       Use Data Averaged Monthly Temperatures ?  NO   YES  "
     LOCATE 7, 14: PRINT "       Normal Mean Monthly "
     IF DIR$("data7.tmp") <> "" THEN
        CLOSE
        OPEN "data7.tmp" FOR INPUT AS #1
        LINE INPUT #1, L$: OLDMETH$ = L$
        LINE INPUT #1, L$
        unt = VAL(L$)
        IF unt <> 2 THEN
           LOCATE 7, 41: PRINT "Temperatures (øF)"
        ELSE
           unt = 1
           LOCATE 7, 41: PRINT "Temperatures (øC)"
        END IF
        CLOSE
     ELSE
        EXIT SUB
     END IF
      LOCATE 8, 40: PRINT "User    Data Averaged"
      LOCATE 10, 18: PRINT "    1.  January                                    "
      LOCATE 11, 18: PRINT "    2.  February                                   "
      LOCATE 12, 18: PRINT "    3.  March                                      "
      LOCATE 13, 18: PRINT "    4.  April                                      "
      LOCATE 14, 18: PRINT "    5.  May                                        "
      LOCATE 15, 18: PRINT "    6.  June                                       "
      LOCATE 16, 18: PRINT "    7.  July                                       "
      LOCATE 17, 18: PRINT "    8.  August                                     "
      LOCATE 18, 18: PRINT "    9.  September                                  "
      LOCATE 19, 18: PRINT "   10.  October                                    "
      LOCATE 20, 18: PRINT "   11.  November                                   "
      LOCATE 21, 18: PRINT "   12.  December                                   "
7335 FOR k% = 1 TO 12
       LOCATE k% + 9, 40: PRINT USING "\    \"; LTRIM$(RTRIM$(SYNTH$(k%)))
       LOCATE k% + 9, 50: PRINT USING "####.#"; VAL(NMMT$(k%))
     NEXT k%
     COLOR 1, 2
     IF IRTET = 2 THEN IRET = 0: GOTO 70360
     IF IRET = 3 THEN IRET = 0: GOTO 20390
     IRET = 0
70334  CALL RCELL(5, 63, 3, NO$)
     IF IRTRN% = 1000 THEN IRTRN% = 100
     IF IRTRN% = 100 THEN GOSUB 20135
     IF IRTRN% = 20 OR IRTRN% = 21 THEN GOSUB 7000: FHELP = 1: IRET = 1: GOTO 70002
     IF IRTRN% = 40 THEN GOSUB 1091
     IF IRTRN% = 5 THEN GOSUB 70370
     IF IRTRN% = 2 OR IRTRN% = 1 THEN
        FOR I = 1 TO 12
          IF VAL(SYNTH$(I)) <> 0 THEN GOSUB 33095: GOTO 20850
        NEXT I
      IRTRN% = 5
      GOTO 70361
     END IF
     IF IRTRN% = 6 OR IRTRN% = 7 THEN GOTO 70360
     GOTO 70334
70360  CALL RCELL(5, 68, 2, YES$)
     IF IRTRN% = 20 OR IRTRN% = 21 THEN GOSUB 7000: FHELP = 1: IRET = 2: GOTO 70002
     IF IRTRN% = 40 THEN GOSUB 1091
     IF IRTRN% = 1000 THEN IRTRN% = 100
     IF IRTRN% = 100 THEN GOTO 20135
     IF IRTRN% = 6 OR IRTRN% = 7 THEN GOTO 70334
70361 IF IRTRN% = 5 OR IRTRN% = 2 OR IRTRN% = 1 THEN
        FOR I = 1 TO 12
         SYNTH$(I) = NMMT$(I)
       NEXT I
       GOSUB 33095
       GOTO 20850
     END IF
     GOTO 70360
70370  MO = 10: CO = 40
20390 I% = MO - 9
     CALL ACELL(MO, CO, 6, SYNTH$(I%), 2)
     IF IRTRN% = 20 OR IRTRN% = 21 THEN GOSUB 7000: FHELP = 1: IRET = 3: GOTO 70002
     IF IRTRN% = 40 THEN GOSUB 1091
     IF IRTRN% = 1000 THEN IRTRN% = 100: GOTO 20135
     IF IRTRN% = 2 OR IRTRN% = 1 THEN
        GOSUB 33095: GOTO 20850
     END IF
     IF IRTRN% = 100 THEN GOTO 70334
'MOVE DOWN
     IF IRTRN% = 4 OR IRTRN% = 5 THEN
        IF MO < 21 THEN
          MO = MO + 1
          GOTO 20390
        ELSE
          MO = 10
        END IF
     END IF
'MOVE UP
  IF IRTRN% = 3 THEN
      IF MO > 10 THEN
        MO = MO - 1
      ELSE
        MO = 21
      END IF
      GOTO 20390
  END IF
'MOVE HOME
      IF IRTRN% = 8 THEN
        MO = 10
        GOTO 20390
      END IF
'MOVE END
      IF IRTRN% = 9 THEN
        MO = 21
      END IF
      GOTO 20390
20850  CLS : PCOPY HPAGE%, VPAGE%
      EXIT SUB
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'subroutine to select city and state then read normal
'mean monthly temperature from tape2 file
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
1091 CALL EVAPIND(ct$, st$, 2)
     IF LEN(ct$) = 0 THEN RETURN
     IF IRTRN% = 100 THEN RETURN
     IF IRTRN% = 1000 THEN EXIT SUB
     LOCATE 3, 38: PRINT USING "\                    \"; ct$
     LOCATE 4, 38: PRINT USING "\                    \"; st$
     IF CUNIT(clim, 2) = 2 THEN
        FOR k% = 1 TO 12
           SYNTH$(k%) = LTRIM$(RTRIM$(STR$(5 / 9 * (deft(k%) - 32))))
           DECPOS = INSTR(SYNTH$(k%), ".")
           SYNTH$(k%) = LEFT$(SYNTH$(k%), DECPOS + 2)
        NEXT k%
     ELSE
        FOR k% = 1 TO 12
          SYNTH$(k%) = LTRIM$(RTRIM$(STR$(deft(k%))))
          DECPOS = INSTR(SYNTH$(I), ".")
          SYNTH$(I) = LEFT$(SYNTH$(I), DECPOS + 2)
        NEXT k%
     END IF
     FOR k% = 1 TO 12
        LOCATE k% + 9, 40: PRINT USING "\    \"; SYNTH$(k%)
        LOCATE k% + 9, 50: PRINT USING "####.#"; VAL(NMMT$(k%))
     NEXT k%
RETURN
'----------------------------------------
'SAVE DATA7.TMP WITH THE NEW NORMAL MEAN
'MONTHLY TEMPERATURES.
'----------------------------------------
33095 CLOSE
     OPEN "DATA7.TMP" FOR INPUT AS #1
     OPEN "TEMPO.TMP" FOR OUTPUT AS #2
     INPUT #1, L$
'     PRINT #2, " 3"
     PRINT #2, L$
     INPUT #1, CUNIT(clim, 2)
     PRINT #2, USING "##"; CUNIT(clim, 2)
     PRINT #2, USING "\                  \"; ct$; st$
     LINE INPUT #1, L$
     LINE INPUT #1, L$
     'NYEARS$ = LEFT$(L$, 4)
     'PRINT #2, USING "####"; VAL(NYEARS$)
     FOR I = 1 TO 12
        IF INSTR(SYNTH$(I), ".") THEN
           PRINT #2, USING "####.#"; VAL(SYNTH$(I));
        ELSE
           SYNTH$(I) = LTRIM$(RTRIM$(SYNTH$(I))) + "."
           PRINT #2, USING "####.#"; VAL(SYNTH$(I));
        END IF
     NEXT I
     PRINT #2,
     FOR I = 1 TO 100
        FOR J = 1 TO 37
          IF EOF(1) THEN GOTO 8001
          LINE INPUT #1, L$
          PRINT #2, L$
        NEXT J
     NEXT I
8001 CLOSE
     KILL "DATA7.TMP"
     NAME "TEMPO.TMP" AS "DATA7.TMP"
RETURN
'---------------------------------------
'subroutine to compute the arithmatic
'monthly mean temperature
'---------------------------------------
23095 CLOSE
      OPEN "data7.tmp" FOR INPUT AS #1
      LINE INPUT #1, L$
      INPUT #1, CUNIT(clim, 2)
      LINE INPUT #1, L$
      IF meansel = 0 THEN
         ct$ = LTRIM$(RTRIM$(LEFT$(L$, 15)))
         st$ = LTRIM$(RTRIM$(MID$(L$, 16, 15)))
      END IF
      LINE INPUT #1, L$
      IF meansel = 0 THEN
         SYNTH$(1) = LTRIM$(RTRIM$(LEFT$(L$, 6)))
         SYNTH$(2) = LTRIM$(RTRIM$(MID$(L$, 7, 6)))
         SYNTH$(3) = LTRIM$(RTRIM$(MID$(L$, 13, 6)))
         SYNTH$(4) = LTRIM$(RTRIM$(MID$(L$, 19, 6)))
         SYNTH$(5) = LTRIM$(RTRIM$(MID$(L$, 25, 6)))
         SYNTH$(6) = LTRIM$(RTRIM$(MID$(L$, 31, 6)))
         SYNTH$(7) = LTRIM$(RTRIM$(MID$(L$, 37, 6)))
         SYNTH$(8) = LTRIM$(RTRIM$(MID$(L$, 43, 6)))
         SYNTH$(9) = LTRIM$(RTRIM$(MID$(L$, 49, 6)))
         SYNTH$(10) = LTRIM$(RTRIM$(MID$(L$, 55, 6)))
         SYNTH$(11) = LTRIM$(RTRIM$(MID$(L$, 61, 6)))
         SYNTH$(12) = LTRIM$(RTRIM$(MID$(L$, 67, 6)))
      END IF
      field1% = 6: field2% = 6
      month(1) = 31: month(2) = 28: month(3) = 31: month(4) = 30: month(5) = 31
      month(6) = 30: month(7) = 31: month(8) = 31: month(9) = 30: month(10) = 31
      month(11) = 30: month(12) = 31
      FOR iy = 1 TO 100
        I = 1: im = 1
        FOR J = 1 TO 37
           IF EOF(1) THEN CLOSE : GOTO 23100
           IF J = 1 THEN NY = iy
           LINE INPUT #1, L$
           v0$ = LEFT$(L$, field1%)
           va$(I) = MID$(L$, field1% + 1, field2%): st% = field1% + 1 + field2%: GOSUB 23105
           va$(I) = MID$(L$, st%, field2%): st% = st% + field2%: GOSUB 23105
           va$(I) = MID$(L$, st%, field2%): st% = st% + field2%: GOSUB 23105
           va$(I) = MID$(L$, st%, field2%): st% = st% + field2%: GOSUB 23105
           va$(I) = MID$(L$, st%, field2%): st% = st% + field2%: GOSUB 23105
           va$(I) = MID$(L$, st%, field2%): st% = st% + field2%: GOSUB 23105
           va$(I) = MID$(L$, st%, field2%): st% = st% + field2%: GOSUB 23105
           va$(I) = MID$(L$, st%, field2%): st% = st% + field2%: GOSUB 23105
           va$(I) = MID$(L$, st%, field2%): st% = st% + field2%: GOSUB 23105
           va$(I) = MID$(L$, st%, field2%): st% = st% + field2%: GOSUB 23105
        NEXT J
        NY = iy
      NEXT iy
      CLOSE
''''''''''''''''''''''''''''''''''''''''''
'average the monthly-average temperatures
''''''''''''''''''''''''''''''''''''''''''
23100 IF NY = 0 THEN RETURN
      FOR im = 1 TO 12
        mon = 0
        FOR iy = 1 TO NY
          mon = mon + ave(im, iy)
        NEXT iy
        NMMT$(im) = STR$(mon / NY)
      NEXT im

RETURN
''''''''''''''''''''''''''''''''''''
'subroutine to average temperatures
''''''''''''''''''''''''''''''''''''
23105 IF I = month(im) THEN
         sum(im, iy) = 0
         FOR k = 1 TO month(im)
           sum(im, iy) = sum(im, iy) + VAL(va$(k))
         NEXT k
         ave(im, iy) = sum(im, iy) / month(im)
         I = 1
         IF im < 12 THEN im = im + 1
       ELSE
         I = I + 1
       END IF
       RETURN
'---------------------------------
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
'---------------------------------
20135 CALL EXOUT(ANSCODE)
      IF ANSCODE = 0 THEN
        GOTO 7333
      ELSE
        IF IRTRN% = 100 THEN CLS : PCOPY HPAGE%, VPAGE%
        EXIT SUB
      END IF
RETURN



'--------------------------------------------
'HELP MESSAGES FOR MEAN TEMPERATURES SCREEN
'--------------------------------------------
7000  COLOR 15, 1
      CLS
     COLOR 4, 3
     LOCATE 25, 1: PRINT "                           Press Any Key to Return                              ";
     COLOR 15, 1
     LOCATE 2, 3: PRINT "                                                                             "
     LOCATE 3, 3: PRINT "Option 1.  Default normal mean monthly temperature values may be selected    "
     LOCATE 4, 3: PRINT "from the synthetic temperature data base by pressing F5.  If selected, these "
     LOCATE 5, 3: PRINT "values will be written in the USER column.  At this point the user may accept"
     LOCATE 6, 3: PRINT "these values by pressing F10, change the user values or press Esc to return  "
     LOCATE 7, 3: PRINT "to the Use Data Averaged Monthly Temperatures? question.                   "
     LOCATE 8, 3: PRINT "                                                                             "
     LOCATE 9, 3: PRINT "Option 2.  Normal mean monthly temperatures may be specified.  Normal mean   "
     LOCATE 10, 3: PRINT "monthly temperature values are available from the National Climatic Center   "
     LOCATE 11, 3: PRINT "of the National Oceanic and Atmospheric Administration (NOAA), climatic      "
     LOCATE 12, 3: PRINT "atlases and reference books on climates of the states.  To use this option,  "
     LOCATE 13, 3: PRINT "answer NO to the Use Data Averaged Monthly Temperatures? question.  Then,  "
     LOCATE 14, 3: PRINT "program will move the cursor to first cell of the USER column.  Then, enter  "
     LOCATE 15, 3: PRINT "enter the twelve monthly values and press F10 to accept and use these values."
     LOCATE 16, 3: PRINT "Otherwise, press Esc to return to the option question.                       "
     LOCATE 17, 3: PRINT "                                                                             "
     LOCATE 18, 3: PRINT "Option 3.  The program will compute estimates of the normal mean monthly     "
     LOCATE 19, 3: PRINT "temperature values, displayed in the column labeled Data Averaged, by      "
     LOCATE 20, 3: PRINT "averaging all daily temperature values for each month from all of the years  "
     LOCATE 21, 3: PRINT "of temperature data that were entered.  To use this option, answer YES to    "
     LOCATE 22, 3: PRINT "the Use Data Averaged Monthly Temperatures? question.  The program will    "
     LOCATE 23, 3: PRINT "accept the averaged values, write the values to the USER column and proceed. "
     LOCATE 24, 3: PRINT "";
70001 ke$ = INKEY$: IF ke$ = "" THEN GOTO 70001
CLS
RETURN
END SUB

SUB RCELL (IROW, ISTART, NS, TXT$)
'
''''''''''''''''''''''''''''''''''''''''
' Subroutine to select path from menu  '
''''''''''''''''''''''''''''''''''''''''
'
'  Locate cursor at first position of cell and write reverse video cell
'
      COLOR 1, 15
      LOCATE IROW, ISTART
      PRINT TXT$;
'
' Locate the cursor at the current postion and capture a new key
'
      LOCATE IROW, ISTART
11221 KSTRG$ = ""
      KSTRG$ = INKEY$: IF KSTRG$ = "" GOTO 11221
      NL = ASC(MID$(KSTRG$, 1))
      IF LEN(KSTRG$) = 2 THEN NH = ASC(MID$(KSTRG$, 2))
'**************************************************************
' Check for PgDn (IRTRN%=1), PgUp (IRTRN%=-1), F10 (IRTRN%=2) '
' UP ARROW (IRTRN%=3), DOWN ARROW (IRTRN%=4), <CR> (IRTRN%=5) '
' RIGHT ARROW (IRTRN%=6), LEFT ARROW (IRTRN%=7)               '
' HOME (IRTRN%=8), END (IRTRN%=9)                             '
' COMMAND HELP (F1) IRTRN%=20                                 '
' TECHNICAL HELP (F2) IRTRN%=21                               '
' HELP SOIL TEXTURE (F3) IRTRN% = 22                          '
' USER SOIL TEXTURE (F4) IRTRN% = 23                          '
' QUIT WITH SAVE (F9) IRTRN% = 24                             '
' Alt A to insert a layer above current layer (IRTRN%=30)
' Alt B to insert a layer below current layer (IRTRN%=31)
' ESC : IRTRN% = 100                                                            '
'**************************************************************
  IF NL = 0 AND NH = 81 THEN IRTRN% = 1: COLOR 15, 1: EXIT SUB
  IF NL = 0 AND NH = 73 THEN IRTRN% = -1: COLOR 15, 1: EXIT SUB
  IF (NL = 0) AND (NH = 68) THEN IRTRN% = 2: COLOR 15, 1: EXIT SUB
  IF (NL = 0) AND (NH = 72) THEN IRTRN% = 3: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF (NL = 0) AND (NH = 80) THEN IRTRN% = 4: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF (ASC(KSTRG$) = 13) THEN IRTRN% = 5: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF (NL = 9) OR (NL = 0 AND NH = 77) THEN IRTRN% = 6: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF (NL = 0 AND NH = 15) OR (NL = 0 AND NH = 75) THEN IRTRN% = 7: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 0 AND NH = 71 THEN IRTRN% = 8: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 0 AND NH = 79 THEN IRTRN% = 9: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 0 AND NH = 61 THEN IRTRN% = 22: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 0 AND NH = 62 THEN IRTRN% = 23: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 0 AND NH = 67 THEN IRTRN% = 1000: EXIT SUB
  IF (ITYPE = 2) AND (NL = 73 OR NL = 105) THEN IRTRN% = 10: COLOR 15, 1: EXIT SUB
  IF NL = 0 AND NH = 59 THEN IRTRN% = 20: COLOR 15, 0: EXIT SUB
  IF NL = 0 AND NH = 60 THEN IRTRN% = 21: COLOR 15, 0: EXIT SUB
  IF NL = 0 AND NH = 30 THEN IRTRN% = 30: COLOR 15, 1: EXIT SUB
  IF NL = 0 AND NH = 48 THEN IRTRN% = 31: COLOR 15, 1: EXIT SUB
  IF NL = 27 THEN IRTRN% = 100: COLOR 15, 1: EXIT SUB
  IF NL = 0 AND NH = 63 THEN IRTRN% = 40: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 0 AND NH = 64 THEN IRTRN% = 41: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 0 AND NH = 65 THEN IRTRN% = 42: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 49 THEN IRTRN% = 101:  COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 50 THEN IRTRN% = 102:  COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 51 THEN IRTRN% = 103:  COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 52 THEN IRTRN% = 104:  COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 53 THEN IRTRN% = 105:  COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 54 THEN IRTRN% = 106:  COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 55 THEN IRTRN% = 107:  COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 89 OR NL = 121 THEN IRTRN% = 108: COLOR 15, 1: EXIT SUB
  IF NL = 78 OR NL = 110 THEN IRTRN% = 109: COLOR 15, 1: EXIT SUB
  IF NL = 89 OR NL = 121 THEN IRTRN% = 108: COLOR 15, 1: EXIT SUB
  IF NL = 78 OR NL = 110 THEN IRTRN% = 109: COLOR 15, 1: EXIT SUB
  GOTO 11221
  EXIT SUB
END SUB

SUB TYPEIN (clim$, YEARNO%, TOT(), NEWYR$)
     DIM V$(37, 10), START(10), COLHEAD$(10)
     PCOPY VPAGE%, HPAGE%
     clim1$ = "     PRECIPITATION     "
     CLIM2$ = "      TEMPERATURE      "
     CLIM3$ = "    SOLAR RADIATION    "
     IRTRN% = 0
     PCOPY VPAGE%, APAGE%
     CLS APAGE%
     SCREEN 0, , VPAGE%, VPAGE%
     COLOR 15, 1
     CLS
'''''''''''''''''''''''''''''''''''''''''''
'CALL SUBROUTINE TO READ DATA IF IT EXISTS
'''''''''''''''''''''''''''''''''''''''''''
     NEWYR$ = LTRIM$(RTRIM$(NEWYR$))
     A$ = DIR$(NEWYR$ + ".TMP")
     IF A$ <> "" THEN
        LOCATE 11, 27: PRINT "READING DAILY "; : PRINT LTRIM$(RTRIM$(clim$));
        LOCATE 13, 35: PRINT "Please Wait ...";
        GOSUB 12222
        FOR I = 1 TO 37
           FOR ii = 1 TO 10
             V$(I, ii) = LTRIM$(RTRIM$(V$(I, ii)))
           NEXT ii
        NEXT I
     END IF
     CLS
     COLOR 4, 3
     LOCATE 25, 1: PRINT "   Tab=Right   Shift Tab=Left   Up Arrow/Down Arrow   F10=Accept   Esc=Cancel   ";
     COLOR 15, 1
     CLOSE
12210 COLOR 11, 6
     LOCATE 24, 1: PRINT "   Type value then Enter    Home=First Day    End=Last Day    PgUp/PgDn=Scroll  ";
     LOCATE 1, 1: PRINT "                            DAILY                                                 ";
     LOCATE 1, 35: PRINT LTRIM$(clim$);
     IF clim$ = clim1$ THEN PRINT precu$; : clim = 1
     IF clim$ = CLIM2$ THEN PRINT tempu$; : clim = 2
     IF clim$ = CLIM3$ THEN PRINT solu$; : clim = 3
     NUMCOL = 10
     FOR I% = 1 TO NUMCOL
         COLHEAD$(I%) = STR$(I%)
     NEXT I%
     NL% = 19: ini% = 1
'DEFINE CELL WIDTH
     WIDE = 6: START(1) = 7
     FOR J% = 2 TO NUMCOL
        START(J%) = START(J% - 1) + WIDE + 1
     NEXT J%
'DEFINE LOCATION OF CELLS FOR EACH ENTRY
     COLOR 0, 3
     LOCATE 3, 1: PRINT "      ": LOCATE 3, 78: PRINT "  "
     FOR I% = 1 TO NUMCOL
        LOCATE 3, START(I%): PRINT USING "\      \"; COLHEAD$(I%)
     NEXT I%
     FOR I% = 1 TO NL% + 2: LOCATE I% + 2, 80: PRINT " ": NEXT I%
     LOCATE 23, 1: FOR I% = 1 TO 80: PRINT " "; : NEXT I%
     IF clim = 1 THEN
         IF CUNIT(1, 3) = 1 THEN
            LOCATE 23, 14: PRINT "Maximum allowed daily precipitation is 99.99 inches."
         END IF
         IF CUNIT(1, 3) = 2 THEN
            LOCATE 23, 12: PRINT "Maximum allowed daily precipitation is 999.9 millimeters."
         END IF
     END IF
     IF clim = 2 THEN
         IF CUNIT(2, 3) = 1 THEN
            LOCATE 23, 17: PRINT "Maximum allowed daily temperature is 9999.9 øF."
         END IF
         IF CUNIT(2, 3) = 2 THEN
            LOCATE 23, 17: PRINT "Maximum allowed daily temperature is 9999.9 øC."
         END IF
     END IF
     IF clim = 3 THEN
         IF CUNIT(3, 3) = 1 THEN
            LOCATE 23, 13: PRINT "Maximum allowed daily solar radiation is 9999.9 Langleys."
         END IF
         IF CUNIT(3, 3) = 2 THEN
            LOCATE 23, 14: PRINT "Maximum allowed daily solar radiation is 999.99 MJ/Mý."
         END IF
     END IF
     COLOR 15, 1
     LOCATE 2, 1: PRINT "      FILE: "; LTRIM$(RTRIM$(NEWYR$ + ".TMP"));
     LOCATE 2, 48: PRINT "YEAR ";
     LOCATE 2, 60: PRINT "MONTH";
     LOCATE 2, 70: PRINT "DAY";
      COLOR 0, 3
12310  L% = 1: J% = 1: ini% = 1: NL% = 19: RO = 2
       r% = 1
       FOR I% = 1 TO 19
         A$ = RIGHT$(STR$(I%), 2)
         LOCATE r% + 3, 1: PRINT USING "\ \"; A$
         r% = r% + 1
       NEXT I%
12340  COLOR 15, 1: LOCATE 2, 53: PRINT NEWYR$;
       GOTO 12343
12345  r% = 1
       IF ini% = 19 THEN NL% = 37
       FOR I% = ini% TO NL%
         A$ = RIGHT$(STR$(I%), 2)
         LOCATE r% + 3, 1: PRINT USING "\ \"; A$;
         r% = r% + 1
       NEXT I%
       IF NL% = 37 AND ini% = 20 THEN
          LOCATE r% + 3, 1: PRINT "    "
          COLOR 15, 1
          LOCATE r% + 3, 4: FOR I = 1 TO 75: PRINT " "; : NEXT I
       END IF
12343  r% = 3
       COLOR 15, 1
       FOR I% = ini% TO NL%
         FOR k% = 1 TO NUMCOL
            LOCATE r% + 1, START(k%): PRINT USING "\    \"; V$(I%, k%)
         NEXT k%
         r% = r% + 1
         NEXT I%
'''''''''''''''''''''''''''''''''''''''''''''''''''''
'CALL SUBROUTINE TO DETERMINE DAY AND MONTH OF YEAR
'''''''''''''''''''''''''''''''''''''''''''''''''''''
11020 GOSUB 12333
11120 COLOR 15, 1
      IF month% = 0 THEN
        LOCATE 2, 66: PRINT "  "; : LOCATE 2, 74: PRINT "  ";
        GOTO 11220
      END IF
      LOCATE 2, 66: PRINT USING "##"; month%;
      LOCATE 2, 74: PRINT USING "##"; DAY%;
11220 LOCATE 3, START(J%): PRINT USING "\    \"; COLHEAD$(J%);
      LOCATE RO + 2, 1: A$ = RIGHT$(STR$(L%), 2): PRINT USING "\ \"; A$;
      CALL ACELL(RO + 2, START(J%), WIDE, V$(L%, J%), 2)
      COLOR 0, 3
      LOCATE 3, START(J%): PRINT USING "\    \"; COLHEAD$(J%);
      LOCATE RO + 2, 1: A$ = RIGHT$(STR$(L%), 2): PRINT USING "\ \"; A$;
'EXIT INPUT SESSION
      IF IRTRN% = 100 THEN GOSUB 33195
'MOVE DOWN
    IF IRTRN% = 4 THEN
        IF L% < NL% THEN
          L% = L% + 1: RO = RO + 1: GOTO 11020
        ELSE
          IF NL% < 37 THEN
            NL% = NL% + 1: ini% = ini% + 1: L% = L% + 1: GOTO 12345
            ELSE GOTO 11020
          END IF
        END IF
     END IF
'MOVE UP
        IF IRTRN% = 3 THEN
          IF RO <> 2 THEN
            L% = L% - 1: RO = RO - 1: GOTO 11020
          ELSE
            IF ini% <> 1 THEN
              ini% = ini% - 1: NL% = NL% - 1: RO = 2: L% = L% - 1: GOTO 12345
            ELSE GOTO 11020
            END IF
          END IF
        END IF
'MOVE RIGHT
        IF (IRTRN% = 6) OR (IRTRN% = 5) THEN
           IF J% < NUMCOL THEN
              J% = J% + 1: GOTO 11020
           ELSEIF J% = NUMCOL AND L% < NL% THEN
              L% = L% + 1: RO = RO + 1: J% = 1: GOTO 11020
           ELSEIF J% = NUMCOL AND L% < 37 THEN
              NL% = NL% + 1: ini% = ini% + 1: L% = L% + 1: J% = 1: GOTO 12345
           ELSEIF J% = NUMCOL AND L% = 37 THEN
              L% = L%: RO = RO: J% = NUMCOL: GOTO 11020
           END IF
        END IF
'MOVE LEFT
        IF IRTRN% = 7 THEN
           IF J% > 1 THEN
              J% = J% - 1: GOTO 11020
           ELSEIF J% = 1 AND L% > ini% THEN
              J% = NUMCOL: L% = L% - 1: RO = RO - 1: GOTO 11020
           ELSEIF J% = 1 AND L% > 1 THEN
              NL% = NL% - 1: ini% = ini% - 1: L% = L% - 1: J% = 10: GOTO 12345
           ELSEIF J% = 1 AND L% = ini% THEN
              GOTO 11220
           END IF
        END IF
'MOVE HOME
        IF IRTRN% = 8 OR IRTRN% = -1 THEN
            L% = 1: RO = 2: J% = 1
            IF ini% = 1 THEN
               GOTO 11020
            ELSE
               ini% = 1: NL% = 19: GOTO 12345
            END IF
        END IF
'PAGE DOWN
        IF IRTRN% = 1 THEN
            L% = 20: RO = 2: J% = 1
            IF NL% = 37 THEN
               GOTO 11020
            ELSE
               ini% = 20: NL% = 37: GOTO 12345
            END IF
        END IF
'MOVE END
        IF IRTRN% = 9 THEN
          L% = 37: RO = 19: J% = NUMCOL: ini% = 20: NL% = 37
          GOTO 12345
        END IF
'
'SAVE DATA TO FILE AND EXIT
'
   IF IRTRN% = 2 THEN
        IF clim$ = clim1$ THEN
          OPEN NEWYR$ + ".TMP" FOR OUTPUT AS #1
        ELSEIF clim$ = CLIM2$ THEN
          OPEN NEWYR$ + ".TMP" FOR OUTPUT AS #1
        ELSEIF clim$ = CLIM3$ THEN
          OPEN NEWYR$ + ".TMP" FOR OUTPUT AS #1
        END IF
      GOSUB 32990
      CLOSE
      'COMPUTE THE TOTAL (ANNUAL) PRECIPATATION
      IF clim$ = clim1$ THEN
         TOT(YEARNO%) = 0
         FOR I = 1 TO 37
           FOR J = 1 TO 10
             TOT(YEARNO%) = VAL(V$(I, J)) + TOT(YEARNO%)
           NEXT J
         NEXT I
         IF YEAR% <> Y1% THEN
           FOR J = 6 TO 10
             TOT(YEARNO%) = TOT(YEARNO%) - VAL(V$(37, J))
           NEXT J
         ELSE
           FOR J = 7 TO 10
             TOT(YEARNO%) = TOT(YEARNO%) - VAL(V$(37, J))
           NEXT J
         END IF
      END IF
      GOTO 922102
   END IF
   GOTO 11220
922102  CLS : PCOPY HPAGE%, VPAGE%: IRTRN% = 0
      EXIT SUB
''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO COMPUTE DAY AND MONTH OF
'YEAR BASED ON CELL LOCATION
''''''''''''''''''''''''''''''''''''''''
'
'CHECK IF YEAR IS A LEAP YEAR
'
12333  YEAR% = VAL(NEWYR$)
       Y% = YEAR% / 4
       Y1% = Y% * 4
       IF YEAR% <> Y1% THEN
'JANUARY
    IF L% = 1 OR L% = 2 OR L% = 3 THEN DAY% = (L% - 1) * 10 + J%: month% = 1: RETURN
    IF L% = 4 AND J% = 1 THEN month% = 1: DAY% = 31: RETURN
'FEBRUARY
    IF L% = 4 AND J% > 1 THEN DAY% = J% - 1: month% = 2: RETURN
    IF L% = 5 THEN month% = 2: DAY% = 9 + J%: RETURN
    IF L% = 6 AND J% < 10 THEN month% = 2: DAY% = 19 + J%: RETURN
'MARCH
    IF L% = 6 AND J% = 10 THEN month% = 3: DAY% = 1: RETURN
    IF L% >= 7 AND L% <= 9 THEN month% = 3: DAY% = (L% - 7) * 10 + J% + 1: RETURN
'APRIL
    IF L% >= 10 AND L% <= 12 THEN month% = 4: DAY% = (L% - 10) * 10 + J%: RETURN
'MAY
    IF L% >= 13 AND L% <= 15 THEN month% = 5: DAY% = (L% - 13) * 10 + J%: RETURN
    IF L% = 16 AND J% = 1 THEN month% = 5: DAY% = 31: RETURN
'JUNE
    IF L% = 16 AND J% > 1 THEN month% = 6: DAY% = J% - 1: RETURN
    IF L% = 17 OR L% = 18 THEN month% = 6: DAY% = (L% - 17) * 10 + 9 + J%: RETURN
    IF L% = 19 AND J% = 1 THEN month% = 6: DAY% = 30: RETURN
'JULY
    IF L% = 19 AND J% > 1 THEN month% = 7: DAY% = J% - 1: RETURN
    IF L% = 20 OR L% = 21 THEN month% = 7: DAY% = (L% - 20) * 10 + 9 + J%: RETURN
    IF L% = 22 AND J% <= 2 THEN month% = 7: DAY% = (L% - 20) * 10 + 9 + J%: RETURN
'AUGUST
    IF L% = 22 AND J% >= 2 THEN month% = 8: DAY% = J% - 2: RETURN
    IF L% = 23 OR L% = 24 THEN month% = 8: DAY% = (L% - 23) * 10 + 8 + J%: RETURN
    IF L% = 25 AND J% <= 3 THEN month% = 8: DAY% = (L% - 23) * 10 + 8 + J%: RETURN
'SEPTEMBER
    IF L% = 25 AND J% >= 4 THEN month% = 9: DAY% = J% - 3: RETURN
    IF L% = 26 OR L% = 27 THEN month% = 9: DAY% = (L% - 26) * 10 + 7 + J%: RETURN
    IF L% = 28 AND J% <= 3 THEN month% = 9: DAY% = (L% - 26) * 10 + 7 + J%: RETURN
'OCTOBER
    IF L% = 28 AND J% >= 4 THEN month% = 10: DAY% = J% - 3: RETURN
    IF L% = 29 OR L% = 30 THEN month% = 10: DAY% = (L% - 29) * 10 + 7 + J%: RETURN
    IF L% = 31 AND J% <= 4 THEN month% = 10: DAY% = (L% - 29) * 10 + 7 + J%: RETURN
'NOVEMBER
    IF L% = 31 AND J% >= 5 THEN month% = 11: DAY% = J% - 4: RETURN
    IF L% = 32 OR L% = 33 THEN month% = 11: DAY% = (L% - 32) * 10 + 6 + J%: RETURN
    IF L% = 34 AND J% <= 4 THEN month% = 11: DAY% = (L% - 32) * 10 + 6 + J%: RETURN
'DECEMBER
    IF L% = 34 AND J% >= 5 THEN month% = 12: DAY% = J% - 4: RETURN
    IF L% = 35 OR L% = 36 THEN month% = 12: DAY% = (L% - 35) * 10 + 6 + J%: RETURN
    IF L% = 37 AND J% <= 5 THEN month% = 12: DAY% = (L% - 35) * 10 + 6 + J%: RETURN
    IF L% = 37 AND J% > 5 THEN month% = -0: DAY% = -0: RETURN
  ELSE
'LEAP YEARS
' JANUARY
    IF L% = 1 OR L% = 2 OR L% = 3 THEN DAY% = (L% - 1) * 10 + J%: month% = 1: RETURN
    IF L% = 4 AND J% = 1 THEN month% = 1: DAY% = 31: RETURN
'FEBRUARY
    IF L% = 4 AND J% > 1 THEN DAY% = J% - 1: month% = 2: RETURN
    IF L% = 5 THEN month% = 2: DAY% = 9 + J%: RETURN
    IF L% = 6 THEN month% = 2: DAY% = 19 + J%: RETURN
'MARCH
    IF L% >= 7 AND L% <= 9 THEN DAY% = (L% - 7) * 10 + J%: month% = 3: RETURN
    IF L% = 10 AND J% = 1 THEN month% = 3: DAY% = 31: RETURN
'APRIL
    IF L% = 10 AND J% >= 2 THEN month% = 4: DAY% = J% - 1: RETURN
    IF L% = 11 THEN month% = 4: DAY% = 9 + J%: RETURN
    IF L% = 12 THEN month% = 4: DAY% = 19 + J%: RETURN
    IF L% = 13 AND J% = 1 THEN month% = 4: DAY% = 30: RETURN
'MAY
    IF L% = 13 AND J% > 1 THEN month% = 5: DAY% = J% - 1: RETURN
    IF L% = 14 THEN month% = 5: DAY% = 9 + J%: RETURN
    IF L% = 15 THEN month% = 5: DAY% = 19 + J%: RETURN
    IF L% = 16 AND J% <= 2 THEN month% = 5: DAY% = 29 + J%: RETURN
'JUNE  (30 DAYS)
    IF L% = 16 AND J% > 3 THEN month% = 6: DAY% = J% - 2: RETURN
    IF L% = 17 THEN month% = 6: DAY% = 8 + J%: RETURN
    IF L% = 18 THEN month% = 6: DAY% = 18 + J%: RETURN
    IF L% = 19 AND J% <= 2 THEN month% = 6: DAY% = 28 + J%: RETURN
'JULY (31 DAYS)
    IF L% = 19 AND J% > 2 THEN month% = 7: DAY% = J% - 2: RETURN
    IF L% = 20 THEN month% = 7: DAY% = 8 + J%: RETURN
    IF L% = 21 THEN month% = 7: DAY% = 18 + J%: RETURN
    IF L% = 22 AND J% <= 3 THEN month% = 7: DAY% = 28 + J%: RETURN
'AUGUST (31 DAYS)
    IF L% = 22 AND J% > 3 THEN month% = 8: DAY% = J% - 3: RETURN
    IF L% = 23 THEN month% = 8: DAY% = 7 + J%: RETURN
    IF L% = 24 THEN month% = 8: DAY% = 17 + J%: RETURN
    IF L% = 25 AND J% <= 4 THEN month% = 8: DAY% = 27 + J%: RETURN
'SEPTEMBER (30 DAYS)
    IF L% = 25 AND J% > 4 THEN month% = 9: DAY% = J% - 4: RETURN
    IF L% = 26 THEN month% = 9: DAY% = 6 + J%: RETURN
    IF L% = 27 THEN month% = 9: DAY% = 16 + J%: RETURN
    IF L% = 28 AND J% <= 4 THEN month% = 9: DAY% = 26 + J%: RETURN
'OCTOBER (31 DAYS)
    IF L% = 28 AND J% > 4 THEN month% = 10: DAY% = J% - 4: RETURN
    IF L% = 29 THEN month% = 10: DAY% = 6 + J%: RETURN
    IF L% = 30 THEN month% = 10: DAY% = 16 + J%: RETURN
    IF L% = 31 AND J% <= 5 THEN month% = 10: DAY% = 26 + J%: RETURN
'NOVEMBER (30 DAYS)
    IF L% = 31 AND J% >= 6 THEN month% = 11: DAY% = J% - 5: RETURN
    IF L% = 32 THEN month% = 11: DAY% = J% + 5: RETURN
    IF L% = 33 THEN month% = 11: DAY% = J% + 15: RETURN
    IF L% = 34 AND J% <= 5 THEN month% = 11: DAY% = 25 + J%: RETURN
'DECEMBER (31 DAYS)
    IF L% = 34 AND J% >= 6 THEN month% = 12: DAY% = J% - 5: RETURN
    IF L% = 35 THEN month% = 12: DAY% = J% + 5: RETURN
    IF L% = 36 THEN month% = 12: DAY% = J% + 15: RETURN
    IF L% = 37 AND J% <= 6 THEN month% = 12: DAY% = 25 + J%: RETURN
    IF L% = 37 AND J% > 6 THEN month% = -0: DAY% = -0: RETURN
 END IF
 RETURN
'''''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO READ DATA FOR YEAR IF IT EXISTS
'FIRST CHECK IF SCRATCH HELP FILE EXISTS
'''''''''''''''''''''''''''''''''''''''''''''''''''
'OPEN APPROPRIATE FILES IF THEY EXIST (CHECK DIRECTORY FIRST)
12222 OPEN NEWYR$ + ".TMP" FOR INPUT AS #2
      IF clim$ = clim1$ THEN
         field1% = 10: field2% = 5: FIELD3% = 5
         'IF CUNIT(1, 3) = 2 THEN FIELD2% = 6
      ELSEIF clim$ = CLIM2$ THEN
         field1% = 5: field2% = 6: FIELD3% = 5
      ELSEIF clim$ = CLIM3$ THEN
         field1% = 5: field2% = 6: FIELD3% = 5
      END IF
       FOR I = 1 TO 37
         IF EOF(2) THEN CLOSE : RETURN
         LINE INPUT #2, L$
         v0$ = LEFT$(L$, field1%)
         V$(I, 1) = MID$(L$, field1% + 1, field2%): st% = field1% + 1 + field2%
         V$(I, 2) = MID$(L$, st%, field2%): st% = st% + field2%
         V$(I, 3) = MID$(L$, st%, field2%): st% = st% + field2%
         V$(I, 4) = MID$(L$, st%, field2%): st% = st% + field2%
         V$(I, 5) = MID$(L$, st%, field2%): st% = st% + field2%
         V$(I, 6) = MID$(L$, st%, field2%): st% = st% + field2%
         V$(I, 7) = MID$(L$, st%, field2%): st% = st% + field2%
         V$(I, 8) = MID$(L$, st%, field2%): st% = st% + field2%
         V$(I, 9) = MID$(L$, st%, field2%): st% = st% + field2%
         V$(I, 10) = MID$(L$, st%, field2%): st% = st% + field2%
       NEXT I
       CLOSE
      RETURN
'''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO SAVE DATA TO FILE
'''''''''''''''''''''''''''''''''''''''''
32990 IF clim$ = clim1$ THEN
         field1$ = "##########": field2$ = "##.##": FIELD3$ = "#####"
         IF CUNIT(1, 3) = 2 THEN field2$ = "###.#"
      ELSEIF clim$ = CLIM2$ THEN
         field1$ = "#####": field2$ = "####.#": FIELD3$ = "#####"
      ELSEIF clim$ = CLIM3$ THEN
         field1$ = "#####": field2$ = "####.#": FIELD3$ = "#####"
         IF CUNIT(1, 3) = 2 THEN field2$ = "###.##"
      END IF
      FOR ii = 1 TO 37
        PRINT #1, USING field1$; VAL(NEWYR$);
        FOR I = 1 TO 10
           '--------------------------------
           'CHECK VALUES FOR MAXIMUM ALLOWED
           '--------------------------------
           IF clim$ = clim1$ THEN
              IF CUNIT(1, 3) = 1 THEN
                 IF VAL(V$(ii, I)) > 99.99 THEN V$(ii, I) = "99.99"
              ELSE
                 IF VAL(V$(ii, I)) > 999.9 THEN V$(ii, I) = "999.9"
              END IF
           ELSEIF clim$ = CLIM2$ THEN
              IF VAL(V$(ii, I)) > 999.9 THEN V$(ii, I) = "999.9"
           ELSEIF clim$ = CLIM3$ THEN
              IF CUNIT(3, 3) = 1 THEN
                IF VAL(V$(ii, I)) > 9999.9 THEN V$(ii, I) = "9999.9"
              ELSE
                IF VAL(V$(ii, I)) > 999.99 THEN V$(ii, I) = "999.99"
              END IF
           END IF
           PRINT #1, USING field2$; VAL(V$(ii, I));
        NEXT I
        PRINT #1, USING FIELD3$; ii
      NEXT ii
      RETURN
'---------------------------------
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
'---------------------------------
33195 CALL EXOUT(ANSCODE)
     IF ANSCODE = 0 THEN
        COLOR 0, 3
        GOTO 12345
     ELSE
        IF IRTRN% = 100 THEN CLS : PCOPY HPAGE%, VPAGE%
        EXIT SUB
     END IF
RETURN
END SUB

