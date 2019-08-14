DECLARE SUB HLPMSG (f2help$)
DECLARE SUB USERHELP ()
DECLARE SUB ACELL (IROW!, ISTART!, NS!, TEXT$, ITYPE!)
DECLARE SUB BOX ()
DECLARE SUB RCELL (IROW!, ISTART!, NS!, TXT$)
   COMMON SHARED D10EDIT$, D11EDIT$, D11SAVE$, d10save$, D10PATH$
   COMMON SHARED HSOIL$(), prop$(), US$(), CW(), UNMOIST$
   COMMON SHARED UNITS$, UNIT$, UN1$, UN2$, UN3$, UN4$, UN5$, UNAREA$, UNSL$
   COMMON SHARED TOHELP%, NLAYER, NL%, TITLE$, irtrn%
   COMMON SHARED MOIST$, SNOW$, PAGE%, SOILN%
   COMMON SHARED APAGE%, VPAGE%, HPAGE%, DIRECT$
   COMMON SHARED BLANKS$, AREA$
   COMMON SHARED SELCN$, CN$, SLOPE$, SLENGTH$, SOILTEX$, VEGET$, PRCNT$
   COMMON SHARED CNII, UNI$, SVPATH$
   COMMON SHARED SROWBOX, SCOLBOX, BOXWIDTH, EROWBOX, LATIT$

11000 SUB ACELL (IROW, ISTART, NS, TEXT$, ITYPE)
    SCREEN , , VPAGE%, VPAGE%
    BLANKS$ = "                                                                          "
    irtrn% = 0
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
' F7 USER SOIL TECTURES   (IRTRN%=42)    F9 QUIT            (IRTRN%=1000)
' F10                     (IRTRN%=2)     ESC                (IRTRN%=100)
'"Y" OR "y"               (IRTRN%=51)    "N" OR "n"         (IRTRN%=50)
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
11017 IF NL = 0 AND NH = 81 THEN irtrn% = 1
   IF NL = 0 AND NH = 73 THEN irtrn% = -1
   IF NL = 0 AND NH = 68 THEN irtrn% = 2
   IF NL = 0 AND NH = 72 THEN irtrn% = 3
   IF NL = 0 AND NH = 80 THEN irtrn% = 4
   IF NL = 13 THEN irtrn% = 5
   IF NL = 9 THEN irtrn% = 6
   IF NL = 0 AND NH = 15 THEN irtrn% = 7
   IF NL = 0 AND NH = 71 THEN irtrn% = 8:
   IF NL = 0 AND NH = 79 THEN irtrn% = 9:
   IF NL = 0 AND NH = 59 THEN irtrn% = 20  ': COLOR 15, 0: GOSUB 11022: EXIT SUB
   IF NL = 0 AND NH = 60 THEN irtrn% = 21  ': COLOR 15, 0: GOSUB 11022: EXIT SUB
   IF NL = 0 AND NH = 61 THEN irtrn% = 22  ': COLOR 15, 1: LOCATE IROW, ISTART, 0: GOSUB 11022: PRINT TXT$; : EXIT SUB
   IF NL = 0 AND NH = 62 THEN irtrn% = 23  ': COLOR 15, 1: LOCATE IROW, ISTART, 0: GOSUB 11022: PRINT TXT$; : EXIT SUB
   IF NL = 0 AND NH = 67 THEN irtrn% = 1000 ': COLOR 15, 1: GOSUB 11022: EXIT SUB
   IF NL = 0 AND NH = 30 THEN irtrn% = 30
   IF NL = 0 AND NH = 48 THEN irtrn% = 31
   IF NL = 0 AND NH = 46 THEN irtrn% = 32
   IF NL = 0 AND NH = 50 THEN irtrn% = 33
   IF NL = 0 AND NH = 32 THEN irtrn% = 13: COLOR 15, 1: GOSUB 11022: EXIT SUB
   IF NL = 27 THEN irtrn% = 100
   IF ITYPE = 3 THEN
      IF NL = 78 OR NL = 110 THEN irtrn% = 50: LOCATE IROW, ISTART, 0: COLOR 15, 1: GOSUB 11022: PRINT TXT$; : EXIT SUB
      IF NL = 89 OR NL = 121 THEN irtrn% = 51: LOCATE IROW, ISTART, 0: COLOR 15, 1: GOSUB 11022: PRINT TXT$; : EXIT SUB
   END IF
   IF NL = 0 AND NH = 63 THEN irtrn% = 40: COLOR 15, 1: LOCATE IROW, ISTART, 0: GOSUB 11022: PRINT TXT$; : EXIT SUB
   IF NL = 0 AND NH = 64 THEN irtrn% = 41: COLOR 15, 1: LOCATE IROW, ISTART, 0: GOSUB 11022: PRINT TXT$; : EXIT SUB
   IF NL = 0 AND NH = 65 THEN irtrn% = 42: COLOR 15, 1: LOCATE IROW, ISTART, 0: GOSUB 11022: PRINT TXT$; : EXIT SUB
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
      ELSEIF (irtrn% < 10 AND irtrn% > 0) OR (irtrn% > 19 AND irtrn% < 34) OR (irtrn% = -1) OR irtrn% = 100 OR irtrn% = 1000 THEN
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

SUB BOX
   ''''''''''''''''
   'DRAW A BOX SUB
   ''''''''''''''''
   'SROWBOX   STARTING ROW FOR THE BOX
   'SCOLBOX   STARTING COLUMN FOR THE BOX
   'BOXWIDTH  WIDTH OF BOX
   'EROWBOX   ENDING ROW OF THE BOX
   'ECOLBOX   ENDING COLUMN OF THE BOX

   ECOLBOX = SCOLBOX + BOXWIDTH - 1

   LOCATE SROWBOX, SCOLBOX: PRINT CHR$(201) + STRING$(BOXWIDTH - 2, 205) + CHR$(187);
   FOR I% = SROWBOX + 1 TO EROWBOX - 1
       LOCATE I%, SCOLBOX: PRINT CHR$(186): LOCATE I%, ECOLBOX: PRINT CHR$(186);
   NEXT
   LOCATE EROWBOX, SCOLBOX: PRINT CHR$(200) + STRING$(BOXWIDTH - 2, 205) + CHR$(188);

   EXIT SUB
END SUB

SUB CHKUSOIL (soilsvd)
DIM diffsoil%(20), temp(20), saveyn$(20), tempo$(6), newsoil$(21, 6)
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'  SUBROUTINE TO REVIEW NON DEFAULT SOIL TEXTURES
'  AND ADD THEM TO USER'S TEXTURE LIST
'
'   NSOILS% = NUMBER OF NEW SOIL TEXTURES
' DIFFSOIL% = FLAG TO INDICATE WHICH SOIL TEXTURES ARE NEW.
'  NEWCOUNT = COUNTER FOR NEW SOILS
'    SOILN% = NUMBER OF SOIL TEXTURES IN THE USER DEFINED SOIL LIST
'  NUMSOILS = NUMBER OF SOILS IN THE SOIL.USR FILE
'     USNUM = NUMBER OF THE LAST SOIL TEXTURE IN THE USER SOILS FILE
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        CLOSE
        SOILN% = 0
        IF DIR$("SOIL.USR") <> "" THEN
           OPEN "SOIL.USR" FOR INPUT AS #1
91522      IF EOF(1) THEN GOTO 95222
           INPUT #1, li$
           SOILN% = SOILN% + 1
           GOTO 91522
        END IF
95222   CLS
        IF SOILN% > 100 THEN SOILN% = 100
91110   f1help$ = "#USERSOIL"
        f2help$ = "#USERSOIL"
        nsoils% = 0
        newcount = 0
        FOR I% = 1 TO 20
           diffsoil%(I%) = 0
           xval = VAL(prop$(I%, 3, 1))
           IF xval > 42 THEN
              IF SOILN% <> 0 THEN
                 IF newcount <> 0 THEN
                   FOR kount = 1 TO newcount
                     IF xval = temp(kount) THEN GOTO 91114
                   NEXT kount
                 END IF
                 diffsoil%(I%) = 1
                 newcount = newcount + 1
                 temp(newcount) = xval
              ELSE
              IF newcount <> 0 THEN
                 FOR kount = 1 TO newcount
                    IF xval = temp(kount) THEN GOTO 91114
                 NEXT kount
              END IF
              newcount = newcount + 1
              temp(newcount) = xval
              diffsoil%(I%) = 1
           END IF
        END IF
91114   NEXT I%
        FOR I% = 1 TO 20
           IF diffsoil%(I%) = 1 THEN nsoils% = nsoils% + 1
        NEXT I%
        IF nsoils% = 0 THEN EXIT SUB
        soilsv = 1
        ''''''''''''''
        'DRAW A BOX
        ''''''''''''''
        COLOR 3, 4
        LOCATE 1, 1: PRINT STRING$(80, 219);
        FOR I% = 2 TO 24
            LOCATE I%, 1: PRINT CHR$(219);
            LOCATE I%, 80: PRINT CHR$(219);
        NEXT I%
        COLOR 4, 3
        LOCATE 1, 25: PRINT USING "##"; nsoils%; : PRINT " USER DEFINED SOIL TEXTURES"
        LOCATE 25, 1:  PRINT " F1=Info     F2=Help     F3=Key     F9=Quit    F10=Accept Labels     Esc=Cancel ";
        COLOR 15, 1
'''''''''''''''''''''''''''''''''''''
'PLACE USER SOILS IN A SEPERATE ARRAY
'SORTED IN ASSENDING ORDER
'''''''''''''''''''''''''''''''''''''
        II = 0
        FOR I = 1 TO 20
           IF diffsoil%(I) = 1 THEN
              II = II + 1
              newsoil$(II, 1) = prop$(I, 3, 1)
              newsoil$(II, 3) = prop$(I, 4, 1)
              newsoil$(II, 4) = prop$(I, 5, 1)
              newsoil$(II, 5) = prop$(I, 6, 1)
              newsoil$(II, 6) = prop$(I, 2, 2)
           END IF
        NEXT I
        FOR I = 1 TO nsoils%
           FOR II = 1 TO nsoils% - 1
              IF VAL(newsoil$(II, 1)) > VAL(newsoil$(II + 1, 1)) THEN
                 FOR IJ = 1 TO 6
                    tempo$(IJ) = newsoil$(II + 1, IJ)
                    newsoil$(II + 1, IJ) = newsoil$(II, IJ)
                    newsoil$(II, IJ) = tempo$(IJ)
                 NEXT IJ
              END IF
           NEXT II
        NEXT I
''''''''''
'END SORT
''''''''''
        CLOSE
        IF DIR$("SOIL.USR") <> "" THEN
           OPEN "SOIL.USR" FOR INPUT AS #1
           l = 1
           FOR I = 1 TO SOILN%
              INPUT #1, solnum, li$
92455         IF VAL(newsoil$(l, 1)) < solnum THEN
                 l = l + 1
                 IF l > nsoils% THEN GOTO 92456
                 GOTO 92455
              END IF
              IF VAL(newsoil$(l, 1)) = solnum THEN
                 newsoil$(l, 2) = LEFT$(li$, 17)
                 l = l + 1
                 IF l > nsoils% THEN GOTO 92456
              END IF
           NEXT I
        END IF
92456   CLOSE
        FOR I = 1 TO nsoils%: saveyn$(I) = "Y": NEXT I
        tit1$ = "    SOIL TEXTURE                           FIELD     WILTING       SAT. HYD. "
        tit2$ = " --------------------    SAVE  POROSITY   CAPACITY    POINT      CONDUCTIVITY"
        tit3$ = " USER       NAME         Y/N  (VOL/VOL)  (VOL/VOL)  (VOL/VOL)      (CM/SEC)  "
91112   LOCATE 2, 2: PRINT tit1$;
        LOCATE 3, 2: PRINT tit2$;
        LOCATE 4, 2: PRINT tit3$;
        II = 5
        FOR I = 1 TO nsoils%
           a5$ = SPACE$(5): a9$ = SPACE$(9): a11$ = SPACE$(11): a8$ = SPACE$(8)
           a17$ = SPACE$(17)
           RSET a5$ = LTRIM$(RTRIM$(newsoil$(I, 1)))
           LOCATE II, 2: PRINT USING "\   \"; a5$;
           LSET a17$ = LTRIM$(RTRIM$(newsoil$(I, 2)))
           LOCATE II, 9: PRINT USING "\               \"; a17$;
           LOCATE II, 28: PRINT saveyn$(I);
           LOCATE II, 34
           PRINT USING "#.###"; VAL(newsoil$(I, 3))
           LOCATE II, 45
           PRINT USING "#.###"; VAL(newsoil$(I, 4))
           LOCATE II, 56
           PRINT USING "#.###"; VAL(newsoil$(I, 5))
           LOCATE II, 66
           PRINT USING "#.#######^^^^"; VAL(newsoil$(I, 6))
           II = II + 1
        NEXT I
        ro = 5: l% = 1: IFIN% = nsoils%
'91222   f2help$ = "#SOILNAME"
91222  CALL ACELL(ro, 9, 17, newsoil$(l%, 2), 1)
        GOSUB 98190
        IF irtrn% = 1000 OR irtrn% = 100 THEN irtrn% = 100: EXIT SUB
        IF irtrn% = 3 OR irtrn% = 7 THEN
           IF ro = 5 THEN
              ro = nsoils% + 4: l% = nsoils%
           ELSE
              ro = ro - 1: l% = l% - 1
           END IF
           IF irtrn% = 7 THEN GOTO 91233
           GOTO 91222
        END IF
        IF irtrn% = 5 THEN GOTO 91233
        IF irtrn% = 4 THEN
           IF l% < nsoils% THEN
              l% = l% + 1
              ro = ro + 1
           ELSE
              ro = 5: l% = 1
           END IF
        END IF
        IF irtrn% = 2 THEN GOSUB 91333
        IF irtrn% = 6 THEN GOTO 91233
        GOTO 91222
91233   CALL ACELL(ro, 28, 1, saveyn$(l%), 1)
        IF UCASE$(saveyn$(l%)) <> "Y" AND UCASE$(saveyn$(l%)) <> "N" THEN
           saveyn$(l%) = "Y"
           LOCATE ro, 28: PRINT saveyn$(l%)
        END IF
        GOSUB 98190
        IF irtrn% = 1000 OR irtrn% = 100 THEN irtrn% = 100: EXIT SUB
        IF irtrn% = 3 THEN
           IF ro = 5 THEN
              ro = nsoils% + 4: l% = nsoils%
           ELSE
              ro = ro - 1: l% = l% - 1
           END IF
           GOTO 91233
        END IF
        IF irtrn% = 7 THEN GOTO 91222
        IF irtrn% = 4 OR irtrn% = 5 OR irtrn% = 6 THEN
           IF l% < nsoils% THEN
              l% = l% + 1
              ro = ro + 1
           ELSE
              ro = 5: l% = 1
           END IF
           IF irtrn% = 5 OR irtrn% = 6 THEN GOTO 91222
           GOTO 91233
        END IF
        IF irtrn% = 2 THEN GOSUB 91333
        GOTO 91233
EXIT SUB
'''''''''''''''''''''''''
'SAVE DATA TO USER FILE '
'''''''''''''''''''''''''
91333 '  FOR I = 1 TO nsoils%
       '    IF LTRIM$(RTRIM$(UCASE$(saveyn$(I)))) = "Y" THEN GOTO 92233
       ' NEXT I
       ' RETURN
92233   BLANK$ = "                 "
        A$ = DIR$("SOIL.USR")
        IF A$ <> "" THEN
           OPEN "SOIL.USR" FOR INPUT AS #1
           OPEN "TEMPCOPY.TMP" FOR OUTPUT AS #2
           I = 1
91113      IF EOF(1) THEN GOTO 92223
           LINE INPUT #1, li$
           IF I = 1 THEN usnum1 = VAL(LEFT$(li$, 5))
           PRINT #2, li$
           I = 2
           GOTO 91113
        END IF
92223   usnum = VAL(LEFT$(li$, 5))
        CLOSE
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''
        'IF THE NEW USER SOILS HAVE A NUMBER GREATER THAN THE
        'LARGEST NUMBER STORED THEN SIMPLY APPEND THE NEW
        'SOILS TO THE SOIL.USR FILE
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''
        IF VAL(newsoil$(1, 1)) > usnum THEN
           OPEN "SOIL.USR" FOR APPEND AS #1
           FOR l = 1 TO nsoils%
              IF LTRIM$(RTRIM$(UCASE$(saveyn$(l)))) = "Y" THEN GOSUB 92244
           NEXT l
           CLOSE
           GOTO 91444
        END IF
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        'IF THE LARGEST NUMBER IN THE NEW SOIL TEXTURES IS
        'SMALLER THAN THE SMALLEST EXISTING NUMBER THEN ADD
        'ALL NEW SOIL TEXTURES TO THE TOP OF THE EXISTING SOIL
        'TEXTURES in soil.usr file
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        IF VAL(newsoil$(nsoils%, 1)) < usnum1 THEN
           OPEN "TEMPCOPY.TMP" FOR INPUT AS #2
           OPEN "SOIL.USR" FOR OUTPUT AS #1
           FOR l = 1 TO nsoils%
              IF LTRIM$(RTRIM$(UCASE$(saveyn$(l)))) = "Y" THEN GOSUB 92244
           NEXT l
           FOR I = 1 TO SOILN%
              LINE INPUT #2, li$
              PRINT #1, li$
           NEXT I
           CLOSE
           GOTO 91444
        END IF
        ''''''''''''''''''''''''''''''''''''''''''''''''''''
        'OTHERWISE PLACE THE NEW SOILS IN SOIL.USR
        'ACCORDING TO THEIR TEXTURE NUMBER.  THIS REQUIRES
        'INSERTION ABOVE, BETWEEN, OR BELOW THE EXISTING
        'USER SOILS.
        ''''''''''''''''''''''''''''''''''''''''''''''''''''
        OPEN "TEMPCOPY.TMP" FOR INPUT AS #2
        OPEN "SOIL.USR" FOR OUTPUT AS #1
        newsoil$(nsoils% + 1, 1) = "9999"
        l = 1
        FOR I = 1 TO SOILN%
           IF l > nsoils% THEN l = nsoils% + 1
           LINE INPUT #2, li$
           SNUMBER = VAL(LEFT$(li$, 5))
91433      IF SNUMBER > VAL(newsoil$(l, 1)) THEN
              IF LTRIM$(RTRIM$(UCASE$(saveyn$(l)))) = "Y" THEN
                 GOSUB 92244
                 l = l + 1
                 IF l > nsoils% THEN GOTO 92433
                 GOTO 91433
              ELSE
                 l = l + 1
                 IF l > nsoils% THEN GOTO 92433
                 GOTO 91433
              END IF
           ELSEIF SNUMBER < VAL(newsoil$(l, 1)) THEN
              PRINT #1, li$
           ELSEIF SNUMBER = VAL(newsoil$(l, 1)) THEN
              IF LTRIM$(RTRIM$(UCASE$(saveyn$(l)))) = "Y" THEN
                 GOSUB 92244
                 l = l + 1
                 IF l > nsoils% THEN GOTO 92433
                 GOTO 92433
              ELSE
                 l = l + 1
              END IF
           END IF
92433   NEXT I
93433   IF l < nsoils% + 1 THEN
           IF LTRIM$(RTRIM$(UCASE$(saveyn$(l)))) = "Y" THEN
              GOSUB 92244
              l = l + 1
              GOTO 93433
           END IF
        END IF
91444   CLOSE
        IF DIR$("TEMPCOPY.TMP") <> "" THEN KILL ("TEMPCOPY.TMP")
        EXIT SUB
      RETURN
EXIT SUB
'''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO WRITE VALUES TO SOIL.USR FILE
'''''''''''''''''''''''''''''''''''''''''''''''
92244  PRINT #1, USING "#####"; VAL(newsoil$(l, 1));
       IF LTRIM$(RTRIM$(newsoil$(l, 2))) = "" THEN newsoil$(l, 2) = "UNDEFINED"
       PRINT #1, "  "; : PRINT #1, USING "\               \"; newsoil$(l, 2);
       PRINT #1, "           "; : PRINT #1, USING "\       \"; newsoil$(l, 3);
       PRINT #1, "  "; : PRINT #1, USING "\       \"; newsoil$(l, 4);
       PRINT #1, "  "; : PRINT #1, USING "\       \"; newsoil$(l, 5);
       PRINT #1, USING "\           \"; newsoil$(l, 6);
       PRINT #1,
RETURN
''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO CALL MANUAL MESSAGES FOR HELP
'''''''''''''''''''''''''''''''''''''''''''''''''
98190  IF irtrn% = 21 THEN CALL HLPMSG(f2help$)
       IF irtrn% = 20 THEN CALL HLPMSG(f1help$)
       IF irtrn% = 22 THEN CALL USERHELP
       RETURN
END SUB

SUB EXOUT (ANSCODE)
'''''''''''''''''''''''''''''''''''''''''''
'THIS SUBROUTINE ASKS IS USED AFTER AN ESC
' OR AN F9 KEY IS PRESSED.
'IT ASKS THE USER WHETHER TO QUIT AND LOSE
'DATA/CHANGES, OR NOT.
'ANSCODE = 0 MEANS YES LOSE DATA AND QUIT
'ANSCODE = 1 MEANS NO DO NOT EXIT
'''''''''''''''''''''''''''''''''''''''''''
76020   INK% = irtrn%
        SROWBOX = 11: SCOLBOX = 27: BOXWIDTH = 27: EROWBOX = 15
        COLOR 15, 1
        CALL BOX
        NO$ = "NO"
        YES$ = "YES"
        LOCATE 12, 28: PRINT " EXIT AND LOSE CHANGES?  ";
        LOCATE 13, 28: PRINT "          YES            ";
        LOCATE 14, 28: PRINT "          NO             ";
76025   CALL RCELL(13, 38, 3, YES$)
        IF irtrn% = 109 THEN
          FOR I = 11 TO 15
             LOCATE I, 27: PRINT "                            ";
          NEXT I
          ANSCODE = 0: irtrn% = 0: EXIT SUB
        END IF
        IF irtrn% = 3 OR irtrn% = 4 THEN GOTO 76022
        IF irtrn% = 5 OR irtrn% = 108 OR irtrn% = 1000 THEN
          irtrn% = INK%
          ANSCODE = 1
          EXIT SUB
        END IF
        GOTO 76025
76022   CALL RCELL(14, 38, 2, NO$)
        IF irtrn% = 3 OR irtrn% = 4 THEN GOTO 76025
        IF irtrn% = 108 OR irtrn% = 1000 THEN ANSCODE = 1: irtrn% = INK%: EXIT SUB
        IF irtrn% = 5 OR irtrn% = 100 OR irtrn% = 109 THEN
          FOR I = 11 TO 15
             LOCATE I, 27: PRINT "                            ";
          NEXT I
          irtrn% = 0: ANSCODE = 0
          EXIT SUB
          END IF
        GOTO 76022
       EXIT SUB

END SUB

SUB filechk (fdrive$, fpath$, filen$, found)
'--------------------------------
'FOUND = 0    INVALID DIRECTORY
'FOUND = 1    ERROR IN PATH
'FOUND = 2    BAD FILE NAME
'FOUND = 3    FILE NOT FOUND
'FOUND = 4    FILE EXISTS
'FOUND = 5    INVALID DRIVE
'------------------------
'CHECK IF DRIVE IS VALID
'------------------------
 IF LTRIM$(RTRIM$(fdrive$)) = "" THEN
      fdrive$ = LEFT$(CURDIR$, 1)
      IF LTRIM$(RTRIM$(fpath$)) = "" THEN fpath$ = RIGHT$(CURDIR$, LEN(CURDIR$) - 3)
 END IF
ON LOCAL ERROR RESUME NEXT
   found = -1: ERR = 0
   DR$ = LEFT$(CURDIR$, 1)
   IF DR$ <> fdrive$ THEN
      CHDRIVE fdrive$
      IF ERR <> 0 THEN
        found = 5                             '---INVALID DRIVE---
        ERR = 0
        CHDIR CURDIR$
        EXIT SUB
      END IF
   END IF
   CHDRIVE DR$
'------------------------------
'CHECK IF DIRECTORY IS VALID
'------------------------------
   DEFDIR$ = CURDIR$
   fpath$ = LTRIM$(RTRIM$(fpath$))
   IF fpath$ = "" THEN GOTO 203
   IF RIGHT$(fpath$, 1) <> "\" THEN
     CHDIR fdrive$ + ":" + fpath$
   ELSE
     DIRLEN = LEN(fpath$)
     FILEP$ = LEFT$(fpath$, DIRLEN - 1)
     CHDIR fdrive$ + ":" + FILEP$
   END IF
   IF ERR <> 0 THEN
     found = 0: ERR = 0              '---DIRECTORY ERROR---
     CHDIR DEFDIR$
     EXIT SUB
   END IF
   CHDIR DEFDIR$
'------------------------
'check if path is invalid
'------------------------
202  IF fpath$ = "" THEN GOTO 203
     A$ = DIR$(fdrive$ + ":" + fpath$ + "\X.X")
     IF ERR <> 0 THEN ERR = 0: found = 1: EXIT SUB    '---ERROR IN PATH--
'------------------------------------
'CHECK IF BAD FILE NAME OR FILE FOUND
'------------------------------------
203  A$ = DIR$(filen$)
   IF ERR <> 0 THEN ERR = 0: found = 2: EXIT SUB    '---ERROR IN FILE NAME--
   IF LTRIM$(RTRIM$(fpath$)) = "" THEN
      A$ = DIR$(fdrive$ + ":" + filen$)
   ELSE
      A$ = DIR$(fdrive$ + ":" + fpath$ + "\" + filen$)
   END IF
   IF ERR <> 0 THEN ERR = 0: found = 1: EXIT SUB  '---Invalid path--
   IF LEN(A$) = 0 THEN
     found = 3: EXIT SUB                           '---File not found--
   ELSE
     found = 4: EXIT SUB                           '---FILE ALREADY EXISTS--
   END IF

END SUB

SUB GETDIR (EXT$, SELECT$, DIRECTRY$)
     DIM F$(120), ICLN%(6), BEG%(120), LENG%(120), TMP$(120)
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' This subroutine gets and displays the directory of files   '
' *.D10                                                      '
' and allows the user to select a file                       '
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
     DIRECTOR$ = LTRIM$(RTRIM$(DIRECTRY$))
     IF RIGHT$(DIRECTOR$, 1) <> "\" THEN DIRECTOR$ = DIRECTOR$ + "\"
     DIRECTRY$ = DIRECTOR$
     PCOPY VPAGE%, HPAGE%
     irtrn% = 0
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
8103 A$ = DIR$(DIRECTOR$ + "*." + EXT$)
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
' K = First row number for lsiting tabulated values          '
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
         IF irtrn% = 100 THEN GOTO 19197
'MOVE UP
         IF irtrn% = 3 THEN
         IF (irtrn% = 3) AND (IROW = FIRSTROW + 1) THEN
           IF FIX(TOTNUM / NUMINCOL) * NUMINCOL >= I * NUMINCOL THEN
              IROW = NUMINCOL + FIRSTROW
              GOTO 19901
           ELSE
              IROW = FIRSTROW + TOTNUM - FIX(TOTNUM / NUMINCOL) * NUMINCOL
              GOTO 19901
           END IF
         END IF
         IF (irtrn% = 3) AND (IROW > 3) THEN IROW = IROW - 1
         IF (irtrn% = 4) AND (IROW = NUMINCOL + 2) THEN
           IROW = FIRSTROW + 1
           GOTO 19901
         END IF
         GOTO 19901
         END IF
'MOVE DOWN THE COLUMN
       IF (irtrn% = 4) AND (IROW <= NUMINCOL + FIRSTROW) THEN
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
        IF irtrn% = 6 THEN
        IF (irtrn% = 6) AND (I = 6) THEN
          I = 1
          GOTO 19901
      END IF
      IF (irtrn% = 6) AND (I < 6) THEN
         IF k + NUMINCOL > TOTNUM THEN
             I = 1
         ELSE
             I = I + 1
         END IF
      END IF
      GOTO 19901
      END IF
'MOVE LEFT
      IF (irtrn% = 7) AND (I = 1) THEN
          IF IROW - FIRSTROW + NUMINCOL <= TOTNUM THEN I = 2
          IF IROW - FIRSTROW + NUMINCOL * 2 <= TOTNUM THEN I = 3
          IF IROW - FIRSTROW + NUMINCOL * 3 <= TOTNUM THEN I = 4
          IF IROW - FIRSTROW + NUMINCOL * 4 <= TOTNUM THEN I = 5
          IF IROW - FIRSTROW + NUMINCOL * 5 <= TOTNUM THEN I = 6
          GOTO 19901
      END IF
      IF (irtrn% = 7) AND (I > 1) THEN I = I - 1: GOTO 19901
'SELECT HIGHLIGHTED ITEM
      IF irtrn% = 5 THEN
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
        LOCATE 24, 15: PRINT DIRECTOR$;
        RETURN
        EXIT SUB
END SUB

SUB HLPMSG (f2help$)
     DIM NAME$(40), LOCHELP%(40, 2), li$(500)
     PCOPY VPAGE%, HPAGE%
     PCOPY VPAGE%, APAGE%
     CLS APAGE%
     SCREEN 0, , VPAGE%, VPAGE%
     A$ = DIR$("SOILHELP.HLP")
     B$ = DIR$("SOILTAGS.HLP")
     IF A$ = "" OR B$ = "" THEN
       IF B$ = "" THEN LOCATE 10, 16: PRINT "SOILTAGS.HLP FILE IS NOT FOUND ON THE DEFAULT DIRECTORY"
       IF A$ = "" THEN LOCATE 11, 16: PRINT "SOILHELP.HLP FILE IS NOT FOUND ON THE DEFAULT DIRECTORY"
       LOCATE 13, 29: PRINT "PRESS ANY KEY TO CONTINUE"
901052  INK$ = INKEY$: IF INK$ = "" THEN GOTO 901052
        GOTO 99197
     END IF
     LINES% = 0
     COLOR 15, 1
     CLS
     COLOR 4, 3
     LOCATE 25, 1: PRINT "     to scroll up and down      PgDn/PgUp      Home      End     F9/ESC =Cancel ";
     LOCATE 25, 3: PRINT CHR$(24); CHR$(25);
     COLOR 15, 1

     OPEN "SOILTAGS.HLP" FOR INPUT AS #2
     FOR I = 1 TO 40
        IF EOF(2) THEN GOTO 90052
        LINE INPUT #2, A$
        NAME$(I) = LTRIM$(RTRIM$(A$))
        INPUT #2, LOCHELP%(I, 1)
        INPUT #2, LOCHELP%(I, 2)
      NEXT I
90052  CLOSE
      OPEN "SOILHELP.HLP" FOR RANDOM AS #1 LEN = 80
      FIELD #1, 80 AS l$
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'DETERMINE STARTING POINT OF THE REQUIRED HELP MESSAGE BASED ON THE
'TAG PASSED FROM THE PROGRAM
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
      FOR I = 1 TO 40
        IF NAME$(I) = LTRIM$(RTRIM$(f2help$)) THEN
          INIT% = LOCHELP%(I, 1)
          IEND% = LOCHELP%(I, 2)
          GOTO 90010
        END IF
      NEXT I
      GOTO 99197
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'READ THE HELP LINES AND STORE THEM INTO AN ARRAY FOR PROCESSING
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
90010   LINES% = 1
        FOR I% = INIT% + 1 TO IEND%
          GET #1, I%
          IF LEFT$(l$, 1) = "#" THEN GOTO 90014
          LINES% = LINES% + 1
          li$(LINES%) = l$
90014   NEXT I%
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'DISPLAY THE HELP MESSAGE ON THE SCEEN 24 LINES AT A TIME
'AND SHOW A LINE MENU AT THE BOTTOM OF THE SCREEN TO GIVE
'GUIDANCE ON SCROLLING UP AND DOWN THE HELP MENU.
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
90020  IF LINES% <= 24 THEN
          FOR I% = 1 TO LINES%
            LOCATE I%, 1: PRINT li$(I%);
          NEXT I%
        ELSE
          INI% = 1
          IFIN% = 24
          FOR I% = INI% TO IFIN%
             LOCATE I%, 1: PRINT li$(I%);
          NEXT I%
        END IF
90030   KSTRG$ = "": KSTRG$ = INKEY$: IF KSTRG$ = "" THEN GOTO 90030
        NL = ASC(MID$(KSTRG$, 1))
        IF LEN(KSTRG$) = 2 THEN NH = ASC(MID$(KSTRG$, 2))
'''''''''''''''''''''''''''''''
'PAGE DOWN
'''''''''''''''''''''''''''''''
           IF NL = 0 AND NH = 81 THEN
             IF LINES% <= 24 THEN GOTO 90030
             IF LINES% - IFIN% <= 24 THEN
                IFIN% = LINES%: INI% = IFIN% - 23
                IF LINES% <= 24 THEN
                   INI% = 1
                END IF
             ELSE
                INI% = INI% + 24
                IFIN% = IFIN% + 24
             END IF
           GOSUB 90050
'''''''''''''''''''''''''''''''
'PAGE UP
'''''''''''''''''''''''''''''''
           ELSEIF NL = 0 AND NH = 73 THEN
             IF LINES% <= 24 THEN
                GOTO 90030
             ELSE
               IF INI% - 24 < 0 THEN
                  INI% = 1: IFIN% = 24
               ELSE
                  INI% = INI% - 24
                  IFIN% = IFIN% - 24
               END IF
             END IF
             GOSUB 90050
'''''''''''''''''''''''''''''''
'MOVE UP
'''''''''''''''''''''''''''''''
           ELSEIF (NL = 0) AND (NH = 72) THEN
              IF LINES% <= 24 THEN GOTO 90030
              IF INI% <> 1 THEN
                 INI% = INI% - 1
                 IF IFIN% > 24 THEN
                   IFIN% = IFIN% - 1
                 ELSE
                   IFIN% = LINES%
                 END IF
              ELSE
                 IFIN% = 24
              END IF
             GOSUB 90050
'''''''''''''''''''''''''''''''
'MOVE DOWN
'''''''''''''''''''''''''''''''
           ELSEIF (NL = 0) AND (NH = 80) THEN
              IF LINES% <= 24 THEN GOTO 90030
              IF IFIN% <> LINES% THEN
                 IFIN% = IFIN% + 1
                 INI% = INI% + 1
              END IF
             GOSUB 90050
'''''''''''''''''''''''''''''''
'MOVE TO HOME
'''''''''''''''''''''''''''''''
           ELSEIF NL = 0 AND NH = 71 THEN
              INI% = 1
              IF LINES% >= 24 THEN
                 IFIN% = 24
              ELSE
                 IFIN% = LINES%
              END IF
             GOSUB 90050
'''''''''''''''''''''''''''''''
'MOVE TO END LOCATION
'''''''''''''''''''''''''''''''
           ELSEIF NL = 0 AND NH = 79 THEN
              IFIN% = LINES%
              IF LINES% > 23 THEN
                 INI% = LINES% - 23
              ELSE
                 INI% = 1
              END IF
             GOSUB 90050
'''''''''''''''''''''''''''''''
'
'''''''''''''''''''''''''''''''
           ELSEIF NL = 0 AND NH = 67 THEN
              GOTO 99197
           ELSEIF NL = 27 THEN
               GOTO 99197
           END IF
           GOTO 90030
       GOTO 99197
''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO DISPLAY HELP MESSAGE
''''''''''''''''''''''''''''''''''''''
90050  CLS
       COLOR 4, 3
       LOCATE 25, 1: PRINT "     to scroll up and down      PgDn/PgUp      Home      End     F9/ESC =Cancel ";
       LOCATE 25, 3: PRINT CHR$(24); CHR$(25);
       COLOR 15, 1
       ro% = 1
        FOR I% = INI% TO IFIN%
          LOCATE ro%, 1
          PRINT li$(I%);
          ro% = ro% + 1
        NEXT I%
        RETURN
99197 CLS : PCOPY HPAGE%, VPAGE%
      EXIT SUB
END SUB

SUB menu
      COLOR 4, 3
      LOCATE 25, 1: PRINT "  F1=Info   F2=Help   F3=Keys   F9=Quit   F10=End   PgUp=Return   PgDn=Proceed  ";
      COLOR 15, 1
  EXIT SUB
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
  IF NL = 0 AND NH = 81 THEN irtrn% = 1: COLOR 15, 1: EXIT SUB
  IF NL = 0 AND NH = 73 THEN irtrn% = -1: COLOR 15, 1: EXIT SUB
  IF (NL = 0) AND (NH = 68) THEN irtrn% = 2: COLOR 15, 1: EXIT SUB
  IF (NL = 0) AND (NH = 72) THEN irtrn% = 3: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$; : EXIT SUB
  IF (NL = 0) AND (NH = 80) THEN irtrn% = 4: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$; : EXIT SUB
  IF (ASC(KSTRG$) = 13) THEN irtrn% = 5: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF (NL = 9) OR (NL = 0 AND NH = 77) THEN irtrn% = 6: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF (NL = 0 AND NH = 15) OR (NL = 0 AND NH = 75) THEN irtrn% = 7: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 0 AND NH = 71 THEN irtrn% = 8: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 0 AND NH = 79 THEN irtrn% = 9: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 0 AND NH = 61 THEN irtrn% = 22: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 0 AND NH = 62 THEN irtrn% = 23: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 0 AND NH = 67 THEN irtrn% = 1000: EXIT SUB
  IF (ITYPE = 2) AND (NL = 73 OR NL = 105) THEN irtrn% = 10: COLOR 15, 1: EXIT SUB
  IF NL = 0 AND NH = 59 THEN irtrn% = 20: COLOR 15, 0: EXIT SUB
  IF NL = 0 AND NH = 60 THEN irtrn% = 21: COLOR 15, 0: EXIT SUB
  IF NL = 0 AND NH = 30 THEN irtrn% = 30: COLOR 15, 1: EXIT SUB
  IF NL = 0 AND NH = 48 THEN irtrn% = 31: COLOR 15, 1: EXIT SUB
  IF NL = 27 THEN irtrn% = 100: COLOR 15, 1: EXIT SUB
  IF NL = 0 AND NH = 63 THEN irtrn% = 40: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 0 AND NH = 64 THEN irtrn% = 41: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 0 AND NH = 65 THEN irtrn% = 42: COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 49 THEN irtrn% = 101:  COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 50 THEN irtrn% = 102:  COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 51 THEN irtrn% = 103:  COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 52 THEN irtrn% = 104:  COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 53 THEN irtrn% = 105:  COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 54 THEN irtrn% = 106:  COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 55 THEN irtrn% = 107:  COLOR 15, 1: LOCATE IROW, ISTART, 0: PRINT TXT$: EXIT SUB
  IF NL = 89 OR NL = 121 THEN irtrn% = 108: COLOR 15, 1: EXIT SUB
  IF NL = 78 OR NL = 110 THEN irtrn% = 109: COLOR 15, 1: EXIT SUB
  GOTO 11221
  EXIT SUB
END SUB

SUB USERHELP
     PCOPY VPAGE%, HPAGE%
     irtrn% = 0
     PCOPY VPAGE%, APAGE%
     CLS APAGE%
     SCREEN 0, , VPAGE%, VPAGE%
     VIEW PRINT 1 TO 25
     LOCATE 1, 8: PRINT "TAB": LOCATE 1, 20: PRINT "SCROLL RIGHT THROUGH CELLS"
     LOCATE 2, 8: PRINT "SHIFT TAB": LOCATE 2, 20: PRINT "SCROLL LEFT THROUGH CELLS"
     LOCATE 3, 8: PRINT CHR$(24): LOCATE 3, 20: PRINT "CURSOR UP"
     LOCATE 4, 8: PRINT CHR$(25): LOCATE 4, 20: PRINT "CURSOR DOWN"
     LOCATE 5, 8: PRINT CHR$(27): LOCATE 5, 20: PRINT "CURSOR LEFT IN CELL"
     LOCATE 6, 8: PRINT CHR$(26): LOCATE 6, 20: PRINT "CURSOR RIGHT IN CELL"
     LOCATE 7, 8: PRINT "<CR>        ENTER A VALUE, SELECT ITEM, OR MOVE TO NEXT CELL"

     LOCATE 9, 8: PRINT "F1/F2/F3    GENERAL HELP/TECHNICAL HELP/INFORMATION ON KEYS"
     LOCATE 10, 8: PRINT "F4          LIST FILES"
     LOCATE 11, 8: PRINT "F5          LIST CITIES"
     LOCATE 12, 8: PRINT "F6          LIST DEFAULT SOIL TEXTURES"
     LOCATE 13, 8: PRINT "F7          LIST USER DEFINED SOIL TEXTURES"
     LOCATE 14, 8: PRINT "F9          QUIT WITHOUT SAVING CHANGES"
     LOCATE 15, 8: PRINT "F10         ACCEPT CHANGES, CONTINUE, OR PROCEED TO FILE SAVING"

     LOCATE 17, 8: PRINT "Alt A       ADD A LINE ABOVE CURSOR LOCATION"
     LOCATE 18, 8: PRINT "Alt B       ADD A LINE BELOW CURSOR LOCATION"
     LOCATE 19, 8: PRINT "Alt C       COPY CURRENT LINE"
     LOCATE 20, 8: PRINT "Alt D       DELETE CURRENT LINE"
     LOCATE 21, 8: PRINT "Alt M       MOVE CURRENT LINE"
     LOCATE 22, 8: PRINT "PgDn        CONTINUE TO NEXT SCREEN";
     LOCATE 23, 8: PRINT "PgUp        RETURN TO PREVIOUS SCREEN";
     LOCATE 24, 8: PRINT "Esc         RETURN TO PREVIOUS OPERATION WITHOUT ACCEPTING CHANGES";
     COLOR 11, 6
     LOCATE 25, 1: PRINT "                            Esc to return to program                            ";
     COLOR 15, 1
10021  KBD$ = "": KBD$ = INKEY$
       IF KBD$ = CHR$(0) + CHR$(59) OR KBD$ = CHR$(13) OR KBD$ = CHR$(27) OR KBD$ = CHR$(0) + CHR$(61) THEN
          CLS : PCOPY HPAGE%, VPAGE%
          EXIT SUB
       ELSE
          GOTO 10021
       END IF
END SUB

