DECLARE SUB FILECHK (FDRIVE$, FPATH$, FILEN$, FOUND!)
DECLARE SUB OUTFILES ()
DECLARE SUB HLPMSG (F2HELP$)
DECLARE SUB USERHELP ()
DECLARE SUB EXOUT (ANSCODE!)
DECLARE SUB RCELL (IROW!, ISTART!, NS!, TXT$)
DECLARE SUB GETDIR (EXT$, SELECT$, directory$)
DECLARE SUB ACELL (IROW!, ISTART!, NS!, TEXT$, ITYPE!)
DECLARE SUB RUNSCREEN ()
DECLARE SUB OUTFREQ (ou%, RY%, D%, M%, A%, S%)
DECLARE SUB BOX ()
COMMON SHARED TOHELP%, irtrn%, PAGE%
COMMON SHARED APAGE%, VPAGE%, HPAGE%, DIRECT$
COMMON SHARED SROWBOX, SCOLBOX, BOXWIDTH, EROWBOX
COMMON SHARED OUTUNITS$, years$, daily$, monthly$, ANNUAL$, summary$
COMMON SHARED DATAFILE$(), FILEPATH$(), FILEOK(), EXT$(), LOCLINE()
DIM DATAFILE$(6), FILEPATH$(6), LOCLINE(6), EXT$(6), FILEOK(6)
   VIEW PRINT 1 TO 25
   APAGE% = 0: VPAGE% = 1: HPAGE% = 2
   SCREEN 0, , VPAGE%, VPAGE%
   KEY OFF: COLOR 15, 1: CLS
   F1HELP$ = "#EXECFILES"
   EXT$(1) = "D4"
   EXT$(2) = "D7"
   EXT$(3) = "D13"
   EXT$(4) = "D11"
   EXT$(5) = "D10"
   EXT$(6) = "OUT"
   A$ = DIR$("OUTPARAM.DAT")
   IF A$ = "" THEN
     OUTUNITS$ = "1"
     daily$ = "N"
     monthly$ = "N"
     ANNUAL$ = "N"
     summary$ = "Y"
   ELSE
      OPEN "OUTPARAM.DAT" FOR INPUT AS #1
      FOR I = 1 TO 6
        INPUT #1, FILENAME$
        LENGTH = LEN(FILENAME$)
        DATAFILE$(I) = MID$(FILENAME$, 1, LENGTH - LEN(EXT$(I)) - 1)
2000    IF INSTR(DATAFILE$(I), "\") <> 0 THEN
           XST = LEN(DATAFILE$(I))
           DATAFILE$(I) = MID$(DATAFILE$(I), INSTR(DATAFILE$(I), "\") + 1, XST)
           GOTO 2000
        END IF
        SLEN = LEN(FILENAME$) - LEN(DATAFILE$(I)) - LEN(EXT$(I)) - 1
        FILEPATH$(I) = LEFT$(FILENAME$, SLEN)
      NEXT I
      INPUT #1, ou%
      INPUT #1, YR%
      INPUT #1, D%
      INPUT #1, M%
      INPUT #1, A%
      INPUT #1, S%
      IF ou% = 2 THEN
          OUTUNITS$ = "2"
      ELSE
          OUTUNITS$ = "1"
      END IF
      years$ = STR$(YR%)
      IF D% = 1 THEN
          daily$ = "Y"
      ELSE
          daily$ = "N"
      END IF
      IF M% = 1 THEN
          monthly$ = "Y"
      ELSE
          monthly$ = "N"
      END IF
      IF A% = 1 THEN
          ANNUAL$ = "Y"
      ELSE
          ANNUAL$ = "N"
      END IF
      IF S% = 1 THEN
          summary$ = "Y"
      ELSE
          summary$ = "N"
      END IF
   END IF
   CLOSE
   nlines = 6
   DEFDIR$ = CURDIR$
   DIRECT$ = CURDIR$
   FOR I = 1 TO nlines
      'FILEPATH$(I) = DIRECT$
      FILEOK(I) = 0
   NEXT I
28000 CALL OUTFILES
        IF irtrn% = 1 THEN GOTO 28500
        IF irtrn% = 1000 OR irtrn% = -1 THEN GOTO 28700
28500 CALL OUTFREQ(ou%, YR%, D%, M%, A%, S%)
      IF irtrn% = -1 THEN GOTO 28000
      IF irtrn% = 1 THEN
         '----------------
         'SHELL TO HELP3O
         '----------------
         GOSUB 16000
         IF DIR$("OUTPARAM.DAT") <> "" THEN
            SCREEN 0, 0, 0, 0
            CLS
            CLEAR
            SHELL "HELP3O"
            SCREEN 0, 1, 1, 1
            COLOR 15, 1: CLS
   SCREEN 0, , VPAGE%, VPAGE%
   CLS
   SCREEN 0, , APAGE%, APAGE%
   CLS
   SCREEN 0, , HPAGE%, HPAGE%
   CLS
         END IF
      END IF
28700 SCREEN 0, , VPAGE%, VPAGE%
   CLS
   SCREEN 0, , APAGE%, APAGE%
   CLS
   SCREEN 0, , HPAGE%, HPAGE%
   CLS
   END
''''''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO WRITE OUTPARAM.DAT FILE
'TO BE READ BY THE COMPUTATIONAL COMPONENT OF HELP
''''''''''''''''''''''''''''''''''''''''''''''''''''
16000 CLOSE #1
      D% = 0: M% = 0: A% = 0: S% = 1
      ou% = 1
      IF OUTUNITS$ = "2" THEN ou% = 2
      YR% = VAL(LTRIM$(RTRIM$(years$)))
      IF UCASE$(daily$) = "Y" THEN D% = 1
      IF UCASE$(monthly$) = "Y" THEN M% = 1
      IF UCASE$(ANNUAL$) = "Y" THEN A% = 1
      OPEN "OUTPARAM.DAT" FOR OUTPUT AS #1
      FOR I = 1 TO 6
        IF RIGHT$(FILEPATH$(I), 1) <> "\" THEN
          PRINT #1, FILEPATH$(I) + "\" + DATAFILE$(I) + "." + EXT$(I)
        ELSE
          PRINT #1, FILEPATH$(I) + DATAFILE$(I) + "." + EXT$(I)
        END IF
      NEXT I
      PRINT #1, USING "##"; ou%
      PRINT #1, USING "####"; YR%
      PRINT #1, USING "#"; D%
      PRINT #1, USING "#"; M%
      PRINT #1, USING "#"; A%
      PRINT #1, USING "#"; S%
      CLOSE #1
RETURN

11000 SUB ACELL (IROW, ISTART, NS, TEXT$, ITYPE)
      SCREEN , , VPAGE%, VPAGE%
      BLANKS$ = "                                                                          "
      irtrn% = 0
'''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Subroutine to allow input of filenames and paths    '
' into a reverse video cell of prescribed dimensions. '
'''''''''''''''''''''''''''''''''''''''''''''''''''''''
' *************************************************************************
' * NS     = Cell width                                                   *
' * ITYPE  = Flag for type of variable                                    *
' *      0 = Numeric (But accepts E - for use in conductivity)            *
' *      1 = Alphanumeric                                                 *
' *      2 = Positive numeric only                                        *
' *      3 = ALPHANUMERIC BUT ONLY FOR YES OR NO ANSWERS                  *
' * IPOS   = Cursor location.  IPOS î [1,NS]                              *
' * NLEN   = Length of variable (used to place a decimal point if needed) *
' *************************************************************************
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

SUB FILECHK (FDRIVE$, FPATH$, FILEN$, FOUND)
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
 IF LTRIM$(RTRIM$(FDRIVE$)) = "" THEN
      FDRIVE$ = LEFT$(CURDIR$, 1)
      IF LTRIM$(RTRIM$(FPATH$)) = "" THEN FPATH$ = RIGHT$(CURDIR$, LEN(CURDIR$) - 3)
 END IF
ON LOCAL ERROR RESUME NEXT
   FOUND = -1: ERR = 0
   DR$ = LEFT$(CURDIR$, 1)
   IF DR$ <> FDRIVE$ THEN
      CHDRIVE FDRIVE$
      IF ERR <> 0 THEN
        FOUND = 5                             '---INVALID DRIVE---
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
   FPATH$ = LTRIM$(RTRIM$(FPATH$))
   IF FPATH$ = "" THEN GOTO 203
   IF RIGHT$(FPATH$, 1) <> "\" THEN
     IF LEFT$(FPATH$, 1) <> "\" THEN
       CHDIR FDRIVE$ + ":\" + FPATH$
     ELSE
       CHDIR FDRIVE$ + ":" + FPATH$
     END IF
   ELSE
     DIRLEN = LEN(FPATH$)
     IF DIRLEN > 1 THEN FILEP$ = LEFT$(FPATH$, DIRLEN - 1)
     IF DIRLEN = 1 THEN FILEP$ = LEFT$(FPATH$, DIRLEN)
     CHDIR FDRIVE$ + ":" + FILEP$
   END IF
   IF ERR <> 0 THEN
     FOUND = 0: ERR = 0              '---DIRECTORY ERROR---
     CHDIR DEFDIR$
     EXIT SUB
   END IF
   CHDIR DEFDIR$
'------------------------
'check if path is invalid
'------------------------
202  IF FPATH$ = "" THEN GOTO 203
   IF RIGHT$(FPATH$, 1) <> "\" THEN
     A$ = DIR$(FDRIVE$ + ":" + FPATH$ + "\X.X")
   ELSE
     A$ = DIR$(FDRIVE$ + ":" + FPATH$ + "X.X")
   END IF
     IF ERR <> 0 THEN ERR = 0: FOUND = 1: EXIT SUB    '---ERROR IN PATH--
'------------------------------------
'CHECK IF BAD FILE NAME OR FILE FOUND
'------------------------------------
203  A$ = DIR$(FILEN$)
   IF ERR <> 0 THEN ERR = 0: FOUND = 2: EXIT SUB    '---ERROR IN FILE NAME--
   IF LTRIM$(RTRIM$(FPATH$)) = "" THEN
      A$ = DIR$(FDRIVE$ + ":" + "\" + FILEN$)
   ELSE
     IF RIGHT$(FPATH$, 1) <> "\" THEN
       A$ = DIR$(FDRIVE$ + ":" + FPATH$ + "\" + FILEN$)
     ELSE
       A$ = DIR$(FDRIVE$ + ":" + FPATH$ + FILEN$)
     END IF
   END IF
   IF ERR <> 0 THEN ERR = 0: FOUND = 1: EXIT SUB  '---Invalid path--
   IF LEN(A$) = 0 THEN
     FOUND = 3: EXIT SUB                           '---File not found--
   ELSE
     FOUND = 4: EXIT SUB                           '---FILE ALREADY EXISTS--
   END IF
EXIT SUB
END SUB

SUB GETDIR (EXT$, SELECT$, directry$)
 DIM F$(120), ICLN%(6), BEG%(120), LENG%(120), TMP$(120)
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' This subroutine gets and displays the directory of files   '
' and allows the user to select a file                       '
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
     director$ = LTRIM$(RTRIM$(directry$))
     IF RIGHT$(director$, 1) <> "\" THEN director$ = director$ + "\"
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
' This portion of the routine lists all available data files '
' and allows the user select a file to retrun to the         '
' calling routine, delete a file, change directory or drive  '
' K = First row number for lsiting tabulated values          '
' NUMINCOL= Number of values per column                      '
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
        LOCATE 24, 15: PRINT director$;
        RETURN
        EXIT SUB


 END SUB

SUB HLPMSG (F2HELP$)
     DIM NAME$(23), LOCHELP%(23, 2), LI$(500)
     PCOPY VPAGE%, HPAGE%
     PCOPY VPAGE%, APAGE%
     CLS APAGE%
     SCREEN 0, , VPAGE%, VPAGE%
     A$ = DIR$("MAINHELP.HLP")
     B$ = DIR$("MAINTAGS.HLP")
     IF A$ = "" OR B$ = "" THEN
       IF B$ = "" THEN LOCATE 10, 16: PRINT "MAINTAGS.HLP FILE IS NOT FOUND ON THE DEFAULT DIRECTORY"
       IF A$ = "" THEN LOCATE 11, 16: PRINT "MAINHELP.HLP FILE IS NOT FOUND ON THE DEFAULT DIRECTORY"
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

     OPEN "MAINTAGS.HLP" FOR INPUT AS #2
     FOR I = 1 TO 23
        IF EOF(2) THEN GOTO 90052
        LINE INPUT #2, A$
        NAME$(I) = LTRIM$(RTRIM$(A$))
        INPUT #2, LOCHELP%(I, 1)
        INPUT #2, LOCHELP%(I, 2)
      NEXT I
90052  CLOSE
      OPEN "MAINHELP.HLP" FOR RANDOM AS #1 LEN = 80
      FIELD #1, 80 AS L$
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'DETERMINE STARTING POINT OF THE REQUIRED HELP MESSAGE BASED ON THE
'TAG PASSED FROM THE PROGRAM
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
      FOR I = 1 TO 23
        IF NAME$(I) = LTRIM$(RTRIM$(F2HELP$)) THEN
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
          IF LEFT$(L$, 1) = "#" THEN GOTO 90014
          LINES% = LINES% + 1
          LI$(LINES%) = L$
90014   NEXT I%
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'DISPLAY THE HELP MESSAGE ON THE SCEEN 24 LINES AT A TIME
'AND SHOW A LINE MENU AT THE BOTTOM OF THE SCREEN TO GIVE
'GUIDANCE ON SCROLLING UP AND DOWN THE HELP MENU.
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
90020  IF LINES% <= 24 THEN
          FOR I% = 1 TO LINES%
            LOCATE I%, 1: PRINT LI$(I%);
          NEXT I%
        ELSE
          INI% = 1
          IFIN% = 24
          FOR I% = INI% TO IFIN%
             LOCATE I%, 1: PRINT LI$(I%);
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
       RO% = 1
        FOR I% = INI% TO IFIN%
          LOCATE RO%, 1
          PRINT LI$(I%);
          RO% = RO% + 1
        NEXT I%
        RETURN
99197 CLS : PCOPY HPAGE%, VPAGE%
      EXIT SUB
END SUB

SUB OUTFILES
   '------------------------------------
   'ASSIGN CURRENT DIRECTORY AS DEFAULT
   'FOR ALL FILE PATHS
   '------------------------------------
   BLANK$ = "                                                        "
   DEFDIR$ = CURDIR$
   DIRECT$ = CURDIR$
   F1HELP$ = "#EXECFILES"
   nlines = 6
   IF LTRIM$(RTRIM$(DATAFILE$(1))) = "" THEN DATAFILE$(1) = "DATA4"
   IF LTRIM$(RTRIM$(DATAFILE$(2))) = "" THEN DATAFILE$(2) = "DATA7"
   IF LTRIM$(RTRIM$(DATAFILE$(3))) = "" THEN DATAFILE$(3) = "DATA13"
   IF LTRIM$(RTRIM$(DATAFILE$(4))) = "" THEN DATAFILE$(4) = "DATA11"
   IF LTRIM$(RTRIM$(DATAFILE$(5))) = "" THEN DATAFILE$(5) = "DATA10"
   IF LTRIM$(RTRIM$(DATAFILE$(6))) = "" THEN DATAFILE$(6) = "OUTDATA"
   FOR I = 1 TO nlines
      'FILEPATH$(I) = DIRECT$
      FILEOK(I) = 0
      LOCLINE(I) = 6 + I
   NEXT I
7101 CLS
   irtrn% = 0
   ON ERROR RESUME NEXT
   FOR I = 1 TO nlines
       LOCLINE(I) = 6 + I
   NEXT I
   COLOR 11, 6
   LOCATE 1, 1:  PRINT "                        EXECUTION FILES - FILE MANAGEMENT                       ";
   COLOR 15, 1
   COLOR 4, 3
   LOCATE 25, 1: PRINT "  F1=Info   F2=Help   F3=Keys   F9=Quit   F10=End   PgUp=Cycle    PgDn=Proceed  ";
  'LOCATE 25, 1: PRINT "     F1=Info        F2=Help        F3=Keys       F9=Quit        PgDn=Proceed    ";
   COLOR 15, 1
   SROWBOX = 5: SCOLBOX = 3: BOXWIDTH = 75: EROWBOX = 13
   CALL BOX
   LOCATE 6, 26: PRINT "-----------------DIRECTORY--------------": LOCATE 6, 68: PRINT "--FILE--"
   LOCATE 7, 5:  PRINT "PRECIPITATION"
   LOCATE 8, 5:  PRINT "TEMPERATURE"
   LOCATE 9, 5:  PRINT "SOLAR RADIATION"
   LOCATE 10, 5: PRINT "EVAPOTRANSPIRATION"
   LOCATE 11, 5: PRINT "SOIL AND DESIGN"
   LOCATE 12, 5: PRINT "OUTPUT"
   F2HELP$ = "#EPATH"
   GOSUB 78165
   FOR I = 1 TO 6
     LOCATE LOCLINE(I), 26: PRINT FILEPATH$(I)
     LOCATE LOCLINE(I), 68: PRINT "        "
     LOCATE LOCLINE(I), 68: PRINT DATAFILE$(I)
   NEXT I
   I = 1
   F2HELP$ = "#EPATH"
   GOSUB 78165
   FOR I = 1 TO nlines
     IF LTRIM$(RTRIM$(FILEPATH$(I))) = "" THEN FILEPATH$ = DEFDIR$
     IF RIGHT$(FILEPATH$(I), 1) <> "\" THEN FILEPATH$(I) = FILEPATH$(I) + "\"
102  IF RIGHT$(FILEPATH$(I), 2) = "\\" THEN
       FILEPATH$(I) = LEFT$(FILEPATH$(I), LEN(FILEPATH$(I)) - 1)
       GOTO 102
     END IF
     LOCATE LOCLINE(I), 26: PRINT FILEPATH$(I)
     LOCATE LOCLINE(I), 68: PRINT "        "
     LOCATE LOCLINE(I), 68: PRINT DATAFILE$(I)
   NEXT I
   '-------------------------
   'BEGIN SCREEN INPUT
   '-------------------------
   I = 1
7140 CALL ACELL(LOCLINE(I), 26, 40, FILEPATH$(I), 1)
     LOCATE LOCLINE(nlines) + 2, 25: PRINT "                                                  "
     LOCATE LOCLINE(nlines) + 3, 25: PRINT "                                                  "
     FILEOK(I) = 0
   F2HELP$ = "#EPATH"
     GOSUB 73190
     FILEPATH$(I) = LTRIM$(RTRIM$(FILEPATH$(I)))
     IF FILEPATH$(I) = "" THEN FILEPATH$(I) = CURDIR$
     IF RIGHT$(FILEPATH$(I), 1) <> "\" THEN FILEPATH$(I) = FILEPATH$(I) + "\"
101  IF RIGHT$(FILEPATH$(I), 2) = "\\" THEN
       FILEPATH$(I) = LEFT$(FILEPATH$(I), LEN(FILEPATH$(I)) - 1)
       GOTO 101
     END IF
     '------------------------------------------------
     'IF PGDN IS PRESSED, CHECK THE EXISTENCE OF
     'ALL FILES ENTERED, AND IF THEY DO EXIST
     'PROCEED TO THE NEXT STAGE (READ DATA...)
     'ALSO IF NO FILE NAMES WERE ENTERED, PROCEED
     'WITH THE PROGRAM.
     '------------------------------------------------
     IF irtrn% = 1 OR irtrn% = 2 THEN
        FOR J = 1 TO nlines
          ERR = 0
          IF LTRIM$(RTRIM$(DATAFILE$(J))) = "" THEN FILEOK(J) = 0
          COLOR 15, 1: LOCATE LOCLINE(I), 26: PRINT FILEPATH$(I)
          I = J
          IF LTRIM$(RTRIM$(FILEPATH$(I))) = "" THEN
             FILEPATH$(I) = CURDIR$
             IF RIGHT$(FILEPATH$(I), 1) <> "\" THEN FILEPATH$(I) = FILEPATH$(I) + "\"
2101         IF RIGHT$(FILEPATH$(I), 2) = "\\" THEN
                FILEPATH$(I) = LEFT$(FILEPATH$(I), LEN(FILEPATH$(I)) - 1)
                GOTO 2101
             END IF
             LOCATE LOCLINE(I), 26: PRINT FILEPATH$(I)
          END IF
          GOSUB 83190
          LOCATE LOCLINE(I), 26: PRINT FILEPATH$(I)
          IF FILEOK(I) = 0 GOTO 7140
        NEXT J
        FOR J = 1 TO nlines
           IF FILEOK(J) = 0 THEN GOTO 7140
        NEXT J
        EXIT SUB
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
           IF RIGHT$(FILEPATH$(I), 1) <> "\" THEN FILEPATH$(I) = FILEPATH$(I) + "\"
3101       IF RIGHT$(FILEPATH$(I), 2) = "\\" THEN
              FILEPATH$(I) = LEFT$(FILEPATH$(I), LEN(FILEPATH$(I)) - 1)
              GOTO 3101
           END IF
           LOCATE LOCLINE(I), 26: PRINT FILEPATH$(I)
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
        CALL GETDIR(EXT$(I), SELECT$, FILEPATH$(I))
        IF irtrn% = 1000 OR irtrn% = -1 THEN EXIT SUB
        IF SELECT$ <> "" THEN
           DATAFILE$(I) = SELECT$
           LOCATE LOCLINE(I), 68: PRINT "        "
           LOCATE LOCLINE(I), 68: PRINT DATAFILE$(I)
           SELECT$ = ""
        END IF
        GOSUB 78165
     END IF
     GOTO 7140
7145 CALL ACELL(LOCLINE(I), 68, 8, DATAFILE$(I), 1)
     ERR = 0
   F2HELP$ = "#EFILE"
     LOCATE LOCLINE(nlines) + 2, 25: PRINT "                                                   "
     LOCATE LOCLINE(nlines) + 3, 25: PRINT "                                                  "
     GOSUB 73190
     DATAFILE$(I) = LTRIM$(RTRIM$(DATAFILE$(I)))
     IF irtrn% = 1 OR irtrn% = 2 THEN
        FOR J = 1 TO nlines
          ERR = 0
          IF LTRIM$(RTRIM$(DATAFILE$(J))) = "" THEN FILEOK(J) = 0
          COLOR 15, 1: LOCATE LOCLINE(I), 26: PRINT FILEPATH$(I)
          I = J
          IF LTRIM$(RTRIM$(FILEPATH$(I))) = "" THEN
             FILEPATH$(I) = CURDIR$
             IF RIGHT$(FILEPATH$(I), 1) <> "\" THEN FILEPATH$(I) = FILEPATH$(I) + "\"
4101         IF RIGHT$(FILEPATH$(I), 2) = "\\" THEN
                FILEPATH$(I) = LEFT$(FILEPATH$(I), LEN(FILEPATH$(I)) - 1)
                GOTO 4101
             END IF
             LOCATE LOCLINE(I), 26: PRINT FILEPATH$(I)
          END IF
          GOSUB 83190
          LOCATE LOCLINE(I), 26: PRINT FILEPATH$(I)
          IF FILEOK(I) = 0 GOTO 7145
        NEXT J
        FOR J = 1 TO nlines
           IF FILEOK(J) = 0 THEN GOTO 7145
        NEXT J
        EXIT SUB
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
        CALL GETDIR(EXT$(I), SELECT$, FILEPATH$(I))
        'IF IRTRN% = 1000 THEN GOSUB 5500
        IF SELECT$ <> "" THEN
          DATAFILE$(I) = LTRIM$(RTRIM$(SELECT$)): SELECT$ = ""
          LOCATE LOCLINE(I), 68: PRINT DATAFILE$(I)
        END IF
        GOSUB 78165
     END IF
    GOTO 7145
  EXIT SUB
'----------------------------------
'SUBROUTINE TO PRINT DIRECTORY LINE
'----------------------------------
78165 COLOR 11, 6
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
          FDRIVE$ = LEFT$(CURDIR$, 1)
          FPATH$ = FILEPATH$(I)
       ELSE
          FDRIVE$ = LEFT$(FILEPATH$(I), A1 - 1)
          FPATH$ = RIGHT$(FILEPATH$(I), LEN(FILEPATH$(I)) - A1)
       END IF
       IF FPATH$ = "\" THEN FPATH$ = ""
       FILEN$ = DATAFILE$(I)
       FOUND = -1
       CALL FILECHK(FDRIVE$, FPATH$, FILEN$ + "." + EXT$(I), FOUND)
       IF FOUND = 0 THEN
         LOCATE LOCLINE(nlines) + 2, 35: PRINT "Invalid Directory"; : FILEPATH$(I) = CURDIR$
         LOCATE LOCLINE(I), 26: PRINT LEFT$(BLANK$, 40)
         IF RIGHT$(FILEPATH$(I), 1) <> "\" THEN FILEPATH$(I) = FILEPATH$(I) + "\"
5101     IF RIGHT$(FILEPATH$(I), 2) = "\\" THEN
            FILEPATH$(I) = LEFT$(FILEPATH$(I), LEN(FILEPATH$(I)) - 1)
            GOTO 5101
         END IF
         LOCATE LOCLINE(I), 26: PRINT FILEPATH$(I)
       ELSEIF FOUND = 1 THEN
         LOCATE LOCLINE(nlines) + 2, 35: PRINT "Error in Path"; : FILEPATH$(I) = CURDIR$
         LOCATE LOCLINE(I), 26: PRINT LEFT$(BLANK$, 40)
         IF RIGHT$(FILEPATH$(I), 1) <> "\" THEN FILEPATH$(I) = FILEPATH$(I) + "\"
6101     IF RIGHT$(FILEPATH$(I), 2) = "\\" THEN
            FILEPATH$(I) = LEFT$(FILEPATH$(I), LEN(FILEPATH$(I)) - 1)
            GOTO 6101
         END IF
         LOCATE LOCLINE(I), 26: PRINT FILEPATH$(I)
       ELSEIF FOUND = 2 THEN
         IF LTRIM$(RTRIM$(DATAFILE$(I))) <> "" THEN LOCATE LOCLINE(nlines) + 2, 35: PRINT "Bad File Name";
         DATAFILE$(I) = "        ": LOCATE LOCLINE(I), 68: PRINT DATAFILE$(I)
       ELSEIF FOUND = 3 THEN
         IF I = 6 THEN
            FILEOK(I) = 1
         ELSE
            IF LTRIM$(RTRIM$(DATAFILE$(I))) <> "" THEN LOCATE LOCLINE(nlines) + 2, 35: PRINT "File Not Found";
            DATAFILE$(I) = "        ": LOCATE LOCLINE(I), 68: PRINT DATAFILE$(I)
         END IF
       ELSEIF FOUND = 5 THEN
         LOCATE LOCLINE(nlines) + 2, 35: PRINT "Invalid Drive"; : FILEPATH$(I) = CURDIR$
         LOCATE LOCLINE(I), 26: PRINT LEFT$(BLANK$, 40)
         IF RIGHT$(FILEPATH$(I), 1) <> "\" THEN FILEPATH$(I) = FILEPATH$(I) + "\"
8101     IF RIGHT$(FILEPATH$(I), 2) = "\\" THEN
            FILEPATH$(I) = LEFT$(FILEPATH$(I), LEN(FILEPATH$(I)) - 1)
            GOTO 8101
         END IF
         LOCATE LOCLINE(I), 26: PRINT FILEPATH$(I)
       END IF
       IF I <> 6 THEN
         IF FOUND = 4 THEN
            FILEOK(I) = 1
         ELSE
            FILEOK(I) = 0
         END IF
       ELSE
          IF FOUND = 4 THEN
          IF irtrn% = 1 OR irtrn% = 2 THEN
             LOCATE LOCLINE(I) + 2, 26
             PRINT FILEPATH$(I) + " "; DATAFILE$(I) + "." + EXT$(I)
             LOCATE LOCLINE(I) + 3, 26: PRINT "File Already Exists.  Overwrite ? Y/N "
             ANSER$ = "Y"
6010         CALL ACELL(LOCLINE(I) + 3, 65, 1, ANSER$, 1)
             IF UCASE$(ANSER$) <> "N" THEN ANSER$ = "Y"
             LOCATE LOCLINE(I) + 3, 65: PRINT UCASE$(ANSER$)
             IF irtrn% = 5 THEN
               IF ANSER$ = "Y" THEN
                 ANS$ = "Y"
                 FILESV$ = FDRIVE$ + ":" + FPATH$ + "\" + FILEN$ + "." + "D10"
                 FILEOK(I) = 1
                 RETURN
               ELSE
                 ANS$ = "N"
                 FILEOK(I) = 0
                 RETURN
               END IF
            END IF
            IF irtrn% = 100 OR irtrn% = 1000 THEN RETURN
            GOTO 6010
         ELSE
            LOCATE LOCLINE(I) + 2, 35: PRINT "FILE ALREADY EXISTS."
         END IF
         RETURN
         END IF
       END IF
RETURN
'----------------------------------
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
'----------------------------------
'5500 CALL EXOUT(ANSCODE)
'     IF ANSCODE = 0 THEN
'       GOTO 7101
'     ELSE
'       EXIT SUB
'     END IF
'RETURN
'----------------------------------------------
'SUBROUTINE TO CALL help MESSAGES FOR HELP
'----------------------------------------------
73190  IF irtrn% = 20 THEN CALL HLPMSG(F1HELP$)
       IF irtrn% = 21 THEN CALL HLPMSG(F2HELP$)
       IF irtrn% = 22 THEN CALL USERHELP
      RETURN
END SUB

SUB OUTFREQ (ou%, RY%, D%, M%, A%, S%)
   CLS
   F1HELP$ = "#OUTPUTITEMS"
   LOCATE 11, 35: PRINT "Please Wait..."
  ' OPEN FILEPATH$(1) + "\" + DATAFILE$(1) + "." + EXT$(1) FOR INPUT AS #1
   OPEN FILEPATH$(1) + DATAFILE$(1) + "." + EXT$(1) FOR INPUT AS #1
   LOCATE 10, 25: PRINT "PRECIPITATION YEARS AVAILABLE:"
   FOR I = 1 TO 3
     IF EOF(1) THEN nprec = 0: GOTO 13100
     LINE INPUT #1, L$
   NEXT I
   FOR I = 1 TO 100
      FOR J = 1 TO 37
         IF EOF(1) THEN GOTO 13100
         LINE INPUT #1, L$
      NEXT J
      nprec = I
      LOCATE 10, 55: PRINT USING "###"; nprec;
   NEXT I
13100 LOCATE 10, 55: PRINT USING "###"; nprec;
      CLOSE
     ' OPEN FILEPATH$(2) + "\" + DATAFILE$(2) + "." + EXT$(2) FOR INPUT AS #1
      OPEN FILEPATH$(2) + DATAFILE$(2) + "." + EXT$(2) FOR INPUT AS #1
      LOCATE 11, 25: PRINT "TEMPERATURE YEARS AVAILABLE:"
      FOR I = 1 TO 3
        IF EOF(1) THEN Ntemp = 0: GOTO 13200
        LINE INPUT #1, L$
    NEXT I
      FOR I = 1 TO 100
        FOR J = 1 TO 37
          IF EOF(1) THEN GOTO 13200
          LINE INPUT #1, L$
        NEXT J
        Ntemp = I
        LOCATE 11, 55: PRINT USING "###"; Ntemp;
      NEXT I
13200  LOCATE 11, 55: PRINT USING "###"; Ntemp;
13220  CLOSE
     ' OPEN FILEPATH$(3) + "\" + DATAFILE$(3) + "." + EXT$(3) FOR INPUT AS #1
      OPEN FILEPATH$(3) + DATAFILE$(3) + "." + EXT$(3) FOR INPUT AS #1
      LOCATE 12, 25: PRINT "SOLAR RADIATION YEARS AVAILABLE:"
      FOR I = 1 TO 3
        IF EOF(1) THEN NSOLR = 0: GOTO 13300
        LINE INPUT #1, L$
      NEXT I
      FOR I = 1 TO 100
        FOR J = 1 TO 37
          IF EOF(1) THEN GOTO 13300
          LINE INPUT #1, L$
        NEXT J
        NSOLR = I
        LOCATE 12, 57: PRINT USING "###"; NSOLR;
      NEXT I
13300 LOCATE 12, 57: PRINT USING "###"; NSOLR;
13330 CLOSE
      years = nprec
      IF Ntemp < years THEN
         years = Ntemp
      ELSEIF NSOLR < years THEN
         years = NSOLR
      END IF
      MINYRS$ = LTRIM$(RTRIM$(STR$(years)))
      years$ = MINYRS$
   FOR I = 1 TO 12000: NEXT I
   CLS
   COLOR 11, 6
   LOCATE 1, 1:  PRINT "                                OUTPUT SELECTION                                "
   COLOR 15, 1
   COLOR 4, 3
   LOCATE 25, 1: PRINT "  F1=Info    F2=Help    F3=Key    F9=Quit    PgUp=Cycle    PgDn=Run Simulation  ";
   COLOR 15, 1
   LOCATE 5, 20: PRINT "OUTPUT UNITS  (1-CUSTOMARY  2-METRIC):"
   LOCATE 6, 20: PRINT "NUMBER OF YEARS TO SIMULATE   (1-   ):"
   LOCATE 6, 53: PRINT USING "###"; years;
   LOCATE 7, 20: PRINT "GENERATE DAILY OUTPUT           (Y/N):"
   LOCATE 8, 20: PRINT "GENERATE MONTHLY OUTPUT         (Y/N):"
   LOCATE 9, 20: PRINT "GENERATE ANNUAL OUTPUT          (Y/N):"
   LOCATE 10, 20: PRINT "SUMMARY OUTPUT WILL BE GENERATED."
   LOCATE 5, 59: PRINT OUTUNITS$
   LOCATE 6, 59: PRINT years$
   LOCATE 7, 59: PRINT daily$
   LOCATE 8, 59: PRINT monthly$
   LOCATE 9, 59: PRINT ANNUAL$
  'LOCATE 10, 59: PRINT summary$
14600 F2HELP$ = "#UNITS"
     CALL ACELL(5, 59, 1, OUTUNITS$, 2)
     GOSUB 73290
     IF OUTUNITS$ <> "2" THEN OUTUNITS$ = "1": LOCATE 5, 59: PRINT OUTUNITS$
     IF irtrn% = 1000 THEN GOSUB 15500
     IF irtrn% = 2 THEN irtrn% = 1
     IF irtrn% = 1 OR irtrn% = -1 THEN EXIT SUB
     IF irtrn% = 3 GOTO 15000
     IF irtrn% = 4 OR irtrn% = 5 THEN GOTO 14700
     GOTO 14600
14700 F2HELP$ = "#OUTPUTYEARS"
      CALL ACELL(6, 59, 3, years$, 2)
     GOSUB 73290
     IF VAL(years$) > VAL(MINYRS$) THEN
        LOCATE 6, 59: PRINT "   ";
        years$ = MINYRS$: LOCATE 6, 59: PRINT years$
     END IF
     IF irtrn% = 1000 THEN GOSUB 15500
     IF irtrn% = 1 OR irtrn% = -1 THEN EXIT SUB
     IF irtrn% = 3 GOTO 14600
     IF irtrn% = 4 OR irtrn% = 5 THEN GOTO 14800
     GOTO 14700
14800  F2HELP$ = "#OUTPUTOPTIONS"
     CALL ACELL(7, 59, 1, daily$, 1)
     GOSUB 73290
     IF UCASE$(daily$) <> "Y" AND UCASE$(daily$) <> "N" THEN daily$ = "N"
      daily$ = UCASE$(daily$)
      LOCATE 7, 59: PRINT UCASE$(daily$)
     IF irtrn% = 1000 THEN GOSUB 15500
     IF irtrn% = 1 OR irtrn% = -1 THEN EXIT SUB
     IF irtrn% = 3 GOTO 14700
     IF irtrn% = 4 OR irtrn% = 5 THEN GOTO 14900
     GOTO 14800
14900 F2HELP$ = "#OUTPUTOPTIONS"
      CALL ACELL(8, 59, 1, monthly$, 1)
      GOSUB 73290
      IF UCASE$(monthly$) <> "Y" AND UCASE$(monthly$) <> "N" THEN monthly$ = "N"
      monthly$ = UCASE$(monthly$)
      LOCATE 8, 59: PRINT UCASE$(monthly$)
     IF irtrn% = 1000 THEN GOSUB 15500
     IF irtrn% = 1 OR irtrn% = -1 THEN EXIT SUB
     IF irtrn% = 3 GOTO 14800
     IF irtrn% = 4 OR irtrn% = 5 THEN GOTO 15000
     GOTO 14900
15000 F2HELP$ = "#OUTPUTOPTIONS"
     CALL ACELL(9, 59, 1, ANNUAL$, 1)
     GOSUB 73290
     IF UCASE$(ANNUAL$) <> "Y" AND UCASE$(ANNUAL$) <> "N" THEN ANNUAL$ = "N"
     ANNUAL$ = UCASE$(ANNUAL$)
     LOCATE 9, 59: PRINT UCASE$(ANNUAL$)
     IF irtrn% = 1000 THEN GOSUB 15500
     IF irtrn% = 1 OR irtrn% = -1 THEN EXIT SUB
     IF irtrn% = 3 GOTO 14900
     IF irtrn% = 4 OR irtrn% = 5 THEN GOTO 14600
     GOTO 15000
'15100 F2HELP$ = "#OUTPUTOPTIONS"
'     CALL ACELL(10, 59, 1, summary$, 1)
'     GOSUB 73290
'     IF UCASE$(summary$) <> "Y" AND UCASE$(summary$) <> "N" THEN summary$ = "Y"
'     summary$ = UCASE$(summary$)
'     LOCATE 10, 59: PRINT UCASE$(summary$)
'     IF irtrn% = 1000 THEN GOSUB 15500
'     IF irtrn% = 1 OR irtrn% = -1 THEN EXIT SUB
'     IF irtrn% = 3 GOTO 15000
'     IF irtrn% = 4 OR irtrn% = 5 THEN GOTO 14600
'     GOTO 15100
''''''''''''''''''''''''''''''''''''''''''''''
'PREPARE VALUES TO BE PRINTED TO OUTPARAM.DAT
''''''''''''''''''''''''''''''''''''''''''''''
15200 EXIT SUB
'--------------------
'RUN HELPO
'--------------------

'''''''''''''''''''''''''''''''''''
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
'''''''''''''''''''''''''''''''''''
15500 CALL EXOUT(ANSCODE)
     IF ANSCODE = 0 THEN
        GOTO 14600
     ELSE
        EXIT SUB
     END IF
RETURN
'''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO CALL MANUAL MESSAGES FOR HELP
'''''''''''''''''''''''''''''''''''''''''''''''''
73290  IF irtrn% = 22 THEN CALL USERHELP
       IF irtrn% = 21 THEN CALL HLPMSG(F2HELP$)
       IF irtrn% = 20 THEN CALL HLPMSG(F1HELP$)
      RETURN
END SUB

SUB RCELL (IROW, ISTART, NS, TXT$)
'
''''''''''''''''''''''''''''''
' Subroutine to select path  '
''''''''''''''''''''''''''''''
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

