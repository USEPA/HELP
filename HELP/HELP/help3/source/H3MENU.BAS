DECLARE SUB move (found!, F1HELP$)
DECLARE SUB filechk (fdrive$, fpath$, filen$, found!)
DECLARE SUB PRNTFILE ()
DECLARE SUB GETDIR (viewext$, SELECT$, DIRECTORY$)
DECLARE SUB ACELL (IROW!, ISTART!, NS!, TEXT$, ITYPE!)
DECLARE SUB menu ()
DECLARE SUB BOX (SROWBOX, SCOLBOX, BOXWIDTH, EROWBOX)
DECLARE SUB VIEWING ()
DECLARE SUB HLPMSG (f2help$)
DECLARE SUB RCELL (IROW!, ISTART!, NS!, TXT$)
DECLARE SUB USERHELP ()
COMMON SHARED irtrn%, PAGE%, VPAGE%, APAGE%, HPAGE%
COMMON SHARED F1HELP$, f2help$, VIEWPATH$, VIEWFILE$, viewext$
20 DIM MDSCRP$(10)
   VIEW PRINT 1 TO 25
   BLANKS$ = "                                                                             "
   NOPT = 9   'Number of modules available and listed below
   MDSCRP$(1) = "           MAIN MENU   "
   MDSCRP$(2) = ""
   MDSCRP$(3) = "Enter/Edit Weather Data"
   MDSCRP$(4) = "Enter/Edit Soil and Design Data"
   MDSCRP$(5) = "Execute Simulation"
   MDSCRP$(6) = "View Results"
   MDSCRP$(7) = "Print Results"
   MDSCRP$(8) = "Display Guidance"
   MDSCRP$(9) = "Quit"
   APAGE% = 0: VPAGE% = 1: HPAGE% = 2
   SCREEN 0, , VPAGE%, VPAGE%
   KEY OFF: COLOR 15, 1: CLS
   F1HELP$ = "#MAINMENU"
   SLINE = INT((25 - (NOPT + 9)) / 2) + 2
   LOCATE SLINE, 1
   PRINT "      ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป      "
   PRINT "      บ  HYDROLOGIC EVALUATION OF LANDFILL PERFORMANCE (HELP) VER. 3.07  บ      "
   PRINT "      ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน      "
   FOR I% = 1 TO NOPT + 2
     LOCATE SLINE + I% + 2, 7: PRINT "บ": LOCATE SLINE + I% + 2, 74: PRINT "บ"
   NEXT I%
   PRINT "      ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
   PRINT "      บ  F1=Info   F2=Help   F3=Keys   <   then enter or number>=Select  บ"
   PRINT "      ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
   LOCATE 20, 41: PRINT CHR$(24); CHR$(25);
   FOR I% = 1 TO 2
     LOCATE SLINE + 3 + I%, 25: PRINT MDSCRP$(I%)
   NEXT I%
   FOR I% = 3 TO NOPT
     LOCATE SLINE + 3 + I%, 23: PRINT USING "###"; I% - 2; : PRINT ".  "
     LOCATE SLINE + 3 + I%, 29: PRINT MDSCRP$(I%)
   NEXT I%
   IF FROM% = 2 THEN GOTO 50
   IF FROM% = 3 THEN GOTO 60
   IF FROM% = 4 THEN GOTO 70
   IF FROM% = 5 THEN GOTO 80
40 XLEN = LEN(MDSCRP$(3))
   CALL RCELL(SLINE + 6, 29, XLEN, MDSCRP$(3))
   f2help$ = "#MAINCLIM": GOSUB 99
    IF irtrn% = 1000 THEN GOTO 97
    GOSUB 98
41  IF irtrn% = 2 GOTO 97
    IF irtrn% = 3 GOTO 87
    IF irtrn% = 4 THEN GOTO 50
    IF irtrn% = 5 THEN
      irtrn% = 0
      SHELL "WEATHER"
      'CLEAR
      FROM% = 1
      GOTO 20
    END IF
    GOTO 40
50  XLEN = LEN(MDSCRP$(4))
    CALL RCELL(SLINE + 7, 29, XLEN, MDSCRP$(4))
    f2help$ = "#MAINSOIL": GOSUB 99
    IF irtrn% = 1000 THEN GOTO 97
    GOSUB 98
51  IF irtrn% = 2 GOTO 97
    IF irtrn% = 3 GOTO 40
    IF irtrn% = 4 THEN GOTO 60
    IF irtrn% = 5 THEN
       irtrn% = 0
       CLS
       SHELL "DESIGN"
       'CLEAR
       FROM% = 2
       GOTO 20
    END IF
    GOTO 50
60  XLEN = LEN(MDSCRP$(5))
    CALL RCELL(SLINE + 8, 29, XLEN, MDSCRP$(5))
      f2help$ = "#MAINEXEC": GOSUB 99
      IF irtrn% = 1000 THEN GOTO 97
      GOSUB 98
61  IF irtrn% = 2 GOTO 97
    IF irtrn% = 3 GOTO 50
    IF irtrn% = 4 THEN GOTO 70
    IF irtrn% = 5 THEN
       irtrn% = 0
       CLS
       SHELL "OUTPUT"
       FROM% = 3
       GOTO 20
    END IF
    GOTO 60
70  XLEN = LEN(MDSCRP$(6))
    CALL RCELL(SLINE + 9, 29, XLEN, MDSCRP$(6))
      f2help$ = "#MAINVIEW": GOSUB 99
    IF irtrn% = 1000 THEN GOTO 97
    GOSUB 98
71  IF irtrn% = 2 GOTO 97
     IF irtrn% = 3 GOTO 60
     IF irtrn% = 4 THEN GOTO 80
     IF irtrn% = 5 THEN
       A$ = DIR$("OUTPARAM.DAT")
       IF A$ = "" THEN
	  VIEWPATH$ = CURDIR$
	  GOTO 2011
       ELSE
	  OPEN "OUTPARAM.DAT" FOR INPUT AS #1
	  VIEWPATH$ = CURDIR$
	  FOR I = 1 TO 5
	    INPUT #1, FILENAME$
	  NEXT I
	  INPUT #1, FILENAME$
	  LENGTH = LEN(FILENAME$)
	  VIEWFILE$ = MID$(FILENAME$, 1, LENGTH - 4)
2010      IF INSTR(VIEWFILE$, "\") <> 0 THEN
	     XST = LEN(VIEWFILE$)
	     VIEWFILE$ = MID$(VIEWFILE$, INSTR(VIEWFILE$, "\") + 1, XST)
	     GOTO 2010
	  END IF
	  SLEN = LEN(FILENAME$) - LEN(VIEWFILE$) - 4
	  VIEWPATH$ = LEFT$(FILENAME$, SLEN)
	  CLOSE
	  GOTO 2011
       END IF
2011   viewext$ = "OUT"
       CALL VIEWING
       COLOR 15, 1: CLS
       FROM% = 4: GOTO 20
     END IF
     GOTO 70
80  XLEN = LEN(MDSCRP$(7))
    CALL RCELL(SLINE + 10, 29, XLEN, MDSCRP$(7))
      f2help$ = "#MAINPRINT": GOSUB 99
     IF irtrn% = 1000 THEN GOTO 97
     GOSUB 98
81   IF irtrn% = 2 GOTO 97
     IF irtrn% = 3 GOTO 70
     IF irtrn% = 5 THEN
       irtrn% = 0
       A$ = DIR$("OUTPARAM.DAT")
       IF A$ = "" THEN
	  VIEWPATH$ = CURDIR$
	  GOTO 2022
       ELSE
	  OPEN "OUTPARAM.DAT" FOR INPUT AS #1
	  VIEWPATH$ = CURDIR$
	  FOR I = 1 TO 5
	    INPUT #1, FILENAME$
	  NEXT I
	  INPUT #1, FILENAME$
	  LENGTH = LEN(FILENAME$)
	  VIEWFILE$ = MID$(FILENAME$, 1, LENGTH - 4)
2020      IF INSTR(VIEWFILE$, "\") <> 0 THEN
	     XST = LEN(VIEWFILE$)
	     VIEWFILE$ = MID$(VIEWFILE$, INSTR(VIEWFILE$, "\") + 1, XST)
	     GOTO 2020
	  END IF
	  SLEN = LEN(FILENAME$) - LEN(VIEWFILE$) - 4
	  VIEWPATH$ = LEFT$(FILENAME$, SLEN)
	  CLOSE
	  GOTO 2022
       END IF
2022   viewext$ = "OUT"
       CALL PRNTFILE
       COLOR 15, 1: CLS
       FROM% = 5: GOTO 20
     END IF
     IF irtrn% = 4 THEN GOTO 85
     GOTO 80
85  XLEN = LEN(MDSCRP$(8))
     CALL RCELL(SLINE + 11, 29, XLEN, MDSCRP$(8))
     f2help$ = "#MAINUSER"
     GOSUB 99
     IF irtrn% = 1000 THEN GOTO 97
     GOSUB 98
86   IF irtrn% = 2 GOTO 97
     IF irtrn% = 3 GOTO 80
     IF irtrn% = 5 THEN
       f2help$ = "#U1HELP"
       irtrn% = 21
       GOSUB 99
       irtrn% = 5
       CLOSE
     END IF
     IF irtrn% = 4 THEN GOTO 87
     GOTO 85
87  XLEN = LEN(MDSCRP$(9))
    CALL RCELL(SLINE + 12, 29, XLEN, MDSCRP$(9))
      f2help$ = "#MAINQUIT": GOSUB 99
     IF irtrn% = 1000 THEN GOTO 97
     GOSUB 98
88   IF irtrn% = 2 GOTO 97
     IF irtrn% = 3 GOTO 85
     IF irtrn% = 5 THEN irtrn% = 0: GOTO 97
     IF irtrn% = 4 THEN GOTO 40
     GOTO 87
97  CLOSE : SCREEN 0, 0: CLS APAGE%: END
'SUBROUTINE FOR MENU ITEM SELECTION BY NUMBER
98      IF irtrn% = 101 THEN irtrn% = 5: GOTO 41
	IF irtrn% = 102 THEN irtrn% = 5: GOTO 51
	IF irtrn% = 103 THEN irtrn% = 5: GOTO 61
	IF irtrn% = 104 THEN irtrn% = 5: GOTO 71
	IF irtrn% = 105 THEN irtrn% = 5: GOTO 81
	IF irtrn% = 106 THEN irtrn% = 5: GOTO 86
	IF irtrn% = 107 THEN irtrn% = 5: GOTO 88
	IF irtrn% = 100 THEN irtrn% = 5: GOTO 88
    RETURN
'''''''''''''''''''''''''
'SUBROUTINE TO CALL HELP
''''''''''''''''''''''''
99   IF irtrn% = 22 THEN CALL USERHELP
     IF irtrn% = 21 THEN CALL HLPMSG(f2help$)
     IF irtrn% = 20 THEN CALL HLPMSG(F1HELP$)
    RETURN
100 END

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
'  * IPOS   = Cursor location.  IPOS ๎ [1,NS]                              *
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
   IF NL = 27 THEN irtrn% = 100: COLOR 15, 1: GOSUB 11022: EXIT SUB
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

SUB BOX (SROWBOX, SCOLBOX, BOXWIDTH, EROWBOX)
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
' and allows the user to select a file                       '
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
     director$ = LTRIM$(RTRIM$(DIRECTRY$))
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
	LOCATE 24, 15: PRINT director$;
	RETURN
	EXIT SUB
END SUB

SUB HLPMSG (f2help$)
     DIM NAME$(23), LOCHELP%(23, 2), LI$(740)
     PCOPY VPAGE%, HPAGE%
     PCOPY VPAGE%, APAGE%
     CLS APAGE%
     SCREEN 0, , VPAGE%, VPAGE%
90011 IF f2help$ = "#U1HELP" THEN
       B$ = DIR$("USE1HELP.HLP")
       A$ = DIR$("USE1TAGS.HLP")
       IF A$ = "" OR B$ = "" THEN
	 IF A$ = "" THEN LOCATE 10, 16: PRINT "USE1TAGS.HLP FILE IS NOT FOUND ON THE DEFAULT DIRECTORY"
	 IF B$ = "" THEN LOCATE 11, 16: PRINT "USE1HELP.HLP FILE IS NOT FOUND ON THE DEFAULT DIRECTORY"
	 LOCATE 13, 29: PRINT "PRESS ANY KEY TO CONTINUE"
901052   INK$ = INKEY$: IF INK$ = "" THEN GOTO 901052
	 GOTO 99197
       END IF
     ELSEIF f2help$ = "#U3HELP" THEN
       B$ = DIR$("USE2HELP.HLP")
       A$ = DIR$("USE2TAGS.HLP")
       IF A$ = "" OR B$ = "" THEN
	 IF A$ = "" THEN LOCATE 10, 16: PRINT "USE2TAGS.HLP FILE IS NOT FOUND ON THE DEFAULT DIRECTORY"
	 IF B$ = "" THEN LOCATE 11, 16: PRINT "USE2HELP.HLP FILE IS NOT FOUND ON THE DEFAULT DIRECTORY"
	 LOCATE 13, 29: PRINT "PRESS ANY KEY TO CONTINUE"
901053   INK$ = INKEY$: IF INK$ = "" THEN GOTO 901053
	 GOTO 99197
       END IF
     ELSE
       B$ = DIR$("MAINHELP.HLP")
       A$ = DIR$("MAINTAGS.HLP")
       IF A$ = "" OR B$ = "" THEN
	 IF A$ = "" THEN LOCATE 10, 16: PRINT "MAINTAGS.HLP FILE IS NOT FOUND ON THE DEFAULT DIRECTORY"
	 IF B$ = "" THEN LOCATE 11, 16: PRINT "MAINHELP.HLP FILE IS NOT FOUND ON THE DEFAULT DIRECTORY"
	 LOCATE 13, 29: PRINT "PRESS ANY KEY TO CONTINUE"
901054   INK$ = INKEY$: IF INK$ = "" THEN GOTO 901054
	 GOTO 99197
       END IF
     END IF
     LINES% = 0
     COLOR 15, 1
     CLS
900111 IF f2help$ = "#U1HELP" THEN
	 OPEN "USE1TAGS.HLP" FOR INPUT AS #2
       ELSEIF f2help$ = "#U2HELP" THEN
	 OPEN "USE1TAGS.HLP" FOR INPUT AS #2
       ELSEIF f2help$ = "#U3HELP" THEN
	 OPEN "USE2TAGS.HLP" FOR INPUT AS #2
       ELSEIF f2help$ = "#U4HELP" THEN
	 OPEN "USE2TAGS.HLP" FOR INPUT AS #2
       ELSE
	 OPEN "MAINTAGS.HLP" FOR INPUT AS #2
       END IF
     COLOR 4, 3
	      IF f2help$ = "#U1HELP" THEN
     LOCATE 25, 1: PRINT "     to scroll   PgDn/PgUp   Home   End    F9/ESC = Cancel    F10 = More Help   ";
     LOCATE 25, 3: PRINT CHR$(24); CHR$(25);
	      ELSEIF f2help$ = "#U2HELP" THEN
     LOCATE 25, 1: PRINT "     to scroll  PgDn/PgUp  Home  End   F9/ESC=Cancel   F10=Weather/Design Help  ";
     LOCATE 25, 3: PRINT CHR$(24); CHR$(25);
	      ELSEIF f2help$ = "#U3HELP" THEN
     LOCATE 25, 1: PRINT "     to scroll   PgDn/PgUp   Home   End   F9/ESC=Cancel   F10=Soil/Design Help  ";
     LOCATE 25, 3: PRINT CHR$(24); CHR$(25);
	      ELSE
     LOCATE 25, 1: PRINT "     to scroll up and down      PgDn/PgUp      Home      End     F9/ESC =Cancel ";
     LOCATE 25, 3: PRINT CHR$(24); CHR$(25);
	      END IF
     COLOR 15, 1

     FOR I = 1 TO 23
	IF EOF(2) THEN GOTO 90052
	LINE INPUT #2, A$
	NAME$(I) = LTRIM$(RTRIM$(A$))
	INPUT #2, LOCHELP%(I, 1)
	INPUT #2, LOCHELP%(I, 2)
      NEXT I
90052  CLOSE
       IF f2help$ = "#U1HELP" THEN
	 OPEN "USE1HELP.HLP" FOR RANDOM AS #1 LEN = 80
       ELSEIF f2help$ = "#U2HELP" THEN
	 OPEN "USE1HELP.HLP" FOR RANDOM AS #1 LEN = 80
       ELSEIF f2help$ = "#U3HELP" THEN
	 OPEN "USE2HELP.HLP" FOR RANDOM AS #1 LEN = 80
       ELSEIF f2help$ = "#U4HELP" THEN
	 OPEN "USE2HELP.HLP" FOR RANDOM AS #1 LEN = 80
       ELSE
	 OPEN "MAINHELP.HLP" FOR RANDOM AS #1 LEN = 80
       END IF
'      OPEN "USE1HELP.HLP" FOR RANDOM AS #1 LEN = 80
      FIELD #1, 80 AS L$
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'DETERMINE STARTING POINT OF THE REQUIRED HELP MESSAGE BASED ON THE
'TAG PASSED FROM THE PROGRAM
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
      FOR I = 1 TO 23
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
90010   LINES% = 0
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
' F10 MORE HELP
'''''''''''''''''''''''''''''''
	   ELSEIF NL = 0 AND NH = 68 THEN
	      IF f2help$ = "#U1HELP" THEN
		f2help$ = "#U2HELP"
		GOTO 900111
	      ELSEIF f2help$ = "#U2HELP" THEN
		f2help$ = "#U3HELP"
		GOTO 90011
	      ELSEIF f2help$ = "#U3HELP" THEN
		f2help$ = "#U4HELP"
		GOTO 900111
	      ELSEIF f2help$ = "#U4HELP" THEN
		GOTO 99197
	      ELSE
		GOTO 90030
	      END IF
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
	      IF f2help$ = "#U1HELP" THEN
     LOCATE 25, 1: PRINT "     to scroll   PgDn/PgUp   Home   End    F9/ESC = Cancel    F10 = More Help   ";
     LOCATE 25, 3: PRINT CHR$(24); CHR$(25);
	      ELSEIF f2help$ = "#U2HELP" THEN
     LOCATE 25, 1: PRINT "     to scroll  PgDn/PgUp  Home  End   F9/ESC=Cancel   F10=Weather/Design Help  ";
     LOCATE 25, 3: PRINT CHR$(24); CHR$(25);
	      ELSEIF f2help$ = "#U3HELP" THEN
     LOCATE 25, 1: PRINT "     to scroll   PgDn/PgUp   Home   End   F9/ESC=Cancel   F10=Soil/Design Help  ";
     LOCATE 25, 3: PRINT CHR$(24); CHR$(25);
	      ELSE
     LOCATE 25, 1: PRINT "     to scroll up and down      PgDn/PgUp      Home      End     F9/ESC =Cancel ";
     LOCATE 25, 3: PRINT CHR$(24); CHR$(25);
	      END IF

'       LOCATE 25, 1: PRINT "     to scroll up and down      PgDn/PgUp      Home      End     F9/ESC =Cancel ";
 '      LOCATE 25, 3: PRINT CHR$(24); CHR$(25);
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

SUB menu
      COLOR 4, 3
      LOCATE 25, 1: PRINT " F1=Info  F2=Help  F3=Keys   F9=Quit   F10=Proceed   PgUp=Return   PgDn=Proceed ";
      COLOR 15, 1
  EXIT SUB
END SUB

SUB move (found, F1HELP$)
   BLANK$ = "                                                             "
   CALL menu
   GOSUB 78165
   LOCATE 7, 61: PRINT VIEWFILE$
     LOCATE 7, 72: PRINT viewext$
7140 CALL ACELL(7, 19, 40, VIEWPATH$, 1)
     f2help$ = "#PATH"
     GOSUB 73290
     LOCATE 9, 20: PRINT LEFT$(BLANK$, 55)
     LOCATE 13, 20:  PRINT LEFT$(BLANK$, 55)
     VIEWPATH$ = LTRIM$(RTRIM$(VIEWPATH$))
     IF irtrn% = 5 OR irtrn% = 6 OR irtrn% = 7 THEN
	IF VIEWPATH$ <> CURDIR$ THEN GOSUB 73190
	IF irtrn% = 5 OR irtrn% = 6 THEN GOTO 7145
	IF irtrn% = 7 THEN GOTO 7150
     END IF
     IF irtrn% = 1 OR irtrn% = 2 THEN
	IF LTRIM$(RTRIM$(VIEWFILE$)) = "" THEN
	   LOCATE 13, 20: PRINT "Specify file name or press F4 for list of files."
	   GOTO 7140
	ELSE
	   GOSUB 73190
	   IF found = 4 THEN EXIT SUB
	END IF
      END IF
      IF irtrn% = 1000 OR irtrn% = -1 OR irtrn% = 100 THEN EXIT SUB
      IF irtrn% = 23 THEN
	IF LTRIM$(RTRIM$(viewext$)) = "" OR LTRIM$(RTRIM$(viewext$)) = "*" THEN
	   LOCATE 13, 20: PRINT "Specify extension then press F4 for list of files."
	   GOTO 7140
	END IF
	 IF ERR <> 0 THEN GOTO 7140
	 CALL GETDIR(viewext$, SELECT$, VIEWPATH$)
	 IF irtrn% = 1000 THEN EXIT SUB
	 IF SELECT$ <> "" THEN VIEWFILE$ = SELECT$: SELECT$ = ""
	 LOCATE 7, 61: PRINT "        ": LOCATE 7, 61: PRINT VIEWFILE$
	 GOSUB 78165
      END IF
     GOTO 7140
7145 CALL ACELL(7, 61, 8, VIEWFILE$, 1)
     LOCATE 9, 20: PRINT LEFT$(BLANK$, 55)
     LOCATE 13, 20:  PRINT LEFT$(BLANK$, 55)
     f2help$ = "#FILE"
     GOSUB 73290
     VIEWFILE$ = LTRIM$(RTRIM$(VIEWFILE$))
     IF irtrn% = 1 OR irtrn% = 2 THEN
       IF LTRIM$(RTRIM$(VIEWFILE$)) = "" THEN
	  LOCATE 13, 20: PRINT "Specify file name or press F4 for list of files."
	  GOTO 7145
       ELSE
	  GOSUB 73190
	  IF found = 4 THEN EXIT SUB
       END IF
     END IF
     IF irtrn% = 5 OR irtrn% = 6 OR irtrn% = 7 THEN
	GOSUB 73190
	IF irtrn% = 6 OR irtrn% = 5 THEN GOTO 7150
	IF irtrn% = 7 THEN GOTO 7140
     END IF
     IF irtrn% = 1000 OR irtrn% = -1 OR irtrn% = 100 THEN EXIT SUB
     IF irtrn% = 23 THEN
	IF LTRIM$(RTRIM$(viewext$)) = "" OR LTRIM$(RTRIM$(viewext$)) = "*" THEN
	   LOCATE 13, 20: PRINT "Specify Extension then Press F4 for List of Files."
	   GOTO 7145
	END IF
	IF ERR <> 0 THEN GOTO 7145
	CALL GETDIR(viewext$, SELECT$, VIEWPATH$)
	IF irtrn% = 1000 THEN EXIT SUB
	IF SELECT$ <> "" THEN VIEWFILE$ = SELECT$: SELECT$ = ""
	LOCATE 7, 61: PRINT "        ": LOCATE 7, 61: PRINT VIEWFILE$
	GOSUB 78165
      END IF
     GOTO 7145
7150 CALL ACELL(7, 72, 3, viewext$, 1)
     LOCATE 9, 20: PRINT LEFT$(BLANK$, 55)
     LOCATE 13, 20:  PRINT LEFT$(BLANK$, 55)
     f2help$ = "#EXTENSION"
     GOSUB 73290
     viewext$ = LTRIM$(RTRIM$(viewext$))
     IF irtrn% = 1 OR irtrn% = 2 THEN
       IF LTRIM$(RTRIM$(VIEWFILE$)) = "" THEN
	  LOCATE 13, 20: PRINT "Specify file name or press F4 for list of files."
	  GOTO 7150
       ELSE
	  GOSUB 73190
	  IF found = 4 THEN EXIT SUB
       END IF
     END IF
     IF irtrn% = 5 OR irtrn% = 6 OR irtrn% = 7 THEN
	IF LTRIM$(RTRIM$(VIEWFILE$)) <> "" THEN GOSUB 73190
	IF irtrn% = 7 THEN GOTO 7145
	IF irtrn% = 6 OR irtrn% = 5 THEN GOTO 7140
     END IF
     IF irtrn% = 1000 OR irtrn% = -1 OR irtrn% = 100 THEN EXIT SUB
     IF irtrn% = 23 THEN
	IF LTRIM$(RTRIM$(viewext$)) = "" OR LTRIM$(RTRIM$(viewext$)) = "*" THEN
	   LOCATE 13, 20: PRINT "Specify Extension then Press F4 for List of Files."
	   GOTO 7150
	END IF
	IF ERR <> 0 THEN GOTO 7150
	CALL GETDIR(viewext$, SELECT$, VIEWPATH$)
	IF irtrn% = 1000 THEN EXIT SUB
	IF SELECT$ <> "" THEN VIEWFILE$ = SELECT$: SELECT$ = ""
	LOCATE 7, 61: PRINT "        ": LOCATE 7, 61: PRINT VIEWFILE$
	GOSUB 78165
      END IF
     GOTO 7150
EXIT SUB
'----------------------------------
'SUBROUTINE TO PRINT DIRECTORY LINE
'----------------------------------
78165  COLOR 11, 6
       LOCATE 24, 1: PRINT "                   F4 = List Files on Specified Directory                       ";
       'LOCATE 24, 15: PRINT DIRECTORY$;
       COLOR 15, 1
     RETURN
'------------------------------
'check if directory is valid
'------------------------------
73190 IF VIEWPATH$ = CURDIR$ AND VIEWFILE$ = "" THEN RETURN
      IF irtrn% = 1 AND VIEWFILE$ = "" THEN RETURN
      A1 = INSTR(VIEWPATH$, ":")
       IF A1 = 0 THEN
	  fdrive$ = LEFT$(CURDIR$, 1)
	  fpath$ = VIEWPATH$
       ELSE
	  fdrive$ = LEFT$(VIEWPATH$, A1 - 1)
	  fpath$ = RIGHT$(VIEWPATH$, LEN(VIEWPATH$) - A1)
       END IF
       IF fpath$ = "\" THEN fpath$ = ""
       filen$ = VIEWFILE$
       found = -1
       CALL filechk(fdrive$, fpath$, filen$ + "." + viewext$, found)
       IF found = 0 THEN
	 LOCATE 9, 35: PRINT "Invalid Directory"; : VIEWPATH$ = CURDIR$
	 LOCATE 7, 19: PRINT LEFT$(BLANK$, 40)
	 LOCATE 7, 19: PRINT VIEWPATH$
       ELSEIF found = 1 THEN
	 LOCATE 9, 35: PRINT "Error in Path"; : VIEWPATH$ = CURDIR$
	 LOCATE 7, 19: PRINT LEFT$(BLANK$, 40)
	 LOCATE 7, 19: PRINT VIEWPATH$
       ELSEIF found = 2 THEN
	 IF LTRIM$(RTRIM$(VIEWFILE$)) <> "" THEN LOCATE 9, 35: PRINT "Bad File Name";
	 VIEWFILE$ = "        ": LOCATE 7, 61: PRINT VIEWFILE$
       ELSEIF found = 3 THEN
	 IF LTRIM$(RTRIM$(VIEWFILE$)) <> "" THEN LOCATE 9, 35: PRINT "File Not Found";
	 VIEWFILE$ = "        ": LOCATE 7, 61: PRINT VIEWFILE$
       ELSEIF found = 5 THEN
	 LOCATE 9, 35: PRINT "Invalid Drive"; : VIEWPATH$ = CURDIR$
	 LOCATE 7, 19: PRINT LEFT$(BLANK$, 40)
	 LOCATE 7, 19: PRINT VIEWPATH$
       END IF
RETURN
''-----------------------------------------
''SUBROUTINE TO CALL help MESSAGES FOR HELP
''-----------------------------------------
73290  IF irtrn% = 22 THEN CALL USERHELP
       IF irtrn% = 21 THEN CALL HLPMSG(f2help$)
       IF irtrn% = 20 THEN CALL HLPMSG(F1HELP$)
       RETURN
END SUB

SUB PRNTFILE
   F1HELP$ = "#PRINTFILE"
   DEFDIR$ = CURDIR$
   viewext$ = "OUT"
17101 CLS
   ON LOCAL ERROR RESUME NEXT
   COLOR 11, 6
   LOCATE 1, 1:  PRINT "                        PRINT RESULTS   -  FILE MANAGEMENT                      "
   COLOR 15, 1
   SROWBOX = 5: SCOLBOX = 2: BOXWIDTH = 77: EROWBOX = 8
   CALL BOX(SROWBOX, SCOLBOX, BOXWIDTH, EROWBOX)
   LOCATE 6, 10: PRINT "         DIRECTORY                                 FILE       EXT"
   LOCATE 7, 3:  PRINT "   PRINT FILE"
   CALL move(found, F1HELP$)
   IF irtrn% = -1 THEN EXIT SUB
   IF irtrn% = 1000 THEN EXIT SUB
   IF irtrn% = 100 THEN EXIT SUB
   IF found = 4 THEN GOSUB 2170
   IF irtrn% = 500 THEN GOTO 17101
EXIT SUB
'''''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO PRINT THE CONTENTS OF THE SPECIFIED'
'FILE USING                                       '
'''''''''''''''''''''''''''''''''''''''''''''''''''
2170   CLS
	SCREEN 0, 0, 0
	VIEWPATH$ = LTRIM$(RTRIM$(VIEWPATH$))
	AEND$ = RIGHT$(VIEWPATH$, 1)
	IF AEND$ <> "\" THEN VIEWPATH$ = VIEWPATH$ + "\"
	PRINTF$ = "PRINT " + LTRIM$(RTRIM$(VIEWPATH$)) + LTRIM$(RTRIM$(VIEWFILE$)) + "." + LTRIM$(RTRIM$(viewext$))
	SHELL PRINTF$
	irtrn% = 500
	SCREEN 0, 1, 1
	COLOR 15, 1: CLS
RETURN
'-----------------------------------------
'SUBROUTINE TO CALL help MESSAGES FOR HELP
'-----------------------------------------
3290  IF irtrn% = 22 THEN CALL USERHELP
       IF irtrn% = 21 THEN CALL HLPMSG(f2help$)
       IF irtrn% = 20 THEN CALL HLPMSG(F1HELP$)
       RETURN
END SUB

SUB RCELL (IROW, ISTART, NS, TXT$)
'
'--------------------------------------'
' Subroutine to select path from menu  '
'--------------------------------------'
'
'  Locate cursor at first position of cell and write reverse video cell
'
      COLOR 1, 15
      LOCATE IROW, ISTART
      PRINT TXT$;
'--------------------------------------------------------------
'Locate the cursor at the current postion and capture a new key
'--------------------------------------------------------------
      LOCATE IROW, ISTART
11221 KSTRG$ = ""
      KSTRG$ = INKEY$: IF KSTRG$ = "" GOTO 11221
      NL = ASC(MID$(KSTRG$, 1))
      IF LEN(KSTRG$) = 2 THEN NH = ASC(MID$(KSTRG$, 2))
'---------------------------------------------------------------
' Check for PgDn (IRTRN%=1), PgUp (IRTRN%=-1), F10 (IRTRN%=2) '
' UP ARROW (IRTRN%=3), DOWN ARROW (IRTRN%=4), <CR> (IRTRN%=5) '
' RIGHT ARROW (IRTRN%=6), LEFT ARROW (IRTRN%=7)               '
' HOME (IRTRN%=8), END (IRTRN%=9)                             '
' COMMAND HELP (F1) IRTRN%=20                                 '
' TECHNICAL HELP (F2) IRTRN%=21                               '
' HELP SOIL TEXTURE (F3) IRTRN% = 22                          '
' USER SOIL TEXTURE (F4) IRTRN% = 23                          '
' QUIT WITH SAVE (F9) IRTRN% = 24                             '
' Alt A to insert a layer above current layer (IRTRN%=30)     '
' Alt B to insert a layer below current layer (IRTRN%=31)     '
' ESC : IRTRN% = 100                                          '                 '
'---------------------------------------------------------------
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

SUB VIEWING
   F1HELP$ = "#VIEWFILE"
   f2help$ = "#VIEWFILE"
   DEFDIR$ = CURDIR$
   viewext$ = "OUT"
7101 CLS
   ON LOCAL ERROR RESUME NEXT
   COLOR 11, 6
   LOCATE 1, 1:  PRINT "                         VIEW RESULTS   -  FILE MANAGEMENT                      "
   COLOR 15, 1
   SROWBOX = 5: SCOLBOX = 2: BOXWIDTH = 77: EROWBOX = 8
   CALL BOX(SROWBOX, SCOLBOX, BOXWIDTH, EROWBOX)
   LOCATE 6, 10: PRINT "         DIRECTORY                                 FILE       EXT"
   LOCATE 7, 3:  PRINT " VIEWING FILE"
   CALL move(found, F1HELP$)
   IF irtrn% = -1 THEN EXIT SUB
   IF irtrn% = 1000 THEN EXIT SUB
   IF irtrn% = 100 THEN EXIT SUB
   IF found = 4 THEN GOSUB 32170
   IF irtrn% = 500 THEN GOTO 7101
EXIT SUB
'-------------------------------------------------
'SUBROUTINE TO LIST THE CONTENTS OF THE SPECIFIED
'FILE USING THE LIST PROGRAM
'-------------------------------------------------
32170   CLS
	SCREEN 0, 0, 0
	VIEWPATH$ = LTRIM$(RTRIM$(VIEWPATH$))
	AEND$ = RIGHT$(VIEWPATH$, 1)
	IF AEND$ <> "\" THEN VIEWPATH$ = VIEWPATH$ + "\"
	LISTFILE$ = "LIST " + LTRIM$(RTRIM$(VIEWPATH$)) + LTRIM$(RTRIM$(VIEWFILE$)) + "." + LTRIM$(RTRIM$(viewext$))
	SHELL LISTFILE$
	irtrn% = 500
	SCREEN 0, 1, 1
	COLOR 15, 1: CLS
RETURN
END SUB

