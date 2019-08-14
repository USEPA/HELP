DECLARE SUB FILECHK (fdrive$, FPATH$, FILEN$, FOUND!)
DECLARE SUB ACELL (IROW!, ISTART!, NS!, TEXT$, ITYPE!)
DECLARE SUB BOX ()
DECLARE SUB ITEMSELECT (FIRSTROW!, NUMINCOL!, TOTNUM!, states$(), SELECT$, ITEMNUM!)
DECLARE SUB RCELL (IROW!, ISTART!, NS!, TXT$)
    
COMMON SHARED tempu$, precu$, solu$, UNITS$, UNIT$, UN1$, UN2$, UN3$, UN4$, UN5$
COMMON SHARED unev$, UNSL$, TOHELP%, NL%, CLIMATF$, IRTRN%, PAGE%, CLIM
COMMON SHARED APAGE%, VPAGE%, HPAGE%, DIRECT$, SELECCT$, SELECST$
COMMON SHARED GROW1$, GROW2$, MAXLAI$, EVBARE$, EVFAIR$, EVEXCL$
COMMON SHARED evdepth$, LAI$, GR1$, GR2$, AREA$, NYEARS$, latitude$
COMMON SHARED HUMID1$, HUMID2$, HUMID3$, HUMID4$, WIND$, LATIT$, meansel
COMMON SHARED WSPEED$, BARE$, FAIR$, EXCL$, UNI$, ETCITY$, ETSTATE$, etunits$
COMMON SHARED defrain, deftemp, cmeth(), CUNIT(), ccity$(), cstate$(), ct$, st$, FILEPATH$()
COMMON SHARED SROWBOX, SCOLBOX, BOXWIDTH, EROWBOX, defp(), deft()

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
76020   INK% = IRTRN%
        SROWBOX = 11: SCOLBOX = 27: BOXWIDTH = 27: EROWBOX = 15
        COLOR 15, 1
        CALL BOX
        NO$ = "NO"
        YES$ = "YES"
        LOCATE 12, 28: PRINT " EXIT AND LOSE CHANGES?  ";
        LOCATE 13, 28: PRINT "          YES            ";
        LOCATE 14, 28: PRINT "          NO             ";
76025   CALL RCELL(13, 38, 3, YES$)
        IF IRTRN% = 109 THEN
          FOR I = 11 TO 15
             LOCATE I, 27: PRINT "                            ";
          NEXT I
          ANSCODE = 0: IRTRN% = 0:  EXIT SUB
        END IF
        IF IRTRN% = 3 OR IRTRN% = 4 THEN GOTO 76022
        IF IRTRN% = 5 OR IRTRN% = 108 OR IRTRN% = 1000 THEN
          IRTRN% = INK%
          ANSCODE = 1
          EXIT SUB
        END IF
        GOTO 76025
76022   CALL RCELL(14, 38, 2, NO$)
        IF IRTRN% = 3 OR IRTRN% = 4 THEN GOTO 76025
        IF IRTRN% = 108 OR IRTRN% = 1000 THEN ANSCODE = 1: IRTRN% = INK%: EXIT SUB
        IF IRTRN% = 5 OR IRTRN% = 100 OR IRTRN% = 109 THEN
          FOR I = 11 TO 15
             LOCATE I, 27: PRINT "                            ";
          NEXT I
          IRTRN% = 0: ANSCODE = 0
          EXIT SUB
        END IF
        GOTO 76022
       EXIT SUB
END SUB

SUB FILECHK (fdrive$, FPATH$, FILEN$, FOUND)
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
      IF LTRIM$(RTRIM$(FPATH$)) = "" THEN FPATH$ = RIGHT$(CURDIR$, LEN(CURDIR$) - 3)
 END IF
ON LOCAL ERROR RESUME NEXT
   FOUND = -1: ERR = 0
   DR$ = LEFT$(CURDIR$, 1)
   IF DR$ <> fdrive$ THEN
      CHDRIVE fdrive$
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
     CHDIR fdrive$ + ":" + FPATH$
   ELSE
     DIRLEN = LEN(FPATH$)
     FILEP$ = LEFT$(FPATH$, DIRLEN - 1)
     CHDIR fdrive$ + ":" + FILEP$
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
     A$ = DIR$(fdrive$ + ":" + FPATH$ + "\X.X")
     IF ERR <> 0 THEN ERR = 0: FOUND = 1: EXIT SUB    '---ERROR IN PATH--
'------------------------------------
'CHECK IF BAD FILE NAME OR FILE FOUND
'------------------------------------
203  A$ = DIR$(FILEN$)
   IF ERR <> 0 THEN ERR = 0: FOUND = 2: EXIT SUB    '---ERROR IN FILE NAME--
   IF LTRIM$(RTRIM$(FPATH$)) = "" THEN
      A$ = DIR$(fdrive$ + ":" + FILEN$)
   ELSE
      A$ = DIR$(fdrive$ + ":" + FPATH$ + "\" + FILEN$)
   END IF
   IF ERR <> 0 THEN ERR = 0: FOUND = 1: EXIT SUB  '---Invalid path--
   IF LEN(A$) = 0 THEN
     FOUND = 3: EXIT SUB                           '---File not found--
   ELSE
     FOUND = 4: EXIT SUB                           '---FILE ALREADY EXISTS--
   END IF
EXIT SUB
END SUB

SUB HLPMSG (F2HELP$)
     DIM NAME$(40), LOCHELP%(40, 2), LI$(500)
     PCOPY VPAGE%, HPAGE%
     PCOPY VPAGE%, APAGE%
     CLS APAGE%
     SCREEN 0, , VPAGE%, VPAGE%
     A$ = DIR$("CLIMHELP.HLP")
     B$ = DIR$("CLIMTAGS.HLP")
     IF A$ = "" OR B$ = "" THEN
       IF B$ = "" THEN LOCATE 10, 16: PRINT "CLIMTAGS.HLP FILE IS NOT FOUND ON THE DEFAULT DIRECTORY"
       IF A$ = "" THEN LOCATE 11, 16: PRINT "CLIMHELP.HLP FILE IS NOT FOUND ON THE DEFAULT DIRECTORY"
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
     OPEN "CLIMTAGS.HLP" FOR INPUT AS #2
     FOR I = 1 TO 40
        IF EOF(2) THEN GOTO 90052
        LINE INPUT #2, A$
        NAME$(I) = LTRIM$(RTRIM$(A$))
        INPUT #2, LOCHELP%(I, 1)
        INPUT #2, LOCHELP%(I, 2)
      NEXT I
90052  CLOSE
      OPEN "CLIMHELP.HLP" FOR RANDOM AS #1 LEN = 80
      FIELD #1, 80 AS L$
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'DETERMINE STARTING POINT OF THE REQUIRED HELP MESSAGE BASED ON THE
'TAG PASSED FROM THE PROGRAM
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
      FOR I = 1 TO 40
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

SUB ITEMSELECT (FIRSTROW, NUMINCOL, TOTNUM, states$(), SELECT$, ITEMNUM)
     DIM ICLN%(4), BEG%(100), LENG%(100)
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' This routine allows the user to move around                '
' column(s) of files and to select a file to retrun its name '
' to the calling routine                                     '
' K = First row number for lsiting tabulated values          '
' NUMINCOL= Numner of values per column                      '
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
     Ifound = 0    'IFOUND=0 ==> CITY/STATE FROM DATA11 IS NOT DOUND
                   'IFOUND=1 ==> CITY/STATE FROM DATA11 IS FOUND
     COLOR 4, 3
     LOCATE 25, 1: PRINT "   TO SELECT: Cursor       Then Enter                        Esc = Cancel       ";
     LOCATE 25, 23: PRINT CHR$(27); CHR$(24); CHR$(25); CHR$(26);
     COLOR 15, 1
       IROW = 0: icol = 0
          K = FIRSTROW
          FOR I = 1 TO NUMINCOL
            K = K + 1
            BEG%(I) = K: LENG%(I) = LEN(states$(I))
            LOCATE K, 4: PRINT states$(I)
            IF states$(I) = SELECT$ THEN
               COLOR 1, 15
               LOCATE K, 4
               PRINT states$(I)
               COLOR 15, 1
               IRAW = K: ICL = 1: Ifound = 1
            END IF
          NEXT I
         K = FIRSTROW
         FOR I = NUMINCOL + 1 TO NUMINCOL * 2
            K = K + 1
            BEG%(I) = K: LENG%(I) = LEN(states$(I))
            LOCATE K, 26: PRINT states$(I)
            IF states$(I) = SELECT$ THEN
               COLOR 1, 15
               LOCATE K, 26
               PRINT states$(I)
               COLOR 15, 1
               IRAW = K: ICL = 2: Ifound = 1
            END IF
         NEXT I
         K = FIRSTROW
         FOR I = NUMINCOL * 2 + 1 TO NUMINCOL * 3
            K = K + 1
            BEG%(I) = K: LENG%(I) = LEN(states$(I))
            LOCATE K, 45: PRINT states$(I)
            IF states$(I) = SELECT$ THEN
               COLOR 1, 15
               LOCATE K, 45
               PRINT states$(I)
               COLOR 15, 1
               IRAW = K: ICL = 3: Ifound = 1
            END IF
         NEXT I
         K = FIRSTROW
         FOR I = NUMINCOL * 3 + 1 TO NUMINCOL * 4
            K = K + 1
            BEG%(I) = K: LENG%(I) = LEN(states$(I))
            LOCATE K, 64: PRINT states$(I)
            IF states$(I) = SELECT$ THEN
               COLOR 1, 15
               LOCATE K, 64
               PRINT states$(I)
               COLOR 15, 1
               IRAW = K: ICL = 4: Ifound = 1
            END IF
         NEXT I
         ICLN%(1) = 4: ICLN%(2) = 26: ICLN%(3) = 45: ICLN%(4) = 64
         I = 1: IROW = FIRSTROW + 1: KK = 1
         IF Ifound = 0 THEN
            I = 1: IROW = FIRSTROW + 1: KK = 1
         ELSE
            IROW = IRAW
            I = ICL
         END IF
18901    K = IROW - FIRSTROW + NUMINCOL * (I - 1)
         icol = ICLN%(I)
         IF K > TOTNUM THEN
           K = KK
           IROW = OLDROW
           icol = OLDCOL
         ELSE
           KK = K
           OLDROW = IROW
           OLDCOL = icol
         END IF
         xleng = LENG%(K)
         CALL RCELL(IROW, icol, xleng, states$(K))
'EXIT
     IF IRTRN% = 100 THEN EXIT SUB
'MOVE UP
         IF (IRTRN% = 3) AND (IROW = FIRSTROW + 1) THEN
           IF FIX(TOTNUM / NUMINCOL) * NUMINCOL >= I * NUMINCOL THEN
              IROW = NUMINCOL + FIRSTROW
              GOTO 18901
           ELSE
              IROW = FIRSTROW + TOTNUM - FIX(TOTNUM / NUMINCOL) * NUMINCOL
              GOTO 18901
           END IF
         END IF
         IF (IRTRN% = 3) AND (IROW > 3) THEN IROW = IROW - 1
         IF (IRTRN% = 4) AND (IROW = NUMINCOL + 2) THEN
           IROW = FIRSTROW + 1
           GOTO 18901
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
         GOTO 18901
       END IF
'MOVE RIGHT
        IF (IRTRN% = 6) AND (I = 4) THEN
          I = 1
          GOTO 18901
      END IF
      IF (IRTRN% = 6) AND (I < 4) THEN
         IF K + NUMINCOL > TOTNUM THEN
             I = 1
         ELSE
             I = I + 1
         END IF
      END IF
'MOVE LEFT
      IF (IRTRN% = 7) AND (I = 1) THEN
          IF IROW - FIRSTROW + NUMINCOL <= TOTNUM THEN I = 2
          IF IROW - FIRSTROW + NUMINCOL * 2 <= TOTNUM THEN I = 3
          IF IROW - FIRSTROW + NUMINCOL * 3 <= TOTNUM THEN I = 4
          GOTO 18901
      END IF
      IF (IRTRN% = 7) AND (I > 1) THEN I = I - 1
'SELECT HIGHLIGHTED ITEM
      IF IRTRN% = 5 THEN
          SELECT$ = states$(K)
          COLOR 1, 15: LOCATE IROW, icol: PRINT states$(K); : COLOR 15, 1
          ITEMNUM = K
          GOTO 18197
      END IF
      GOTO 18901
18197 EXIT SUB
END SUB

SUB LOCATION (CLIM$, OK, CLIMETH$)
       DIM city$(52, 15), ncities(52), states$(52), A$(52)
       ON LOCAL ERROR RESUME NEXT
       PCOPY VPAGE%, HPAGE%
       IRTRN% = 0
       PCOPY VPAGE%, APAGE%
       CLS APAGE%
       SCREEN 0, , VPAGE%, VPAGE%
       FOR I = 1 TO 52
          states$(I) = "           "
       NEXT I
       CLIM1$ = "     PRECIPITATION     "
       CLIM2$ = "      TEMPERATURE      "
       CLIM3$ = "    SOLAR RADIATION    "
     IF CLIMATF$ = "DEFRAIN" THEN HEADING$ = "อออออออออออ DEFAULT PRECIPITATION อออออออออ"
     IF CLIMATF$ = "SYNRAIN" THEN HEADING$ = "ออออออออออ SYNTHETIC PRECIPITATION ออออออออ"
     IF CLIMATF$ = "SYNTEMP" THEN HEADING$ = " SYNTHETIC TEMPERATURE AND SOLAR RADIATION "
   IF CLIMATF$ = "DEFRAIN" THEN
     ''''''''''''''''''''''''''''''''''''''''''
     'DEFAULT PRECIPITATION STATES AND CITIES
     ''''''''''''''''''''''''''''''''''''''''''
       nstates = 46
     states$(1) = "ALASKA": ncities(1) = 3
       city$(1, 1) = "ANNETTE": city$(1, 2) = "BETHEL": city$(1, 3) = "FAIRBANKS"
     states$(2) = "ARIZONA": ncities(2) = 3
       city$(2, 1) = "FLAGSTAFF": city$(2, 2) = "PHOENIX": city$(2, 3) = "TUCSON"
     states$(3) = "ARKANSAS": ncities(3) = 1
       city$(3, 1) = "LITTLE ROCK"
     states$(4) = "CALIFORNIA": ncities(4) = 5
       city$(4, 1) = "FRESNO": city$(4, 2) = "LOS ANGELES": city$(4, 3) = "SACRAMENTO": city$(4, 4) = "SAN DIEGO": city$(4, 5) = "SANTA MARIA"
     states$(5) = "COLORADO": ncities(5) = 2
       city$(5, 1) = "DENVER": city$(5, 2) = "GRAND JUNCTION"
     states$(6) = "CONNECTICUT": ncities(6) = 3
       city$(6, 1) = "BRIDGEPORT": city$(6, 2) = "HARTFORD": city$(6, 3) = "NEW HAVEN"
     states$(7) = "FLORIDA": ncities(7) = 6
       city$(7, 1) = "JACKSONVILLE": city$(7, 2) = "MIAMI": city$(7, 3) = "ORLANDO": city$(7, 4) = "TALLAHASSEE": city$(7, 5) = "TAMPA": city$(7, 6) = "W. PALM BEACH":
     states$(8) = "GEORGIA": ncities(8) = 2
       city$(8, 1) = "ATLANTA": city$(8, 2) = "WATKINSVILLE"
     states$(9) = "HAWAII": ncities(9) = 1
       city$(9, 1) = "HONOLULU"
     states$(10) = "IDAHO": ncities(10) = 2
       city$(10, 1) = "BOISE": city$(10, 2) = "POCATELLO":
     states$(11) = "ILLINOIS": ncities(11) = 2
       city$(11, 1) = "CHICAGO": city$(11, 2) = "E. ST. LOUIS":
     states$(12) = "INDIANA": ncities(12) = 1
       city$(12, 1) = "INDIANAPOLIS"
     states$(13) = "IOWA": ncities(13) = 1
       city$(13, 1) = "DES MOINES"
     states$(14) = "KANSAS": ncities(14) = 2
       city$(14, 1) = "DODGE CITY": city$(14, 2) = "TOPEKA":
     states$(15) = "KENTUCKY": ncities(15) = 1
       city$(15, 1) = "LEXINGTON"
     states$(16) = "LOUISIANA": ncities(16) = 3
       city$(16, 1) = "LAKE CHARLES": city$(16, 2) = "NEW ORLEANS": city$(16, 3) = "SHREVEPORT":
     states$(17) = "MAINE": ncities(17) = 4
       city$(17, 1) = "AUGUSTA": city$(17, 2) = "BANGOR": city$(17, 3) = "CARIBOU": city$(17, 4) = "PORTLAND":
     states$(18) = "MASSACHUSETTS": ncities(18) = 3
       city$(18, 1) = "BOSTON": city$(18, 2) = "PLAINFIELD": city$(18, 3) = "WORCESTER":
     states$(19) = "MICHIGAN": ncities(19) = 2
       city$(19, 1) = "EAST LANSING": city$(19, 2) = "SAULT STE. MARIE":
     states$(20) = "MINNESOTA": ncities(20) = 1
       city$(20, 1) = "ST. CLOUD"
     states$(21) = "MISSOURI": ncities(21) = 1
       city$(21, 1) = "COLUMBIA"
     states$(22) = "MONTANA": ncities(22) = 2
       city$(22, 1) = "GLASGOW": city$(22, 2) = "GREAT FALLS":
     states$(23) = "NEBRASKA": ncities(23) = 2
       city$(23, 1) = "GRAND ISLAND": city$(23, 2) = "NORTH OMAHA":
     states$(24) = "NEVADA": ncities(24) = 2
       city$(24, 1) = "ELY  ": city$(24, 2) = "LAS VEGAS"
     states$(25) = "NEW HAMPSHIRE": ncities(25) = 2
       city$(25, 1) = "CONCORD": city$(25, 2) = "NASHUA"
     states$(26) = "NEW JERSEY": ncities(26) = 2
       city$(26, 1) = "EDISON": city$(26, 2) = "SEABROOK"
     states$(27) = "NEW MEXICO": ncities(27) = 1
       city$(27, 1) = "ALBUQUERQUE"
     states$(28) = "NEW YORK": ncities(28) = 5
       city$(28, 1) = "ALBANY": city$(28, 2) = "CENTRAL PARK": city$(28, 3) = "ITHACA": city$(28, 4) = "NEW YORK": city$(28, 5) = "SYRACUSE":
     states$(29) = "NORTH CAROLINA": ncities(29) = 1
       city$(29, 1) = "GREENSBORO"
     states$(30) = "NORTH DAKOTA": ncities(30) = 1
       city$(30, 1) = "BISMARCK"
     states$(31) = "OHIO": ncities(31) = 4
       city$(31, 1) = "CINCINNATI": city$(31, 2) = "CLEVELAND": city$(31, 3) = "COLUMBUS": city$(31, 4) = "PUT-IN-BAY"
     states$(32) = "OKLAHOMA": ncities(32) = 2
       city$(32, 1) = "OKLAHOMA CITY": city$(32, 2) = "TULSA":
     states$(33) = "OREGON": ncities(33) = 3
       city$(33, 1) = "ASTORIA": city$(33, 2) = "MEDFORD": city$(33, 3) = "PORTLAND":
     states$(34) = "PENNSYLVANIA": ncities(34) = 2
       city$(34, 1) = "PHILADELPHIA": city$(34, 2) = "PITTSBURGH"
     states$(35) = "RHODE ISLAND": ncities(35) = 1
       city$(35, 1) = "PROVIDENCE"
     states$(36) = "SOUTH CAROLINA": ncities(36) = 1
       city$(36, 1) = "CHARLESTON"
     states$(37) = "SOUTH DAKOTA": ncities(37) = 1
       city$(37, 1) = "RAPID CITY":
     states$(38) = "TENNESSEE": ncities(38) = 2
       city$(38, 1) = "KNOXVILLE": city$(38, 2) = "NASHVILLE"
     states$(39) = "TEXAS": ncities(39) = 5
       city$(39, 1) = "BROWNSVILLE": city$(39, 2) = "DALLAS": city$(39, 3) = "EL PASO": city$(39, 4) = "MIDLAND": city$(39, 5) = "SAN ANTONIO"
     states$(40) = "UTAH": ncities(40) = 2
       city$(40, 1) = "CEDAR CITY": city$(40, 2) = "SALT LAKE CITY":
     states$(41) = "VERMONT": ncities(41) = 3
       city$(41, 1) = "BURLINGTON": city$(41, 2) = "MONTPELIER": city$(41, 3) = "RUTLAND":
     states$(42) = "VIRGINIA": ncities(42) = 2
       city$(42, 1) = "LYNCHBURG": city$(42, 2) = "NORFOLK":
     states$(43) = "WASHINGTON": ncities(43) = 3
       city$(43, 1) = "PULLMAN": city$(43, 2) = "SEATTLE": city$(43, 3) = "YAKIMA":
     states$(44) = "WISCONSIN": ncities(44) = 1
       city$(44, 1) = "MADISON"
     states$(45) = "WYOMING": ncities(45) = 2
       city$(45, 1) = "CHEYENNE": city$(45, 2) = "LANDER":
     states$(46) = "PUERTO RICO": ncities(46) = 1
       city$(46, 1) = "SAN JUAN"
   END IF
   IF CLIMATF$ = "SYNRAIN" THEN
      '''''''''''''''''''''
      'SYNTHETIC RAINFALL
      '''''''''''''''''''''
      nstates = 48
      states$(1) = "ALABAMA": ncities(1) = 3
        city$(1, 1) = "BIRMINGHAM": city$(1, 2) = "MOBILE": city$(1, 3) = "MONTGOMERY"
      states$(2) = "ARIZONA": ncities(2) = 3
        city$(2, 1) = "FLAGSTAFF": city$(2, 2) = "PHOENIX": city$(2, 3) = "YUMA ":
      states$(3) = "ARKANSAS": ncities(3) = 2
        city$(3, 1) = "FORT SMITH": city$(3, 2) = "LITTLE ROCK"
      states$(4) = "CALIFORNIA": ncities(4) = 7
        city$(4, 1) = "BAKERSFIELD": city$(4, 2) = "BLUE CANYON": city$(4, 3) = "EUREKA": city$(4, 4) = "FRESNO": city$(4, 5) = "MT. SHASTA": city$(4, 6) = "SAN DIEGO": city$(4, 7) = "SAN FRANCISCO":
      states$(5) = "COLORADO": ncities(5) = 4
        city$(5, 1) = "COLORADO SPRINGS": city$(5, 2) = "DENVER": city$(5, 3) = "GRAND JUNCTION": city$(5, 4) = "PUEBLO"
      states$(6) = "CONNECTICUT": ncities(6) = 1
        city$(6, 1) = "WINDSOR LOCKS"
      states$(7) = "DELAWARE": ncities(7) = 1
        city$(7, 1) = "WILMINGTON"
      states$(8) = "DISTRICT OF COLUMBIA": ncities(8) = 1
        city$(8, 1) = "WASHINGTON"
      states$(9) = "FLORIDA": ncities(9) = 4
        city$(9, 1) = "JACKSONVILLE": city$(9, 2) = "MIAMI": city$(9, 3) = "TALLAHASSEE":         city$(9, 4) = "TAMPA"
      states$(10) = "GEORGIA": ncities(10) = 4
        city$(10, 1) = "ATLANTA": city$(10, 2) = "AUGUSTA": city$(10, 3) = "MACON": city$(10, 4) = "SAVANNAH"
     ' states$(11) = "HAWAII": 'ncities(11) = 1
      '  city$(11, 1) = "HONOLULU"
      states$(11) = "IDAHO": ncities(11) = 2
        city$(11, 1) = "BOISE": city$(11, 2) = "POCATELLO"
      states$(12) = "ILLINOIS": ncities(12) = 1
        city$(12, 1) = "CHICAGO"
      states$(13) = "INDIANA": ncities(13) = 3
        city$(13, 1) = "EVANSVILLE": city$(13, 2) = "FORT WAYNE": city$(13, 3) = "INDIANAPOLIS"
      states$(14) = "IOWA ": ncities(14) = 2
        city$(14, 1) = "DES MOINES": city$(14, 2) = "DUBUQUE"
      states$(15) = "KANSAS": ncities(15) = 3
        city$(15, 1) = "DODGE CITY": city$(15, 2) = "TOPEKA": city$(15, 3) = "WICHITA"
      states$(16) = "KENTUCKY": ncities(16) = 3
        city$(16, 1) = "COVINGTON": city$(16, 2) = "LEXINGTON": city$(16, 3) = "LOUISVILLE"
      states$(17) = "LOUISIANA": ncities(17) = 3
        city$(17, 1) = "BATON ROUGE": city$(17, 2) = "NEW ORLEANS": city$(17, 3) = "SHREVEPORT"
      states$(18) = "MAINE": ncities(18) = 2
        city$(18, 1) = "CARIBOU": city$(18, 2) = "PORTLAND"
      states$(19) = "MARYLAND": ncities(19) = 1
        city$(19, 1) = "BALTIMORE"
      states$(20) = "MASSACHUSETTS": ncities(20) = 2
        city$(20, 1) = "BOSTON": city$(20, 2) = "NANTUCKET"
      states$(21) = "MICHIGAN": ncities(21) = 2
        city$(21, 1) = "DETROIT": city$(21, 2) = "GRAND RAPIDS"
      states$(22) = "MINNESOTA": ncities(22) = 2
        city$(22, 1) = "DULUTH": city$(22, 2) = "MINNEAPOLIS"
      states$(23) = "MISSISSIPPI": ncities(23) = 2
        city$(23, 1) = "JACKSON": city$(23, 2) = "MERIDIAN"
      states$(24) = "MISSOURI": ncities(24) = 3
        city$(24, 1) = "COLUMBIA": city$(24, 2) = "KANSAS CITY": city$(24, 3) = "ST. LOUIS"
      states$(25) = "MONTANA": ncities(25) = 6
        city$(25, 1) = "BILLINGS": city$(25, 2) = "GREAT FALLS":  city$(25, 3) = "HAVRE":  city$(25, 4) = "HELENA":  city$(25, 5) = "KALISPELL":  city$(25, 6) = "MILES CITY":
      states$(26) = "NEBRASKA": ncities(26) = 3
        city$(26, 1) = "GRAND ISLAND":  city$(26, 2) = "NORTH PLATTE":  city$(26, 3) = "SCOTTSBLUFF":
      states$(27) = "NEVADA": ncities(27) = 4
        city$(27, 1) = "ELKO ":  city$(27, 2) = "LAS VEGAS":  city$(27, 3) = "RENO ":  city$(27, 4) = "WINNEMUCCA":
      states$(28) = "NEW HAMPSHIRE": ncities(28) = 2
        city$(28, 1) = "CONCORD":  city$(28, 2) = "MT. WASHINGTON":
      states$(29) = "NEW JERSEY": ncities(29) = 1
        city$(29, 1) = "NEWARK"
      states$(30) = "NEW MEXICO": ncities(30) = 2
        city$(30, 1) = "ALBUQUERQUE":  city$(30, 2) = "ROSWELL"
      states$(31) = "NEW YORK": ncities(31) = 4
        city$(31, 1) = "ALBANY":  city$(31, 2) = "BUFFALO":  city$(31, 3) = "NEW YORK":  city$(31, 4) = "SYRACUSE"
      states$(32) = "NORTH CAROLINA": ncities(32) = 4
        city$(32, 1) = "ASHEVILLE":  city$(32, 2) = "CHARLOTTE":  city$(32, 3) = "GREENSBORO":  city$(32, 4) = "RALEIGH"
      states$(33) = "NORTH DAKOTA": ncities(33) = 2
        city$(33, 1) = "BISMARCK":  city$(33, 2) = "WILLISTON"
      states$(34) = "OHIO ": ncities(34) = 3
        city$(34, 1) = "CLEVELAND":  city$(34, 2) = "COLUMBUS":  city$(34, 3) = "TOLEDO"
      states$(35) = "OKLAHOMA": ncities(35) = 2
        city$(35, 1) = "OKLAHOMA CITY":  city$(35, 2) = "TULSA":
      states$(36) = "OREGON": ncities(36) = 7
        city$(36, 1) = "BURNS":  city$(36, 2) = "MEACHEM":  city$(36, 3) = "MEDFORD":  city$(36, 4) = "PENDLETON":  city$(36, 5) = "PORTLAND":  city$(36, 6) = "SALEM":  city$(36, 7) = "SEXTON SUMMIT":
      states$(37) = "PENNSYLVANIA": ncities(37) = 2
        city$(37, 1) = "PHILADELPHIA":  city$(37, 2) = "PITTSBURGH":
      states$(38) = "RHODE ISLAND": ncities(38) = 1
        city$(38, 1) = "PROVIDENCE"
      states$(39) = "SOUTH CAROLINA": ncities(39) = 2
        city$(39, 1) = "CHARLESTON": city$(39, 2) = "COLUMBIA"
      states$(40) = "SOUTH DAKOTA": ncities(40) = 2
        city$(40, 1) = "HURON":  city$(40, 2) = "RAPID CITY":
      states$(41) = "TENNESSEE": ncities(41) = 4
        city$(41, 1) = "CHATTANOOGA":  city$(41, 2) = "KNOXVILLE":  city$(41, 3) = "MEMPHIS":  city$(41, 4) = "NASHVILLE"
      states$(42) = "TEXAS": ncities(42) = 12
        city$(42, 1) = "ABILENE":  city$(42, 2) = "AMARILLO":  city$(42, 3) = "AUSTIN":  city$(42, 4) = "BROWNSVILLE":  city$(42, 5) = "CORPUS CHRISTI":  city$(42, 6) = "DALLAS":  city$(42, 7) = "EL PASO":  city$(42, 8) = "GALVESTON":  city$(42, 9)  _
= "HOUSTON": city$(42, 10) = "SAN ANTONIO": city$(42, 11) = "TEMPLE": city$(42, 12) = "WACO ":
      states$(43) = "UTAH ": ncities(43) = 2
        city$(43, 1) = "MILFORD":  city$(43, 2) = "SALT LAKE CITY":
      states$(44) = "VIRGINIA": ncities(44) = 2
        city$(44, 1) = "NORFOLK":  city$(44, 2) = "RICHMOND":
      states$(45) = "WASHINGTON": ncities(45) = 5
        city$(45, 1) = "OLYMPIA":  city$(45, 2) = "SPOKANE":  city$(45, 3) = "STAMPEDE PASS":  city$(45, 4) = "WALLA WALLA":  city$(45, 5) = "YAKIMA":
      states$(46) = "WEST VIRGINIA": ncities(46) = 1
        city$(46, 1) = "CHARLESTON"
      states$(47) = "WISCONSIN": ncities(47) = 4
        city$(47, 1) = "GREEN BAY":  city$(47, 2) = "LACROSSE":  city$(47, 3) = "MADISON":  city$(47, 4) = "MILWAUKEE":
      states$(48) = "WYOMING": ncities(48) = 1
        city$(48, 1) = "CHEYENNE"
     'states$(50) = "PUERTO RICO": 'ncities(50) = 1
       'city$(50, 1) = "SAN JUAN"
        END IF
   IF CLIMATF$ = "SYNTEMP" THEN
     ''''''''''''''''''''''''''''''''''''''''''''
     'SYNTHETIC TEMPERATURE AND SOLAR RADIATION
     ''''''''''''''''''''''''''''''''''''''''''''
      nstates = 52
      states$(1) = "ALABAMA": ncities(1) = 3
        city$(1, 1) = "BIRMINGHAM": city$(1, 2) = "MOBILE": city$(1, 3) = "MONTGOMERY"
      states$(2) = "ALASKA": ncities(2) = 3
        city$(2, 1) = "ANNETTE": city$(2, 2) = "BETHEL": city$(2, 3) = "FAIRBANKS"
      states$(3) = "ARIZONA": ncities(3) = 4
        city$(3, 1) = "FLAGSTAFF": city$(3, 2) = "PHOENIX": city$(3, 3) = "TUCSON": city$(3, 4) = "YUMA "
      states$(4) = "ARKANSAS": ncities(4) = 2
        city$(4, 1) = "FORT SMITH": city$(4, 2) = "LITTLE ROCK"
      states$(5) = "CALIFORNIA": ncities(5) = 10
        city$(5, 1) = "BAKERSFIELD": city$(5, 2) = "BLUE CANYON": city$(5, 3) = "EUREKA": city$(5, 4) = "LOS ANGELES": city$(5, 5) = "FRESNO"
        city$(5, 6) = "MT. SHASTA": city$(5, 7) = "SACRAMENTO": city$(5, 8) = "SAN DIEGO": city$(5, 9) = "SAN FRANCISCO": city$(5, 10) = "SANTA MARIA"
      states$(6) = "COLORADO": ncities(6) = 4
        city$(6, 1) = "COLORADO SPRINGS": city$(6, 2) = "DENVER":  city$(6, 3) = "GRAND JUNCTION":  city$(6, 4) = "PUEBLO"
      states$(7) = "CONNECTICUT": ncities(7) = 4
        city$(7, 1) = "BRIDGEPORT": city$(7, 2) = "HARTFORD": city$(7, 3) = "NEW HAVEN": city$(7, 4) = "WINDSOR LOCKS"
      states$(8) = "DELAWARE": ncities(8) = 1
        city$(8, 1) = "WILMINGTON"
      states$(9) = "DISTRICT OF COLUMBIA": ncities(9) = 1
        city$(9, 1) = "WASHINGTON"
      states$(10) = "FLORIDA": ncities(10) = 6
        city$(10, 1) = "JACKSONVILLE": city$(10, 2) = "MIAMI": city$(10, 3) = "ORLANDO": city$(10, 4) = "TALLAHASSEE": city$(10, 5) = "TAMPA": city$(10, 6) = "W. PALM BEACH"
      states$(11) = "GEORGIA": ncities(11) = 5
        city$(11, 1) = "ATLANTA": city$(11, 2) = "AUGUSTA": city$(11, 3) = "MACON": city$(11, 4) = "SAVANNAH": city$(11, 5) = "WATKINSVILLE"
      states$(12) = "HAWAII": ncities(12) = 1
        city$(12, 1) = "HONOLULU"
      states$(13) = "IDAHO": ncities(13) = 2
        city$(13, 1) = "BOISE": city$(13, 2) = "POCATELLO"
      states$(14) = "ILLINOIS": ncities(14) = 2
        city$(14, 1) = "CHICAGO": city$(14, 2) = "EAST ST. LOUIS"
      states$(15) = "INDIANA": ncities(15) = 3
        city$(15, 1) = "EVANSVILLE": city$(15, 2) = "FORT WAYNE":  city$(15, 3) = "INDIANAPOLIS"
      states$(16) = "IOWA ": ncities(16) = 2
        city$(16, 1) = "DES MOINES": city$(16, 2) = "DUBUQUE":
      states$(17) = "KANSAS": ncities(17) = 3
        city$(17, 1) = "DODGE CITY": city$(17, 2) = "TOPEKA": city$(17, 3) = "WICHITA"
      states$(18) = "KENTUCKY": ncities(18) = 3
        city$(18, 1) = "COVINGTON": city$(18, 2) = "LEXINGTON": city$(18, 3) = "LOUISVILLE"
      states$(19) = "LOUISIANA": ncities(19) = 4
        city$(19, 1) = "BATON ROUGE": city$(19, 2) = "LAKE CHARLES": city$(19, 3) = "NEW ORLEANS": city$(19, 4) = "SHREVEPORT"
      states$(20) = "MAINE": ncities(20) = 4
        city$(20, 1) = "AUGUSTA": city$(20, 2) = "BANGOR": city$(20, 3) = "CARIBOU": city$(20, 4) = "PORTLAND"
      states$(21) = "MARYLAND": ncities(21) = 1
        city$(21, 1) = "BALTIMORE"
      states$(22) = "MASSACHUSETTS": ncities(22) = 4
        city$(22, 1) = "BOSTON": city$(22, 2) = "NANTUCKET": city$(22, 3) = "PLAINFIELD": city$(22, 4) = "WORCHESTER"
      states$(23) = "MICHIGAN": ncities(23) = 4
        city$(23, 1) = "DETROIT": city$(23, 2) = "E. LANSING": city$(23, 3) = "GRAND RAPIDS": city$(23, 4) = "SAULT STE. MARIE"
      states$(24) = "MINNESOTA": ncities(24) = 3
        city$(24, 1) = "DULUTH": city$(24, 2) = "MINNEAPOLIS": city$(24, 3) = "ST. CLOUD"
      states$(25) = "MISSISSIPPI": ncities(25) = 2
        city$(25, 1) = "JACKSON": city$(25, 2) = "MERIDIAN"
      states$(26) = "MISSOURI": ncities(26) = 3
        city$(26, 1) = "COLUMBIA": city$(26, 2) = "KANSAS CITY": city$(26, 3) = "ST. LOUIS"
      states$(27) = "MONTANA": ncities(27) = 7
        city$(27, 1) = "BILLINGS": city$(27, 2) = "GLASGOW": city$(27, 3) = "GREAT FALLS": city$(27, 4) = "HAVRE": city$(27, 5) = "HELENA": city$(27, 6) = "KALISPELL": city$(27, 7) = "MILES CITY"
      states$(28) = "NEBRASKA": ncities(28) = 4
        city$(28, 1) = "GRAND ISLAND": city$(28, 2) = "NORTH PLATTE": city$(28, 3) = "OMAHA": city$(28, 4) = "SCOTTSBLUFF":
      states$(29) = "NEVADA": ncities(29) = 5
        city$(29, 1) = "ELKO ": city$(29, 2) = "ELY  ": city$(29, 3) = "LAS VEGAS": city$(29, 4) = "RENO ": city$(29, 5) = "WINNEMUCCA":
      states$(30) = "NEW HAMPSHIRE": ncities(30) = 3
        city$(30, 1) = "CONCORD": city$(30, 2) = "MT. WASHINGTON": city$(30, 3) = "NASHUA"
      states$(31) = "NEW JERSEY": ncities(31) = 3
        city$(31, 1) = "EDISON": city$(31, 2) = "NEWARK": city$(31, 3) = "SEABROOK"
      states$(32) = "NEW MEXICO": ncities(32) = 2
        city$(32, 1) = "ALBUQUERQUE": city$(32, 2) = "ROSWELL"
      states$(33) = "NEW YORK": ncities(33) = 7
        city$(33, 1) = "ALBANY": city$(33, 2) = "BUFFALO": city$(33, 3) = "CENTRAL PARK": city$(33, 4) = "ITHACA": city$(33, 5) = "NEW YORK": city$(33, 6) = "SCHENECTADY": city$(33, 7) = "SYRACUSE"
      states$(34) = "NORTH CAROLINA": ncities(34) = 4
        city$(34, 1) = "ASHEVILLE": city$(34, 2) = "CHARLOTTE": city$(34, 3) = "GREENSBORO": city$(34, 4) = "RALEIGH"
      states$(35) = "NORTH DAKOTA": ncities(35) = 2
        city$(35, 1) = "BISMARCK": city$(35, 2) = "WILLISTON":
      states$(36) = "OHIO ": ncities(36) = 5
        city$(36, 1) = "CINCINNATI": city$(36, 2) = "CLEVELAND": city$(36, 3) = "COLUMBUS": city$(36, 4) = "PUT-IN-BAY": city$(36, 5) = "TOLEDO"
      states$(37) = "OKLAHOMA": ncities(37) = 2
        city$(37, 1) = "OKLAHOMA CITY": city$(37, 2) = "TULSA"
      states$(38) = "OREGON": ncities(38) = 8
        city$(38, 1) = "ASTORIA": city$(38, 2) = "BURNS": city$(38, 3) = "MEACHAM": city$(38, 4) = "MEDFORD": city$(38, 5) = "PENDLETON"
        city$(38, 6) = "PORTLAND": city$(38, 7) = "SALEM": city$(38, 8) = "SEXTON SUMMIT"
      states$(39) = "PENNSYLVANIA": ncities(39) = 2
        city$(39, 1) = "PHILADELPHIA": city$(39, 2) = "PITTSBURGH"
      states$(40) = "RHODE ISLAND": ncities(40) = 1
        city$(40, 1) = "PROVIDENCE"
      states$(41) = "SOUTH CAROLINA": ncities(41) = 2
        city$(41, 1) = "CHARLESTON": city$(41, 2) = "COLUMBIA"
      states$(42) = "SOUTH DAKOTA": ncities(42) = 2
        city$(42, 1) = "HURON": city$(42, 2) = "RAPID CITY"
      states$(43) = "TENNESSEE": ncities(43) = 4
        city$(43, 1) = "CHATTANOOGA": city$(43, 2) = "KNOXVILLE": city$(43, 3) = "MEMPHIS": city$(43, 4) = "NASHVILLE"
      states$(44) = "TEXAS": ncities(44) = 13
        city$(44, 1) = "ABILENE":  city$(44, 2) = "AMARILLO":  city$(44, 3) = "AUSTIN":  city$(44, 4) = "BROWNSVILLE":  city$(44, 5) = "CORPUS CHRISTI":  city$(44, 6) = "DALLAS":  city$(44, 7) = "EL PASO"
        city$(44, 8) = "GALVESTON":  city$(44, 9) = "HOUSTON": city$(44, 10) = "MIDLAND": city$(44, 11) = "SAN ANTONIO": city$(44, 12) = "TEMPLE": city$(44, 13) = "WACO ":
      states$(45) = "UTAH ": ncities(45) = 3
        city$(45, 1) = "CEDAR CITY": city$(45, 2) = "MILFORD": city$(45, 3) = "SALT LAKE CITY"
      states$(46) = "VERMONT": ncities(46) = 3
        city$(46, 1) = "BURLINGTON": city$(46, 2) = "MONTPELIER": city$(46, 3) = "RUTLAND"
      states$(47) = "VIRGINIA": ncities(47) = 3
        city$(47, 1) = "LYNCHBURG": city$(47, 2) = "NORFOLK": city$(47, 3) = "RICHMOND"
      states$(48) = "WASHINGTON": ncities(48) = 7
        city$(48, 1) = "OLYMPIA": city$(48, 2) = "PULLMAN": city$(48, 3) = "SEATTLE": city$(48, 4) = "SPOKANE": city$(48, 5) = "STAMPEDE PASS": city$(48, 6) = "WALLA WALLA": city$(48, 7) = "YAKIMA"
      states$(49) = "WEST VIRGINIA": ncities(49) = 1
        city$(49, 1) = "CHARLESTON"
      states$(50) = "WISCONSIN": ncities(50) = 4
        city$(50, 1) = "GREEN BAY": city$(50, 2) = "LACROSSE": city$(50, 3) = "MADISON": city$(50, 4) = "MILWAUKEE"
      states$(51) = "WYOMING": ncities(51) = 2
        city$(51, 1) = "CHEYENNE": city$(51, 2) = "LANDER"
      states$(52) = "PUERTO RICO": ncities(52) = 1
        city$(52, 1) = "SAN JUAN"
      END IF
400 CLS
   SCREEN 0: COLOR 15, 1      ':cls
   sline = 1
   LOCATE sline, 1
   PRINT "ษออออออออออออออออออ"; : PRINT HEADING$; : PRINT "อออออออออออออออออป"
   FOR I% = 1 TO nopt + 16
     LOCATE sline + I%, 1: PRINT "บ": LOCATE sline + I%, 80: PRINT "บ"
   NEXT I%
   LOCATE 18, 1
   PRINT "ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน"
   FOR I% = 19 TO 22
     LOCATE I%, 1: PRINT "บ": LOCATE I%, 80: PRINT "บ"
   NEXT I%
   PRINT "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
   K = 2
   SELECST$ = ETSTATE$
   CALL ITEMSELECT(K, 13, nstates, states$(), SELECST$, ITEMNUM)
   s1$ = LEFT$(SELECST$, 4)
   IF IRTRN% = 2 OR IRTRN% = -1 OR IRTRN% = 1 OR IRTRN% = 100 THEN EXIT SUB
   TOTNUM = ncities(ITEMNUM)
   FOR I = 1 TO TOTNUM
     A$(I) = city$(ITEMNUM, I)
   NEXT I
   FOR I = TOTNUM + 1 TO 20
      A$(I) = " "
   NEXT I
   IF SELECST$ = ETSTATE$ THEN SELECCT$ = ETCITY$
4002   CALL ITEMSELECT(18, 4, TOTNUM, A$(), SELECCT$, ITEMNUM)
       s2$ = LEFT$(SELECCT$, 4)
   IF CLIMATF$ = "DEFRAIN" THEN
     IF IRTRN% = 5 THEN
        IRTRN% = 0
        IF DEFPAT$ = "" THEN DEFPAT$ = CURDIR$
       LOCATE 24, 1: PRINT " Path to default data files:                                                  ";
4001   CALL ACELL(24, 30, 51, DEFPAT$, 1)
       IF IRTRN% = 100 THEN EXIT SUB
       IF IRTRN% = 5 THEN
          DEFPAT$ = LTRIM$(RTRIM$(DEFPAT$))
          IF RIGHT$(DEFPAT$, 1) = "\" THEN
             XDEFPAT = LEN(DEFPAT$) - 1
             DEFPAT$ = LEFT$(DEFPAT$, XDEFPAT)
          END IF
          A1 = INSTR(DEFPAT$, ":")
          IF A1 = 0 THEN
             fdrive$ = LEFT$(CURDIR$, 1)
             FPATH$ = DEFPAT$
          ELSE
             fdrive$ = LEFT$(DEFPAT$, A1 - 1)
             FPATH$ = RIGHT$(DEFPAT$, LEN(DEFPAT$) - A1)
          END IF
          IF FPATH$ = "\" THEN FPATH$ = ""
          FILEN$ = "DUMMYF"
          FOUND = -1
          CALL FILECHK(fdrive$, FPATH$, FILEN$, FOUND)
          IF FOUND = 0 OR FOUND = 1 OR FOUND = 5 THEN
             BEEP
             COLOR 11, 6
             LOCATE 24, 1: PRINT "                    Invalid Directory or Path.  Try again...                    ";
             COLOR 15, 1
             GOTO 4002
          END IF
          DEFPAT$ = LTRIM$(RTRIM$(DEFPAT$))
          lefs1$ = LEFT$(s1$, 1)
          IF lefs1$ >= "A" AND lefs1$ <= "H" THEN DEFFILE$ = DEFPAT$ + "\TAPE3.A"
          IF lefs1$ >= "I" AND lefs1$ <= "M" THEN DEFFILE$ = DEFPAT$ + "\TAPE3.I"
          IF lefs1$ >= "N" AND lefs1$ <= "O " THEN DEFFILE$ = DEFPAT$ + "\TAPE3.N"
          IF lefs1$ >= "P" AND lefs1$ <= "W" THEN DEFFILE$ = DEFPAT$ + "\TAPE3.P"
          CLOSE
          GOSUB 64190
          CLOSE
          IF LEN(A$) = 0 THEN GOTO 400
        ELSE
          GOTO 4001
        END IF
      END IF
   END IF
   IF IRTRN% = 2 OR IRTRN% = -1 OR IRTRN% = 100 THEN EXIT SUB
907 EXIT SUB
''''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO READ DEFAULT PRECIPITATION VALUES
'and write data to save file
''''''''''''''''''''''''''''''''''''''''''''''''''
64190 A$ = DIR$(DEFFILE$)
      IF LEN(A$) = 0 THEN
        COLOR 11, 6
        LOCATE 24, 1: PRINT "                          DEFAULT DATA FILE NOT FOUND                           ";
        COLOR 15, 1
        COLOR 4, 3
        LOCATE 25, 1: PRINT " Load File:                                        PRESS ANY KEY TO CONTINUE... ";
        LOCATE 25, 13: PRINT DEFFILE$;
        COLOR 15, 1
64220   INK$ = INKEY$: IF INK$ = "" THEN GOTO 64220
        RETURN
     ELSE
        OPEN DEFFILE$ FOR INPUT AS #1
        FOR J = 1 TO 200
          IF EOF(1) THEN
            COLOR 11, 6
            LOCATE 25, 1:  PRINT " INAPPROPRIATE FILE CONTENTS... CHECK DATA IN FILE. PRESS ANY KEY TO CONTINUE ";
            COLOR 15, 1
64210       INK$ = INKEY$: IF INK$ = "" THEN GOTO 64210
            RETURN
          END IF
          LINE INPUT #1, L$
          IF LEFT$(L$, 4) = s1$ AND MID$(L$, 6, 4) = s2$ THEN
            OPEN "DATA4.TMP" FOR OUTPUT AS #2
            PRINT #2, " 1"
            PRINT #2, USING "##"; VAL(UNITS$)
            PRINT #2, USING "\                  \"; SELECCT$; SELECST$
            PRINT #2, "        "
            COLOR 11, 6
            LOCATE 25, 1:  PRINT "          READING AND SAVING DEFAULT PRECIPITATION DATA... PLEASE WAIT          ";
            COLOR 15, 1
            FOR I = 1 TO 185
               LINE INPUT #1, L$
               PRINT #2, L$
            NEXT I
            RETURN
          ELSE
            COLOR 11, 6
            LOCATE 25, 1:  PRINT "           SEARCHING FOR DEFAULT PRECIPITATION DATA... PLEASE WAIT              ";
            COLOR 15, 1
            FOR I = 1 TO 185
               LINE INPUT #1, L$
            NEXT I
          END IF
        NEXT J
      END IF
      RETURN
END SUB

SUB menu
      COLOR 4, 3
      LOCATE 25, 1: PRINT "  F1=Info   F2=Help   F3=Keys   F9=Quit   F10=End    PgUp=Cycle   PgDn=Proceed  ";
      COLOR 15, 1
  EXIT SUB
END SUB

SUB USERHELP
     PCOPY VPAGE%, HPAGE%
     IRTRN% = 0
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

