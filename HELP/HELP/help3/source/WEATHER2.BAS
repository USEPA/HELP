DECLARE SUB BOX ()
DECLARE SUB GETDIR (EXT$, SELECT$, directory$)
DECLARE SUB CLIMHELP2 (CLIM$, clfile$, cit$, sta$)
DECLARE SUB ACELL (IROW!, ISTART!, NS!, TEXT$, ITYPE!)
DECLARE SUB EXOUT (ANSCODE!)
DECLARE SUB FILECHK (fdrive$, FPATH$, FILEN$, found!)
DECLARE SUB HLPMSG (F2HELP$)
DECLARE SUB LOCATION (CLIM$, ok!, CLIMETH$)
DECLARE SUB menu ()
DECLARE SUB RCELL (IROW!, ISTART!, NS!, TXT$)
DECLARE SUB USERHELP ()
DECLARE SUB synprec (synstate$, syncity$, CLIM$, ok!, tape2lin$())
DECLARE SUB SYNTEMP (synstate$, syncity$, CLIM$, ok!, tape2lin$())
DECLARE SUB SYNSOLR (synstate$, syncity$, CLIM$, ok!, tape2lin$())
DECLARE SUB meantemp (cit$, sta$, unt!)
DECLARE SUB YEARSHEET (CLIM$, CLIMETH$, UNI$, COL!)

COMMON SHARED tempu$, PRECU$, solu$, UNITS$, UNIT$, UN1$, UN2$, UN3$, UN4$, UN5$
COMMON SHARED unev$, UNSL$, TOHELP%, NL%, CLIMATF$, irtrn%, PAGE%, CLIM
COMMON SHARED APAGE%, VPAGE%, HPAGE%, DIRECT$, SELECCT$, SELECST$
COMMON SHARED GROW1$, GROW2$, MAXLAI$, EVBARE$, EVFAIR$, EVEXCL$
COMMON SHARED evdepth$, LAI$, GR1$, GR2$, AREA$, NYEARS$, latitude$
COMMON SHARED HUMID1$, HUMID2$, HUMID3$, HUMID4$, WIND$, LATIT$, meansel
COMMON SHARED WSPEED$, BARE$, FAIR$, EXCL$, UNI$, ETCITY$, ETSTATE$, etunits$
COMMON SHARED defrain, deftemp, cmeth(), CUNIT(), ccity$(), cstate$(), ct$, st$, FILEPATH$()
COMMON SHARED SROWBOX, SCOLBOX, BOXWIDTH, EROWBOX, defp(), deft()

SUB CLIMATE (SELECST$, SELECCT$, CLIM$)
   DIM tape2lin$(10)
   ERR = 0
   ON LOCAL ERROR RESUME NEXT
620 F1HELP$ = "#WEATHER"
   COLOR 15, 1: CLS
   COLOR 11, 6
   LOCATE 1, 1:  PRINT "         WEATHER DATA - PRECIPITATION, TEMPERATURE AND SOLAR RADIATION          "
   COLOR 15, 1
   LOCATE 2, 2:  PRINT "ษอออออออออออออออออออออออป ษอออออออออออออออออออออออป ษอออออออออออออออออออออออป"
   LOCATE 3, 2:  PRINT "บ     PRECIPITATION     บ บ       TEMPERATURE     บ บ    SOLAR RADIATION    บ"
   LOCATE 4, 2:  PRINT "บ                       บ บ                       บ บ                       บ"
   LOCATE 5, 2:  PRINT "ฬอออออออออออออออออออออออน ฬอออออออออออออออออออออออน ฬอออออออออออออออออออออออน"
   LOCATE 6, 2:  PRINT "บ       Default         บ บ                       บ บ                       บ"
   LOCATE 7, 2:  PRINT "บ       Synthetic       บ บ       Synthetic       บ บ       Synthetic       บ"
   LOCATE 8, 2:  PRINT "บ                       บ บ                       บ บ                       บ"
   LOCATE 9, 2:  PRINT "บ       User            บ บ       User            บ บ       User            บ"
   LOCATE 10, 2: PRINT "บ         Create/Edit   บ บ         Create/Edit   บ บ         Create/edit   บ"
   LOCATE 11, 2: PRINT "บ         NOAA Tape     บ บ         NOAA Tape     บ บ         NOAA Tape     บ"
   LOCATE 12, 2: PRINT "บ         Climatedata   บ บ         Climatedata   บ บ         Climatedata   บ"
   LOCATE 13, 2: PRINT "บ         ASCII         บ บ         ASCII         บ บ         ASCII         บ"
   LOCATE 14, 2: PRINT "บ         HELP 2        บ บ         HELP 2        บ บ         HELP 2        บ"
   LOCATE 15, 2: PRINT "บ         Canadian      บ บ         Canadian      บ บ         Canadian      บ"
   LOCATE 16, 2: PRINT "ศอออออออออออออออออออออออผ ศอออออออออออออออออออออออผ ศอออออออออออออออออออออออผ"
   CL = CLIM
   FOR I = 1 TO 3
     CLIM = I: METH = cmeth(I): GOSUB 9050
   NEXT I
   CLIM = CL
   CALL menu
   GOSUB 78000
   CLIM1$ = "     PRECIPITATION     "
   CLIM2$ = "      TEMPERATURE      "
   CLIM3$ = "    SOLAR RADIATION    "
   CLIMETH1$ = "Default": CLIMETH2$ = "Synthetic": CLIMETH3$ = "Create/Edit"
   CLIMETH5$ = "NOAA Tape": CLIMETH6$ = "Climatedata"
   CLIMETH7$ = "ASCII": CLIMETH8$ = "HELP 2": CLIMETH9$ = "Canadian"
   DIRECT$ = CURDIR$
   col1 = 3: col2 = 29: col3 = 55
   GOSUB 9050
   IF COL <> 0 THEN GOTO 623
622  COL = col1: ACOL = ACOL1: CLIM$ = CLIM1$: CLIM = 1
623  XLEN = LEN(CLIM$)
     COLOR 1, 15: LOCATE 3, COL: PRINT CLIM$
     IF CLIMETH$ = CLIMETH1$ THEN
        IF CLIM$ = CLIM1$ THEN
          METH = 1
          oldmeth = METH
          GOTO 625
        END IF
     END IF
     IF CLIMETH$ = CLIMETH2$ THEN METH = 2: GOTO 626
     IF CLIMETH$ = CLIMETH3$ THEN METH = 3: GOTO 627
     IF CLIMETH$ = CLIMETH5$ THEN METH = 5: GOTO 628
     IF CLIMETH$ = CLIMETH6$ THEN METH = 6: GOTO 629
     IF CLIMETH$ = CLIMETH7$ THEN METH = 7: GOTO 630
     IF CLIMETH$ = CLIMETH8$ THEN METH = 8: GOTO 631
     IF CLIMETH$ = CLIMETH9$ THEN METH = 9: GOTO 638
     IF COL <> col1 THEN GOTO 626
     IF COL = col1 THEN GOTO 625
625  ''''DEFAULT PRECIPITATION DATA
     XLEN = LEN(CLIMETH1$)
     CLIMETH$ = CLIMETH1$: METH = 1
     oldmeth = METH
     F2HELP$ = "#DEFAULT"
     CUNIT(CLIM, METH) = 1: GOSUB 9050
     CALL RCELL(6, COL + 7, XLEN, CLIMETH1$)
     GOSUB 63190
     IF irtrn% = 2 OR irtrn% = -1 OR irtrn% = 1 THEN EXIT SUB
     IF irtrn% = 1000 THEN GOSUB 63195
     IF irtrn% = 5 THEN
       LOCATE 17, 1: PRINT "                                                                              ";
       LOCATE 18, 1: PRINT "                                                                            ";
       LOCATE 19, 1: PRINT "                                                                            ";
       LOCATE 20, 1: PRINT "                                                                            ";
       LOCATE 21, 1: PRINT "                                                                            ";
       LOCATE 22, 1: PRINT "                                                                            ";
       LOCATE 23, 1: PRINT "                                                                            ";
       CLIMATF$ = "DEFRAIN"
       COLOR 1, 15: LOCATE 6, COL + 7: PRINT CLIMETH1$: COLOR 15, 1
       CALL LOCATION(CLIM$, ok, CLIMETH$)
       IF irtrn% = 2 THEN EXIT SUB
       PCOPY HPAGE%, VPAGE%
       IF irtrn% <> 100 THEN oldmeth = METH: GOSUB 87000
       CUNIT(CLIM, METH) = 1: GOSUB 9050
       GOTO 625
     END IF
     IF irtrn% = 4 THEN GOTO 626
     IF irtrn% = 2 OR irtrn% = -1 OR irtrn% = 1 THEN EXIT SUB
     IF irtrn% = 3 THEN GOTO 638
     IF irtrn% = 6 THEN GOSUB 6060
     IF irtrn% = 7 THEN GOSUB 6070
     GOTO 625
626  '''''SYNTHETIC DATA
     XLEN = LEN(CLIMETH2$)
     CLIMETH$ = CLIMETH2$: METH = 2
     GOSUB 9050
     IF CUNIT(CLIM, METH) <> 2 THEN CUNIT(CLIM, 2) = 1: GOSUB 9050
     F2HELP$ = "#SYNTHETIC"
     CALL RCELL(7, COL + 7, XLEN, CLIMETH2$)
     GOSUB 63190
     COLOR 15, 1
     LOCATE 20, 15: PRINT "                                                   "
     LOCATE 21, 15: PRINT "                                               ";
     IF irtrn% = 2 OR irtrn% = -1 OR irtrn% = 1 THEN EXIT SUB
     IF irtrn% = 1000 THEN GOSUB 63195
     IF irtrn% = 5 THEN
        IF (CLIM$ = CLIM2$ OR CLIM$ = CLIM3$) AND DIR$("DATA4.TMP") = "" THEN
           LOCATE 20, 15: PRINT "SYNTHETIC TEMPERATURE AND SOLAR RADIATION CANNOT BE"
           LOCATE 21, 15: PRINT "GENERATED WITHOUT PRECIPITATION DATA.";
           GOTO 626
        END IF
        GOSUB 6195
        IF irtrn% = 1000 THEN GOSUB 63195
        IF irtrn% <> 5 THEN GOTO 626
        GOSUB 63190
        COLOR 1, 15: LOCATE 7, COL + 7: PRINT CLIMETH2$: COLOR 15, 1
        IF CLIM$ = CLIM1$ THEN
           ok = 1
           CLIMATF$ = "SYNRAIN"
           SELECCT$ = ""
           CALL LOCATION(CLIM$, ok, CLIMETH$)
           IF irtrn% = 2 THEN EXIT SUB
           IF SELECCT$ <> "" THEN
              CUNIT(CLIM, 2) = VAL(UNI$)
              PRNT = 1
              GOSUB 9050
              PRNT = 0
              GOSUB 6080
              CLS : PCOPY HPAGE%, VPAGE%
              CALL synprec(SELECST$, SELECCT$, CLIM$, ok, tape2lin$())
              IF irtrn% = 1000 THEN EXIT SUB
              IF irtrn% = 100 THEN GOTO 626
           END IF
           CLS : PCOPY HPAGE%, VPAGE%
           IF irtrn% = 100 THEN GOTO 626
           LOCATE 20, 20: PRINT "SYNTHETIC PRECIPITATION DATA IS BEING GENERATED...";
           SHELL "SYNGEN"
           CUNIT(CLIM, 3) = CUNIT(CLIM, METH)
           GOSUB 87000
           oldmeth = METH
           LOCATE 20, 20: PRINT "                                                  ";
           GOTO 626
        END IF
        CLIMATF$ = "SYNTEMP"
        SELECCT$ = "": SELECST$ = ""
        CALL LOCATION(CLIM$, ok, CLIMETH$)
        IF irtrn% = 100 THEN CLS : PCOPY HPAGE%, VPAGE%: GOTO 626
        IF irtrn% = 2 THEN EXIT SUB
        IF SELECCT$ <> "" THEN
           CUNIT(CLIM, 2) = VAL(UNI$)
           PRNT = 1
           GOSUB 9050
           PRNT = 0
           GOSUB 6080
           CLS : PCOPY HPAGE%, VPAGE%
           IF irtrn% = 100 THEN GOTO 626
           IF CLIM$ = CLIM2$ THEN
              CALL SYNTEMP(SELECST$, SELECCT$, CLIM$, ok, tape2lin$())
           END IF
           IF CLIM$ = CLIM3$ THEN
              CALL SYNSOLR(SELECST$, SELECCT$, CLIM$, ok, tape2lin$())
           END IF
           IF irtrn% = 1000 THEN EXIT SUB
           IF irtrn% = 100 THEN GOTO 626
        END IF
        CLS : PCOPY HPAGE%, VPAGE%
        IF CLIM = 2 THEN LOCATE 20, 20: PRINT "SYNTHETIC TEMPERATURE DATA IS BEING GENERATED..."
        IF CLIM = 3 THEN LOCATE 20, 20: PRINT "SYNTHETIC SOLAR RADIATION DATA IS BEING GENERATED..."
        LOCATE 21, 1
        SHELL "SYNGEN"
        CUNIT(CLIM, 3) = CUNIT(CLIM, METH)
        GOSUB 87000
        oldmeth = METH
        LOCATE 20, 20: PRINT "                                                    ";
        GOTO 626
      END IF
        IF irtrn% = 4 THEN GOTO 627
        IF irtrn% = 3 AND COL = col1 THEN GOTO 625
        IF irtrn% = 3 AND COL <> col1 THEN GOTO 638
        IF irtrn% = 6 THEN GOSUB 6060
        IF irtrn% = 7 THEN GOSUB 6070
        GOTO 626
627 '-----------------------
    'CREATE/EDIT OPTION
    '-----------------------
    XLEN = LEN(CLIMETH3$)
    CLIMETH$ = CLIMETH3$: METH = 3
    GOSUB 9050
    F2HELP$ = "#CREATEEDIT"
    CALL RCELL(10, COL + 9, XLEN, CLIMETH3$)
    GOSUB 63190
    IF irtrn% = 2 OR irtrn% = -1 OR irtrn% = 1 THEN EXIT SUB
    IF irtrn% = 1000 THEN GOSUB 63195
    IF irtrn% = 5 THEN
      '--------------
      'PRECIPITATION
      '--------------
      IF CLIM$ = CLIM1$ THEN
        IF cmeth(1) = 0 THEN
           CUNIT(1, 3) = 1
           UNI$ = "1"
        ELSE
           CUNIT(1, 3) = CUNIT(1, cmeth(1))
           UNI$ = STR$(CUNIT(1, cmeth(1)))
        END IF
        IF DIR$("DATA4.TMP") = "" THEN
           GOSUB 6095
           IF irtrn% = 1000 THEN GOSUB 63195
           IF irtrn% <> 5 THEN GOTO 627
           IF METH = 9 THEN CUNIT(1, 9) = 2: UNI$ = "2"
           CALL YEARSHEET(CLIM$, CLIMETH$, UNI$, COL)
           IF irtrn% = 1000 THEN EXIT SUB
           IF irtrn% <> 100 THEN GOSUB 87000
           GOTO 620
        ELSE
           CLOSE
           OPEN "data4.tmp" FOR INPUT AS #1
           LINE INPUT #1, l$
           INPUT #1, CUNIT(CLIM, METH)
           CLOSE
           GOSUB 9050
           CALL YEARSHEET(CLIM$, CLIMETH$, UNI$, COL)
           IF irtrn% = 1000 THEN EXIT SUB
           IF irtrn% <> 100 THEN GOSUB 87000
           GOTO 620
        END IF
      '------------
      'TEMPERATURE
      '------------
      ELSEIF CLIM$ = CLIM2$ THEN
        CLIM = 2
        IF cmeth(1) = 0 THEN
           CUNIT(1, 3) = 1
           UNI$ = "1"
        ELSE
           CUNIT(1, 3) = CUNIT(1, cmeth(1))
           UNI$ = STR$(CUNIT(1, cmeth(1)))
        END IF
        IF DIR$("DATA7.TMP") = "" THEN
          GOSUB 6095
          IF irtrn% = 1000 THEN GOSUB 63195
          IF irtrn% <> 5 THEN GOTO 627
          CALL YEARSHEET(CLIM$, CLIMETH$, UNI$, COL)
          IF irtrn% = 1000 THEN EXIT SUB
          IF irtrn% <> 100 THEN GOSUB 87000
          COLOR 15, 1: CALL meantemp(cit$, sta$, VAL(UNI$))
          IF irtrn% = 1000 THEN EXIT SUB
          IF irtrn% <> 100 THEN GOSUB 87000
          CLIM = 2
          GOTO 620
        ELSE
          CLOSE
          OPEN "data7.tmp" FOR INPUT AS #1
          LINE INPUT #1, l$
          INPUT #1, CUNIT(CLIM, METH)
          CLOSE
          GOSUB 9050
          ed1$ = " Edit Daily Temperature Data           "
          ed2$ = " Edit Normal Mean Monthly Temperatures "
          LOCATE 18, col2 - 7: PRINT "ษอออออออออออออออออออออออออออออออออออออออป";
          LOCATE 19, col2 - 7: PRINT "บ Edit Daily Temperature Data           บ";
          LOCATE 20, col2 - 7: PRINT "บ Edit Normal Mean Monthly Temperatures บ";
          LOCATE 21, col2 - 7: PRINT "ศอออออออออออออออออออออออออออออออออออออออผ";
20102     F2HELP$ = "#TEMPSELECT"
          CALL RCELL(19, col2 - 6, 16, ed1$)
          GOSUB 63190
          IF irtrn% = 100 OR irtrn% = -1 OR irtrn% = 1 THEN GOTO 20111
          IF irtrn% = 3 OR irtrn% = 4 THEN GOTO 20103
          IF irtrn% = 5 THEN
            CALL YEARSHEET(CLIM$, CLIMETH$, UNI$, COL)
            IF irtrn% = 1000 THEN EXIT SUB
            IF irtrn% = 100 THEN
               found = -1
               'GOSUB 87000
            ELSE
               COLOR 15, 1: CALL meantemp(cit$, sta$, VAL(UNI$))
              IF irtrn% = 1000 THEN EXIT SUB
              IF irtrn% <> 100 THEN GOSUB 87000
              CLIM = 2
            END IF
            GOTO 620
          END IF
          GOTO 20102
20103     F2HELP$ = "#TEMPSELECT"
          CALL RCELL(20, col2 - 6, 16, ed2$)
          GOSUB 63190
          IF irtrn% = 100 OR irtrn% = -1 OR irtrn% = 1 OR irtrn% = 2 THEN GOTO 20111
          IF irtrn% = 3 OR irtrn% = 4 THEN GOTO 20102
          IF irtrn% = 5 THEN
            COLOR 15, 1: CALL meantemp(cit$, sta$, VAL(UNI$))
            IF irtrn% = 1000 THEN EXIT SUB
            IF irtrn% <> 100 THEN GOSUB 87000
            CLIM = 2
            GOTO 620
          END IF
          GOTO 20103
20111     LOCATE 18, col2 - 7: PRINT "                                         ";
          LOCATE 19, col2 - 7: PRINT "                                         ";
          LOCATE 20, col2 - 7: PRINT "                                         ";
          LOCATE 21, col2 - 7: PRINT "                                         ";
        END IF
      '-----------------
      'SOLAR RADIATION
      '-----------------
      ELSEIF CLIM$ = CLIM3$ THEN
        IF cmeth(1) = 0 THEN
           CUNIT(1, 3) = 1
           UNI$ = "1"
        ELSE
           CUNIT(1, 3) = CUNIT(1, cmeth(1))
           UNI$ = STR$(CUNIT(1, cmeth(1)))
        END IF
        IF DIR$("DATA13.TMP") = "" THEN
           GOSUB 6095
           IF irtrn% = 1000 THEN GOSUB 63195
           IF irtrn% <> 5 THEN GOTO 627
           CALL YEARSHEET(CLIM$, CLIMETH$, UNI$, COL)
           IF irtrn% = 1000 THEN EXIT SUB
           IF irtrn% <> 100 THEN GOSUB 87000
           GOTO 620
        ELSE
           CLOSE
           OPEN "data13.tmp" FOR INPUT AS #1
           LINE INPUT #1, l$
           INPUT #1, CUNIT(CLIM, METH)
           CLOSE
           GOSUB 9050
           CALL YEARSHEET(CLIM$, CLIMETH$, UNI$, COL)
           IF irtrn% = 1000 THEN EXIT SUB
           IF irtrn% <> 100 THEN GOSUB 87000
           GOTO 620
        END IF
        irtrn% = 5
      END IF
20019 'COLOR 1, 15: LOCATE 10, col + 9: PRINT CLIMETH3$: COLOR 15, 1
      END IF
'-------------------
'F10, PGUP OR PGDN
'-------------------
      IF irtrn% = 4 THEN GOTO 628
      IF irtrn% = 3 THEN GOTO 626
      IF irtrn% = 6 THEN GOSUB 6060
      IF irtrn% = 7 THEN GOSUB 6070
      GOTO 627
628 '----------
    'NOAA TAPE
    '----------
    XLEN = LEN(CLIMETH5$)
    F2HELP$ = "#NOAA"
    CUNIT(CLIM, 5) = 1: METH = 5: GOSUB 9050
    CALL RCELL(11, COL + 9, XLEN, CLIMETH5$)
    GOSUB 63190
    CLIMETH$ = CLIMETH5$
    IF irtrn% = 2 OR irtrn% = -1 OR irtrn% = 1 THEN EXIT SUB
    IF irtrn% = 1000 THEN GOSUB 63195
    IF irtrn% = 5 THEN
      COLOR 1, 15: LOCATE 11, COL + 9: PRINT CLIMETH5$: COLOR 15, 1
      IF CLIM$ = CLIM1$ OR CLIM$ = CLIM3$ THEN
          GOSUB 7095
          IF irtrn% = 1000 THEN GOSUB 63195
          IF irtrn% <> 5 THEN GOTO 628
      END IF
      IF CLIM$ = CLIM2$ THEN GOSUB 8095
      IF irtrn% = 1000 THEN GOSUB 63195
      IF found <> 4 THEN METH = oldmeth: GOTO 620
      IF irtrn% = 5 THEN
         IF CLIM$ = CLIM1$ AND LTRIM$(RTRIM$(DFILE$)) <> "" THEN SHELL "NOAAPREC" ': CLIM = 1
         IF CLIM$ = CLIM2$ AND (LTRIM$(RTRIM$(MAXTEMP$)) <> "" OR LTRIM$(RTRIM$(MINTEMP$)) <> "") THEN SHELL "NOAATEMP"     ': CLIM = 2
         IF CLIM$ = CLIM3$ AND LTRIM$(RTRIM$(DFILE$)) <> "" THEN SHELL "NOAASRAD"  ': CLIM = 3
         UNI$ = "1": CUNIT(CLIM, 5) = 1
         IF CLIM = 2 AND DIR$("data7.tmp") <> "" THEN
           COLOR 15, 1: CALL meantemp(cit$, sta$, CUNIT(CLIM, 5))
           IF irtrn% = 1000 THEN EXIT SUB
         END IF
         GOSUB 87000
      END IF
      GOTO 620
    END IF
    COLOR 15, 1: LOCATE 11, COL + 9: PRINT CLIMETH5$
    IF irtrn% = 4 THEN GOTO 629
    IF irtrn% = 3 THEN GOTO 627
    IF irtrn% = 6 THEN GOSUB 6060
    IF irtrn% = 7 THEN GOSUB 6070
    GOTO 628
629 XLEN = LEN(CLIMETH6$)
    F2HELP$ = "#CLIMATEDATA"
    CUNIT(CLIM, 6) = 1: METH = 6: GOSUB 9050
    CALL RCELL(12, COL + 9, XLEN, CLIMETH6$)
    GOSUB 63190
    CLIMETH$ = CLIMETH6$
    IF irtrn% = 2 OR irtrn% = -1 OR irtrn% = 1 THEN EXIT SUB
    IF irtrn% = 1000 THEN GOSUB 63195
    IF irtrn% = 5 THEN
      COLOR 1, 15: LOCATE 12, COL + 9: PRINT CLIMETH6$: COLOR 15, 1
      IF CLIM$ = CLIM1$ OR CLIM$ = CLIM3$ THEN
         GOSUB 7095
         IF irtrn% = 1000 THEN GOSUB 63195
         IF irtrn% <> 5 THEN GOTO 629
      END IF
      IF CLIM$ = CLIM2$ THEN GOSUB 8095
      IF irtrn% = 1000 THEN GOSUB 63195
      IF found <> 4 THEN GOTO 620
      IF irtrn% = 5 THEN
         IF CLIM$ = CLIM1$ AND LTRIM$(RTRIM$(DFILE$)) <> "" THEN SHELL "CLIMPREC": ' CLIM = 1
         IF CLIM$ = CLIM2$ AND (LTRIM$(RTRIM$(MAXTEMP$)) <> "" OR LTRIM$(RTRIM$(MINTEMP$)) <> "") THEN SHELL "CLIMTEMP": ' CLIM = 2
         IF CLIM$ = CLIM3$ AND LTRIM$(RTRIM$(DFILE$)) <> "" THEN SHELL "CLIMSRAD": ' CLIM = 3
         UNI$ = "1": CUNIT(CLIM, 6) = 1
         IF CLIM = 2 AND DIR$("data7.tmp") <> "" THEN
           COLOR 15, 1
           CALL meantemp(cit$, sta$, CUNIT(CLIM, 6))
           IF irtrn% = 1000 THEN EXIT SUB
         END IF
         GOSUB 87000
      END IF
      GOTO 620
    END IF
    COLOR 15, 1: LOCATE 12, COL + 9: PRINT CLIMETH6$
    IF irtrn% = 4 THEN GOTO 630
    IF irtrn% = 3 THEN GOTO 628
    IF irtrn% = 6 THEN GOSUB 6060
    IF irtrn% = 7 THEN GOSUB 6070
    GOTO 629
630 XLEN = LEN(CLIMETH7$)
    F2HELP$ = "#ASCII"
    CALL RCELL(13, COL + 9, XLEN, CLIMETH7$)
    GOSUB 63190
    CLIMETH$ = CLIMETH7$
    IF CUNIT(CLIM, 7) <> 2 THEN CUNIT(CLIM, 7) = 1: METH = 7: GOSUB 9050
    IF irtrn% = 2 OR irtrn% = -1 OR irtrn% = 1 THEN EXIT SUB
    IF irtrn% = 1000 THEN GOSUB 63195
    IF irtrn% = 5 THEN
       COLOR 1, 15: LOCATE 13, COL + 9: PRINT CLIMETH7$: COLOR 15, 1
       GOSUB 6095
       IF irtrn% = 1000 THEN GOSUB 63195
       IF irtrn% <> 5 THEN GOTO 630
       CLOSE
       OPEN "scan.tmp" FOR OUTPUT AS #1
       PRINT #1, USING "\                  \"; ccity$(CLIM)
       PRINT #1, USING "\                  \"; cstate$(CLIM)
       PRINT #1, USING "#"; VAL(UNI$)
       CLOSE
       IF irtrn% = 2 OR irtrn% = -1 OR irtrn% = 1 THEN EXIT SUB
       IF irtrn% = 1000 THEN GOSUB 63195
       IF irtrn% <> 5 THEN GOTO 620
       SCREEN 0, 0, 0
       IF CLIM$ = CLIM1$ THEN SHELL "ASCIPREC": ' CLIM = 1
       IF CLIM$ = CLIM2$ THEN SHELL "ASCITEMP": ' CLIM = 2
       IF CLIM$ = CLIM3$ THEN SHELL "ASCISRAD": ' CLIM = 3
       SCREEN 0, 1, 1
       COLOR 15, 1: CLS
       IF CLIM = 2 AND DIR$("data7.tmp") <> "" AND DIR$("scan.tmp") <> "" THEN
          CALL meantemp(cit$, sta$, CUNIT(CLIM, 7))
          IF irtrn% = 1000 THEN EXIT SUB
       END IF
       GOSUB 87000
       GOTO 620
    END IF
    COLOR 15, 1: LOCATE 13, COL + 9: PRINT CLIMETH7$
    IF irtrn% = 4 THEN GOTO 631
    IF irtrn% = 3 THEN GOTO 629
    IF irtrn% = 6 THEN GOSUB 6060
    IF irtrn% = 7 THEN GOSUB 6070
    GOTO 630
631 XLEN = LEN(CLIMETH8$)
    F2HELP$ = "#HELP2"
    CUNIT(CLIM, 8) = 1: METH = 8: GOSUB 9050
    CALL RCELL(14, COL + 9, XLEN, CLIMETH8$)
    GOSUB 63190
    CLIMETH$ = CLIMETH8$
    IF irtrn% = 2 OR irtrn% = -1 OR irtrn% = 1 THEN EXIT SUB
    IF irtrn% = 1000 THEN GOSUB 63195
    IF irtrn% = 5 THEN
       COLOR 1, 15: LOCATE 14, COL + 9: PRINT CLIMETH8$: COLOR 15, 1
       GOSUB 7095
       IF irtrn% = 1000 THEN GOSUB 63195
       IF irtrn% <> 5 THEN GOTO 631
       IF found <> 4 THEN GOTO 620
       IF irtrn% = 5 AND LTRIM$(RTRIM$(DFILE$)) <> "" THEN
           CALL CLIMHELP2(CLIM$, DFILE$, cit$, sta$)
           IF CLIM = 2 AND DIR$("data7.tmp") <> "" THEN
             CALL meantemp(cit$, sta$, CUNIT(CLIM, 8))
             IF irtrn% = 1000 THEN EXIT SUB
           END IF
       END IF
       GOSUB 87000
       GOTO 620
    END IF
    COLOR 15, 1: LOCATE 14, COL + 9: PRINT CLIMETH8$
    IF irtrn% = 4 THEN GOTO 638
    IF irtrn% = 3 THEN GOTO 630
    IF irtrn% = 6 THEN GOSUB 6060
    IF irtrn% = 7 THEN GOSUB 6070
    GOTO 631
638 XLEN = LEN(CLIMETH9$)
    F2HELP$ = "#CANADIAN"
    CUNIT(CLIM, 9) = 2: METH = 9: GOSUB 9050
    CALL RCELL(15, COL + 9, XLEN, CLIMETH9$)
    GOSUB 63190
    CLIMETH$ = CLIMETH9$
    IF irtrn% = 2 OR irtrn% = -1 OR irtrn% = 1 THEN EXIT SUB
    IF irtrn% = 1000 THEN GOSUB 63195
    IF irtrn% = 5 THEN
      COLOR 1, 15: LOCATE 15, COL + 9: PRINT CLIMETH9$: COLOR 15, 1
      GOSUB 7095
      IF irtrn% = 1000 THEN GOSUB 63195
      IF irtrn% <> 5 THEN GOTO 638
      IF found <> 4 THEN GOTO 620
      IF irtrn% = 5 THEN
         IF CLIM$ = CLIM1$ AND LTRIM$(RTRIM$(DFILE$)) <> "" THEN SHELL "CANPREC"
         IF CLIM$ = CLIM2$ AND LTRIM$(RTRIM$(DFILE$)) <> "" THEN SHELL "CANTEMP"
         IF CLIM$ = CLIM3$ AND LTRIM$(RTRIM$(DFILE$)) <> "" THEN SHELL "CANSRAD"
         UNI$ = "2": METH = 9: CUNIT(CLIM, 9) = 2: cmeth(CLIM) = 9
         IF CLIM = 2 AND DIR$("data7.tmp") <> "" THEN
            CALL meantemp(cit$, sta$, CUNIT(CLIM, 9))
            IF irtrn% = 1000 THEN EXIT SUB
         END IF
      END IF
      GOSUB 87000
      GOTO 620
    END IF
    COLOR 15, 1: LOCATE 15, COL + 9: PRINT CLIMETH9$
    IF irtrn% = 4 THEN CLIMETH$ = CLIMETH1$: GOTO 623
    IF irtrn% = 3 THEN GOTO 631
    IF irtrn% = 6 THEN GOSUB 6060
    IF irtrn% = 7 THEN GOSUB 6070
    GOTO 638
EXIT SUB
'-----------------------------
' SUBROUTINE FOR MOVING RIGHT
'-----------------------------
6060    COLOR 15, 1:  LOCATE 3, COL: PRINT CLIM$
        IF COL = col1 THEN
           COL = col2: ACOL = ACOL2
           CLIM$ = CLIM2$: CLIM = 2
        ELSEIF COL = col2 THEN
           COL = col3: ACOL = ACOL3
           CLIM$ = CLIM3$: CLIM = 3
        ELSEIF COL = col3 THEN
           COL = col1: ACOL = ACOL1
           CLIM$ = CLIM1$: CLIM = 1
        END IF
        GOTO 623
        RETURN
'----------------------------
' SUBROUTINE FOR MOVING LEFT
'----------------------------
6070    COLOR 15, 1:  LOCATE 3, COL: PRINT CLIM$
        IF COL = col1 THEN
           COL = col3: ACOL = ACOL3: CLIM$ = CLIM3$: CLIM = 3
        ELSEIF COL = col3 THEN
           COL = col2: ACOL = ACOL2: CLIM$ = CLIM2$: CLIM = 2
        ELSEIF COL = col2 THEN
           COL = col1: ACOL = ACOL1: CLIM$ = CLIM1$: CLIM = 1
        END IF
        GOTO 623
        RETURN
'''''''''''''''''''''''''''
'SUBROUTINE TO READ TAPE2
'''''''''''''''''''''''''''
6080 CLOSE
     def$ = DIR$("TAPE2.NEW")
     IF LEN(def$) <> 0 THEN
       OPEN "TAPE2.NEW" FOR INPUT AS #1
       s1$ = LEFT$(SELECST$, 5): s2$ = LEFT$(SELECCT$, 5)
       LOCATE 24, 1: PRINT "         Reading Default Data From Tape2 File.  Please Wait...               ";
       FOR I = 1 TO 200
         LINE INPUT #1, l$
         IF LEFT$(l$, 5) = s1$ AND MID$(l$, 7, 5) = s2$ THEN
            FOR J = 1 TO 5
               LINE INPUT #1, l$
               tape2lin$(J) = l$
               IF J = 1 THEN latitude$ = LTRIM$(LEFT$(l$, 7))
            NEXT J
            st% = 1
            FOR K% = 1 TO 12
               deft(K%) = VAL(MID$(l$, st%, 6))
               st% = st% + 6
            NEXT K%
            FOR J = 1 TO 4
               LINE INPUT #1, l$
               tape2lin$(J + 5) = l$
            NEXT J
            st% = 1
            FOR K% = 1 TO 12
               defp(K%) = VAL(MID$(l$, st%, 6))
               st% = st% + 6
            NEXT K%
            LINE INPUT #1, l$
            tape2lin$(10) = l$
            CLOSE 1
            RETURN
          ELSE
            FOR J = 1 TO 10
               LINE INPUT #1, l$
            NEXT J
          END IF
        NEXT I
      ELSE
        COLOR 11, 6
        LOCATE 25, 1: PRINT "   File TAPE2.NEW is not loaded on current directory.  Press any key to continue... ";
        COLOR 15, 1
90089    KBD$ = INKEY$
        IF KBD$ = "" THEN GOTO 90089
        GOTO 620
      END IF
      RETURN
''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'Subroutine to input file name for user defined files
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''
6095 UNIT1$ = "CUSTOMARY": UNIT2$ = "METRIC"
     IF CUNIT(CLIM, METH) = 2 THEN
       COLOR 1, 15: LOCATE 22, ACOL + 14: PRINT UNIT2$
     ELSE
       COLOR 1, 15: LOCATE 21, ACOL + 14: PRINT UNIT1$
     END IF
     COLOR 15, 1
     LOCATE 17, 1: PRINT "                                                                              ";
     LOCATE 18, 1: PRINT "                                                                              ";
     LOCATE 19, 1: PRINT "                                                                              ";
     LOCATE 20, 1: PRINT "                                                                              ";
     LOCATE 21, 1: PRINT "                                                                              ";
     LOCATE 22, 1: PRINT "                                                                              ";
     LOCATE 23, 1: PRINT "                                                                              ";
     LOCATE 17, ACOL - 1: PRINT "ษอออออออออออออออออออออออออออป";
     LOCATE 18, ACOL - 1: PRINT "บ CITY:                     บ";
     LOCATE 19, ACOL - 1: PRINT "บSTATE:                     บ";
     LOCATE 20, ACOL - 1: PRINT "บUNITS:      1-CUSTOMARY    บ";
     LOCATE 21, ACOL - 1: PRINT "บ            2-METRIC       บ";
     LOCATE 22, ACOL - 1: PRINT "ศอออออออออออออออออออออออออออผ";
     LOCATE 18, ACOL + 7: PRINT ccity$(CLIM)
     LOCATE 19, ACOL + 7: PRINT cstate$(CLIM)
     UNI$ = LTRIM$(RTRIM$(STR$(CUNIT(CLIM, METH))))
     LOCATE 20, ACOL + 7: PRINT UNI$
     IF CUNIT(CLIM, METH) = 2 THEN
       COLOR 1, 15: LOCATE 21, ACOL + 14: PRINT UNIT2$
     ELSE
       COLOR 1, 15: LOCATE 20, ACOL + 14: PRINT UNIT1$
     END IF
6097 F2HELP$ = "#LOCATION"
     CALL ACELL(18, ACOL + 7, 20, ccity$(CLIM), 1)
     GOSUB 63190
     IF irtrn% = 5 OR irtrn% = 4 THEN cit$ = ccity$(CLIM): GOTO 6099
     IF irtrn% = 3 THEN GOTO 6190
     IF irtrn% = 100 OR irtrn% = 1000 OR irtrn% = -1 OR irtrn% = 1 OR irtrn% = 2 THEN
        LOCATE 17, 1: PRINT "                                                                              ";
        LOCATE 18, 1: PRINT "                                                                              ";
        LOCATE 19, 1: PRINT "                                                                              ";
        LOCATE 20, 1: PRINT "                                                                              ";
        LOCATE 21, 1: PRINT "                                                                              ";
        LOCATE 22, 1: PRINT "                                                                              ";
        LOCATE 23, 1: PRINT "                                                                              ";
        RETURN
     END IF
     GOTO 6097
6099 F2HELP$ = "#LOCATION"
     CALL ACELL(19, ACOL + 7, 20, cstate$(CLIM), 1)
     GOSUB 63190
     IF irtrn% = 5 OR irtrn% = 4 THEN sta$ = cstate$(CLIM): GOTO 6190
     IF irtrn% = 3 THEN GOTO 6097
     IF irtrn% = 100 OR irtrn% = 1000 OR irtrn% = -1 OR irtrn% = 1 OR irtrn% = 2 THEN
        LOCATE 17, 1: PRINT "                                                                              ";
        LOCATE 18, 1: PRINT "                                                                              ";
        LOCATE 19, 1: PRINT "                                                                              ";
        LOCATE 20, 1: PRINT "                                                                              ";
        LOCATE 21, 1: PRINT "                                                                              ";
        LOCATE 22, 1: PRINT "                                                                              ";
        LOCATE 23, 1: PRINT "                                                                              ";
        RETURN
     END IF
     GOTO 6099
6190 F2HELP$ = "#UNITS"
     UNI$ = LTRIM$(RTRIM$(STR$(CUNIT(CLIM, METH))))
     CALL ACELL(20, ACOL + 7, 1, UNI$, 2)
     GOSUB 63190
     IF VAL(UNI$) <> 2 THEN
        UNI$ = "1"
     ELSE
        UNI$ = "2"
     END IF
     LOCATE 20, ACOL + 7: PRINT UNI$
     IF UNI$ = "1" THEN
        COLOR 15, 1: LOCATE 21, ACOL + 14: PRINT UNIT2$
        COLOR 1, 15: LOCATE 20, ACOL + 14: PRINT UNIT1$
        IF CLIM = 1 THEN PRECU$ = "(IN)"
        IF CLIM = 2 THEN tempu$ = "(๘F)"
        IF CLIM = 3 THEN solu$ = "(Langleys)"
     ELSE
        COLOR 15, 1: LOCATE 20, ACOL + 14: PRINT UNIT1$
        COLOR 1, 15: LOCATE 21, ACOL + 14: PRINT UNIT2$
        IF CLIM = 1 THEN PRECU$ = "(MM)"
        IF CLIM = 2 THEN tempu$ = "(๘C)"
        IF CLIM = 3 THEN solu$ = "   MJ/Mý  "
     END IF
      CUNIT(CLIM, METH) = VAL(UNI$)
      CUNIT(CLIM, 3) = CUNIT(CLIM, METH)
      COLOR 15, 1
      IF irtrn% = 5 THEN GOSUB 9050: RETURN
      IF irtrn% = 3 THEN GOTO 6099
      IF irtrn% = 4 THEN GOTO 6097
      IF irtrn% = 100 OR irtrn% = 1000 OR irtrn% = -1 OR irtrn% = 1 OR irtrn% = 2 THEN
         LOCATE 17, 1: PRINT "                                                                              ";
         LOCATE 18, 1: PRINT "                                                                              ";
         LOCATE 19, 1: PRINT "                                                                              ";
         LOCATE 20, 1: PRINT "                                                                              ";
         LOCATE 21, 1: PRINT "                                                                              ";
         LOCATE 22, 1: PRINT "                                                                              ";
         LOCATE 23, 1: PRINT "                                                                              ";
         RETURN
      END IF
      GOTO 6190
      RETURN
'''''''''''''''''''''''''''''''''''''''''''''''
'Subroutine to SELECT UNITS FOR SYNTHETIC DATA
'''''''''''''''''''''''''''''''''''''''''''''''
6195 UNIT1$ = "CUSTOMARY": UNIT2$ = "METRIC"
     COLOR 15, 1
     LOCATE 17, 1: PRINT "                                                                              ";
     LOCATE 18, 1: PRINT "                                                                              ";
     LOCATE 19, 1: PRINT "                                                                              ";
     LOCATE 20, 1: PRINT "                                                                              ";
     LOCATE 21, 1: PRINT "                                                                              ";
     LOCATE 22, 1: PRINT "                                                                              ";
     LOCATE 23, 1: PRINT "                                                                              ";
     LOCATE 17, COL - 1: PRINT "ษอออออออออออออออออออออออป";
     LOCATE 18, COL - 1: PRINT "บUNITS:    1-CUSTOMARY  บ";
     LOCATE 19, COL - 1: PRINT "บ          2-METRIC     บ";
     LOCATE 20, COL - 1: PRINT "ศอออออออออออออออออออออออผ";
     UNI$ = LTRIM$(RTRIM$(STR$(CUNIT(CLIM, METH))))
     LOCATE 18, COL + 7: PRINT UNI$
     IF CUNIT(CLIM, METH) <> 2 THEN
        COLOR 1, 15: LOCATE 18, COL + 12: PRINT UNIT1$
     ELSE
        COLOR 1, 15: LOCATE 19, COL + 12: PRINT UNIT2$
     END IF
6290  F2HELP$ = "#UNITS"
      'UNI$ = LTRIM$(RTRIM$(STR$(CUNIT(CLIM, METH))))
      CALL ACELL(18, COL + 7, 1, UNI$, 2)
     GOSUB 63190
      IF UNI$ <> "2" THEN UNI$ = "1": LOCATE 18, COL + 7: PRINT UNI$
      IF UNI$ = "1" THEN
         COLOR 15, 1: LOCATE 19, COL + 12: PRINT UNIT2$
         COLOR 1, 15: LOCATE 18, COL + 12: PRINT UNIT1$
      ELSE
         COLOR 15, 1: LOCATE 18, COL + 12: PRINT UNIT1$
         COLOR 1, 15: LOCATE 19, COL + 12: PRINT UNIT2$
      END IF
      'CUNIT(CLIM, METH) = VAL(UNI$)
      COLOR 15, 1
      'GOSUB 9050
      IF irtrn% = 5 OR irtrn% = 100 OR irtrn% = 1000 OR irtrn% = -1 OR irtrn% = 1 OR irtrn% = 2 THEN
         LOCATE 17, 1: PRINT "                                                                              ";
         LOCATE 18, 1: PRINT "                                                                              ";
         LOCATE 19, 1: PRINT "                                                                              ";
         LOCATE 20, 1: PRINT "                                                                              ";
         LOCATE 21, 1: PRINT "                                                                              ";
         LOCATE 22, 1: PRINT "                                                                              ";
         LOCATE 23, 1: PRINT "                                                                              ";
         RETURN
      END IF
      GOTO 6290
      RETURN
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
'Subroutine to input city, state and file name for user defined files
' of the NOAA, EarthInfo type.
' (Earth Infor solar radiation files do not exist at the current time
' (Jan. 15, 1992)
'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
7095 COLOR 15, 1
     DFILE$ = ""
     CCOL = col2 - 20
     LOCATE 17, 1: PRINT "                                                                              ";
     LOCATE 18, 1: PRINT "                                                                            ";
     LOCATE 19, 1: PRINT "                                                                            ";
     LOCATE 20, 1: PRINT "                                                                            ";
     LOCATE 21, 1: PRINT "                                                                            ";
     LOCATE 22, 1: PRINT "                                                                            ";
     LOCATE 23, 1: PRINT "                                                                            ";
     LOCATE 17, CCOL - 1: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป";
     LOCATE 18, CCOL - 1: PRINT "บ CITY:                                                             บ";
     LOCATE 19, CCOL - 1: PRINT "บSTATE:                                                             บ";
     LOCATE 20, CCOL - 1: PRINT "บ FILE:                                                             บ";
     LOCATE 21, CCOL - 1: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ";
     LOCATE 18, CCOL + 7: PRINT ccity$(CLIM);
     LOCATE 19, CCOL + 7: PRINT cstate$(CLIM);
     LOCATE 20, CCOL + 7: PRINT DFILE$;
7097 F2HELP$ = "#LOCATION"
     CALL ACELL(18, CCOL + 7, 20, ccity$(CLIM), 1)
     GOSUB 63190
     IF irtrn% = 5 OR irtrn% = 4 THEN cit$ = ccity$(CLIM): GOTO 7099
     IF irtrn% = 3 THEN GOTO 7190
     IF irtrn% = 100 OR irtrn% = 1000 OR irtrn% = -1 OR irtrn% = 1 OR irtrn% = 2 THEN GOTO 70109
     GOTO 7097
7099 F2HELP$ = "#LOCATION"
     CALL ACELL(19, CCOL + 7, 20, cstate$(CLIM), 1)
     GOSUB 63190
     IF irtrn% = 5 OR irtrn% = 4 THEN sta$ = cstate$(CLIM): GOTO 7190
     IF irtrn% = 3 THEN GOTO 7097
     IF irtrn% = 100 OR irtrn% = 1000 OR irtrn% = -1 OR irtrn% = 1 OR irtrn% = 2 THEN GOTO 70109
     GOTO 7099
7190 F2HELP$ = "#IMPORTFILES"
      CALL ACELL(20, CCOL + 7, 60, DFILE$, 1)
     GOSUB 63190
      LOCATE 23, 10: PRINT "                                                                       ";
      LOCATE 24, 35: PRINT "                                    ";
      IF irtrn% = 5 THEN
         IF DFILE$ = "" THEN found = -1: RETURN
         FILEN$ = DFILE$
         GOSUB 88000
         IF found <> 4 THEN GOTO 7190
         OPEN "PASSNAME.DAT" FOR OUTPUT AS #1
         PRINT #1, ccity$(CLIM)
         PRINT #1, cstate$(CLIM)
         PRINT #1, DFILE$
         IF CLIM$ = CLIM1$ THEN
            PRINT #1, "DATA4.TMP"
         ELSEIF CLIM$ = CLIM2$ THEN
            PRINT #1, "DATA7.TMP"
         ELSEIF CLIM$ = CLIM3$ THEN
            PRINT #1, "DATA13.TMP"
         END IF
         CLOSE #1
         RETURN
      END IF
      IF irtrn% = 3 THEN GOTO 7099
      IF irtrn% = 4 THEN GOTO 7097
      IF irtrn% = 100 OR irtrn% = 1000 OR irtrn% = -1 OR irtrn% = 1 OR irtrn% = 2 THEN GOTO 70109
      GOTO 7190
70109  LOCATE 17, 1: PRINT "                                                                            ";
       LOCATE 18, 1: PRINT "                                                                            ";
       LOCATE 19, 1: PRINT "                                                                            ";
       LOCATE 20, 1: PRINT "                                                                            ";
       LOCATE 21, 1: PRINT "                                                                            ";
       LOCATE 22, 1: PRINT "                                                                            ";
       LOCATE 23, 1: PRINT "                                                                            ";
RETURN
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Subroutine to input city, state and file names for user defined
'minimum and maximum temperature files of the NOAA and EarthInfo type.
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
8095 COLOR 15, 1
     MAXTEMP$ = ""
     MINTEMP$ = ""
     CCOL = col2 - 22
     LOCATE 17, 1: PRINT "                                                                              ";
     LOCATE 18, 1: PRINT "                                                                            ";
     LOCATE 19, 1: PRINT "                                                                            ";
     LOCATE 20, 1: PRINT "                                                                            ";
     LOCATE 21, 1: PRINT "                                                                            ";
     LOCATE 22, 1: PRINT "                                                                            ";
     LOCATE 23, 1: PRINT "                                                                            ";
     LOCATE 17, CCOL - 5: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป";
     LOCATE 18, CCOL - 5: PRINT "บ         CITY:                                                             บ";
     LOCATE 19, CCOL - 5: PRINT "บ        STATE:                                                             บ";
     LOCATE 20, CCOL - 5: PRINT "บMAX TEMP FILE:                                                             บ";
     LOCATE 21, CCOL - 5: PRINT "บMIN TEMP FILE:                                                             บ";
     LOCATE 22, CCOL - 5: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ";
     LOCATE 18, CCOL + 11: PRINT ccity$(CLIM);
     LOCATE 19, CCOL + 11: PRINT cstate$(CLIM);
     LOCATE 20, CCOL + 11: PRINT MAXTEMP$;
     LOCATE 21, CCOL + 11: PRINT MINTEMP$;
8097 F2HELP$ = "#LOCATION"
     CALL ACELL(18, CCOL + 11, 20, ccity$(CLIM), 1)
     GOSUB 63190
     IF irtrn% = 5 OR irtrn% = 4 THEN cit$ = ccity$(CLIM): GOTO 8099
     IF irtrn% = 3 THEN GOTO 8190
     IF irtrn% = 100 OR irtrn% = 1000 OR irtrn% = -1 OR irtrn% = 1 OR irtrn% = 2 THEN GOTO 80109
     GOTO 8097
8099 F2HELP$ = "#LOCATION"
     CALL ACELL(19, CCOL + 11, 20, cstate$(CLIM), 1)
     GOSUB 63190
     IF irtrn% = 5 OR irtrn% = 4 THEN sta$ = cstate$(CLIM): GOTO 8110
     IF irtrn% = 3 THEN GOTO 8097
     IF irtrn% = 100 OR irtrn% = 1000 OR irtrn% = -1 OR irtrn% = 1 OR irtrn% = 2 THEN GOTO 80109
     GOTO 8099
8110 F2HELP$ = "#IMPORTFILES"
     CALL ACELL(20, CCOL + 11, 60, MAXTEMP$, 1)
     GOSUB 63190
     IF irtrn% = 5 OR irtrn% = 4 THEN
       GOTO 8190
     END IF
     IF irtrn% = 3 THEN GOTO 8099
     IF irtrn% = 100 OR irtrn% = 1000 OR irtrn% = -1 OR irtrn% = 1 OR irtrn% = 2 THEN GOTO 80109
     GOTO 8110
8190 F2HELP$ = "#IMPORTFILES"
     CALL ACELL(21, CCOL + 11, 60, MINTEMP$, 1)
     GOSUB 63190
     IF irtrn% = 5 THEN
       IF MAXTEMP$ = "" AND MINTEMP$ = "" THEN found = -1: RETURN
       FILEN$ = MAXTEMP$
       GOSUB 88000
       IF found <> 4 THEN GOTO 8110
       FILEN$ = MINTEMP$
       GOSUB 88000
       IF found <> 4 THEN GOTO 8190
       IF LTRIM$(RTRIM$(MAXTEMP$)) = LTRIM$(RTRIM$(MINTEMP$)) THEN
          COP$ = "COPY " + MINTEMP$ + " DUMMY.TMP"
          LOCATE 17, 5
          SHELL COP$
          MINTEMP$ = "DUMMY.TMP"
        END IF
       OPEN "PASSNAME.DAT" FOR OUTPUT AS #1
       PRINT #1, ccity$(CLIM)
       PRINT #1, cstate$(CLIM)
       PRINT #1, MAXTEMP$
       PRINT #1, MINTEMP$
       IF CLIM$ = CLIM1$ THEN
          PRINT #1, "DATA4.TMP"
       ELSEIF CLIM$ = CLIM2$ THEN
          PRINT #1, "DATA7.TMP"
       ELSEIF CLIM$ = CLIM3$ THEN
          PRINT #1, "DATA13.TMP"
       END IF
       CLOSE #1
       RETURN
      END IF
      IF irtrn% = 3 THEN GOTO 8110
      IF irtrn% = 4 THEN GOTO 8097
      IF irtrn% = 100 OR irtrn% = 1000 OR irtrn% = -1 OR irtrn% = 1 OR irtrn% = 2 THEN GOTO 80109
      GOTO 8190
80109  LOCATE 17, 1: PRINT "                                                                            ";
       LOCATE 18, 1: PRINT "                                                                            ";
       LOCATE 19, 1: PRINT "                                                                            ";
       LOCATE 20, 1: PRINT "                                                                            ";
       LOCATE 21, 1: PRINT "                                                                            ";
       LOCATE 22, 1: PRINT "                                                                            ";
       LOCATE 23, 1: PRINT "                                                                            ";
RETURN
''''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO DISPLAY UNITS FOR CLIMATE ELEMENTS
'ACCORDING TO EITHER USER INPUT OR DEFAULT VALUES
''''''''''''''''''''''''''''''''''''''''''''''''''
9050 col1 = 3: col2 = 29: col3 = 55: ACOL1 = 3: ACOL2 = 27: ACOL3 = 51
''''''''''''''''''''''
     ' PRECIPITATION
     ''''''''''''''''''''''
     IF CLIM = 1 THEN
     IF CUNIT(1, METH) <> 2 THEN
        CUNIT(1, METH) = 1
        PRECU$ = "(IN)"
     ELSE
        PRECU$ = "(MM)"
     END IF
     END IF
     '''''''''''''''''''
     ' TEMPERATURE
     '''''''''''''''''''
     IF CLIM = 2 THEN
     IF CUNIT(2, METH) <> 2 THEN
        CUNIT(2, METH) = 1
        tempu$ = "(๘F)"
     ELSE
        tempu$ = "(๘C)"
     END IF
     END IF
     ''''''''''''''''''''''
     ' SOLAR RADIATION
     ''''''''''''''''''''''
     IF CLIM = 3 THEN
     IF CUNIT(3, METH) <> 2 THEN
        CUNIT(3, METH) = 1
        solu$ = "(Langleys)"
     ELSE
        solu$ = "  (MJ/Mý)  "
     END IF
     END IF
     COLOR 15, 1
     IF PRNT = 0 THEN
       IF CLIM = 1 THEN LOCATE 4, col1 + 9: PRINT PRECU$
       IF CLIM = 2 THEN LOCATE 4, col2 + 10: PRINT tempu$
       IF CLIM = 3 THEN LOCATE 4, col3 + 6: PRINT solu$
     END IF
     COLOR 15, 1
     RETURN
'-------------------------------------------------
'ROUTINE TO REDEFINE METHOD OF DATA GENERATION
'THEN TO CALL SUBROUTINE TO POINT AN ARROW AT THE
'PROPER METHOD.
'-------------------------------------------------
87000  IF COL = col1 THEN cmeth(1) = METH
       IF COL = col2 THEN cmeth(2) = METH
       IF COL = col3 THEN cmeth(3) = METH
       FOR iclr = 1 TO 10
          LOCATE 5 + iclr, COL: PRINT "       ";
       NEXT iclr
       GOSUB 78000
RETURN
'-------------------------------------------------
'routine to place an arrow indicating the method
'by which the data was generated
'-------------------------------------------------
78000  FOR IMETH = 1 TO 3
          IF IMETH = 1 THEN coll = 7
          IF IMETH = 2 THEN coll = 33
          IF IMETH = 3 THEN coll = 59
          IF cmeth(IMETH) = 1 THEN LOCATE 6, coll: PRINT CHR$(26)
          IF cmeth(IMETH) = 2 THEN LOCATE 7, coll: PRINT CHR$(26)
          IF cmeth(IMETH) = 3 OR cmeth(IMETH) = 4 THEN LOCATE 10, coll: PRINT CHR$(26)
          IF cmeth(IMETH) = 5 THEN LOCATE 11, coll: PRINT CHR$(26)
          IF cmeth(IMETH) = 6 THEN LOCATE 12, coll: PRINT CHR$(26)
          IF cmeth(IMETH) = 7 THEN LOCATE 13, coll: PRINT CHR$(26)
          IF cmeth(IMETH) = 8 THEN LOCATE 14, coll: PRINT CHR$(26)
          IF cmeth(IMETH) = 9 THEN LOCATE 15, coll: PRINT CHR$(26)
       NEXT IMETH
RETURN
'-----------------------------------------------
'SUBROUTINE TO DECOMPOSE FILE NAME AND CALL
'FILECHK TO CHECK IF FILE IS OK AND DOES EXIST
'-----------------------------------------------
88000  xln = LEN(FILEN$)
       FF$ = FILEN$
       A1 = INSTR(FF$, ":")
       IF A1 = 0 THEN
          fdrive$ = LEFT$(CURDIR$, 1)
          fp$ = FILEN$
       ELSE
          fdrive$ = LEFT$(FF$, A1 - 1)
          fp$ = RIGHT$(FILEN$, LEN(FILEN$) - A1)
       END IF
       FF$ = fp$
       FOR I = 1 TO 10
          A1 = INSTR(FF$, "\")
          FF$ = RIGHT$(FF$, LEN(FF$) - A1)
          IF A1 = 0 THEN
             lenpath = LEN(fp$) - LEN(FF$)
             IF I = 1 OR I = 2 THEN
                FPATH$ = ""
             ELSE
                FPATH$ = LEFT$(fp$, LEN(fp$) - LEN(FF$))
             END IF
             CALL FILECHK(fdrive$, FPATH$, FF$, found)
             IF found = 0 THEN
               LOCATE 24, 35: PRINT "Invalid Directory";
             ELSEIF found = 1 THEN
               LOCATE 24, 35: PRINT "Error in Path";
             ELSEIF found = 2 THEN
               LOCATE 24, 35: PRINT "Bad File Name";
             ELSEIF found = 3 THEN
               LOCATE 24, 35: PRINT "File Not Found";
             ELSEIF found = 5 THEN
               LOCATE 24, 35: PRINT "Invalid Drive";
             END IF
             RETURN
          END IF
       NEXT I
  RETURN
''''''''''''''''''''''''''''''''''''''''
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
''''''''''''''''''''''''''''''''''''''''
63195 CALL EXOUT(ANSCODE)
     IF ANSCODE = 0 THEN
        GOTO 620
     ELSE
        EXIT SUB
     END IF
RETURN
'''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO CALL MANUAL MESSAGES FOR HELP
'''''''''''''''''''''''''''''''''''''''''''''''''
63190  IF irtrn% = 20 THEN CALL HLPMSG(F1HELP$)
       IF irtrn% = 21 THEN CALL HLPMSG(F2HELP$)
       IF irtrn% = 22 THEN CALL USERHELP
       RETURN
EXIT SUB
END SUB

SUB CLIMSAVE (FILEPATH$())
DIM DATAFILE$(4), locline(4), EXT$(4), SAVEYN$(4), fdrive$(4)
'------------------------------------------------------------------
'THIS ROUTINE IS USED TO ENTER THE SAVE FILE NAMES OF THE WEATHER
'DATA FILES.  IT ALLOWS A SELECTION OF A FILE NAME FROM THE LIST
'FILES WITH THE SEPCIFIC EXTENSIONS.
'  D4            PRECIPITATION
'  D7            TEMPERATURE
'  D11           ET
'  D13           SOLAR RADIATION
'DEFINITION OF VARIABLES........
'  DATAFILE$()   ARRAY OF FILE NAMES
'  FILEPATH$()   ARRAY FO PATH TO DATA FILE
'  NLINES        NUMBER OF LINES (FILES)
'  LOLINE()      ROW LOCATION OF ENTRY ON SCREEN
'  EXT$()        FILE EXTENSION FOR EACH WEATHER FILE
'  FILEOK()      = 0 IF FILE IS NOT GOOD, = 1 IF FILE IS OK
'ASSIGN CURRENT DIRECTORY AS DEFAULT
'FOR ALL FILE PATHS
'----------------------------------------------------------------
   DEFDIR$ = CURDIR$
   DIRECT$ = CURDIR$
   NLINES = 4
   DATAFILE$(1) = "DATA4": EXT$(1) = "D4"
   DATAFILE$(2) = "DATA7": EXT$(2) = "D7"
   DATAFILE$(3) = "DATA13": EXT$(3) = "D13"
   DATAFILE$(4) = "DATA11": EXT$(4) = "D11"
   FOR I = 1 TO NLINES
     'FILEPATH$(I) = DIRECT$
      SAVEYN$(I) = "Y"
      locline(I) = 6 + I
   NEXT I
   IF DIR$("data4.tmp") = "" THEN SAVEYN$(1) = "N"
   IF DIR$("data7.tmp") = "" THEN SAVEYN$(2) = "N"
   IF DIR$("data13.tmp") = "" THEN SAVEYN$(3) = "N"
   IF VAL(GR2$) = 0 THEN SAVEYN$(4) = "N"
7101  COLOR 15, 1
      CLS
      ON LOCAL ERROR RESUME NEXT
      '----------------------------------
      'DISPLAY HEADER LINES AND MENU LINES
      'DISPLAY WEATHER ITEM DESCRIPTION
      'DISPLAY PATH
      '----------------------------------
      COLOR 11, 6
      LOCATE 1, 1:  PRINT "                          WEATHER DATA  -  FILE SAVING                          ";
      COLOR 15, 1
      CALL menu
      SROWBOX = 4: SCOLBOX = 2: BOXWIDTH = 78: EROWBOX = 11
      CALL BOX
      LOCATE 5, 3: PRINT "                    SAVE"
      LOCATE 6, 3: PRINT "                    Y/N  ----------------DIRECTORY---------------": LOCATE 6, 70: PRINT "--FILE--"
      LOCATE 7, 3:  PRINT "PRECIPITATION"
      LOCATE 8, 3:  PRINT "TEMPERATURE"
      LOCATE 9, 3:  PRINT "SOLAR RADIATION"
      LOCATE 10, 3: PRINT "EVAPOTRANSPIRATION"
      LOCATE 16, 9: PRINT "Enter names for files in which the weather data are to be saved."
      F1HELP$ = "#CLIMSAVE"
      GOSUB 78165
      FOR I = 1 TO 4
        IF LTRIM$(RTRIM$(FILEPATH$(I))) = "" THEN FILEPATH$(I) = DEFDIR$
        LOCATE locline(I), 24: PRINT SAVEYN$(I)
        LOCATE locline(I), 28: PRINT FILEPATH$(I)
        LOCATE locline(I), 70: PRINT DATAFILE$(I)
      NEXT I
      '-------------------------
      'BEGIN SCREEN INPUT
      '-------------------------
      I = 1
7130  CALL ACELL(locline(I), 24, 1, SAVEYN$(I), 1)
      F2HELP$ = "#SAVETAG"
      IF UCASE$(SAVEYN$(I)) <> "N" THEN SAVEYN$(I) = "Y"
      LOCATE locline(I), 24: PRINT UCASE$(SAVEYN$(I));
      LOCATE locline(NLINES) + 2, 26: PRINT "                                               "
      LOCATE locline(NLINES) + 3, 26: PRINT "                                               "
      GOSUB 73190
      IF irtrn% = -1 THEN EXIT SUB
      IF irtrn% = 1 OR irtrn% = 2 THEN GOSUB 6000: GOTO 7130
      IF irtrn% = 1000 THEN GOSUB 5500
      IF irtrn% = 3 THEN
         IF I = 1 THEN
           I = NLINES
         ELSE
           I = I - 1
         END IF
         GOTO 7130
      END IF
      IF irtrn% = 4 THEN
        IF I <> NLINES THEN
           I = I + 1
        ELSE
           I = 1
        END IF
        GOTO 7130
      END IF
      IF irtrn% = 6 OR irtrn% = 5 OR irtrn% = 50 OR irtrn% = 51 THEN GOTO 7140
     IF irtrn% = 7 THEN
        IF I = 1 THEN
           I = NLINES
        ELSE
           I = I - 1
        END IF
        GOTO 7145
     END IF
     IF irtrn% = 23 THEN GOSUB 6500
     GOTO 7130
7140 CALL ACELL(locline(I), 28, 40, FILEPATH$(I), 1)
     LOCATE locline(NLINES) + 2, 26: PRINT "                                               "
     LOCATE locline(NLINES) + 3, 26: PRINT "                                               "
      F2HELP$ = "#PATH"
     GOSUB 73190
     FILEPATH$(I) = LTRIM$(RTRIM$(FILEPATH$(I)))
     IF LTRIM$(RTRIM$(FILEPATH$(I))) = "" THEN
        FILEPATH$(I) = DEFDIR$
        LOCATE locline(I), 28: PRINT FILEPATH$(I)
     END IF
     IF RIGHT$(FILEPATH$(I), 1) <> "\" THEN FILEPATH$(I) = FILEPATH$(I) + "\"
101  IF RIGHT$(FILEPATH$(I), 2) = "\\" THEN
       FILEPATH$(I) = LEFT$(FILEPATH$(I), LEN(FILEPATH$(I)) - 1)
       GOTO 101
     END IF
     IF irtrn% = -1 THEN EXIT SUB
     IF irtrn% = 1 OR irtrn% = 2 THEN GOSUB 6000
     IF irtrn% = 1000 THEN GOSUB 5500
     IF irtrn% = 3 THEN
        IF I = 1 THEN
          I = NLINES
        ELSE
          I = I - 1
        END IF
        GOTO 7140
     END IF
     IF irtrn% = 4 THEN
        IF I <> NLINES THEN
           I = I + 1
        ELSE
           I = 1
        END IF
        GOTO 7140
     END IF
     IF irtrn% = 6 OR irtrn% = 5 THEN
        GOSUB 6250
        GOTO 7145
     END IF
     IF irtrn% = 7 THEN GOTO 7130
     IF irtrn% = 23 THEN GOSUB 6500
     GOTO 7140
7145 CALL ACELL(locline(I), 70, 8, DATAFILE$(I), 1)
     LOCATE locline(NLINES) + 2, 26: PRINT "                                               "
     LOCATE locline(NLINES) + 3, 26: PRINT "                                               "
      F2HELP$ = "#FILE"
     GOSUB 73190
     DATAFILE$(I) = LTRIM$(RTRIM$(DATAFILE$(I)))
     IF irtrn% = -1 THEN EXIT SUB
     IF irtrn% = 1 OR irtrn% = 2 THEN GOSUB 6000
     IF irtrn% = 1000 THEN GOSUB 5500
     IF irtrn% = 3 THEN
       IF I = 1 THEN
          I = NLINES
       ELSE
          I = I - 1
       END IF
       GOTO 7145
     END IF
     IF irtrn% = 4 THEN
        IF I <> NLINES THEN
           I = I + 1
        ELSE
           I = 1
        END IF
        GOTO 7145
     END IF
     IF irtrn% = 7 THEN GOTO 7140
     IF irtrn% = 6 OR irtrn% = 5 THEN
        GOSUB 6250
        IF I = NLINES THEN
           I = 1
        ELSE
           I = I + 1
        END IF
        GOTO 7130
     END IF
     IF irtrn% = 23 THEN GOSUB 6500
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
'SAVE WEATHER DATA SUBROUTINE
'------------------------------
216000 CLOSE
       IF UCASE$(SAVEYN$(1)) = "Y" THEN
          '------------------
          'PRECIPITATION SAVE
          '------------------
          IF RIGHT$(FILEPATH$(1), 1) <> "\" THEN
             d4save$ = FILEPATH$(1) + "\" + DATAFILE$(1) + "." + EXT$(1)
          ELSE
             d4save$ = FILEPATH$(1) + DATAFILE$(1) + "." + EXT$(1)
          END IF
          FDIR$ = DIR$("DATA4.TMP")
          IF FDIR$ <> "" THEN
             IF DIR$(d4save$) <> "" THEN KILL d4save$
             FCOP$ = "COPY " + "DATA4.TMP " + d4save$
             LOCATE 20, 1
             SHELL FCOP$
             LOCATE 20, 1: PRINT "                           ";
          END IF
       END IF
       IF UCASE$(SAVEYN$(2)) = "Y" THEN
          '-----------------
          'TEMPERATURE SAVE
          '-----------------
          FDIR$ = DIR$("DATA7.TMP")
          IF RIGHT$(FILEPATH$(2), 1) <> "\" THEN
             D7SAVE$ = FILEPATH$(2) + "\" + DATAFILE$(2) + "." + EXT$(2)
          ELSE
              D7SAVE$ = FILEPATH$(2) + DATAFILE$(2) + "." + EXT$(2)
          END IF
          IF FDIR$ <> "" THEN
            IF DIR$(D7SAVE$) <> "" THEN KILL D7SAVE$
            FCOP$ = "COPY " + "DATA7.TMP " + D7SAVE$
            LOCATE 21, 1
            SHELL FCOP$
            LOCATE 21, 1: PRINT "                           ";
          END IF
       END IF
       IF UCASE$(SAVEYN$(3)) = "Y" THEN
          '--------------------
          'SOLAR RADIATION SAVE
          '--------------------
          FDIR$ = DIR$("DATA13.TMP")
          IF RIGHT$(FILEPATH$(3), 1) <> "\" THEN
              D13SAVE$ = FILEPATH$(3) + "\" + DATAFILE$(3) + "." + EXT$(3)
          ELSE
              D13SAVE$ = FILEPATH$(3) + DATAFILE$(3) + "." + EXT$(3)
          END IF
          IF FDIR$ <> "" THEN
             IF DIR$(D13SAVE$) <> "" THEN KILL D13SAVE$
             FCOP$ = "COPY " + "DATA13.TMP " + D13SAVE$
             LOCATE 22, 1
             SHELL FCOP$
             LOCATE 22, 1: PRINT "                           ";
          END IF
        END IF
        IF UCASE$(SAVEYN$(4)) = "Y" THEN
          '-------------------------------------
          'SAVE EVAPOTRANSPIRATION DATA (DATA11)
          '-------------------------------------
          IF RIGHT$(FILEPATH$(4), 1) <> "\" THEN
              D11SAVE$ = FILEPATH$(4) + "\" + DATAFILE$(4) + "." + EXT$(4)
          ELSE
              D11SAVE$ = FILEPATH$(4) + DATAFILE$(4) + "." + EXT$(4)
          END IF
          OPEN D11SAVE$ FOR OUTPUT AS #1
          PERIOD$ = "."
          PRINT #1, USING "##"; VAL(etunits$)
          PRINT #1, USING "\                    \"; ETCITY$; ETSTATE$
          TEXT1$ = LATIT$: GOSUB 30916: LATIT$ = TEXT1$
          PRINT #1, USING "\        \"; LATIT$;
          PRINT #1, USING "####"; VAL(GR1$); VAL(GR2$); : PRINT #1, " ";
          TEXT1$ = LAI$: GOSUB 30916: LAI$ = TEXT1$
          TEXT1$ = evdepth$: GOSUB 30916: evdepth$ = TEXT1$
          TEXT1$ = WIND$: GOSUB 30916: WIND$ = TEXT1$
          TEXT1$ = HUMID1$: GOSUB 30916: HUMID1$ = TEXT1$
          TEXT1$ = HUMID2$: GOSUB 30916: HUMID2$ = TEXT1$
          TEXT1$ = HUMID3$: GOSUB 30916: HUMID3$ = TEXT1$
          TEXT1$ = HUMID4$: GOSUB 30916: HUMID4$ = TEXT1$
          PRINT #1, USING "\     \"; LAI$; evdepth$;
          PRINT #1, USING "\   \"; WIND$; HUMID1$; HUMID2$; HUMID3$; HUMID4$
        END IF
      CLOSE
      irtrn% = 1000
      FOR I = 1 TO 4
        IF SAVEYN$(I) = "Y" THEN GOTO 5400
      NEXT I
      irtrn% = 100
5400  EXIT SUB
RETURN
'------------------------------------
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
'------------------------------------
5500 CALL EXOUT(ANSCODE)
     IF ANSCODE = 0 THEN
       GOTO 7101
     ELSE
       EXIT SUB
     END IF
RETURN
'----------------------------------
'ADD A DECIMAL POINT IF ONE IS NEEDED
'------------------------------------
30916   IF INSTR(TEXT1$, PERIOD$) THEN
            'NO DECIMAL POINT IS NEEDED
        ELSE
            TEXT1$ = LTRIM$(RTRIM$(TEXT1$)) + PERIOD$
        END IF
RETURN
'-------------------------------------------------
'CHECK WHETHER DIRECTORIES, PATHS, AND FILES NAMES
'ARE VALID AND WHETHER SPECIFIED FILES EXIST.....
'ISSUE WARNINGS....
'-------------------------------------------------
6000  LOCATE locline(NLINES) + 3, 26: ' PRINT "Verifying Directories, Paths, and File Names..."
      irtrn% = 0
      FOR II = 1 TO NLINES
        IF II = 1 AND DIR$("DATA4.TMP") = "" THEN GOTO 6050
        IF II = 2 AND DIR$("DATA7.TMP") = "" THEN GOTO 6050
        IF II = 3 AND DIR$("DATA13.TMP") = "" THEN GOTO 6050
        LOCATE locline(NLINES) + 2, 26: PRINT "                                               "
        LOCATE locline(NLINES) + 3, 26: PRINT "                                               "
        IF SAVEYN$(II) = "Y" THEN
           A1 = INSTR(FILEPATH$(II), ":")
           IF A1 = 0 THEN
              fdrive$ = LEFT$(CURDIR$, 1)
              FPATH$ = FILEPATH$(II)
           ELSE
              fdrive$ = LEFT$(FILEPATH$(II), A1 - 1)
              FPATH$ = RIGHT$(FILEPATH$(II), LEN(FILEPATH$(II)) - A1)
           END IF
           IF FPATH$ = "\" THEN FPATH$ = ""
           FILEN$ = DATAFILE$(II)
           found = -1
           CALL FILECHK(fdrive$, FPATH$, FILEN$ + "." + EXT$(II), found)
           IF found = 3 THEN GOTO 6050
           LOCATE locline(NLINES) + 3, 26: PRINT "                                               "
           IF found = 0 THEN LOCATE locline(NLINES) + 3, 26: PRINT "Invalid Directory": I = II: RETURN
           IF found = 1 THEN LOCATE locline(NLINES) + 3, 26: PRINT "Error in Path": I = II: RETURN
           IF found = 2 THEN LOCATE locline(NLINES) + 3, 26: PRINT "Bad File Name": I = II: RETURN
           IF found = 5 THEN LOCATE locline(NLINES) + 3, 26: PRINT "Invalid Drive": I = II: RETURN
           IF found = 4 THEN
              LOCATE locline(NLINES) + 2, 26
              PRINT FILEPATH$(II) + " "; DATAFILE$(II) + "."; EXT$(II)
              LOCATE locline(NLINES) + 3, 26
              PRINT "File Already Exists.  Overwrite ? Y/N "
              ANSER$ = "Y"
6010          CALL ACELL(locline(NLINES) + 3, 65, 1, ANSER$, 1)
              IF UCASE$(ANSER$) <> "N" THEN ANSER$ = "Y"
              LOCATE locline(NLINES) + 3, 65: PRINT UCASE$(ANSER$)
              IF irtrn% = 5 THEN
                 IF ANSER$ = "Y" THEN
                   GOTO 6050
                   SAVEYN$(II) = "Y"
                 ELSE
                   SAVEYN$(II) = "N"
                   I = II: RETURN
                 END IF
              END IF
              IF irtrn% = 100 OR irtrn% = 1000 THEN RETURN
              GOTO 6010
           END IF
           RETURN
        END IF
6050  NEXT II
'ALL FILES SPECIFIED ARE OK.
GOSUB 216000
RETURN
'-------------------------------------------------
'SUBROUTINE TO CHECK DIRECTORY, PATH AND FILENAME
'-------------------------------------------------
6250 LOCATE locline(NLINES) + 3, 26: ' PRINT "Verifying Directory, Path, and File Name...    "
     IF SAVEYN$(I) = "Y" THEN
        A1 = INSTR(FILEPATH$(I), ":")
        IF A1 = 0 THEN
           fdrive$ = LEFT$(CURDIR$, 1)
           FPATH$ = FILEPATH$(I)
        ELSE
           fdrive$ = LEFT$(FILEPATH$(I), A1 - 1)
           FPATH$ = RIGHT$(FILEPATH$(I), LEN(FILEPATH$(I)) - A1)
        END IF
        IF FPATH$ = "\" THEN FPATH$ = ""
        FILEN$ = DATAFILE$(I)
        found = -1
        CALL FILECHK(fdrive$, FPATH$, FILEN$ + "." + EXT$(I), found)
        LOCATE locline(NLINES) + 3, 26: PRINT "                                               "
        IF found = 0 THEN FILEPATH$(I) = DIRECT$: LOCATE locline(I), 28: PRINT "                                        ": LOCATE locline(I), 28: PRINT FILEPATH$(I): LOCATE locline(NLINES) + 3, 26: PRINT "Invalid Directory": RETURN
        IF found = 1 THEN FILEPATH$(I) = DIRECT$: LOCATE locline(I), 28: PRINT "                                        ": LOCATE locline(I), 28: PRINT FILEPATH$(I): LOCATE locline(NLINES) + 3, 26: PRINT "Error in Path": RETURN
        IF found = 2 THEN LOCATE locline(NLINES) + 3, 26: PRINT "Bad File Name": RETURN
        IF found = 4 THEN LOCATE locline(NLINES) + 3, 26: PRINT "File Already Exists": RETURN
        IF found = 5 THEN FILEPATH$(I) = DIRECT$: LOCATE locline(I), 28: PRINT "                                        ": LOCATE locline(I), 28: PRINT FILEPATH$(I): LOCATE locline(NLINES) + 3, 26: PRINT "Invalid Drive": RETURN
     END IF
RETURN
'--------------------------------------------------------------
'SUBROUTINE TO GET DIRECTORY OF FILES OF A SPECIFIED EXTENSION
'--------------------------------------------------------------
6500    CALL GETDIR(EXT$(I), SELECT$, FILEPATH$(I))
        IF irtrn% = 1000 THEN GOSUB 5500
        IF SELECT$ <> "" THEN
          DATAFILE$(I) = LTRIM$(RTRIM$(SELECT$)): SELECT$ = ""
          LOCATE locline(I), 70: PRINT "        "
          LOCATE locline(I), 70: PRINT DATAFILE$(I)
        END IF
        GOSUB 78165
RETURN
'----------------------------------------------
'SUBROUTINE TO CALL help MESSAGES FOR HELP
'----------------------------------------------
73190  IF irtrn% = 20 THEN CALL HLPMSG(F1HELP$)
       IF irtrn% = 21 THEN CALL HLPMSG(F2HELP$)
       IF irtrn% = 22 THEN CALL USERHELP
      RETURN
END SUB

