DECLARE SUB TYPEIN (CLIM$, YEARNO%, TOT!(), NEWYR$)
DECLARE SUB HLPMSG (F2HELP$)
DECLARE SUB USERHELP ()
DECLARE SUB EVAPIND (SELECCT$, SELECST$, fromsub!)
DECLARE SUB ACELL (IROW!, ISTART!, NS!, TEXT$, ITYPE!)
DECLARE SUB EXOUT (ANSCODE!)
DECLARE SUB RCELL (IROW!, ISTART!, NS!, TXT$)

COMMON SHARED tempu$, precu$, solu$, UNITS$, UNIT$, UN1$, UN2$, UN3$, UN4$, UN5$
COMMON SHARED unev$, UNSL$, TOHELP%, nl%, CLIMATF$, IRTRN%, PAGE%, CLIM
COMMON SHARED APAGE%, VPAGE%, HPAGE%, DIRECT$, SELECCT$, SELECST$
COMMON SHARED GROW1$, GROW2$, MAXLAI$, EVBARE$, EVFAIR$, EVEXCL$
COMMON SHARED evdepth$, LAI$, GR1$, GR2$, AREA$, nyears$, latitude$
COMMON SHARED HUMID1$, HUMID2$, HUMID3$, HUMID4$, WIND$, LATIT$, meansel
COMMON SHARED WSPEED$, BARE$, FAIR$, EXCL$, UNI$, ETCITY$, ETSTATE$, etunits$
COMMON SHARED defrain, deftemp, CMETH(), CUNIT(), ccity$(), cstate$(), ct$, st$, FILEPATH$()
COMMON SHARED SROWBOX, SCOLBOX, BOXWIDTH, EROWBOX, defp(), deft()

SUB synprec (synstate$, syncity$, CLIM$, ok, tape2lin$())
    DIM SYNTH$(12), unmmp$(12)
    '---------------------------------'---------------------------------------
    'defrain = 0  no default normal mean monthly  precip has been selected
    '          1  default normal mean monthly precip has been selected
    'defrainu = 1 previously selected default rain units is CUSTOMARY
    '         = 2     =         =        =     =     =    = metric
    'unmmp$   = user defined normal mean monthly precip
    'synth$   = user defined values used on the screen
    '           at the end of the synthetic selection, these values are
    '           copied into unmmp$ or unmmt$ or are not used if the user
    '           selects the default values
    '----------------------------------------------------------------------
    PCOPY VPAGE%, HPAGE%
    IRTRN% = 0
    PCOPY VPAGE%, APAGE%
    CLS APAGE%
    SCREEN 0, , VPAGE%, VPAGE%
    '------------------------------------------------------
    'determine the maximum number of years for temperature   '
    'and solar radiation based on the precip number of years '
    '------------------------------------------------------
    FHELP = 0
    CLOSE
    a$ = DIR$("DATA4.TMP")
    IF a$ <> "" THEN
      OPEN "DATA4.TMP" FOR INPUT AS #2
      FOR I = 1 TO 3
        IF EOF(2) THEN GOTO 701
        LINE INPUT #2, l$
      NEXT I
      FOR I = 1 TO 100
        FOR II = 1 TO 37
           IF EOF(2) THEN GOTO 701
           LINE INPUT #2, l$
        NEXT II
        NNN = I
      NEXT I
701 CLOSE
    MAXIYR = NNN
    nyears$ = STR$(NNN)
    END IF
80002    COLOR 4, 3
    LOCATE 25, 1: PRINT "    F1=Info      F2=Help      F9=Quit      F10=Generate Data      Esc=Cancel    ";
    COLOR 15, 1
'ADD A F1 HELP MESSAGE
    CLIM1$ = "     PRECIPITATION     "
    CLIM = 1: CO = 0
    COLOR 11, 6
    LOCATE 1, 1:  PRINT "                           SYNTHETIC PRECIPITATION DATA                         "
7334  COLOR 15, 1
    LOCATE 3, 14:  PRINT "  City:                     State:                         "
    LOCATE 4, 14:  PRINT "  Number of Years for Synthetic Data Generation            "
    LOCATE 5, 14:  PRINT "  Use Default Normal Mean Monthly Precipitation  YES  NO   "
    LOCATE 6, 14: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
    LOCATE 7, 14: PRINT "บ         Normal Mean Monthly                             บ"
    IF CUNIT(CLIM, 2) = 2 THEN
      LOCATE 7, 44: PRINT "Precipitation (MM)"
    ELSE
      LOCATE 7, 44: PRINT "Precipitation (IN)"
    END IF
    LOCATE 8, 14: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
    LOCATE 9, 18: PRINT "                           USER          DEFAULT   "
    LOCATE 10, 18: PRINT "    1.  January   "
    LOCATE 11, 18: PRINT "    2.  February  "
    LOCATE 12, 18: PRINT "    3.  March     "
    LOCATE 13, 18: PRINT "    4.  April     "
    LOCATE 14, 18: PRINT "    5.  May       "
    LOCATE 15, 18: PRINT "    6.  June      "
    LOCATE 16, 18: PRINT "    7.  July      "
    LOCATE 17, 18: PRINT "    8.  August    "
    LOCATE 18, 18: PRINT "    9.  September "
    LOCATE 19, 18: PRINT "   10.  October   "
    LOCATE 20, 18: PRINT "   11.  November  "
    LOCATE 21, 18: PRINT "   12.  December  "
    LOCATE 4, 63: PRINT nyears$
    LOCATE 3, 22: PRINT syncity$;
    LOCATE 3, 49: PRINT synstate$;
    GOSUB 26000
    '--------------------------------------------------------
    'If default precipitation has been previously selected,  '
    'the program should not read values from .tmp file       '
    '--------------------------------------------------------
    'IF defrain = 0 THEN
       IF DIR$("DATA4.TMP") <> "" THEN
          OPEN "data4.tmp" FOR INPUT AS #1
          GOSUB 23000
       ELSE
          IF CUNIT(CLIM, 2) = 1 THEN CO = 45: COSUM = CO - 1
          IF CUNIT(CLIM, 2) = 2 THEN CO = 44: COSUM = CO - 1
          'GOSUB 26000
       END IF
    'ELSE
     'GOSUB 24000
    'END IF
    sum = 0
    IF CO <> 0 THEN
      FOR k% = 1 TO 12
        IF CUNIT(CLIM, 2) = 1 THEN decp = 2
        IF CUNIT(CLIM, 2) = 2 THEN decp = 1
        PNTPOS = INSTR(SYNTH$(k%), ".")
        SYNTH$(k%) = LTRIM$(RTRIM$(LEFT$(SYNTH$(k%), PNTPOS + decp)))
        IF CUNIT(CLIM, 2) = 1 THEN LOCATE 9 + k%, CO: PRINT USING "\    \"; SYNTH$(k%);
        IF CUNIT(CLIM, 2) = 2 THEN LOCATE 9 + k%, CO: PRINT USING "\    \"; SYNTH$(k%);
        sum = sum + VAL(SYNTH$(k%))
      NEXT k%
      IF CUNIT(CLIM, 2) = 1 THEN LOCATE 23, COSUM: PRINT USING "####.##"; sum;
      IF CUNIT(CLIM, 2) = 2 THEN LOCATE 23, COSUM: PRINT USING "####.#"; sum;
    END IF
    YES$ = "YES": NO$ = "NO"
    nyears$ = LTRIM$(RTRIM$(nyears$))
    IF IRET = 2 THEN IRET = 0: GOTO 750
    IF IRET = 3 THEN IRET = 0: GOTO 760
    IF IRET = 4 THEN IRET = 0: GOTO 2900
    IRET = 0
740  'ADD A F2 HELP MESSAGE
    LOCATE 4, 63: PRINT "    "
       CALL ACELL(4, 63, 3, nyears$, 2)
       IF IRTRN% = 20 THEN GOSUB 8000: FHELP = 1: IRET = 1: GOTO 80002
       IF IRTRN% = 21 THEN GOSUB 8500: FHELP = 1: IRET = 1: GOTO 80002
       IF IRTRN% = 1000 OR IRTRN% = 100 THEN GOSUB 23195
       IF IRTRN% = 2 THEN IRTRN% = 5: GOTO 751   'GOSUB 2950: GOTO 2850
       IF VAL(nyears$) = 0 THEN GOTO 740
       IF IRTRN% = 5 OR IRTRN% = 4 OR IRTRN% = 3 THEN GOTO 750
       GOTO 740
750  'ADD A F2 HELP MESSAGE
     CALL RCELL(5, 63, 3, YES$)
     IF IRTRN% = 20 THEN GOSUB 8000: FHELP = 1: IRET = 2: GOTO 80002
     IF IRTRN% = 21 THEN GOSUB 8500: FHELP = 1: IRET = 2: GOTO 80002
     IF IRTRN% = 1000 OR IRTRN% = 100 THEN GOSUB 23195
751  IF IRTRN% = 2 OR IRTRN% = 5 THEN
        IF CUNIT(CLIM, 2) = 1 THEN
            FOR k% = 1 TO 12
              SYNTH$(k%) = STR$(defp(k%))
            NEXT k%
        ELSE
            FOR k% = 1 TO 12
              SYNTH$(k%) = STR$(defp(k%) * 25.4)
            NEXT k%
        END IF
        GOSUB 2950: GOTO 2850
     END IF
     IF IRTRN% = 6 OR IRTRN% = 7 THEN GOTO 760
     IF IRTRN% = 3 OR IRTRN% = 4 THEN GOTO 740
     GOTO 750
760  CALL RCELL(5, 68, 2, NO$)
     IF IRTRN% = 20 THEN GOSUB 8000: FHELP = 1: IRET = 3: GOTO 80002
     IF IRTRN% = 21 THEN GOSUB 8500: FHELP = 1: IRET = 3: GOTO 80002
     IF IRTRN% = 1000 OR IRTRN% = 100 THEN GOTO 23195
     'IF irtrn% = 2 THEN GOSUB 2950: GOTO 2850
     IF IRTRN% = 6 OR IRTRN% = 7 THEN GOTO 750
     IF IRTRN% = 3 OR IRTRN% = 4 THEN GOTO 740
     IF IRTRN% = 5 THEN GOSUB 770
     GOTO 760
770  MO = 10
2900 'ADD A F2 HELP MESSAGE
      I% = MO - 9
      SYNTH$(I%) = LTRIM$(RTRIM$(SYNTH$(I%)))
      IF COSUM = 0 THEN COSUM = CO - 3
      CALL ACELL(MO, CO, 6, SYNTH$(I%), 2)
      sum = 0
      FOR III% = 1 TO 12
        sum = sum + VAL(SYNTH$(III%))
      NEXT III%
      LOCATE 23, CO: PRINT "            "
      IF CUNIT(CLIM, 2) = 1 THEN LOCATE 23, COSUM: PRINT USING "####.##"; sum;
      IF CUNIT(CLIM, 2) = 2 THEN LOCATE 23, COSUM: PRINT USING "####.#"; sum;
      'LOCATE 23, COSUM: PRINT USING "####.##"; Sum;
      IF IRTRN% = 20 THEN GOSUB 8000: FHELP = 1: IRET = 4: GOTO 80002
      IF IRTRN% = 21 THEN GOSUB 8500: FHELP = 1: IRET = 4: GOTO 80002
      IF IRTRN% = 1000 THEN 23195
      IF IRTRN% = 2 THEN
           IF sum = 0 THEN GOTO 760
           defrain = 1: GOSUB 2950: GOTO 2850
      END IF
      IF IRTRN% = 100 THEN GOTO 760
     'MOVE DOWN
     IF IRTRN% = 4 OR IRTRN% = 5 THEN
        IF MO < 21 THEN
          MO = MO + 1
          GOTO 2900
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
      GOTO 2900
  END IF
'MOVE HOME
      IF IRTRN% = 8 THEN
        MO = 10
        GOTO 2900
      END IF
'MOVE END
      IF IRTRN% = 9 THEN
        MO = 21
      END IF
      GOTO 2900
2850  CLS : PCOPY HPAGE%, VPAGE%: 'IRTRN% = 0
      EXIT SUB
'---------------------------------------------
'SUBROUTINE TO SAVE SYNTHETIC DATA REQUIREMENTS
'INTO SYNGEN.TMP FILE WHICH WILL BE READ BY THE
'SYNTHETIC WEATHER GENERATOR ALONG WITH DATA11
'---------------------------------------------
2950 OPEN "SYNGEN.TMP" FOR OUTPUT AS #1
     PRINT #1, " 1"
     PRINT #1, USING "##"; CUNIT(CLIM, 2)
     PRINT #1, USING "\                  \"; syncity$; synstate$
     PRINT #1, USING "####"; VAL(nyears$)
     defrain = 1
     FOR I = 1 TO 12
      ' IF INSTR(synth$(I), ".") THEN
         IF CUNIT(CLIM, 2) = 1 THEN PRINT #1, USING "###.##"; VAL(SYNTH$(I));
         IF CUNIT(CLIM, 2) = 2 THEN PRINT #1, USING "####.#"; VAL(SYNTH$(I));
      ' ELSE
      '   synth$(I) = LTRIM$(RTRIM$(synth$(I))) + "."
      '   PRINT #1, USING "\    \"; synth$(I);
      'END IF
      NEXT I
      PRINT #1,
      '-----------------------------
      'save values as user defined in
      'CUSTOMARY units
      '-----------------------------
      IF CUNIT(CLIM, 2) = 1 THEN
        FOR k% = 1 TO 12
           unmmp$(k%) = SYNTH$(k%)
        NEXT k%
      ELSE
        FOR k% = 1 TO 12
          unmmp$(k%) = STR$(VAL(SYNTH$(k%)) / 25.4)
        NEXT k%
      END IF
     FOR I = 1 TO 10
       PRINT #1, tape2lin$(I)
     NEXT I
     CLOSE
     RETURN
'----------------------------------------
'subroutine to read normal mean monthly
'values from .tmp file if it exists.
'----------------------------------------
23000 LINE INPUT #1, l$
      LINE INPUT #1, l$
      UUNI = VAL(LTRIM$(RTRIM$(l$)))
      LINE INPUT #1, l$
      LINE INPUT #1, l$
      SYNTH$(1) = LTRIM$(RTRIM$(LEFT$(l$, 6)))
      SYNTH$(2) = LTRIM$(RTRIM$(MID$(l$, 7, 6)))
      SYNTH$(3) = LTRIM$(RTRIM$(MID$(l$, 13, 6)))
      SYNTH$(4) = LTRIM$(RTRIM$(MID$(l$, 19, 6)))
      SYNTH$(5) = LTRIM$(RTRIM$(MID$(l$, 25, 6)))
      SYNTH$(6) = LTRIM$(RTRIM$(MID$(l$, 31, 6)))
      SYNTH$(7) = LTRIM$(RTRIM$(MID$(l$, 37, 6)))
      SYNTH$(8) = LTRIM$(RTRIM$(MID$(l$, 43, 6)))
      SYNTH$(9) = LTRIM$(RTRIM$(MID$(l$, 49, 6)))
      SYNTH$(10) = LTRIM$(RTRIM$(MID$(l$, 55, 6)))
      SYNTH$(11) = LTRIM$(RTRIM$(MID$(l$, 61, 6)))
      SYNTH$(12) = LTRIM$(RTRIM$(MID$(l$, 67, 6)))
      FOR k% = 1 TO 12
         SYNTH$(k%) = LTRIM$(RTRIM$(SYNTH$(k%)))
      NEXT k%
      IF CLIM$ = CLIM1$ THEN
         defrainu = UUNI
      ELSE
         deftempu = UUNI
      END IF
'-------------------------------
'MAKE VALUES COMPATIBLE WITH
'SELECTED UNITS
'-------------------------------
      IF UUNI = CUNIT(CLIM, 2) THEN
         CO = 45
         COSUM = CO - 3
         IF UUNI = 2 THEN COSUM = COSUM + 1
         GOSUB 26000
         CLOSE : RETURN
      END IF
'------------------------------------
'TAKE CARE OF PRECIPITATION DATA
'------------------------------------
      mult = 25.4
      IF UUNI = 2 AND CUNIT(CLIM, 2) = 1 THEN
        '----------------------
        'convert to CUSTOMARY
        '----------------------
        CO = 45
        sum = 0
        FOR k% = 1 TO 12
          SYNTH$(k%) = LTRIM$(RTRIM$(STR$(VAL(SYNTH$(k%)) / mult)))
        NEXT k%
        COSUM = CO - 3
        GOSUB 26000
      END IF
      IF UUNI = 1 AND CUNIT(CLIM, 2) = 2 THEN
        '------------------
        'Convert to Metric
        '------------------
           CO = 44
           sum = 0
           FOR k% = 1 TO 12
             SYNTH$(k%) = LTRIM$(RTRIM$(STR$(VAL(SYNTH$(k%)) * 25.4)))
           NEXT k%
           COSUM = CO - 1
           GOSUB 26000
        END IF
      GOSUB 26000
    CLOSE
RETURN
'----------------------------------------
'Subroutine to convert precip values into  '
'proper units                              '
'----------------------------------------
24000  IF CUNIT(CLIM, 2) = 2 THEN             'need to display in Metric units
          CO = 45
          mult = 25.4
          sum = 0
          FOR k% = 1 TO 12
             SYNTH$(k%) = STR$(VAL(unmmp$(k%)) * mult)
          NEXT k%
          COSUM = CO
       ELSE
          CO = 45
          mult = 1
          sum = 0
          FOR k% = 1 TO 12
             SYNTH$(k%) = STR$(VAL(unmmp$(k%)) * mult)
          NEXT k%
          COSUM = CO - 2
       END IF
       '---------------------
       'convert user values
       '---------------------
       CO = 45
       sum = 0
       FOR k% = 1 TO 12
          SYNTH$(k%) = STR$(VAL(unmmp$(k%)) * mult)
       NEXT k%
       COSUM = 45
       GOSUB 26000
RETURN
'-------------------------------------
'convert default values to proper units
'and display
'------------------------------------
26000     IF CUNIT(CLIM, 2) = 1 THEN
              'CO = 58
              mult = 1
              sum = 0
              FOR k% = 1 TO 12
                LOCATE k% + 9, 58: PRINT USING "###.##"; defp(k%) * mult
                sum = sum + defp(k%) * mult
              NEXT k%
              LOCATE 23, 58: PRINT USING "###.##"; sum;
          ELSE
              'CO = 59
              mult = 25.4
              sum = 0
              FOR k% = 1 TO 12
                LOCATE k% + 9, 59: PRINT USING "####.#"; defp(k%) * mult
                sum = sum + defp(k%) * mult
              NEXT k%
              LOCATE 23, 57: PRINT USING "######.#"; sum;
              'CO = 44
            END IF
RETURN
'--------------------------------------
'Subroutine to set the proper values to
'save in syngen.tmp if default values
'are selected.
'--------------------------------------
27000   FOR k% = 1 TO 12
           IF CUNIT(CLIM, 2) = 1 THEN
                 SYNTH$(k%) = STR$(defp(k%))
              ELSE
                 SYNTH$(k%) = STR$(defp(k%) * 25.4)
              END IF
        NEXT k%
RETURN
'------------------------------------
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
'------------------------------------
23195 CALL EXOUT(ANSCODE)
     IF ANSCODE = 0 THEN
        FOR k% = 1 TO 6
          IF CUNIT(CLIM, 2) = 1 THEN LOCATE 9 + k%, CO: PRINT USING "\    \"; SYNTH$(k%);
          IF CUNIT(CLIM, 2) = 2 THEN LOCATE 9 + k%, CO + 1: PRINT USING "\    \"; SYNTH$(k%);
        NEXT k%
        GOTO 7334
     ELSE
        IRTRN% = 100
        CLS : PCOPY HPAGE%, VPAGE%
        EXIT SUB
     END IF
RETURN
'-------------------------------------------------
'HELP MESSAGES FOR SYNTHETIC PRECIPITATION SCREEN
'                       F1
'-------------------------------------------------
8000  COLOR 15, 1
      CLS
     COLOR 4, 3
     LOCATE 25, 1: PRINT "                           Press Any Key to Return                              ";
     COLOR 15, 1
     LOCATE 2, 3: PRINT "                                                                            "
     LOCATE 3, 3: PRINT "1.  Synthetic Precipitation Option (Customary or Metric Units).  The program"
     LOCATE 4, 3: PRINT "    will generate from 1 to 100 years of daily precipitation data           "
     LOCATE 5, 3: PRINT "    stochastically for the selected location using a synthetic weather      "
     LOCATE 6, 3: PRINT "    generator.  The synthetic precipitation data will have approximately the"
     LOCATE 7, 3: PRINT "    same statistical characteristics as the historic data at the selected   "
     LOCATE 8, 3: PRINT "    location.  If desired, the user can enter normal mean monthly           "
     LOCATE 9, 3: PRINT "    precipitation values for the specific location to improve the           "
     LOCATE 10, 3: PRINT "    statistical characteristics of the resulting daily values.  The user is "
     LOCATE 11, 3: PRINT "    advised to enter normal mean monthly precipitation values if the project"
     LOCATE 12, 3: PRINT "    site is located some distance from the city selected from Table 3.  The "
     LOCATE 13, 3: PRINT "    daily values will vary from month to month and from year to year and    "
     LOCATE 14, 3: PRINT "    will not equal the normal values entered.  The same data is produced    "
     LOCATE 15, 3: PRINT "    every time the option is used for a given location.  The data required  "
     LOCATE 16, 3: PRINT "    by the synthetic weather generator are:                                 "
     LOCATE 17, 3: PRINT "                                                                            "
     LOCATE 18, 3: PRINT "    *  Number of years of data to be generated                              "
     LOCATE 19, 3: PRINT "                                                                            "
     LOCATE 20, 3: PRINT "    *  Normal mean monthly precipitation (Optional, default values are      "
     LOCATE 21, 3: PRINT "       available.)                                                          "
     LOCATE 22, 3: PRINT "                                                                            "
     LOCATE 23, 3: PRINT ""
     LOCATE 24, 3: PRINT "";
80001 ke$ = INKEY$: IF ke$ = "" THEN GOTO 80001
CLS
RETURN
'-------------------------------------------------
'HELP MESSAGES FOR SYNTHETIC PRECIPITATION SCREEN
'                       F2
'-------------------------------------------------
8500  COLOR 15, 1
      CLS
     COLOR 4, 3
     LOCATE 25, 1: PRINT "                           Press Any Key to Return                              ";
     COLOR 15, 1
     LOCATE 2, 3: PRINT "                                                                            "
     LOCATE 3, 3: PRINT "     The program can generate from 1 to 100 years of daily precipitation    "
     LOCATE 4, 3: PRINT "data stochastically for the selected location using a synthetic weather     "
     LOCATE 5, 3: PRINT "generator.  The data will have approximately the same statistics as the     "
     LOCATE 6, 3: PRINT "historic data at the selected location.  If desired, the user can enter     "
     LOCATE 7, 3: PRINT "normal mean monthly precipitation values for the specific location to       "
     LOCATE 8, 3: PRINT "improve the resulting daily values.  The user is advised to enter normal    "
     LOCATE 9, 3: PRINT "mean monthly values if the project site is located a distance from the      "
     LOCATE 10, 3: PRINT "selected city because precipitation is very variable spatially and has a    "
     LOCATE 11, 3: PRINT "significant effect on the water balance of a site.  The daily values        "
     LOCATE 12, 3: PRINT "will vary from month to month and from year to year and will not equal the  "
     LOCATE 13, 3: PRINT "normal values entered.  The same data is produced every time the option is  "
     LOCATE 14, 3: PRINT "used for a given location.                                                  "
     LOCATE 15, 3: PRINT "                                                                            "
     LOCATE 16, 3: PRINT "     The user must enter the number of years of data to be generated.       "
     LOCATE 17, 3: PRINT "Optionally, the user may enter the normal mean monthly precipitation values."
     LOCATE 18, 3: PRINT "These values are available from the National Climatic Center of the National"
     LOCATE 19, 3: PRINT "Oceanic and Atmospheric Administration (NOAA) climatic atlases and other    "
     LOCATE 20, 3: PRINT "reference books.  If specified, the program will scale the computed daily   "
     LOCATE 21, 3: PRINT "values by the ratio of the user-specified normal mean monthly value to the  "
     LOCATE 22, 3: PRINT "default normal mean monthly value.                                          "
     LOCATE 23, 3: PRINT ""
     LOCATE 24, 3: PRINT "";
85001 ke$ = INKEY$: IF ke$ = "" THEN GOTO 85001
CLS
RETURN
END SUB

SUB SYNSOLR (synstate$, syncity$, CLIM$, ok, tape2lin$())
    PCOPY VPAGE%, HPAGE%
    IRTRN% = 0
    PCOPY VPAGE%, APAGE%
    CLS APAGE%
    SCREEN 0, , VPAGE%, VPAGE%
'ADD A F1 HELP MESSAGE
    CLIM3$ = "    SOLAR RADIATION    "
    CLIM$ = CLIM3$
    CLIM = 3
'------------------------------------------------------
'If synthetic data is to be generated for temperature  '
'or solar radiation, check the precip file for the     '
'number of years to generate                           '
'------------------------------------------------------
      '-----------------------------------------------------------
      'determine the maximum number of years for SOLAR RADIATION  '
      'and solar radiation based on the precip number of years    '
      '-----------------------------------------------------------
      OPEN "DATA4.TMP" FOR INPUT AS #2
      FOR I = 1 TO 3
        IF EOF(2) THEN GOTO 901
        LINE INPUT #2, l$
      NEXT I
      FOR I = 1 TO 999
        FOR II = 1 TO 37
           IF EOF(2) THEN GOTO 901
           LINE INPUT #2, l$
        NEXT II
        NNN = I
      NEXT I
901 CLOSE
    MAXIYR = NNN
    nyears$ = STR$(NNN)
   ' IF DIR$("data13.tmp") <> "" THEN
   '   CLOSE
   '   OPEN "data13.tmp" FOR INPUT AS #1
   '   FOR I = 1 TO 4
   '     IF EOF(1) THEN GOTO 901
   '     LINE INPUT #1, l$
   '   NEXT I
   '   'latitude$ = LTRIM$(RTRIM$(l$))
   '   CLOSE
   ' END IF
60002    COLOR 4, 3
    LOCATE 25, 1: PRINT "    F1=Info      F2=Help      F9=Quit      F10=Generate Data      Esc=Cancel    ";
    COLOR 11, 6
    LOCATE 1, 1:  PRINT "                          SYNTHETIC SOLAR RADIATION DATA                        ";
334  COLOR 15, 1
   LOCATE 3, 14:  PRINT "  City:                     State:                         "
   LOCATE 5, 11:  PRINT "  Number of Years for Synthetic Data Generation            "
   LOCATE 7, 11: PRINT "  Station Latitude =          Degrees   (Negative for southern"
   LOCATE 8, 11:  PRINT "                                             hemisphere)"
   LOCATE 7, 32: PRINT latitude$
   LOCATE 5, 60: PRINT nyears$
734  LOCATE 3, 22: PRINT syncity$;
     LOCATE 3, 49: PRINT synstate$;
     YES$ = "YES": NO$ = "NO"
     nyears$ = LTRIM$(RTRIM$(nyears$))
     IF IRET = 2 THEN IRET = 0: GOTO 2825
     IRET = 0
40   ' ADD A F2 HELP MESSAGE
       LOCATE 5, 60: PRINT "    "
       CALL ACELL(5, 60, 3, nyears$, 2)
       IF IRTRN% = 20 THEN GOSUB 6000: FHELP = 1: IRET = 1: GOTO 60002
       IF IRTRN% = 21 THEN GOSUB 6500: FHELP = 1: IRET = 1: GOTO 60002
       IF IRTRN% = 1000 OR IRTRN% = 100 THEN GOSUB 3195
       IF VAL(nyears$) > MAXIYR THEN nyears$ = LTRIM$(RTRIM$(STR$(MAXIYR))): LOCATE 5, 60: PRINT "    ": LOCATE 5, 60: PRINT nyears$
       IF VAL(nyears$) = 0 THEN GOTO 40
       IF IRTRN% = 2 THEN GOSUB 950: GOTO 150
       IF IRTRN% = 5 OR IRTRN% = 4 OR IRTRN% = 3 THEN GOTO 50
       GOTO 40
50  'ADD A F2 HELP MESSAGE
2825  'ADD A F2 HELP MESSAGE FOR LATITUDE
      CALL ACELL(7, 32, 8, latitude$, 2)
      IF IRTRN% = 20 THEN GOSUB 6000: FHELP = 1: IRET = 2: GOTO 60002
      IF IRTRN% = 21 THEN GOSUB 6500: FHELP = 1: IRET = 2: GOTO 60002
      IF IRTRN% = 1000 OR IRTRN% = 100 THEN GOTO 3195
      IF IRTRN% = 2 OR IRTRN% = 5 THEN GOSUB 950: GOTO 150
      IF IRTRN% = 3 OR IRTRN% = 4 THEN GOTO 40
      'IF irtrn% = 5 THEN GOTO 40
      GOTO 2825
150 CLS : PCOPY HPAGE%, VPAGE%: 'IRTRN% = 0
    EXIT SUB
'-----------------------------------------------
'SUBROUTINE TO SAVE SYNTHETIC DATA REQUIREMENTS
'INTO SYNGEN.TMP FILE WHICH WILL BE READ BY THE
'SYNTHETIC WEATHER GENERATOR ALONG WITH DATA11
'-----------------------------------------------
950 OPEN "SYNGEN.TMP" FOR OUTPUT AS #1
     PRINT #1, " 3"
     PRINT #1, USING "##"; CUNIT(CLIM, 2)
     PRINT #1, USING "\                  \"; syncity$; synstate$
     PRINT #1, USING "####"; VAL(nyears$)
     PRINT #1, USING "\    \"; latitude$
     FOR I = 1 TO 10
       PRINT #1, tape2lin$(I)
     NEXT I
     CLOSE
     RETURN
'------------------------------------
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
'------------------------------------
3195 CALL EXOUT(ANSCODE)
     IF ANSCODE = 0 THEN
        GOTO 334
     ELSE
        IRTRN% = 100
        CLS : PCOPY HPAGE%, VPAGE%
        EXIT SUB
     END IF
RETURN
'---------------------------------------------------
'HELP MESSAGES FOR SYNTHETIC SOLAR RADIATION SCREEN
'                       F1
'---------------------------------------------------
6000  COLOR 15, 1
      CLS
     COLOR 4, 3
     LOCATE 25, 1: PRINT "                           Press Any Key to Return                              ";
     COLOR 15, 1
     LOCATE 2, 3: PRINT "                                                                            "
     LOCATE 3, 3: PRINT "3.  Synthetic Solar Radiation Option (Customary or Metric Units).  The      "
     LOCATE 4, 3: PRINT "    program will generate from 1 to 100 years of daily solar radiation data "
     LOCATE 5, 3: PRINT "    stochastically for the selected location.  The synthetic generation of  "
     LOCATE 6, 3: PRINT "    daily solar radiation values is a strong function of precipitation and  "
     LOCATE 7, 3: PRINT "    as such the user must first specify the precipitation.  Generation of   "
     LOCATE 8, 3: PRINT "    solar radiation data is limited to the number of years of precipitation "
     LOCATE 9, 3: PRINT "    data available.  The synthetic solar radiation data will have           "
     LOCATE 10, 3: PRINT "    approximately the same statistical characteristics as the historic data "
     LOCATE 11, 3: PRINT "    at the selected location.  If desired, the user can enter the latitude  "
     LOCATE 12, 3: PRINT "    for the specific location to improve the computation of potential solar "
     LOCATE 13, 3: PRINT "    radiation and the resulting daily values (a negative value should be    "
     LOCATE 14, 3: PRINT "    used for locations in the southern hemisphere).  The user is advised to "
     LOCATE 15, 3: PRINT "    enter the latitude if the project site is more than 50 miles north or   "
     LOCATE 16, 3: PRINT "    south of the city selected from Table 1.  The data required by the      "
     LOCATE 17, 3: PRINT "    synthetic weather generator are:                                        "
     LOCATE 18, 3: PRINT "                                                                            "
     LOCATE 19, 3: PRINT "    *  Number of years of data to be generated                              "
     LOCATE 20, 3: PRINT "                                                                            "
     LOCATE 21, 3: PRINT "    *  Latitude (Optional, default value is available.)                     "
     LOCATE 22, 3: PRINT "                                                                            "
     LOCATE 23, 3: PRINT ""
     LOCATE 24, 3: PRINT ""
    'LOCATE 24, 3: PRINT "";
60001 ke$ = INKEY$: IF ke$ = "" THEN GOTO 60001
CLS
RETURN
'---------------------------------------------------
'HELP MESSAGES FOR SYNTHETIC SOLAR RADIATION SCREEN
'                       F2
'---------------------------------------------------
6500  COLOR 15, 1
      CLS
     COLOR 4, 3
     LOCATE 25, 1: PRINT "                           Press Any Key to Return                              ";
     COLOR 15, 1
     LOCATE 2, 3: PRINT "                                                                             "
     LOCATE 3, 3: PRINT "     The program can generate from 1 to 100 years of daily solar radiation   "
     LOCATE 4, 3: PRINT "data stochastically for the selected location using a synthetic weather      "
     LOCATE 5, 3: PRINT "generator.  However, the number of years of data generation is limited to    "
     LOCATE 6, 3: PRINT "the number of years of available precipitation data because the solar        "
     LOCATE 7, 3: PRINT "radiation generation is a strong function of the occurrence of rainfall.     "
     LOCATE 8, 3: PRINT "As such, the user must first specify the precipitation.  The program         "
     LOCATE 9, 3: PRINT "displays the maximum number of years that can be generated based on the size "
     LOCATE 10, 3: PRINT "of the precipitation data file.  If desired, the user can enter latitude of  "
     LOCATE 11, 3: PRINT "the project site to improve the computation of potential solar radiation and "
     LOCATE 12, 3: PRINT "the resulting daily values.  The user is advised to enter the latitude if    "
     LOCATE 13, 3: PRINT "the project site is more than 50 miles north or south of the selected city.  "
     LOCATE 14, 3: PRINT "A negative value should be used for locations in the southern hemisphere.    "
     LOCATE 15, 3: PRINT "The daily values will vary from month to month and from year to year and     "
     LOCATE 16, 3: PRINT "will not equal the normal values entered.  The same data is produced every   "
     LOCATE 17, 3: PRINT "time the option is used for a given location and precipitation data set.     "
     LOCATE 18, 3: PRINT "                                                                             "
     LOCATE 19, 3: PRINT "     The user must enter the number of years of data to be generated.        "
     LOCATE 20, 3: PRINT "Optionally, the user may enter the latitude which are available from atlases."
     LOCATE 21, 3: PRINT "If specified, the program will compute the daily values as a function of the "
     LOCATE 22, 3: PRINT "potential solar radiation values computed using the user-specified latitude  "
     LOCATE 23, 3: PRINT "instead of the default latitude.  (Remember:  Negative values should be used "
     LOCATE 24, 3: PRINT "for locations in the southern hemisphere.)                                   "
    'LOCATE 24, 3: PRINT "";
65001 ke$ = INKEY$: IF ke$ = "" THEN GOTO 65001
CLS
RETURN
END SUB

SUB SYNTEMP (synstate$, syncity$, CLIM$, ok, tape2lin$())
    DIM SYNTH$(12), unmmt$(12)
    'deftemp = 0  no default normal mean monthly temp has been selected
    '        = 1  default normal mean monthly precip has been selected
    'deftempu = 1 previously selectted default temperature units is CUSTOMARY
    '         = 2     =         =         =      =           =    =  metric
    'unmmt$   = user defined normal mean monthly temp
    'synth$   = user defined values used on the screen
    '           at the end of the synthetic selection, these values are
    '           copied into unmmp$ or unmmt$ or are not used if the user
    '           selects the default values
    '----------------------------------------------------------------------
    FHELP = 0
    PCOPY VPAGE%, HPAGE%
    IRTRN% = 0
    PCOPY VPAGE%, APAGE%
    CLS APAGE%
    SCREEN 0, , VPAGE%, VPAGE%
  '  COLOR 4, 3
  '  LOCATE 25, 1: PRINT "    F1=Info    F2=Help    F3=Key    F9=Quit    F10=Generate Data    Esc=Cancel  ";
  '  COLOR 15, 1
'ADD A F1 HELP MESSAGE
    CLIM2$ = "      TEMPERATURE      "
    CLIM = 2
'------------------------------------------------------
'If synthetic data is to be generated for temperature  '
'or solar radiation, check the precip file for the     '
'number of years to generate                           '
'------------------------------------------------------
      '------------------------------------------------------
      'determine the maximum number of years for temperature   '
      'and solar radiation based on the precip number of years '
      '------------------------------------------------------
      OPEN "DATA4.TMP" FOR INPUT AS #2
      FOR I = 1 TO 3
        IF EOF(2) THEN GOTO 801
        LINE INPUT #2, l$
      NEXT I
      FOR I = 1 TO 999
        FOR II = 1 TO 37
           IF EOF(2) THEN GOTO 801
           LINE INPUT #2, l$
        NEXT II
        NNN = I
      NEXT I
801 CLOSE
    MAXIYR = NNN
    nyears$ = STR$(NNN)
'---------------------------------------------
'Assign proper cliamte default values           '
'to a variable sysnth$()                        '
'Read in the user defined normal mean monthly   '
'values from the appropriate .tmp file          '
'----------------------------------------------
       '---------------------------------------------------------------
       'If default temperature has been previously selected either      '
       'in this subroutine or in the normal mean monthly temperature    '
       'subroutine, the program should not read values from .tmp file   '
       '---------------------------------------------------------------
        IF DIR$("data7.tmp") <> "" THEN
          OPEN "data7.tmp" FOR INPUT AS #1
          GOSUB 23001
        ELSE
          FOR k% = 1 TO 12
            SYNTH$(k%) = ""
          NEXT k%
          GOSUB 26001
        END IF
90002 IF FHELP <> 0 THEN GOSUB 26001
    COLOR 4, 3
    LOCATE 25, 1: PRINT "    F1=Info      F2=Help      F9=Quit      F10=Generate Data      Esc=Cancel    ";
    COLOR 15, 1
   COLOR 11, 6
    LOCATE 1, 1:  PRINT "                            SYNTHETIC TEMPERATURE DATA                          ";
8334  COLOR 15, 1
   LOCATE 3, 14:  PRINT "  City:                     State:                         "
   LOCATE 4, 14:  PRINT "  Number of Years for Synthetic Data Generation            "
   LOCATE 5, 14: PRINT "  Use Default Normal Mean Monthly Temperatures   YES  NO   "
   LOCATE 6, 14: PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
   LOCATE 7, 14: PRINT "บ         Normal Mean Monthly                             บ"
   IF CUNIT(CLIM, 2) = 2 THEN
     LOCATE 7, 44: PRINT "Temperature (๘C)"
   ELSE
     LOCATE 7, 44: PRINT "Temperature (๘F)"
   END IF
   LOCATE 8, 14: PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"
   LOCATE 9, 18: PRINT "                           USER          DEFAULT   "
   LOCATE 10, 18: PRINT "    1.  January   "
   LOCATE 11, 18: PRINT "    2.  February  "
   LOCATE 12, 18: PRINT "    3.  March     "
   LOCATE 13, 18: PRINT "    4.  April     "
   LOCATE 14, 18: PRINT "    5.  May       "
   LOCATE 15, 18: PRINT "    6.  June      "
   LOCATE 16, 18: PRINT "    7.  July      "
   LOCATE 17, 18: PRINT "    8.  August    "
   LOCATE 18, 18: PRINT "    9.  September "
   LOCATE 19, 18: PRINT "   10.  October   "
   LOCATE 20, 18: PRINT "   11.  November  "
   LOCATE 21, 18: PRINT "   12.  December  "
   '-----------------------------------------------
   'CALL SUBROUITES TO DISPLAY VALUES IN PROPER UNITS
   '-----------------------------------------------
   LOCATE 3, 22: PRINT syncity$;
   LOCATE 3, 49: PRINT synstate$;
   LOCATE 4, 63: PRINT nyears$
   decp = 1
   IF UUNI = 2 THEN decp = 1
   FOR k% = 1 TO 12
      POINTPOS = INSTR(SYNTH$(k%), ".")
      SYNTH$(k%) = LEFT$(SYNTH$(k%), POINTPOS + decp)
      LOCATE k% + 9, CO: PRINT USING "\    \"; SYNTH$(k%)
   NEXT k%
   YES$ = "YES": NO$ = "NO"
   nyears$ = LTRIM$(RTRIM$(nyears$))
    IF IRET = 2 THEN IRET = 0: GOTO 850
    IF IRET = 3 THEN IRET = 0: GOTO 860
    IF IRET = 4 THEN IRET = 0: GOTO 3900
    IRET = 0
840  'ADD A F2 HELP MESSAGE
   LOCATE 4, 63: PRINT "    "
       CALL ACELL(4, 63, 3, nyears$, 2)
       IF VAL(nyears$) > MAXIYR THEN nyears$ = LTRIM$(RTRIM$(STR$(MAXIYR))): LOCATE 4, 63: PRINT "    ": LOCATE 4, 63: PRINT nyears$
       IF IRTRN% = 20 THEN GOSUB 9000: FHELP = 1: IRET = 1: GOTO 90002
       IF IRTRN% = 21 THEN GOSUB 9500: FHELP = 1: IRET = 1: GOTO 90002
       IF IRTRN% = 1000 OR IRTRN% = 100 THEN GOSUB 23191
       IF IRTRN% = 2 THEN GOSUB 3950: GOTO 3850
       IF VAL(nyears$) = 0 THEN GOTO 840
       IF IRTRN% = 5 OR IRTRN% = 4 OR IRTRN% = 3 THEN GOTO 850
      GOTO 840
850  'ADD A F2 HELP MESSAGE
     CALL RCELL(5, 63, 3, YES$)
     IF IRTRN% = 20 THEN GOSUB 9000: FHELP = 1: IRET = 2: GOTO 90002
     IF IRTRN% = 21 THEN GOSUB 9500: FHELP = 1: IRET = 2: GOTO 90002
     IF IRTRN% = 1000 OR IRTRN% = 100 THEN GOSUB 23191
     IF IRTRN% = 2 OR IRTRN% = 5 THEN
        IF CUNIT(CLIM, 2) = 1 THEN
          FOR k% = 1 TO 12
             SYNTH$(k%) = STR$(deft(k%))
          NEXT k%
        ELSE
          FOR k% = 1 TO 12
             SYNTH$(k%) = STR$(deft(k%) * .556 - 32 * .556)
          NEXT k%
        END IF
        GOSUB 3950: GOTO 3850
     END IF
     IF IRTRN% = 6 OR IRTRN% = 7 THEN GOTO 860
     IF IRTRN% = 3 OR IRTRN% = 4 THEN GOTO 840
     GOTO 850
860  CALL RCELL(5, 68, 2, NO$)
     IF IRTRN% = 20 THEN GOSUB 9000: FHELP = 1: IRET = 3: GOTO 90002
     IF IRTRN% = 21 THEN GOSUB 9500: FHELP = 1: IRET = 3: GOTO 90002
     IF IRTRN% = 1000 OR IRTRN% = 100 THEN GOTO 23191
     'IF irtrn% = 2 THEN GOSUB 3950: GOTO 3850
     IF IRTRN% = 6 OR IRTRN% = 7 THEN GOTO 850
     IF IRTRN% = 3 OR IRTRN% = 4 THEN GOTO 840
     IF IRTRN% = 5 THEN GOSUB 870
     GOTO 860
870  MO = 10
3900  'ADD A F2 HELP MESSAGE
      I% = MO - 9
      SYNTH$(I%) = LTRIM$(RTRIM$(SYNTH$(I%)))
      IF COSUM = 0 THEN COSUM = CO - 3
      CALL ACELL(MO, CO, 6, SYNTH$(I%), 2)
      IF IRTRN% = 20 THEN GOSUB 9000: FHELP = 1: IRET = 4: GOTO 90002
      IF IRTRN% = 21 THEN GOSUB 9500: FHELP = 1: IRET = 4: GOTO 90002
      IF IRTRN% = 1000 THEN 23191
     IF IRTRN% = 2 THEN defrain = 1: GOSUB 3950: GOTO 3850
     IF IRTRN% = 100 THEN
       GOTO 860
    END IF
'MOVE DOWN
     IF IRTRN% = 4 OR IRTRN% = 5 THEN
        IF MO < 21 THEN
          MO = MO + 1
          GOTO 3900
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
      GOTO 3900
  END IF
'MOVE HOME
      IF IRTRN% = 8 THEN
        MO = 10
        GOTO 3900
      END IF
'MOVE END
      IF IRTRN% = 9 THEN
        MO = 21
      END IF
      GOTO 3900
3850   CLS : PCOPY HPAGE%, VPAGE%: 'IRTRN% = 0
    EXIT SUB
'---------------------------------------------
'SUBROUTINE TO SAVE SYNTHETIC DATA REQUIREMENTS
'INTO SYNGEN.TMP FILE WHICH WILL BE READ BY THE
'SYNTHETIC WEATHER GENERATOR ALONG WITH DATA11
'---------------------------------------------
3950 OPEN "SYNGEN.TMP" FOR OUTPUT AS #1
     PRINT #1, " 2"
     PRINT #1, USING "##"; CUNIT(CLIM, 2)
     PRINT #1, USING "\                  \"; syncity$; synstate$
     PRINT #1, USING "####"; VAL(nyears$)
     deftemp = 1
          FOR I = 1 TO 12
             IF INSTR(SYNTH$(I), ".") THEN
                PRINT #1, USING "####.#"; VAL(SYNTH$(I));
             ELSE
                SYNTH$(I) = LTRIM$(RTRIM$(SYNTH$(I))) + "."
                PRINT #1, USING "####.#"; VAL(SYNTH$(I));
             END IF
           NEXT I
           PRINT #1,
           '--------------------------------
           'SAVE VALUES AS USER DEFINED IN
           'CUSTOMARY UNITS
           '--------------------------------
           IF CUNIT(CLIM, 2) = 1 THEN
               FOR k% = 1 TO 12
                  unmmt$(k%) = SYNTH$(k%)
               NEXT k%
           ELSE
              FOR k% = 1 TO 12
                 unmmt$(k%) = STR$(VAL(SYNTH$(k%)) * 1.8 + 32)
              NEXT k%
          END IF
     FOR I = 1 TO 10
       PRINT #1, tape2lin$(I)
     NEXT I
     CLOSE
     RETURN
'----------------------------------------
'subroutine to read normal mean monthly
'values from .tmp file if it exists.
'----------------------------------------
23001 LINE INPUT #1, l$
      LINE INPUT #1, l$
      UUNI = VAL(LTRIM$(RTRIM$(l$)))
      LINE INPUT #1, l$
      LINE INPUT #1, l$
      SYNTH$(1) = LTRIM$(RTRIM$(LEFT$(l$, 6)))
      SYNTH$(2) = LTRIM$(RTRIM$(MID$(l$, 7, 6)))
      SYNTH$(3) = LTRIM$(RTRIM$(MID$(l$, 13, 6)))
      SYNTH$(4) = LTRIM$(RTRIM$(MID$(l$, 19, 6)))
      SYNTH$(5) = LTRIM$(RTRIM$(MID$(l$, 25, 6)))
      SYNTH$(6) = LTRIM$(RTRIM$(MID$(l$, 31, 6)))
      SYNTH$(7) = LTRIM$(RTRIM$(MID$(l$, 37, 6)))
      SYNTH$(8) = LTRIM$(RTRIM$(MID$(l$, 43, 6)))
      SYNTH$(9) = LTRIM$(RTRIM$(MID$(l$, 49, 6)))
      SYNTH$(10) = LTRIM$(RTRIM$(MID$(l$, 55, 6)))
      SYNTH$(11) = LTRIM$(RTRIM$(MID$(l$, 61, 6)))
      SYNTH$(12) = LTRIM$(RTRIM$(MID$(l$, 67, 6)))
      FOR k% = 1 TO 12
         SYNTH$(k%) = LTRIM$(RTRIM$(SYNTH$(k%)))
      NEXT k%
      deftempu = UUNI
'-------------------------------
'MAKE VALUES COMPATIBLE WITH
'SELECTED UNITS
'-------------------------------
      IF UUNI = CUNIT(CLIM, 2) THEN
         CO = 45
         GOSUB 26001
         CLOSE : RETURN
      END IF
'-----------------------------------------------
'take care of temperature data
'------------------------------------------------
     '---------------------------
     'Convert to metric
     '---------------------------
     mult = .556: subt = .556 * 32
     IF UUNI = 1 AND CUNIT(CLIM, 2) = 2 THEN
        CO = 45
        FOR k% = 1 TO 12
          SYNTH$(k%) = LTRIM$(RTRIM$(STR$(VAL(SYNTH$(k%)) * mult - subt)))
        NEXT k%
     END IF
     IF UUNI = 2 AND CUNIT(CLIM, 2) = 1 THEN
     '---------------------
     'convert to CUSTOMARY
     '----------------------
        CO = 45
        FOR k% = 1 TO 12
          SYNTH$(k%) = LTRIM$(RTRIM$(STR$(VAL(SYNTH$(k%)) / mult + 32)))
        NEXT k%
     END IF
     GOSUB 26001
CLOSE
RETURN
'----------------------------------------
'Subroutine to convert temp. values into
'proper units
'-------------------------------------
25000  IF CUNIT(CLIM, 2) = 2 THEN   'need to display in Metric units
          mult = .556: subt = .556 * 32
          CO = 45
          FOR k% = 1 TO 12
            SYNTH$(k%) = STR$(VAL(unmmt$(k%)) * mult - subt)
            POINTPOS = INSTR(SYNTH$(k%), ".")
            SYNTH$(k%) = LEFT$(SYNTH$(k%), POINTPOS + 1)
            LOCATE k% + 9, CO: PRINT USING "\    \"; LTRIM$(RTRIM$(SYNTH$(k%)))
          NEXT k%
       ELSE
          mult = 1: subt = 0
          CO = 45
          FOR k% = 1 TO 12
             SYNTH$(k%) = STR$(VAL(unmmt$(k%)) * mult - subt)
             POINTPOS = INSTR(SYNTH$(k%), ".")
             SYNTH$(k%) = LEFT$(SYNTH$(k%), POINTPOS + 1)
             LOCATE k% + 9, CO: PRINT USING "\    \"; LTRIM$(RTRIM$(SYNTH$(k%)))
          NEXT k%
       END IF
       '--------------------
       'convert DEFAULT values
       '--------------------
     GOSUB 26001
RETURN
'-------------------------------------
'convert default values to proper units
'and display
'------------------------------------
26001  IF CUNIT(CLIM, 2) = 1 THEN
          mult = 1: subt = 0
       ELSE
          mult = .556: subt = .556 * 32
       END IF
       FOR k% = 1 TO 12
          LOCATE k% + 9, 59: PRINT USING "###.#"; deft(k%) * mult - subt
       NEXT k%
       CO = 45
RETURN
'--------------------------------------
'Subroutine to set the proper values to
'save in syngen.tmp if default values
'are selected.
'--------------------------------------
27001  FOR k% = 1 TO 12
          IF CUNIT(CLIM, 2) = 1 THEN
             SYNTH$(k%) = STR$(deft(k%))
          ELSE
             SYNTH$(k%) = STR$(deft(k%) * .556 - .556 * 32)
          END IF
       NEXT k%
RETURN
'------------------------------------
'CALL SUBROUTINE TO ALLOW USER TO
'EXIT PROGRAM
'------------------------------------
23191 CALL EXOUT(ANSCODE)
     IF ANSCODE = 0 THEN
        FOR k% = 1 TO 6
          LOCATE k% + 9, 45: PRINT USING "\    \"; SYNTH$(k%)
        NEXT k%
        GOTO 8334
     ELSE
        IRTRN% = 100
        CLS : PCOPY HPAGE%, VPAGE%
        EXIT SUB
     END IF
RETURN
'-------------------------------------------------
'HELP MESSAGES FOR SYNTHETIC TEMPERATURE SCREEN
'                       F1
'-------------------------------------------------
9000  COLOR 15, 1
      CLS
     COLOR 4, 3
     LOCATE 25, 1: PRINT "                           Press Any Key to Return                              ";
     COLOR 15, 1
     LOCATE 2, 3: PRINT "                                                                            "
     LOCATE 3, 3: PRINT "2.  Synthetic Temperature Option (Customary or Metric Units).  The program  "
     LOCATE 4, 3: PRINT "    will generate from 1 to 100 years of temperature data stochastically for"
     LOCATE 5, 3: PRINT "    the selected location.  The synthetic generation of daily temperature   "
     LOCATE 6, 3: PRINT "    values is a weak function of precipitation and as such the user must    "
     LOCATE 7, 3: PRINT "    first specify the precipitation.  Generation of temperature data is     "
     LOCATE 8, 3: PRINT "    limited to the number of years of precipitation data available.  The    "
     LOCATE 9, 3: PRINT "    synthetic temperature data will have approximately the same statistical "
     LOCATE 10, 3: PRINT "    characteristics as the historic data at the selected location.  If      "
     LOCATE 11, 3: PRINT "    desired, the user can enter normal mean monthly temperature values for  "
     LOCATE 12, 3: PRINT "    the specific location to improve the statistical characteristics of the "
     LOCATE 13, 3: PRINT "    resulting daily values.  The user is advised to enter normal mean       "
     LOCATE 14, 3: PRINT "    monthly temperature values if the project site is located some distance "
     LOCATE 15, 3: PRINT "    from the city selected from Table 1.  The data required by the synthetic"
     LOCATE 16, 3: PRINT "    weather generator are:                                                  "
     LOCATE 17, 3: PRINT "                                                                            "
     LOCATE 18, 3: PRINT "    *  Number of years of data to be generated                              "
     LOCATE 19, 3: PRINT "                                                                            "
     LOCATE 20, 3: PRINT "    *  Normal mean monthly temperature (Optional, default values are        "
     LOCATE 21, 3: PRINT "       available.)                                                          "
     LOCATE 22, 3: PRINT "                                                                            "
     LOCATE 23, 3: PRINT ""
     LOCATE 24, 3: PRINT "";
90001 ke$ = INKEY$: IF ke$ = "" THEN GOTO 90001
CLS
RETURN
'-------------------------------------------------
'HELP MESSAGES FOR SYNTHETIC TEMPERATURE SCREEN
'                       F2
'-------------------------------------------------
9500  COLOR 15, 1
      CLS
     COLOR 4, 3
     LOCATE 25, 1: PRINT "                           Press Any Key to Return                              ";
     COLOR 15, 1
     LOCATE 2, 3: PRINT "                                                                            "
     LOCATE 3, 3: PRINT "     The program can generate from 1 to 100 years of mean daily temperature "
     LOCATE 4, 3: PRINT "data stochastically for the selected location using a synthetic weather     "
     LOCATE 5, 3: PRINT "generator.  However, the number of years of data generation is limited to   "
     LOCATE 6, 3: PRINT "the number of years of available precipitation data because the temperature "
     LOCATE 7, 3: PRINT "generation is a weak function of the occurrence of precipitation.  As such, "
     LOCATE 8, 3: PRINT "the user must first specify the precipitation.  The program displays the    "
     LOCATE 9, 3: PRINT "maximum number of years that can be generated based on the size of the      "
     LOCATE 10, 3: PRINT "precipitation data file.  If desired, the user can enter normal mean monthly"
     LOCATE 11, 3: PRINT "temperature values for the specific location to improve the resulting daily "
     LOCATE 12, 3: PRINT "values.  The user is advised to enter normal mean monthly temperature values"
     LOCATE 13, 3: PRINT "if the project site is located at a significantly different elevation or    "
     LOCATE 14, 3: PRINT "some distance from the selected city.                                       "
     LOCATE 15, 3: PRINT "                                                                            "
     LOCATE 16, 3: PRINT "     The user must enter the number of years of data to be generated.       "
     LOCATE 17, 3: PRINT "Optionally, the user may enter the normal mean monthly temperature values.  "
     LOCATE 18, 3: PRINT "These values are available from the National Climatic Center of the National"
     LOCATE 19, 3: PRINT "Oceanic and Atmospheric Administration (NOAA) climatic atlases and other    "
     LOCATE 20, 3: PRINT "reference books.  If specified, the program will scale the computed daily   "
     LOCATE 21, 3: PRINT "values by the ratio of the user-specified normal mean monthly temperature   "
     LOCATE 22, 3: PRINT "value to the default normal mean monthly value.                             "
     LOCATE 23, 3: PRINT "                                                                            "
     LOCATE 24, 3: PRINT "";
95001 ke$ = INKEY$: IF ke$ = "" THEN GOTO 95001
CLS
RETURN

END SUB

SUB YEARSHEET (CLIM$, CLIMETH$, UNI$, COL)
DIM START(4), COLHEAD$(5), TOT(101), elem$(101, 4)
'-----------------------------------------------------------------------
'INI% = FIRST NUMBER SHOWN ON THE LEFT OF THE SCREEN
'       VARIES FROM 1-100
'K%  = 1 FOR PRECIPITATION
'    = 2     NOT USED
'    = 3     TEMPERATURE
'    = 4     SOLAR RADIATION
'L%  =  CURRENT YEAR NUMBER (HIGHLIGHTED)
'NL% = THE LAST YEAR NUMBER THAT APPEARS ON THE CURRENT SCREEN ON ROW 23
'RO  = ROW NUMBER ON THE SCREEN (3-23)
'------------------------------------------------------------------------
   CLIM1$ = "     PRECIPITATION     "
   CLIM2$ = "      TEMPERATURE      "
   CLIM3$ = "    SOLAR RADIATION    "
   F1HELP$ = "#YEAREDIT"
   F2HELP$ = "#YEAREDIT"
   CLIMETH3$ = "Create/Edit": CLIMETH4$ = "Edit"
'----------------------------------------------
'DETERMINE THE COLUMN ON SPREADSHEET AND
'THE TEMPORARY FILE TO BE EDITED...HELPPREC,
'HELPTEMP, OR HELPSOLR THESE ARE THE RANDOM
'ACCESS FILES THAT ARE USED TO STORE DATA
'TEMPORARILY
'----------------------------------------------
   ON LOCAL ERROR RESUME NEXT
        IF UNI$ = "1" THEN UPR = 1
        IF UNI$ = "2" THEN UPR = 2
        IF DIR$("data4.tmp") <> "" THEN
           CLOSE
           OPEN "data4.tmp" FOR INPUT AS #1
           LINE INPUT #1, l$
           LINE INPUT #1, l$
           U = VAL(l$)
           IF U <> 2 THEN
              UPR = 1
              precu$ = "(IN)"
           ELSE
              UPR = 2
              precu$ = "(MM)"
           END IF
           CLOSE
        END IF
        IF DIR$("data7.tmp") <> "" THEN
           CLOSE
           OPEN "data7.tmp" FOR INPUT AS #1
           LINE INPUT #1, l$
           LINE INPUT #1, l$
           U = VAL(l$)
           IF U <> 2 THEN
              tempu$ = "(๘F)"
           ELSE
              tempu$ = "(๘C)"
           END IF
           CLOSE
        END IF
        IF DIR$("data13.tmp") <> "" THEN
           CLOSE
           OPEN "data13.tmp" FOR INPUT AS #1
           LINE INPUT #1, l$
           LINE INPUT #1, l$
           U = VAL(l$)
           IF U <> 2 THEN
              solu$ = "(Langleys)"
           ELSE
              solu$ = "  (MJ/Mý) "
           END IF
           CLOSE
        END IF
      IF CLIM$ = CLIM1$ THEN
        J% = 1
      ELSEIF CLIM$ = CLIM2$ THEN
        J% = 3
      ELSEIF CLIM$ = CLIM3$ THEN
        J% = 4
      END IF
      SCREEN 0
   COLHD1$ = "       PRECIPITATION               TEMPERATURE          SOLAR RADIATION      "
   COLHEAD$(1) = "YEAR": COLHEAD$(2) = "YEARLY TOTAL": COLHEAD$(3) = "YEAR": COLHEAD$(4) = "YEAR"
   CLS
'--------------------------------------------------
'IF CLIMATE METHOD IS EDIT GO TO SUBROUTINE
'AND READ DATA FROM D4EDIT$, D7EDIT, AND D13EDIT
'FILES.
'IF CLIMATE METHOD IS CREATE THEN START ENTERING
'DATA INTO YEAR SPREADSHEET.
'--------------------------------------------------
   GOSUB 62111
   CLOSE
   nl% = 20: ini% = 1
'DEFINE CELL WIDTH
   START(1) = 7: START(2) = 16: START(3) = 42: START(4) = 65
   CLS
   COLOR 4, 3
   LOCATE 25, 1: PRINT "F1=Info  F2=Help  F3=Key  F9=Quit  F10=End  Esc=Cancel  PgUp/PgDn  Enter=Proceed";
   COLOR 15, 1
   COLOR 11, 6
   LOCATE 24, 1: PRINT " Alt A=Add Above     Alt B=Add Below    Alt C=Copy    Alt D=Delete   Alt M=Move ";
   COLOR 15, 1
   COLOR 0, 3
'--------------------------------------
'print spreadsheet headings and lables
'--------------------------------------
       LOCATE 1, 1: PRINT "   "
       LOCATE 3, 1: FOR I = 1 TO 80: PRINT " "; : NEXT I
       LOCATE 2, 1: FOR I% = 1 TO 80: PRINT " "; : NEXT I%
       LOCATE 1, 4: PRINT COLHD1$
       FOR I% = 2 TO nl% + 3: LOCATE I%, 80: PRINT " ": NEXT I%
       LOCATE 3, 1: FOR I% = 1 TO 80: PRINT " "; : NEXT I%
       FOR I% = 1 TO 4
         LOCATE 3, START(I%): PRINT COLHEAD$(I%)
       NEXT I%
       LOCATE 2, 15: PRINT precu$
       LOCATE 2, 42: PRINT tempu$
       LOCATE 2, 62: PRINT solu$
62310  l% = 1: ini% = 1: nl% = 20: ro = 4
       R% = 3
       FOR I% = 1 TO 20
         a$ = RIGHT$(STR$(I%), 3)
         LOCATE R% + 1, 1: PRINT USING "\  \"; a$
         R% = R% + 1
       NEXT I%
       GOSUB 62345
       GOSUB 62343
       ''''''''''''''''''''''''''''''
       'higlight the fist year
       'and begin creating/editing
       ''''''''''''''''''''''''''''''
61020  COLOR 15, 1: LOCATE 3, START(J%): PRINT COLHEAD$(J%)
       LOCATE ro, 1
       IF IPATH = 2 THEN l% = l% - 1
       IF IPATH = 4 THEN l% = l% + 1
       IF IPATH = 7 THEN l% = l% + 1
       IF l% > 100 THEN l% = 100
       a$ = RIGHT$(STR$(l%), 3): PRINT USING "\  \"; a$
       WIDE = LEN(COLHEAD$(J%))
       OLDELEM$ = LTRIM$(RTRIM$(elem$(l%, J%)))
       CALL ACELL(ro, START(J%), WIDE, elem$(l%, J%), 2)
           COLOR 11, 6
           LOCATE 24, 1: PRINT " Alt A=Add Above     Alt B=Add Below    Alt C=Copy    Alt D=Delete   Alt M=Move ";
           COLOR 15, 1
       IPATH = 0
       GOSUB 65190
       COLOR 0, 3
       LOCATE 3, START(J%): PRINT COLHEAD$(J%)
       LOCATE ro, 1: a$ = RIGHT$(STR$(l%), 3): PRINT USING "\  \"; a$
'EXIT INPUT SESSION
       IF IRTRN% = 1000 OR IRTRN% = 100 THEN IRTRN% = 100: GOSUB 66020
       IF IRTRN% = 2 THEN GOSUB 62444: EXIT SUB
'EDIT/CREATE DAILY DATA <CR>
    IF IRTRN% = 5 THEN
       GOSUB 67550
       '''''''''''''''''''''''''''''''''''''''''''
       'KEEP VALUES OF VARIABLES IN ANOTHER NAME '
       'THE CALL DAILY DATA EDIT ROUTINE         '
       '''''''''''''''''''''''''''''''''''''''''''
       GOSUB 67040
       jj% = J%: NNL% = nl%: INII% = ini%: RRO = ro: LL% = l%
       IF VAL(elem$(l%, J%)) = 0 THEN GOTO 61020
       NEWYR$ = elem$(l%, J%)
       CLOSE
       CALL TYPEIN(CLIM$, l%, TOT(), NEWYR$)
       'IF IRTRN% = 1000 THEN EXIT SUB
       FOR iv% = 1 TO 100
         IF VAL(elem$(iv%, J%)) = VAL(NEWYR$) THEN
            TOT(iv%) = TOT(l%)
            IF iv% >= ini% AND iv% <= nl% THEN
               ROWNUM = iv% - ini% + 4
               COLOR 15, 1: LOCATE ROWNUM, 17: PRINT "          "
               IF UPR = 1 THEN LOCATE ROWNUM, 17: PRINT USING "####.##"; TOT(iv%)
               IF UPR = 2 THEN LOCATE ROWNUM, 17: PRINT USING "#####.#"; TOT(iv%)
            END IF
         END IF
       NEXT iv%
       COLOR 1, 15
       J% = jj%: nl% = NNL%: ini% = INII%: ro = RRO: l% = LL%
       IF J% = 1 THEN
          COLOR 15, 1
          IF UPR = 1 THEN LOCATE ro, 17: PRINT USING "####.##"; TOT(l%)
          IF UPR = 2 THEN LOCATE ro, 17: PRINT USING "#####.#"; TOT(l%)
          COLOR 1, 15
       END IF
    END IF
'-------------------
'MOVE HOME LOCATION
'-------------------
    IF IRTRN% = 8 THEN GOSUB 61021
'------------
'MOVE TO END
'------------
    IF IRTRN% = 9 THEN GOSUB 61022
'--------
'PAGE UP
'--------
61023 IF IRTRN% = -1 THEN
        GOSUB 67550
        GOSUB 67040
        GOSUB 62925
        GOSUB 62345
     END IF
'
'PAGE DOWN
'
61024 IF IRTRN% = 1 THEN
          GOSUB 67550
          GOSUB 67040
          COLOR 0, 3
          GOSUB 62950
          GOSUB 62345
      END IF
'-----------
'CURSOR DOWN
'-----------
     IF IRTRN% = 4 THEN : GOSUB 67550:    GOSUB 67040: GOSUB 62500: GOTO 61020
'-----------
'CURSOR UP
'-----------
      IF IRTRN% = 3 THEN GOSUB 67550: GOSUB 67040: GOSUB 62000: GOTO 61020
'---------------------------------
'INSERT A YEAR ABOVE CURRENT YEAR
'---------------------------------
     IF IRTRN% = 30 THEN GOSUB 62800
'---------------------------------
'INSERT A YEAR BELOW CURRENT YEAR
'----------------------------------
     IF IRTRN% = 31 THEN GOSUB 62900
'----------------------------------
'COPY A YEAR INTO ANOTHER LOCATION
'----------------------------------
      IF IRTRN% = 32 THEN
        COLOR 11, 6
        LOCATE 24, 1: PRINT "   Cursor then Alt A to Copy Above or Alt B to Copy below; then Replace 9999.  ";
        COLOR 15, 1
        IPATH = 1
        COLOR 0, 3
        cl% = l%
        COPYELEM$ = elem$(l%, J%)
        COPYTOT = TOT(l%)
        IF elem$(l%, J%) = "" THEN
           LOCATE 24, 1: PRINT "       An empty year of data cannot be copied.   Press Enter to continue.       ";
           IRTRN% = 0
           GOTO 61020
        END IF
79990   IF elem$(l%, J%) = "" THEN elem$(l%, J%) = "    "
        WIDE = 4
        CALL RCELL(ro, START(J%), WIDE, elem$(l%, J%))
        GOSUB 65190      'CHECK IF A HELP KEY IS PRESSED
        IF IRTRN% = 100 THEN
           COLOR 11, 6
           LOCATE 24, 1: PRINT " Alt A=Add Above     Alt B=Add Below    Alt C=Copy    Alt D=Delete   Alt M=Move ";
           COLOR 15, 1
           GOTO 61020
        END IF
        COLOR 0, 3
        IF IRTRN% = -1 THEN
           GOSUB 67550
           GOSUB 67040
           GOSUB 62925
           GOSUB 62345
           GOTO 79990
        END IF
        IF IRTRN% = 1 THEN
           GOSUB 67550
           GOSUB 67040
           COLOR 0, 3
           GOSUB 62950
           GOSUB 62345
           GOTO 79990
        END IF
        IF IRTRN% = 3 THEN
           GOSUB 62000
           GOTO 79990
        END IF
        IF IRTRN% = 4 THEN
           GOSUB 62500
           GOTO 79990
        END IF
        IF IRTRN% = 8 THEN
           GOSUB 67550
           GOSUB 67040
           COLOR 0, 3
           GOSUB 61021
           GOSUB 62345
           GOTO 79990
        END IF
        IF IRTRN% = 9 THEN
           GOSUB 67550
           GOSUB 67040
           COLOR 0, 3
           GOSUB 61022
           GOSUB 62345
           GOTO 79990
        END IF
        '---------------------------------------
        'COPY TO A NEW LAYER ABOVE CURRENT LAYER
        '---------------------------------------
        IF IRTRN% = 30 OR IRTRN% = 31 THEN
           ALTABRO = ro
           ALTABINI% = ini%
           ALTABL% = l%
           IIRTRN% = IRTRN%
           nyr$ = "9999"
           COLOR 15, 1
           LOCATE 11, 27: PRINT "New Label";
           LOCATE 12, 29: PRINT USING "\  \"; nyr$;
88880      CALL ACELL(12, 29, 4, nyr$, 2)
           IF IRTRN% = 5 THEN
              IPATH = 0
              IF VAL(nyr$) = 0 THEN GOTO 88880
             IPATH = 1
              FOR iv% = 1 TO 100
                 IF VAL(elem$(iv%, J%)) = VAL(nyr$) THEN
                    nyr$ = "    "
                    GOTO 88880
                 END IF
              NEXT iv%
              IF VAL(nyr$) = 0 THEN GOTO 88880
              LOCATE 11, 27: PRINT "         ";
              LOCATE 12, 29: PRINT "         ";
             IPATH = 1
              IF IIRTRN% = 30 THEN
                 GOSUB 62800
                 IPATH = 6
              END IF
              IF IIRTRN% = 31 THEN
                 GOSUB 62900
                 IPATH = 7
              END IF
              GOTO 61020
           END IF
           IF IRTRN% = 100 THEN
              COLOR 11, 6
              LOCATE 24, 1: PRINT " Alt A=Add Above     Alt B=Add Below    Alt C=Copy    Alt D=Delete   Alt M=Move ";
              COLOR 15, 1
              LOCATE 11, 27: PRINT "         ";
              LOCATE 12, 29: PRINT "         ";
              IPATH = 0
              GOTO 61020
           END IF
           GOTO 88880
        END IF
        GOTO 79990
     END IF
'-------------------------------------------------------
'MOVE A YEAR TO ANOTHER LOCATION (DEL + INS A OR INS B)
'-------------------------------------------------------
      IF IRTRN% = 33 THEN
        COLOR 11, 6
        LOCATE 24, 1: PRINT "            Cursor then Alt A=Move Above, Alt B=Move below                      ";
        COLOR 15, 1
        'IPATH = 1
        COLOR 0, 3
        cl% = l%
        COPYELEM$ = elem$(l%, J%)
        COPYTOT = TOT(l%)
79995   IF elem$(l%, J%) = "" THEN elem$(l%, J%) = "    "
        WIDE = 4
        CALL RCELL(ro, START(J%), WIDE, elem$(l%, J%))
        GOSUB 65190      'CHECK IF A HELP KEY IS PRESSED
        IF IRTRN% = 100 THEN
           COLOR 11, 6
           LOCATE 24, 1: PRINT " Alt A=Add Above     Alt B=Add Below    Alt C=Copy    Alt D=Delete   Alt M=Move ";
           COLOR 15, 1
           GOTO 61020
        END IF
        IF IRTRN% = 1000 THEN GOSUB 66020
        COLOR 0, 3
        IF IRTRN% = -1 THEN
           GOSUB 67550
           GOSUB 67040
           GOSUB 62925
           GOSUB 62345
           GOTO 79995
        END IF
        IF IRTRN% = 1 THEN
           GOSUB 67550
           GOSUB 67040
           COLOR 0, 3
           GOSUB 62950
           GOSUB 62345
           GOTO 79995
        END IF
        IF IRTRN% = 3 THEN
           GOSUB 62000
           GOTO 79995
        END IF
        IF IRTRN% = 4 THEN
           GOSUB 62500
           GOTO 79995
        END IF
        IF IRTRN% = 8 THEN
           GOSUB 67550
           GOSUB 67040
           COLOR 0, 3
           GOSUB 61021
           GOSUB 62345
           GOTO 79995
        END IF
        IF IRTRN% = 9 THEN
           GOSUB 67550
           GOSUB 67040
           COLOR 0, 3
           GOSUB 61022
           GOSUB 62345
           GOTO 79995
        END IF
        '---------------------------------------
        'MOVE TO A NEW LAYER ABOVE CURRENT LAYER
        '---------------------------------------
        IF IRTRN% = 30 OR IRTRN% = 31 THEN
           ALTABRO = ro
           ALTABL% = l%
           ALTABINI% = ini%
           IIRTRN% = IRTRN%
           COLOR 15, 1
           ML% = l%
           l% = cl%
           GOSUB 62750
           l% = ML%
           IF IIRTRN% = 30 THEN
              IF cl% < ML% THEN
                 l% = l% - 1
                 IPATH = 2
              ELSE
                 IPATH = 3
              END IF
              GOSUB 62800
              l% = ML%
           END IF
           IF IIRTRN% = 31 THEN
              IF cl% > ML% THEN
                 l% = l% + 1
                 IPATH = 4
              ELSE
                 IPATH = 5
              END IF
              GOSUB 62900
              l% = ML%
           END IF
          ' ipath = 0
           GOTO 61020
        END IF
        IF IRTRN% = 100 THEN GOTO 61020     ':ipath 0
        GOTO 79995
        END IF
'------------
'DELETE YEAR
'------------
     IF IRTRN% = 13 THEN GOSUB 62750
     GOTO 61020
61120  EXIT SUB
'---------------------
'CURSOR UP SUBROUTINE
'---------------------
62000  IF ro <> 4 THEN
         l% = l% - 1: ro = ro - 1: RETURN
       ELSE
          IF ini% <> 1 THEN
            ini% = ini% - 1: nl% = nl% - 1: ro = 4: l% = l% - 1: GOSUB 62345
          ELSE
            RETURN
          END IF
       END IF
    RETURN
'''''''''''''''''''''''
'CURSOR DOWN SUBROUTINE
'''''''''''''''''''''''
62500  IF l% < nl% THEN
          l% = l% + 1: ro = ro + 1:  RETURN
       ELSE
          IF nl% < 100 THEN
            nl% = nl% + 1: ini% = ini% + 1: l% = l% + 1: GOSUB 62345: RETURN
          ELSE
            RETURN
          END IF
       END IF
RETURN
'-------------------------
'DELETE YEAR SUBROUTINE
'-------------------------
62750    k% = l%
         FOR I% = l% TO 99
            elem$(I%, J%) = elem$(I% + 1, J%)
            IF J% = 1 THEN TOT(I%) = TOT(I% + 1)
            k% = k% + 1
         NEXT I%
         elem$(100, J%) = ""
         IF J% = 1 THEN TOT(100) = 0
         GOSUB 65090
       RETURN
'''''''''''''''''''''''''''''''''''''''''''''
'INSERT A YEAR ABOVE CURRENT YEAR SUBROUTINE
'''''''''''''''''''''''''''''''''''''''''''''
62800   COLOR 11, 6
        LOCATE 24, 1: PRINT " Alt A=Add Above     Alt B=Add Below    Alt C=Copy    Alt D=Delete   Alt M=Move ";
        COLOR 15, 1
        IF NYRS = 100 THEN GOTO 61020
        NK% = 100
        FOR I% = l% + 1 TO 100
           elem$(NK%, J%) = elem$(NK% - 1, J%)
           IF J% = 1 THEN TOT(NK%) = TOT(NK% - 1)
           NK% = NK% - 1
        NEXT I%
        IF IPATH = 0 THEN elem$(l%, J%) = "": TOT(l%) = 0
        IF IPATH = 1 THEN
          elem$(l%, J%) = nyr$
          CLOSE
          OPEN COPYELEM$ + ".TMP" FOR INPUT AS #1
          OPEN elem$(l%, J%) + ".TMP" FOR OUTPUT AS #2
          DO UNTIL EOF(1)
            LINE INPUT #1, l$
            IF J% = 1 THEN
              l$ = RIGHT$(l$, LEN(l$) - 10)
              PRINT #2, USING "##########"; VAL(elem$(l%, J%)); : PRINT #2, l$
            ELSE
              l$ = RIGHT$(l$, LEN(l$) - 5)
              PRINT #2, USING "#####"; VAL(elem$(l%, J%)); : PRINT #2, l$
            END IF
            LOOP
          IF J% = 1 THEN TOT(l%) = COPYTOT
        END IF
        IF IPATH > 1 AND IPATH < 6 THEN
           elem$(l%, J%) = COPYELEM$
           IF J% = 1 THEN TOT(l%) = COPYTOT
        END IF
       ' ipath = 0
       IF IPATH = 1 THEN IPATH = 6
        GOSUB 62345
        CLOSE
RETURN
'''''''''''''''''''''''''''''''''''''''''''''
'INSERT A YEAR BELOW CURRENT YEAR SUBROUTINE
'''''''''''''''''''''''''''''''''''''''''''''
62900   COLOR 11, 6
        LOCATE 24, 1: PRINT " Alt A=Add Above     Alt B=Add Below    Alt C=Copy    Alt D=Delete   Alt M=Move ";
        COLOR 15, 1
        IF l% = 100 THEN IPATH = 0: RETURN
        k% = 100
        FOR I% = l% + 1 TO 100
           elem$(k%, J%) = elem$(k% - 1, J%)
           IF J% = 1 THEN TOT(k%) = TOT(k% - 1)
           k% = k% - 1
        NEXT I%
        IF IPATH = 0 THEN elem$(l% + 1, J%) = "": TOT(l% + 1) = 0
        IF IPATH = 1 THEN
           elem$(l% + 1, J%) = nyr$
           CLOSE
           OPEN COPYELEM$ + ".TMP" FOR INPUT AS #1
           OPEN elem$(l% + 1, J%) + ".TMP" FOR OUTPUT AS #2
           DO UNTIL EOF(1)
             LINE INPUT #1, l$
             IF J% = 1 THEN
               l$ = RIGHT$(l$, LEN(l$) - 10)
               'PRINT USING "##########"; VAL(ELEM$(L% + 1, J%)); : PRINT L$
               PRINT #2, USING "##########"; VAL(elem$(l% + 1, J%)); : PRINT #2, l$
            ELSE
               l$ = RIGHT$(l$, LEN(l$) - 5)
               PRINT #2, USING "#####"; VAL(elem$(l% + 1, J%)); : PRINT #2, l$
               'PRINT USING "#####"; VAL(ELEM$(L% + 1, J%)); : PRINT L$
            END IF
            LOOP
           IF J% = 1 THEN TOT(l% + 1) = COPYTOT
        END IF
        IF IPATH > 1 AND IPATH < 6 THEN
           elem$(l%, J%) = COPYELEM$
           IF J% = 1 THEN TOT(l%) = COPYTOT
        END IF
       ' ipath = 0
       IF IPATH = 1 THEN IPATH = 7
        GOSUB 62345
        CLOSE
RETURN
''''''''''''''''''''
'PAGE UP SUBROTINE
'''''''''''''''''''''   
62925  IF nl% > 40 THEN
         ini% = ini% - 20: l% = ini%: nl% = nl% - 20: ro = 4
       ELSE
         l% = 1: nl% = 20: ini% = 1: ro = 4': J% = 1
       END IF
       RETURN
''''''''''''''''''''''
'PAGE DOWN SUBROUTINE
''''''''''''''''''''''
62950  IF nl% <= 80 THEN
          l% = nl% + 1: ini% = ini% + 20: nl% = nl% + 20: ro = 4': J% = 1
       ELSE
           l% = 81: nl% = 100: ini% = 81: ro = 4': J% = 1
       END IF
       RETURN
'-----------------------------------------
'SUBROUTINE TO MOVE TO END OF YEAR SHEET
'-----------------------------------------
61022     GOSUB 67550
          GOSUB 67040
          l% = 100: ini% = 81: nl% = 100: ro = 23: GOSUB 62345
RETURN
'------------------------------------
'SUBROUTINE TO MOVE TO HOME LOCATION
'------------------------------------
61021    GOSUB 67550
         GOSUB 67040
         l% = 1: ro = 4
         IF ini% = 1 THEN
           GOTO 61020
         ELSE
           ini% = 1: nl% = 20: GOSUB 62345
         END IF
RETURN
'''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO REDISPLAY VALUES ON THE SCREEN
'AFTER EDITING (INSERT, DELETE, COPY, MOVE)
'''''''''''''''''''''''''''''''''''''''''''''''''
65090   R% = 4
        COLOR 15, 1
        FOR I% = ini% TO nl%
          LOCATE R%, START(J%): PRINT USING "\    \"; elem$(I%, J%)
           IF J% = 1 THEN
              IF VAL(elem$(I%, J%)) <> 0 THEN
                 IF UPR = 1 THEN LOCATE R%, 17: PRINT USING "####.##"; TOT(I%)
                 IF UPR = 2 THEN LOCATE R%, 17: PRINT USING "#####.#"; TOT(I%)
              ELSE
                  LOCATE R%, 17: PRINT "        ";
              END IF
           END IF
         R% = R% + 1
       NEXT I%
       RETURN
''''''''''''''''''''''''''''''''''
'SUBROUTINE TO MOVE TO A NEW PAGE
'''''''''''''''''''''''''''''''''''
65095  COLOR 0, 3
       R% = 3
       FOR I% = ini% TO nl%
          a$ = RIGHT$(STR$(I%), 3): IF I% > 100 THEN a$ = "   "
          LOCATE R% + 1, 1: PRINT USING "\  \"; a$
          R% = R% + 1
       NEXT I%
       R% = 4
       COLOR 15, 1
       ''''''''''''''''''''''''''''''''''''''
       'print precipitation, precip total
       'temperature, and sol. rad. years
       'in appropriate columns if they exist
       ''''''''''''''''''''''''''''''''''''''
       FOR I% = ini% TO nl%
          FOR k% = 1 TO 4
             IF k% <> 2 THEN
                LOCATE R%, START(k%): PRINT USING "\    \"; elem$(I%, k%)
                IF k% = 1 THEN
                   IF VAL(elem$(I%, k%)) <> 0 THEN
                      IF UPR = 1 THEN LOCATE R%, 17: PRINT USING "####.##"; TOT(I%)
                      IF UPR = 2 THEN LOCATE R%, 17: PRINT USING "#####.#"; TOT(I%)
                   ELSE
                      LOCATE R%, 17: PRINT "          "
                   END IF
                END IF
             END IF
          NEXT k%
          R% = R% + 1
       NEXT I%
       '''''''''''''''''''''''''''
       'higlight the fist year
       'and begin creating/editing
       '''''''''''''''''''''''''''
       'COLOR 15, 1: LOCATE 3, start(J%): PRINT COLHEAD$(J%)
       RETURN 'GOTO 60201
'''''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO READ YEAR IF ANY
'READ FROM SEQUENTIAL EDIT FILE AND SAVE IN
'SCRATCH FILE
'
'OPEN APPROPRIATE FILES IF THEY EXIST
'(CHECK DIRECTORY FIRST)
'''''''''''''''''''''''''''''''''''''''''''''''''''
62111 CLOSE
     D4 = 0: D7 = 0: D13 = 0
     IF DIR$("DATA4.TMP") <> "" THEN D4 = 1
     IF DIR$("DATA7.TMP") <> "" THEN D7 = 1
     IF DIR$("DATA13.TMP") <> "" THEN D13 = 1
     IF CLIM$ = CLIM1$ THEN
        IF DIR$("DATA4.TMP") <> "" THEN
           OPEN "DATA4.TMP" FOR INPUT AS #2
           field1% = 10: field2% = 5: FIELD3% = 10
        ELSE
           GOTO 62114
        END IF
     ELSEIF CLIM$ = CLIM2$ THEN
        IF DIR$("DATA7.TMP") <> "" THEN
           OPEN "DATA7.TMP" FOR INPUT AS #2
           field1% = 5: field2% = 6: FIELD3% = 5
        ELSE
           GOTO 62114
        END IF
     ELSEIF CLIM$ = CLIM3$ THEN
        IF DIR$("DATA13.TMP") <> "" THEN
           OPEN "DATA13.TMP" FOR INPUT AS #2
           field1% = 5: field2% = 6: FIELD3% = 5
        ELSE
           GOTO 62114
        END IF
     END IF
''''''''''''''''''''''''''''''''''''
'READ DATA AND SAVE IT IN FILE
'....FIRST READ HEADER INFORMATION
''''''''''''''''''''''''''''''''''''
      LINE INPUT #2, a1$
      LINE INPUT #2, A2$: CUNIT(1, J%) = VAL(LTRIM$(RTRIM$(A2$)))
      LINE INPUT #2, A3$
           ccity$(1) = LTRIM$(RTRIM$(LEFT$(A3$, 20)))
           cstate$(1) = LTRIM$(RTRIM$(MID$(A3$, 21, 20)))
      LINE INPUT #2, A4$
      ''''''''''''''''''''''''''''''''''
      'READ YEARLY DATA AND STORE
      'DAILY VALUES
      ''''''''''''''''''''''''''''''''''
     LOCATE 8, 30: PRINT "PLEASE WAIT ..."
     IF CLIM$ = CLIM1$ AND D4 = 1 THEN
62112   k = 1
       FOR NN = 1 TO 100
       FOR I = 1 TO 37
         IF EOF(2) THEN CLOSE : GOTO 62114
         LINE INPUT #2, l$
         v0$ = LEFT$(l$, field1%)
         IF I = 1 THEN
             elem$(NN, J%) = LTRIM$(RTRIM$(v0$))
             TOT(NN) = 0
             OPEN elem$(NN, J%) + ".TMP" FOR OUTPUT AS #1
             LOCATE 10, 20: PRINT "READING PRECIPITATION DATA: YEAR  "; : PRINT USING "\    \"; elem$(NN, J%);
         END IF
         PRINT #1, l$
         GOSUB 67020
       NEXT I
      CLOSE #1
      NEXT NN
    ELSE
      IF CLIM$ = CLIM2$ AND D7 = 0 THEN GOTO 62114
      IF CLIM$ = CLIM3$ AND D13 = 0 THEN GOTO 62114
    FOR NN = 1 TO 100
      FOR I = 1 TO 37
         IF EOF(2) THEN CLOSE : GOTO 62114
         LINE INPUT #2, l$
         v0$ = LEFT$(l$, field1%)
         elem$(NN, J%) = LTRIM$(RTRIM$(v0$))
         IF I = 1 THEN
             IF CLIM$ = CLIM2$ THEN
               LOCATE 10, 20: PRINT "READING TEMPERATURE DATA: YEAR  "; : PRINT USING "\    \"; elem$(NN, J%);
             ELSE
               LOCATE 10, 20: PRINT "READING SOLAR RADIATION DATA: YEAR  "; : PRINT USING "\    \"; elem$(NN, J%);
             END IF
             OPEN elem$(NN, J%) + ".TMP" FOR OUTPUT AS #1
         END IF
         PRINT #1, l$
     NEXT I
     CLOSE #1
    NEXT NN
    END IF
'''''''''''''''''''''''''''''''''''''''''''''''''''''''
'GET THE YEARS(ONLY) FOR THE REST OF THE WEATHER DATA
'''''''''''''''''''''''''''''''''''''''''''''''''''''''
62114 CLOSE
      IF CLIM$ = CLIM1$ THEN
       IF D7 = 0 THEN GOTO 62215
       OPEN "DATA7.TMP" FOR INPUT AS #1
       GOSUB 67500
       FOR NN = 1 TO 100
        FOR I = 1 TO 37
         IF EOF(1) THEN CLOSE : GOTO 62215
           LINE INPUT #1, l$
           IF I = 1 THEN
              LOCATE 12, 20: PRINT "READING TEMPERATURE YEARS"
              v0$ = LEFT$(l$, 5)
              elem$(NN, 3) = LTRIM$(RTRIM$(v0$))
           END IF
         NEXT I
       NEXT NN
       CLOSE
62215  IF D13 = 0 THEN RETURN
       OPEN "DATA13.TMP" FOR INPUT AS #1
       GOSUB 67500
       FOR NN = 1 TO 100
        FOR I = 1 TO 37
         IF EOF(1) THEN CLOSE : RETURN
           LINE INPUT #1, l$
           IF I = 1 THEN
              LOCATE 13, 20: PRINT "READING SOLAR RADIATION YEARS"
              v0$ = LEFT$(l$, 5)
              elem$(NN, 4) = LTRIM$(RTRIM$(v0$))
           END IF
         NEXT I
       NEXT NN
       CLOSE
    ELSEIF CLIM$ = CLIM2$ THEN
       IF D4 = 0 THEN GOTO 62216
       OPEN "DATA4.TMP" FOR INPUT AS #1
       GOSUB 67500
       FOR NN = 1 TO 100
        FOR I = 1 TO 37
         IF EOF(1) THEN CLOSE : GOTO 62216
            LINE INPUT #1, l$
            IF I = 1 THEN
              TOT(NN) = 0
              LOCATE 12, 20: PRINT "READING PRECIPITATION YEARS"
              v0$ = LEFT$(l$, 10)
              elem$(NN, 1) = LTRIM$(RTRIM$(v0$))
           END IF
           GOSUB 67020
         NEXT I
       NEXT NN
       CLOSE
62216  IF D13 = 0 THEN RETURN
       OPEN "DATA13.TMP" FOR INPUT AS #1
       GOSUB 67500
       FOR NN = 1 TO 100
        FOR I = 1 TO 37
         IF EOF(1) THEN CLOSE : RETURN
           LINE INPUT #1, l$
           IF I = 1 THEN
              LOCATE 13, 20: PRINT "READING SOLAR RADIATION YEARS"
              v0$ = LEFT$(l$, 5)
              elem$(NN, 4) = LTRIM$(RTRIM$(v0$))
           END IF
         NEXT I
       NEXT NN
       CLOSE
     ELSEIF CLIM$ = CLIM3$ THEN
       IF D4 = 0 THEN GOTO 62217
       OPEN "DATA4.TMP" FOR INPUT AS #1
       GOSUB 67500
       FOR NN = 1 TO 100
         FOR I = 1 TO 37
          IF EOF(1) THEN CLOSE : GOTO 62217
           LINE INPUT #1, l$
           IF I = 1 THEN
              TOT(NN) = 0
              LOCATE 12, 20: PRINT "READING PRECIPITATION YEARS"
              v0$ = LEFT$(l$, 10)
              elem$(NN, 1) = LTRIM$(RTRIM$(v0$))
           END IF
           GOSUB 67020
         NEXT I
       NEXT NN
       CLOSE
62217  IF D7 = 0 THEN RETURN
       OPEN "DATA7.TMP" FOR INPUT AS #1
       GOSUB 67500
       FOR NN = 1 TO 100
        FOR I = 1 TO 37
         IF EOF(1) THEN CLOSE : RETURN
           LINE INPUT #1, l$
           IF I = 1 THEN
              LOCATE 13, 20: PRINT "READING TEMPERATURE YEARS"
              v0$ = LEFT$(l$, 5)
              elem$(NN, 3) = LTRIM$(RTRIM$(v0$))
           END IF
         NEXT I
       NEXT NN
      END IF
      CLOSE
RETURN
''''''''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO SAVE DATA INTO A tmp
'READ FROM INDIVIDUAL FILES AND SAVE TO ONE FILE
''''''''''''''''''''''''''''''''''''''''''''''''''''''
62444 CLOSE
      IF CLIM$ = CLIM1$ THEN
        CLIM = 1
        OPEN "DATA4.TMP" FOR OUTPUT AS #2
      ELSEIF CLIM$ = CLIM2$ THEN
        CLIM = 2
        OPEN "DATA7.TMP" FOR OUTPUT AS #2
      ELSEIF CLIM$ = CLIM3$ THEN
        CLIM = 3
        OPEN "DATA13.TMP" FOR OUTPUT AS #2
      END IF
      IF a1$ = "" THEN a1$ = " 3"
      IF A2$ = "" THEN
         A2$ = STR$(CUNIT(CLIM, 3))
         A2$ = " " + LTRIM$(RTRIM$(A2$))
      END IF
      IF A3$ = "" THEN
        A3$ = ccity$(CLIM) + cstate$(CLIM)
      END IF
      PRINT #2, a1$
      PRINT #2, A2$
      PRINT #2, A3$
      PRINT #2, A4$
      nempty% = 0
62250 FOR II = 1 TO 100
        IF LTRIM$(RTRIM$(elem$(II, J%))) = "" THEN
           nempty% = nempty% + 1
           GOTO 62253
        END IF
        a$ = DIR$(elem$(II, J%) + ".TMP")
        IF a$ = "" THEN GOTO 62253
        OPEN elem$(II, J%) + ".TMP" FOR INPUT AS #1
        FOR I = 1 TO 37
          IF EOF(1) THEN CLOSE #1: GOTO 62253
          LINE INPUT #1, l$
          PRINT #2, l$
        NEXT I
       CLOSE #1
62253   NEXT II
        IF nempty% = 100 THEN
          CLOSE
          IF CLIM$ = CLIM1$ THEN
             IF DIR$("DATA4.TMP") <> "" THEN KILL ("DATA4.TMP")
          ELSEIF CLIM$ = CLIM2$ THEN
             IF DIR$("DATA7.TMP") <> "" THEN KILL ("DATA7.TMP")
          ELSEIF CLIM$ = CLIM3$ THEN
             IF DIR$("DATA13.TMP") <> "" THEN KILL ("DATA13.TMP")
          END IF
        END IF
      CLOSE
      GOSUB 66020
      RETURN
''''''''''''''''''''''''''
'DELETE ALL SCRATCH FILES
''''''''''''''''''''''''''
66020 IF IRTRN% = 100 OR IRTRN% = 1000 THEN
          CALL EXOUT(ANSCODE)
          IF ANSCODE = 0 THEN
              COLOR 0, 3
              GOSUB 62345
              GOTO 61020
          END IF
      END IF
66027 FOR II = 1 TO 100
         IF elem$(II, J%) = "" THEN GOTO 62252
         a$ = DIR$(elem$(II, J%) + ".TMP")
         IF a$ = "" THEN GOTO 62252
         KILL elem$(II, J%) + ".TMP"
         elem$(II, J%) = "": TOT(II) = 0
62252 NEXT II
      IF IRTRN% = 100 OR IRTRN% = 1000 THEN EXIT SUB
      RETURN
'''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO TOTAL PRECIPITATION
'''''''''''''''''''''''''''''''''''''
67020  IF precu$ = "2" THEN
          DEL% = 6
       ELSE
          DEL% = 5
       END IF
       V1$ = MID$(l$, 11, DEL%): st% = 11 + DEL%
       V2$ = MID$(l$, st%, DEL%): st% = st% + DEL%
       V3$ = MID$(l$, st%, DEL%): st% = st% + DEL%
       V4$ = MID$(l$, st%, DEL%): st% = st% + DEL%
       V5$ = MID$(l$, st%, DEL%): st% = st% + DEL%
       V6$ = MID$(l$, st%, DEL%): st% = st% + DEL%
       V7$ = MID$(l$, st%, DEL%): st% = st% + DEL%
       V8$ = MID$(l$, st%, DEL%): st% = st% + DEL%
       V9$ = MID$(l$, st%, DEL%): st% = st% + DEL%
       V10$ = MID$(l$, st%, DEL%): st% = st% + DEL%
       V11$ = MID$(l$, st%, DEL%): st% = st% + DEL%
       TOT(NN) = TOT(NN) + VAL(V1$) + VAL(V2$) + VAL(V3$) + VAL(V4$) + VAL(V5$) + VAL(V6$) + VAL(V7$) + VAL(V8$) + VAL(V9$) + VAL(V10$)
    RETURN
'''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO COPY THE CONTENTS OF A
'FILE INTO ANOTHER FILE IF THE USER
'CHANGES THE YEAR NAME
'''''''''''''''''''''''''''''''''''''''
67040  OLDELEM$ = LTRIM$(RTRIM$(OLDELEM$))
       elem$(l%, J%) = LTRIM$(RTRIM$(elem$(l%, J%)))
       IF elem$(l%, J%) = "" THEN RETURN
       IF DIR$(elem$(l%, J%) + ".TMP") <> "" THEN GOTO 67044
       IF OLDELEM$ <> elem$(l%, J%) THEN
         IF OLDELEM$ = "" THEN RETURN
         IF DIR$(OLDELEM$ + ".TMP") = "" THEN RETURN
         CLOSE
         OPEN OLDELEM$ + ".TMP" FOR INPUT AS #1
         OPEN elem$(l%, J%) + ".TMP" FOR OUTPUT AS #2
         DO UNTIL EOF(1)
           LINE INPUT #1, l$
           PRINT #2, l$
         LOOP
67044    CLOSE
         ''''''''''''''''''''''''''''''''''''''''''
         'adjust the total PRECIP ACCORDING TO
         'THE NEW YEAR TOTAL
         ''''''''''''''''''''''''''''''''''''''''''
         IF J% = 1 THEN
           FOR iv% = 1 TO 100
             IF VAL(elem$(iv%, J%)) = VAL(elem$(l%, J%)) THEN
               TOT(l%) = TOT(iv%)
                 COLOR 15, 1
                 LOCATE ro, 17: PRINT "          "
                 IF UPR = 1 THEN LOCATE ro, 17: PRINT USING "####.##"; TOT(l%)
                 IF UPR = 2 THEN LOCATE ro, 17: PRINT USING "#####.#"; TOT(l%)
                 RETURN
             END IF
           NEXT iv%
         END IF
       END IF
         RETURN
'''''''''''''''''''''''''''''''''''''''
'SUBROUITINE TO READ HEADER LINES IN
'WEATHER FILES
'''''''''''''''''''''''''''''''''''''''
67500 LINE INPUT #1, a$
      LINE INPUT #1, a$
      LINE INPUT #1, a$
      LINE INPUT #1, a$
   RETURN
'------------------------------------------
'Make sure there are no duplicate years
'------------------------------------------
67550  current = VAL(elem$(l%, J%))
        IF current = 0 THEN RETURN
        FOR iv% = 1 TO 100
           IF iv% <> l% AND VAL(elem$(iv%, J%)) = current THEN
             elem$(l%, J%) = LEFT$("               ", WIDE)
             LOCATE ro, START(J%): PRINT elem$(l%, J%)
             GOTO 61020
           END IF
        NEXT iv%
        RETURN
'-----------------------------------------
'SUBROUTINE TO REDISPLAY LINE NUMBERS
'ON THE SCREEN
'-----------------------------------------
62345 IF IPATH > 1 THEN GOSUB 42243
       COLOR 0, 3
       R% = 3
       FOR I% = ini% TO nl%
         a$ = RIGHT$(STR$(I%), 3): IF I% > 100 THEN a$ = "   "
         LOCATE R% + 1, 1: PRINT USING "\  \"; a$
         R% = R% + 1
       NEXT I%
       GOSUB 62343
  RETURN
'--------------------------------------------
'SECOND SUBROUTINE TO REDISPLAY VALUES ON
'SCREEN
'--------------------------------------------
62343  R% = 4
       COLOR 15, 1
       '----------------------------------------
       'print precipitation years, precip total
       'temperature years, and sol. rad. years
       'in appropriate columns if they exist
       '----------------------------------------
       FOR I% = ini% TO nl%
          FOR k% = 1 TO 4
             IF k% <> 2 THEN
                LOCATE R%, START(k%): PRINT USING "\    \"; elem$(I%, k%)
                IF k% = 1 THEN
                   IF VAL(elem$(I%, k%)) <> 0 THEN
                      IF UPR = 1 THEN LOCATE R%, 17: PRINT USING "####.##"; TOT(I%)
                      IF UPR = 2 THEN LOCATE R%, 17: PRINT USING "#####.#"; TOT(I%)
                   ELSE
                      LOCATE R%, 17: PRINT "          "
                   END IF
                END IF
             END IF
          NEXT k%
          R% = R% + 1
       NEXT I%
RETURN
'---------------------------------------------------------------------------
'subroutine to compute INI% after a MOVE
'This is needed for the dispaly of appropriate cells
'---------------------------------------------------------------------------
42243 ini% = ALTABINI%
      nl% = ini% + 19
      l% = ALTABL%
      IF IPATH = 2 THEN ro = ALTABRO - 1
      IF IPATH = 3 THEN ro = ALTABRO
      IF IPATH = 4 THEN ro = ALTABRO + 1
      IF IPATH = 5 THEN ro = ALTABRO
      IF IPATH = 7 THEN
         ro = ALTABRO + 1
         IF ro > 23 THEN
            ro = 23
            IF nl% = 100 THEN l% = l% - 1: ro = ro - 1: RETURN
            ini% = ini% + 1
            nl% = nl% + 1
         END IF
      END IF
RETURN
''''''''''''''''''''''''''''''''''''''''''''''''
'SUBROUTINE TO CALL MANUAL MESSAGES FOR HELP
''''''''''''''''''''''''''''''''''''''''''''''''
65190  IF IRTRN% = 20 THEN CALL HLPMSG(F1HELP$)
       IF IRTRN% = 21 THEN CALL HLPMSG(F2HELP$)
       IF IRTRN% = 22 THEN CALL USERHELP
       RETURN
END SUB

