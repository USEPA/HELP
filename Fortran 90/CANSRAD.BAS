DECLARE SUB actualday (iday%, counter!, daymis%, writelen, year%, month%, dayn%, countday%, countyr%, comonth%)
DIM station$(5), monthday%(12)
DECLARE FUNCTION leapyear% (year%)   'Returns 1 for leapyear, 0 for others
DECLARE SUB readin (station$(), a$) 'Separates first 10 fields of input
                                    'into array [1..10] stations
DECLARE SUB formatlen ()            'Initializes positions of first 4
                                    'fields and any other variables
DECLARE SUB formatmonth ()          'Initializes the days of months
                                    ' p1..p4 are positions of the first 5 fields
COMMON SHARED p1%, p2%, p3%, p4%, p5%, p6%, s1%, s2%
COMMON SHARED elem$, elem2$, monthday%()
'''''''''''''''''''''''''''''''''''''''''''
'READ INPUT AND OUTPUT FILES THAT ARE
'PASSED FROM HELPI IN FILE "PASSFILE.DATA"
'THEN DELETE THE FILE
'''''''''''''''''''''''''''''''''''''''''''
COLOR 15, 1
OPEN "PASSNAME.DAT" FOR INPUT AS #1
'    OPEN "can13.dat" FOR INPUT AS #1
'    OPEN "letr1" FOR INPUT AS #1
'    OPEN "ccan13.dat" FOR INPUT AS #1
'    OPEN "clt13b.dat" FOR INPUT AS #1
INPUT #1, CITY$
INPUT #1, STATE$
INPUT #1, IN$
INPUT #1, OT$
CLOSE #1
'''''''''''''''''''''''''''''''''''''''''''''
OPEN IN$ FOR INPUT AS #1
OPEN OT$ FOR OUTPUT AS #2
'    OPEN "OT$" FOR OUTPUT AS #2
'''''''''''''''''''''''''''''''''''''''''''''
'PRINT CLIMATE DATA FILE HEADER LINE
'''''''''''''''''''''''''''''''''''''''''''''
PRINT #2, " 9"
PRINT #2, " 2"
PRINT #2, USING "\                  \"; CITY$; STATE$
PRINT #2,
'''''''''''''''''''''''''''''''''''''''''''''
CALL formatlen
CALL formatmonth
imonth% = 0
hour& = 0
daymis% = 0
iday% = -1
filetype = 1
writelen = 0
counter = 1
countday% = 1
comonth% = 1
countyr% = 1900
DO UNTIL EOF(1)
   INPUT #1, a$
   IF filetype = 1 THEN
      xreclen = LEN(a$)
         filetype = 2
   END IF
   IF a$ <> "" THEN
      CALL readin(station$(), a$)
      dayn% = VAL(station$(4))

      IF elem$ = "" THEN elem$ = station$(5)
      IF elem$ > "061" AND elem$ < "069" THEN GOTO 12
      IF elem$ <> "061" THEN GOTO 10
      year% = VAL(station$(2))
      IF year% < 900 THEN year% = year% + 1000
      year% = year% + 1000
      IF countyr% = 1900 THEN countyr% = year%
      month% = VAL(station$(3))

'     this code is to check for a delete day or missing day in data file

'                keep record of exact day of the month

        vdata = 0
        IF writelen = 0 THEN
           LOCATE 23, 1: PRINT "                                                                        ";
           LOCATE 23, 1: PRINT " Converting Year ";
           PRINT USING "####"; year%;
           PRINT #2, USING "#####"; year%;
           LOCATE 24, 1: PRINT "                                                           ";

           writelen = 1
        END IF
        vvdata = 0
        FOR hr% = 1 TO 24
            IF xreclen < 120 THEN
              vvdata = ((VAL(MID$(a$, (p6% + ((hr% - 1) * s2%)), 4))) - 6000)
              IF vvdata = -99999 THEN
                 vvdata = -5
                 hour& = hour& + 1
                 flag$ = "missing"
              END IF
              vdata = vdata + vvdata
            ELSE
              vvdata = (VAL(MID$(a$, (p6% + ((hr% - 1) * s1%)), 6)))
              IF vvdata = -99999 THEN
                 hour& = hour& + 1
                 vvdata = -5
                 flag$ = "missing"
              END IF
              vdata = vdata + vvdata

            END IF
        NEXT hr%

13 : IF (imonth% <> month%) THEN imonth% = imonth% + 1
      IF imonth% > 12 THEN imonth% = 1

'****************************************************************************

      IF imonth% <> month% THEN
        lmonth% = monthday%(imonth%)
        IF year% MOD 4 = 0 THEN
           IF imonth% = 2 THEN lmonth% = 29
        END IF

        FOR Y% = 1 TO lmonth%
         IF writelen = 10 THEN
            LOCATE 23, 1: PRINT "                                                     ";
            LOCATE 23, 1: PRINT " Converting Year ";
            PRINT USING "####"; year%;
            IF Y% = 28 THEN
               LOCATE 24, 1: PRINT " - Missing Month (";
               PRINT USING "##"; imonth%;
               PRINT CHR$(7);
               PRINT ")   Press any key to continue.....                     ";
100 :          IF INKEY$ = "" THEN GOTO 100
            END IF
         ELSE
            LOCATE 23, 1: PRINT "                                                     ";
            LOCATE 23, 1: PRINT " Converting Year ";
            PRINT USING "####"; year%;
            IF Y% = 28 THEN
               LOCATE 24, 1: PRINT " - Missing Month (";
               PRINT USING "##"; imonth%;
               PRINT CHR$(7);
               PRINT ")   Press any key to continue.....                     ";
101 :          IF INKEY$ = "" THEN GOTO 101
            END IF
         END IF
         wdata = -5 * 24 / 1000

         IF writelen = 10 THEN
            PRINT #2, USING "###.##"; wdata;
            PRINT #2, USING "#####"; counter
            counter = counter + 1
            IF counter > 37 THEN counter = 1
            writelen = 1
            PRINT #2, USING "#####"; year%;
         ELSE
            PRINT #2, USING "###.##"; wdata;
            writelen = writelen + 1
         END IF
       NEXT Y%
      GOTO 13
      END IF


      iday% = iday% + 1
      IF iday% > maxday% THEN iday% = 1
      IF iday% = 0 THEN iday% = 1
        IF iday% <> dayn% THEN
           CALL actualday(iday%, counter, daymis%, writelen, year%, month%, dayn%, countday%, countyr%, comonth%)
           
        END IF

        lmonth% = monthday%(imonth%)
        IF year% MOD 4 = 0 THEN
           IF imonth% = 2 THEN lmonth% = 29
        END IF
      
'                                  What is the maximum day of this month?
       maxday% = monthday%(imonth%)
       IF (imonth% = 2) AND (year% MOD 4) = 0 AND lmonth% = 29 THEN maxday% = 29
       IF iday% > maxday% THEN iday% = 1

      IF iday% <> dayn% THEN

         FOR z% = 1 TO dayn% - 1
         IF writelen = 10 THEN
            LOCATE 23, 1: PRINT "                                                     ";
            LOCATE 23, 1: PRINT " Converting Year ";
            PRINT USING "####"; year%;
            IF z% = 28 THEN
               LOCATE 24, 1: PRINT " - Missing Month (";
               PRINT USING "##"; imonth%;
               PRINT CHR$(7);
               PRINT ")   Press any key to continue.....                     ";
102 :          IF INKEY$ = "" THEN GOTO 102
            END IF
            
         ELSE
            LOCATE 23, 1: PRINT "                                                     ";
            LOCATE 23, 1: PRINT " Converting Year ";
            PRINT USING "####"; year%;
            IF z% = 28 THEN
               LOCATE 24, 1: PRINT " - Missing Month (";
               PRINT USING "##"; imonth%;
               PRINT CHR$(7);
               PRINT ")   Press any key to continue.....                     ";
103 :          IF INKEY$ = "" THEN GOTO 103
            END IF
         END IF

         wdata = -5 * 24 / 1000

         IF writelen = 10 THEN
            PRINT #2, USING "###.##"; wdata;
            PRINT #2, USING "#####"; counter
            counter = counter + 1
            IF counter > 37 THEN counter = 1
            writelen = 1
            PRINT #2, USING "#####"; year%;
         ELSE
            PRINT #2, USING "###.##"; wdata;
            writelen = writelen + 1
         END IF

         NEXT z%
         iday% = dayn%
     END IF

        wdata = vdata / 1000
        PRINT #2, USING "###.##"; wdata;

        IF (writelen MOD 10) = 0 THEN
           PRINT #2, USING "#####"; counter
           counter = counter + 1
           writelen = 1
           PRINT #2, USING "#####"; year%;
         ELSE
           writelen = writelen + 1
        END IF
      IF month% = 12 AND dayn% = 31 THEN
         DO UNTIL (writelen MOD 10) = 1
            PRINT #2, USING "###.##"; 0;
            writelen = writelen + 1
         LOOP
         PRINT #2, USING "#####"; counter
         counter = 1
         writelen = 0
      END IF
   END IF
12 : elem$ = ""
LOOP
11 : CLOSE #1
'               Check for missing values at end of file

IF month% <> 12 AND dayn% <> 31 THEN
'                Calculated the date the data stop
   stopdate% = 0
   FOR m% = 1 TO month% - 1


      stopdate% = stopdate% + monthday(m%)
   NEXT m%

   stopdate% = stopdate% + dayn%
   mm% = stopdate% + 1
   FOR m% = stopdate% + 1 TO 370
         imonth% = m% / 30
         IF imonth% < 13 THEN
         IF writelen = 10 THEN
            LOCATE 23, 1: PRINT "                                                     ";
            LOCATE 23, 1: PRINT " Converting Year ";
            PRINT USING "####"; year%;
            IF m% = mm% THEN
               LOCATE 24, 1: PRINT " - Missing Month (";
               PRINT USING "##"; imonth%;
               PRINT CHR$(7);
               PRINT ")   Press any key to continue.....                     ";
105 :          IF INKEY$ = "" THEN GOTO 105
              mm% = mm% + 30
            END IF
         ELSE
            LOCATE 23, 1: PRINT "                                                     ";
            LOCATE 23, 1: PRINT " Converting Year ";
            PRINT USING "####"; year%;
            IF m% = mm% THEN
               LOCATE 24, 1: PRINT " - Missing Month (";
               PRINT USING "##"; imonth%;
               PRINT CHR$(7);
               PRINT ")   Press any key to continue.....                     ";
106 :          IF INKEY$ = "" THEN GOTO 106
               mm% = mm% + 30
            END IF
         END IF
         END IF

         wdata = -5 * 24 / 1000

         IF (m% > 366) THEN wdata = 0

         IF writelen = 10 THEN
            PRINT #2, USING "###.##"; wdata;
            PRINT #2, USING "#####"; counter
            counter = counter + 1
            IF (m% < 370) THEN PRINT #2, USING "#####"; year%;
            IF counter > 37 THEN counter = 1
            writelen = 1
         ELSE
            PRINT #2, USING "###.##"; wdata;
            writelen = writelen + 1
         END IF

   NEXT m%
END IF

IF daymis% > 0 OR hour& > 0 THEN
   LOCATE 23, 1: PRINT CHR$(7); " The total number of missing hours is "; hour&;
   PRINT "  and missing days is "; daymis%;
   PRINT "   Press any key to continue.....                                 ";
'                                               set up pause
107 IF INKEY$ = "" THEN GOTO 107
END IF


CLOSE #2
END
10 :
'PRINT
LOCATE 23, 2:  PRINT CHR$(7); "The data file is not a Canadian solar radiation file!  Check the file."
CLOSE #1
CLOSE #2
END

SUB actualday (iday%, counter, daymis%, writelen, year%, month%, dayn%, countday%, countyr%, comonth%)

'This routine make sure the day that being written is the correct date.
' The days are not equal indicates a deleted or missing day has been encounted.

400 :   wdata = -5! * 24 / 1000
   daymis% = daymis% + 1
   iday% = iday% + 1
   jmaxday% = monthday%(month%)
   IF year% MOD 4 = 0 THEN          'check for leap year
      IF month% = 2 THEN jmaxday% = 29
   END IF
   IF iday% > jmaxday% THEN
      iday% = 1
      comonth% = comonth% + 1
      IF comonth% > 12 THEN
         comonth% = 1
         countyr% = countyr% + 1
      END IF
   END IF

   IF writelen = 0 THEN
      PRINT #2, USING "#####"; year%;
      writelen = 1
   END IF
   IF writelen = 10 THEN
      PRINT #2, USING "###.##"; wdata;
      PRINT #2, USING "#####"; counter
      counter = counter + 1
      IF counter > 37 THEN counter = 1
      writelen = 1
      PRINT #2, USING "#####"; year%;
   ELSE
     PRINT #2, USING "###.##"; wdata;
     writelen = writelen + 1
   END IF
   IF iday% <> dayn% THEN GOTO 400

END SUB

SUB formatlen
  p1% = 1  'Location of Station ID
  p2% = 8  'Location of Year
  p3% = 11 'Location of Month
  p4% = 13 'Location of Day
  p5% = 15 'Location of Element
  s1% = 7  'Distance between data entry values
  p6% = 18 'Location of first data entry
  s2% = 4   'Distance between data entry values
END SUB

SUB formatmonth
    monthday%(1) = 31: monthday%(2) = 28: monthday%(3) = 31: monthday%(4) = 30
    monthday%(5) = 31: monthday%(6) = 30: monthday%(7) = 31: monthday%(8) = 31
    monthday%(9) = 30: monthday%(10) = 31: monthday%(11) = 30: monthday%(12) = 31

END SUB

SUB io (IN$, OT$)
    PRINT "GLOBAL RADIATION CONVERSION"
    PRINT
    dfile$ = "A:\RADINFO.DAT"
    PRINT "Enter input file name ["; dfile$; "]: ";
    INPUT IN$
    dfile$ = "radinfo.dat"
    IF IN$ = "" THEN IN$ = dfile$
    dfile$ = "A:\DATA13.D13"
    PRINT "Enter output file name ["; dfile$; "]:";
    INPUT OT$
    dfile$ = "Data13.D13"
    IF OT$ = "" THEN OT$ = dfile$
END SUB

SUB readin (station$(), a$)
    station$(1) = MID$(a$, p1%, p2% - p1%)
    station$(2) = MID$(a$, p2%, p3% - p2%)
    station$(3) = MID$(a$, p3%, p4% - p3%)
    station$(4) = MID$(a$, p4%, p5% - p4%)
    station$(5) = MID$(a$, p5%, p6% - p5%)
END SUB

