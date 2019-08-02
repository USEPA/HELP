DIM station$(4), monthday%(12)
'DECLARE FUNCTION num (convstr$)     'Converts numerical string to number
DECLARE FUNCTION leapyear% (year%)   'Returns 1 for leapyear, 0 for others
DECLARE SUB readin (station$(), a$) 'Separates first 10 fields of input
                                    'into array [1..10] stations
DECLARE SUB formatlen ()            'Initializes positions of first 4
                                    'fields and any other variables
DECLARE SUB formatmonth ()          'Initializes the days of months
'DECLARE SUB io (IN$, OT$)           'Gets filenames
                                    ' p1..p5 are positions of the first 5 fields
COMMON SHARED p1%, p2%, p3%, p4%, p5%, s1%, s2%
COMMON SHARED monthday%(), elem$, elem2$
CALL formatlen
CALL formatmonth
'''''''''''''''''''''''''''''''''''''''''''
'READ INPUT AND OUTPUT FILES THAT ARE
'PASSED FROM HELPI IN FILE "PASSFILE.DATA"
'THEN DELETE THE FILE
'''''''''''''''''''''''''''''''''''''''''''
COLOR 15, 1
OPEN "PASSNAME.DAT" FOR INPUT AS #1
'    OPEN "can7.dat" FOR INPUT AS #1
'    OPEN "lett1" FOR INPUT AS #1
'    OPEN "ccan7.dat" FOR INPUT AS #1
'    OPEN "lett2" FOR INPUT AS #1
'    OPEN "clt7a.dat" FOR INPUT AS #1
INPUT #1, CITY$
INPUT #1, STATE$
INPUT #1, IN$
INPUT #1, OT$
CLOSE #1
'KILL "PASSNAME.DAT"
'------------------------------------
OPEN IN$ FOR INPUT AS #1
OPEN OT$ FOR OUTPUT AS #2
'    OPEN "OT$" FOR OUTPUT AS #2
'------------------------------------
'PRINT CLIMATE DATA FILE HEADER LINE
'------------------------------------
PRINT #2, " 9"
PRINT #2, " 2"
PRINT #2, USING "\                  \"; CITY$; STATE$
PRINT #2,
'''''''''''''''''''''''''''''''''''''''''''''
writelen = 0
counter = 1
daymis% = 0

DO UNTIL EOF(1)

   INPUT #1, a$
   IF a$ <> "" THEN
      CALL readin(station$(), a$)
      IF elem$ = "" THEN elem$ = station$(4)
        IF elem$ = "001" OR elem$ = "002" THEN GOTO 11
        IF elem$ <> "003" THEN GOTO 10
      year% = VAL(station$(2))
      IF leapyear%(year%) <> 0 THEN
         monthday%(2) = 29
      ELSE
         monthday%(2) = 28
      END IF
2 :   imonth% = imonth% + 1
      IF imonth% > 12 THEN imonth% = 1
      month% = VAL(station$(3))

      IF imonth% <> month% THEN

'*********************************** Missing Months ************************

      IF imonth% = 12 THEN year% = year% - 1
      
      LOCATE 23, 1: PRINT "                                                                ";
      LOCATE 23, 1: PRINT " Converting Year";
      PRINT USING "#####"; year%
      LOCATE 24, 1: PRINT " - Missing Month (";
      PRINT USING "##"; imonth%;
      PRINT ") ";
      PRINT CHR$(7);
      PRINT "   Press any key to continue.....";
'                                               set up pause
100   IF INKEY$ = "" THEN GOTO 100


        FOR y% = 1 TO (monthday%(imonth%))
         IF writelen = 0 THEN
            PRINT #2, USING "#####"; year%;
            writelen = 1
         END IF
         wdata = 10!
         PRINT #2, USING "####.#"; wdata;
         IF (writelen MOD 10) = 0 THEN
            PRINT #2, USING "#####"; counter
            counter = counter + 1
            writelen = 1
            PRINT #2, USING "#####"; year%;
         ELSE
            writelen = writelen + 1
         END IF
      NEXT y%

      IF imonth% = 12 THEN
         DO UNTIL (writelen = 11)
            wdata = 10
            IF (writelen > 6) THEN wdata = 0
            PRINT #2, USING "####.#"; wdata;
            writelen = writelen + 1
         LOOP
         PRINT #2, USING "#####"; counter
         counter = 1
         writelen = 0
      END IF

      IF imonth% = 12 THEN year% = year% + 1

      END IF

      IF imonth% <> month% GOTO 2

      dayavg% = 0
      mistrue$ = "false"

      FOR x% = 1 TO (monthday%(month%))
         IF writelen = 0 THEN
            PRINT #2, USING "#####"; year%; : writelen = 1
            LOCATE 23, 1: PRINT "                                                                            ";
            LOCATE 23, 1: PRINT " Converting Year "; : PRINT USING "####"; year%;
         END IF
         IF LEN(a$) > 200 THEN
            b$ = MID$(a$, ((p5%) + ((x% - 1) * s1%)), 6)
         ELSE
            b$ = MID$(a$, ((p5%) + ((x% - 1) * s2%)), 4)
         END IF
         IF b$ = "-99999" OR b$ = "0 -999" THEN
            daymis% = daymis% + 1
            dayavg% = dayavg% + 1
            mistrue$ = "true"
            GOTO secread
         END IF
         IF mistrue$ <> "true" THEN
            vdata = VAL(b$)
            wdata = vdata / 10
            nextwdata = wdata
            PRINT #2, USING "####.#"; wdata;
            IF (writelen MOD 10) = 0 THEN
               PRINT #2, USING "#####"; counter
               counter = counter + 1
               writelen = 1
               PRINT #2, USING "#####"; year%;
            ELSE
               writelen = writelen + 1
            END IF
         ELSE
            FOR z% = 1 TO dayavg% + 1
               vdata = VAL(b$)
               vdata = vdata / 10
               lastdata% = VAL(b$)
               wdata = (vdata + nextwdata) / 2

               IF (z% = dayavg% + 1) THEN
                  wdata = lastdata% / 10
                  mistrue$ = "false"
               END IF

               PRINT #2, USING "####.#"; wdata;
               IF (writelen MOD 10) = 0 THEN
                  PRINT #2, USING "#####"; counter
                  counter = counter + 1
                  writelen = 1
                  PRINT #2, USING "#####"; year%;
               ELSE
                  writelen = writelen + 1
               END IF
            NEXT z%
            dayavg% = 0
         END IF
secread:
      NEXT x%
      IF month% = 12 THEN
         DO UNTIL (writelen MOD 10) = 1
            PRINT #2, USING "####.#"; 0;
            writelen = writelen + 1
         LOOP
         PRINT #2, USING "#####"; counter
         counter = 1
         writelen = 0
        'PRINT
      END IF
   END IF
11 : elem$ = ""
LOOP
CLOSE #1

'******************** Check for missing values at end of file or yr ***************

IF month% <> 12 AND dayn% <> 31 THEN
'                Calculated the date the data stop
   stopdate% = 0
   FOR m% = 1 TO month% - 1
      stopdate% = stopdate% + monthday%(m%)
   NEXT m%

   stopdate% = stopdate% + x%

   FOR m% = stopdate% TO 370

      IF (m% MOD 30) = 0 THEN
         jmonth% = m% / 30
         LOCATE 23, 1: PRINT "                                                                ";
         LOCATE 23, 1: PRINT " Converting Year";
         PRINT USING "#####"; year%
         LOCATE 24, 1: PRINT " - Missing Month (";
         PRINT USING "##"; jmonth%;
         PRINT ") ";
         PRINT CHR$(7);
         PRINT "   Press any key to continue.....";
'                                               set up pause
109      IF INKEY$ = "" THEN GOTO 109
      END IF


         wdata = 10
         IF m% > 366 THEN wdata = 0
         IF writelen = 10 THEN
            PRINT #2, USING "####.#"; wdata;
            PRINT #2, USING "#####"; counter
            counter = counter + 1
            IF counter < 38 THEN PRINT #2, USING "#####"; year%;
            IF counter > 37 THEN counter = 1
            writelen = 1
         ELSEIF writelen < 10 THEN
            PRINT #2, USING "####.#"; wdata;
            writelen = writelen + 1
         END IF

   NEXT m%
END IF

LOCATE 23, 1: PRINT "                                                                ";
IF daymis% > 0 THEN
   LOCATE 23, 1: PRINT CHR$(7); " The total number of missing days is "; daymis%;
   PRINT " (Does not include missing months)";
   PRINT "   Press any key to continue.....                                 ";
'                                               set up pause
101 IF INKEY$ = "" THEN GOTO 101
END IF

CLOSE #2

END

10 :
'PRINT
LOCATE 23, 4: PRINT CHR$(7); "File is not a Canadian temperature file!  Check the data file!";
CLOSE #1
CLOSE #2
END

SUB formatlen
  p1% = 1  'Location of Station ID
  p2% = 8  'Location of Year
  p3% = 11 'Location of Month
  p4% = 13 'Location of Element Number
  p5% = 16 'Location of first data entry
  s1% = 7  'Distance between data entry values
  s2% = 4  'Distance between data entry values
END SUB

SUB formatmonth
    monthday%(1) = 31: monthday%(2) = 28: monthday%(3) = 31: monthday%(4) = 30
    monthday%(5) = 31: monthday%(6) = 30: monthday%(7) = 31: monthday%(8) = 31
    monthday%(9) = 30: monthday%(10) = 31: monthday%(11) = 30: monthday%(12) = 31
END SUB

FUNCTION leapyear% (year%) STATIC
   IF year% < 900 THEN year% = year% + 1000
   year% = year% + 1000
   IF (year% MOD 4) = 0 THEN
      leapyear% = 1
   ELSE leapyear% = 0
   END IF
END FUNCTION

SUB readin (station$(), a$)
    station$(1) = MID$(a$, p1%, p2% - p1%)
    station$(2) = MID$(a$, p2%, p3% - p2%)
    station$(3) = MID$(a$, p3%, p4% - p3%)
    station$(4) = MID$(a$, p4%, p5% - p4%)
END SUB

