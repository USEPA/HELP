DIM a$(2), station$(11), station2$(11), monthday(12), dailydata(2, 31), total(2)
DECLARE FUNCTION num (convstr$)
DECLARE FUNCTION leapyear (year$)
DECLARE SUB readin (station$(), a$)
DECLARE SUB formatlen ()
DECLARE SUB formatmonth ()
COMMON SHARED p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, s1
COMMON SHARED d1, d2, d3, d4, d5, d6
COMMON SHARED monthday(), elem$, elem2$
ON ERROR GOTO 10000
COLOR 15, 1
CALL formatlen
CALL formatmonth
'''''''''''''''''''''''''''''''''''''''''''
'READ INPUT AND OUTPUT FILES THAT ARE
'PASSED FROM HELPI IN FILE "PASSFILE.DATA"
'THEN DELETE THE FILE
'''''''''''''''''''''''''''''''''''''''''''
OPEN "PASSNAME.DAT" FOR INPUT AS #1
INPUT #1, CITY$
INPUT #1, STATE$
INPUT #1, IN$
INPUT #1, IN2$
INPUT #1, OT$
CLOSE #1
KILL "PASSNAME.DAT"
'''''''''''''''''''''''''''''''''''''''''''''
OPEN IN$ FOR INPUT AS #1
OPEN IN2$ FOR INPUT AS #2
OPEN OT$ FOR OUTPUT AS #3
'''''''''''''''''''''''''''''''''''''''''''''
'PRINT CLIMATE DATA FILE HEADER LINE
'''''''''''''''''''''''''''''''''''''''''''''
PRINT #3, " 5"
PRINT #3, " 1"
PRINT #3, USING "\                  \"; CITY$; STATE$
PRINT #3,
'''''''''''''''''''''''''''''''''''''''''''''
writelen = 0
counter = 1
DO UNTIL EOF(1)
   INPUT #1, a$(1)
   INPUT #2, a$(2)
   IF a$(1) <> "" THEN
      CALL readin(station$(), a$(1))
      CALL readin(station2$(), a$(2))
      IF elem$ = "" THEN elem$ = station$(3)
      IF elem2$ = "" THEN elem2$ = station2$(3)
      IF (elem$ <> "TMAX") AND (elem$ <> "TMIN") THEN GOTO 10
      IF ((elem2$ <> "TMAX") AND (elem2$ <> "TMIN")) OR (elem$ = elem2$) THEN GOTO 10
      IF leapyear(station$(5)) <> 0 THEN
         monthday(2) = 29
      ELSE monthday(2) = 28
      END IF
      year = num(station$(5))
      month = num(station$(6))
      FOR x = 1 TO num(station$(10))
         FOR y = 1 TO 2
            sign$ = MID$(a$(y), ((p11) + ((x - 1) * s1) + d3) - 1, 1)
            flag$ = MID$(a$(y), ((p11) + ((x - 1) * s1) + d5) - 1, 1)
            dailydata(y, x) = num(MID$(a$(y), ((p11) + ((x - 1) * s1) + d4) - 1, 5))
            IF sign$ = "-" THEN dailydata(y, x) = 0 - dailydata(y, x)
            IF (flag$ = "M") OR (dailydata(y, x) = -99999) THEN
               dailydata(y, x) = 32767
            ELSE total(y) = total(y) + dailydata(y, x)
            END IF
         NEXT y
      NEXT x
      total(1) = total(1) / num(station$(10))
      total(2) = total(2) / num(station$(10))
      FOR x = 1 TO monthday(month)
         avg = 0
         FOR y = 1 TO 2
            IF dailydata(y, x) = 32767 THEN
               dailydata(y, x) = total(y)
               PRINT CHR$(7)
               PRINT USING "##"; month;
               PRINT "/";
               PRINT USING "##"; x;
               PRINT "/";
               PRINT USING "####"; year;
               PRINT " - Missing data - ";
               IF y = 1 THEN
                  PRINT elem$;
               ELSE PRINT elem2$;
               END IF
            END IF
            avg = avg + dailydata(y, x)
         NEXT y
         IF writelen = 0 THEN
            PRINT #3, USING "#####"; year; : writelen = 1
            LOCATE 24, 2: PRINT "                                                                             ";
            LOCATE 24, 2: PRINT "Converting year ";
            PRINT USING "####"; year;
         END IF
         PRINT #3, USING "####.#"; (avg / 2);
         IF (writelen MOD 10) = 0 THEN
            PRINT #3, USING "#####"; counter: writelen = 1: counter = counter + 1
            PRINT #3, USING "#####"; year;
            PRINT ".";
         ELSE writelen = writelen + 1
         END IF
      NEXT x
      IF month = 12 THEN
         DO UNTIL (writelen MOD 10) = 1
            PRINT #3, USING "####.#"; 0;
            writelen = writelen + 1
         LOOP
         PRINT #3, USING "#####"; counter
         counter = 1
         writelen = 0
      END IF
   END IF
LOOP

CLOSE #1
CLOSE #2
CLOSE #3
END
10 :
LOCATE 24, 2: PRINT CHR$(7); "Data file is not a NOAA max. or min. file... Press any key to continue...";
123 K$ = INKEY$: IF K$ = "" THEN GOTO 123
10000 CLOSE #1
CLOSE #2
CLOSE #3
END

SUB formatlen
   p1 = 1    'location of Record-type
   p2 = 4    'location of STATION-ID 01102 (WBAN Station Numbers).
   p3 = 12   'location of ELEMENT-TYPE
   p4 = 16   'location of ELEMENT-UNITS (Left Justified).
   p5 = 18   'location of YEAR
   p6 = 22   'location of MONTH
   p7 = 24   'location of SOURCE CODE 1 (Manuscript)
   p8 = 25   'location of SOURCE CODE 2 (Manuscript)
   p9 = 26   'location of FILLER
   p10 = 28  'location of NUM-VALUES (number of data entries)
   p11 = 31  'location of the first data entry
   s1 = 12   'distance between data entry values
   d1 = 1    'relative location of DAY OF MONTH to start of data entry
   d2 = 3    'relative location of HOUR to start of data entry
   d3 = 5    'relative location of SIGN to start of data entry
   d4 = 6    'relative location of DATA VALUE to start of data entry
   d5 = 11   'relative location of FLAG-1 to start of data entry
   d6 = 12   'relative location of FLAG-2 to start of data entry
   elem$ = "": elem2$ = ""
END SUB

SUB formatmonth
monthday(1) = 31: monthday(2) = 28: monthday(3) = 31: monthday(4) = 30
monthday(5) = 31: monthday(6) = 30: monthday(7) = 31: monthday(8) = 31
monthday(9) = 30: monthday(10) = 31: monthday(11) = 30: monthday(12) = 31
END SUB

SUB io (IN$, IN2$, OT$)
dfile$ = "d:\qb\noaa\stmax.dat"
PRINT "Enter input file name #1 ["; dfile$; "] : ";
INPUT IN$
IF IN$ = "" THEN IN$ = dfile$

dfile$ = "d:\qb\noaa\stmin.dat"
PRINT "Enter input file name #2 ["; dfile$; "] : ";
INPUT IN2$
IF IN2$ = "" THEN IN2$ = dfile$

dfile$ = "d:\qb\noaa\noaa.avg"
PRINT "Enter output file name ["; dfile$; "] : ";
INPUT OT$
IF OT$ = "" THEN OT$ = dfile$
END SUB

FUNCTION leapyear (year$) STATIC
   year = num(year$)
   IF (year MOD 4) = 0 THEN
      leapyear = 1
   ELSE leapyear = 0
   END IF
END FUNCTION

FUNCTION num (convstr$) STATIC
zero = 0
FOR I = 1 TO LEN(convstr$)
   cha$ = MID$(convstr$, I, 1)
   IF (cha$ <> "") THEN cha = ASC(cha$) - ASC("0")
   IF (cha >= 0) AND (cha <= 9) THEN zero = (10 * zero) + cha
NEXT I
num = zero
END FUNCTION

SUB readin (station$(), a$)
  station$(1) = MID$(a$, p1, p2 - p1)
  station$(2) = MID$(a$, p2, p3 - p2)
  station$(3) = MID$(a$, p3, p4 - p3)
  station$(4) = MID$(a$, p4, p5 - p4)
  station$(5) = MID$(a$, p5, p6 - p5)
  station$(6) = MID$(a$, p6, p7 - p6)
  station$(7) = MID$(a$, p7, p8 - p7)
  station$(8) = MID$(a$, p8, p9 - p8)
  station$(9) = MID$(a$, p9, p10 - p9)
  station$(10) = MID$(a$, p10, p11 - p10)
  
END SUB

