COLOR 15, 1
DIM yr$(12, 31), total(15), total$(15)
yearloss$ = "false" '    A 30 days of a given months has missing values (when true)
'''''''''''''''''''''''''''''''''''''''''''
'READ INPUT AND OUTPUT FILES THAT ARE
'PASSED FROM HELPI IN FILE "PASSFILE.DATA"
'THEN DELETE THE FILE
'''''''''''''''''''''''''''''''''''''''''''
OPEN "PASSNAME.DAT" FOR INPUT AS #1
INPUT #1, CITY$
INPUT #1, STATE$
INPUT #1, IN$
INPUT #1, OT$
CLOSE #1
KILL "PASSNAME.DAT"
'CITY$ = "PHOENIX"
'STATE$ = "AZ"
'IN$ = "AZ2.PRT"
'OT$ = "AZ2.OUT"
'''''''''''''''''''''''''''''''''''''''''''''
OPEN IN$ FOR INPUT AS #1
OPEN OT$ FOR OUTPUT AS #2
'''''''''''''''''''''''''''''''''''''''''''''
'PRINT CLIMATE DATA FILE HEADER LINE
'''''''''''''''''''''''''''''''''''''''''''''
PRINT #2, " 6"
PRINT #2, " 1"
PRINT #2, USING "\                  \"; CITY$; STATE$
PRINT #2,
'''''''''''''''''''''''''''''''''''''''''''''
linectr = 1
day = 1
DO UNTIL EOF(1)
   wrtptr = 1
   INPUT #1, a$
   IF a$ <> "" THEN
      length = LEN(a$)
      value = ASC(MID$(a$, 1, 1))
      IF (value >= ASC("0")) AND (value <= ASC("9")) THEN
         IF length >= 89 THEN
            year$ = MID$(a$, 1, 4)
            month = 1
        '   LOCATE 22, 2: PRINT "                                           "
            LOCATE 23, 2: PRINT "Converting year "; year$;
         END IF
         IF length < 89 AND length > 76 THEN
             FOR x = 1 TO length
                 c$ = MID$(a$, x, 1)
                 day$ = "      "
                 IF MID$(a$, x, 1) = "." THEN day$ = MID$(a$, x - 4, 6): month = month + 1
                 IF MID$(a$, x, 1) = "-" THEN
                    '                   For months ending with 28 days
                    IF month = 2 AND day >= 29 THEN
                      
                       '                Check for leap year status and missing
                       year = VAL(year$)
                       IF year MOD 4 = 0 AND day = 29 THEN
                          day$ = "m": x = x + 4
                       ELSE
                          day$ = "n": x = x + 4
                       END IF
                       month = month + 1
                    ELSEIF day > 30 AND (month = 4 OR month = 6 OR month = 9 OR month = 11) THEN
                       day$ = "n": x = x + 4
                       month = month + 1
                    ELSE
                       day$ = "m": x = x + 4
                       month = month + 1
 '                      PRINT
                     '  COLOR 15
                       LOCATE 22, 1: PRINT "Warning -- Missing values encounted in month"; month - 1; " "; year$; ".                "
                     '  COLOR 7
                          
                    END IF
                 END IF
                 IF (c$ = ".") OR (c$ = "-") THEN
                    yr$(month - 1, day) = day$
                 END IF
             NEXT x
             month = 1
'             PRINT ".";
            day = day + 1
         END IF
      END IF
   END IF
  
   IF day = 32 THEN
      month = 1
      ON ERROR GOTO 100      'test for end of file
     
      INPUT #1, a$
      INPUT #1, a$
      INPUT #1, a$
      INPUT #1, a$
'
'     Find monthly avg and place it into missing value address.
'
      length = LEN(a$)
      FOR x = 1 TO length
         c$ = MID$(a$, x, 1)
         day$ = "      "
         IF MID$(a$, x, 1) = "." THEN day$ = MID$(a$, x - 3, 6): month = month + 1
        
         IF MID$(a$, x, 1) = "-" THEN
            SOUND 500, .5
            LOCATE 22, 1: PRINT "          All values are missing for month "; month; " "; year$; ".      "
            LOCATE 23, 1: PRINT " This year will not be converted.   Press any key to continue..."
            DO
           
            LOOP WHILE INKEY$ = ""
            LOCATE 22, 1: PRINT "                                                                         "
            LOCATE 23, 1: PRINT "                                                                "

            yearloss$ = "true"
            x = length
            day = 1
            month = 1
            linectr = 1
            ptrctr = 1
         END IF

         IF (c$ = ".") THEN
             '         shift this day string so that the blank will be a the begining rather than the end.
             shift$ = " " + MID$(day$, 1, 5)
             total$(month - 1) = shift$
         END IF
      NEXT x
' end obtaining yearly totals
' calculations and output done now

      IF yearloss$ <> "true" THEN
         day = 1
         month = 1
         ptrctr = 1
         FOR x = 1 TO 12
'            PRINT ".";
            FOR y = 1 TO 31
               IF (yr$(x, y) = "m") THEN
                 yr$(x, y) = total$(x)
               END IF
           
               IF ptrctr = 11 THEN
                  ptrctr = 1
                  PRINT #2, USING "#####"; linectr
                  linectr = linectr + 1
               END IF

               ' n denote that the month end on the 28th or the 30th day.
               IF (ptrctr = 1) AND (yr$(x, y) <> "n") THEN
                  PRINT #2, " " + year$;
               END IF
           
               ' m denote that this is a missing value.
               IF (yr$(x, y) <> "n") THEN
'
'This is where we convert the values to langley's per day
'
                    PRINT #2, USING "####.#"; VAL(yr$(x, y)) * (24 / 11.63);

'This next line prints values in units of w-h/m2

'                   PRINT #2, (yr$(x, y));
                  ptrctr = ptrctr + 1
               END IF
            NEXT y
         NEXT
  
      IF linectr = 37 THEN
         '                          check for leap year
         year = VAL(year$)
         IF year MOD 4 = 0 THEN
            PRINT #2, "   0.0   0.0   0.0   0.0   37"
         ELSE
            PRINT #2, "   0.0   0.0   0.0   0.0   0.0   37"
         END IF

      ELSE
         PRINT #2, USING "#####"; linectr
      END IF
      linectr = 1
  
      END IF
   END IF

   '  Close files if form feed FF is read from the data file.
      f$ = MID$(a$, 1, 1)

   yearloss$ = "false"
LOOP
100 CLOSE #1
CLOSE #2

END

