DIM a$(140), b$(140), yr(12, 31), fin(370)
COLOR 15, 1
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
PRINT #3, " 6"
PRINT #3, " 1"
PRINT #3, USING "\                  \"; CITY$; STATE$
PRINT #3,
'''''''''''''''''''''''''''''''''''''''''''''
ctr = 1
year$ = "0000"
month = 12
day = 1

DO UNTIL EOF(1)
   ctr = 1
   INPUT #1, a$
   INPUT #2, b$
   IF a$ = "" THEN
      DO UNTIL a$ <> ""
         INPUT #1, a$
         INPUT #2, b$
      LOOP
   END IF
IF ((ASC(MID$(a$, 1, 1)) >= ASC("0")) AND (ASC(MID$(a$, 1, 1)) <= ASC("9"))) THEN
   month = 12
   length = LEN(a$)
   IF length > 120 THEN
      year$ = MID$(a$, 1, 4)
      'PRINT "Analyzing year "; year$; ". Please wait";
      day = 1
      INPUT #1, a$
      INPUT #2, b$
      INPUT #1, a$
      INPUT #2, b$
      xpos = 1
      ypos = 1
      length = LEN(a$)
   END IF
   'PRINT ".";
   ptr = length

   DO UNTIL (ptr <= 1) OR (month < 1)
      pwr = 1
      temp = 0
      IF (ASC(MID$(a$, ptr, 1)) >= ASC("0")) AND (ASC(MID$(a$, ptr, 1)) <= ASC("9")) OR (MID$(a$, ptr, 1) = "*") THEN
         DO UNTIL (ptr <= 1) OR (month < 1) OR (ASC(MID$(a$, ptr, 1)) < ASC("0")) OR (ASC(MID$(a$, ptr, 1)) > ASC("9"))
            temp = temp + pwr * (ASC(MID$(a$, ptr, 1)) - ASC("0"))
            pwr = pwr * 10
            ptr = ptr - 1
         LOOP
         IF MID$(a$, ptr, 1) = "-" THEN
            temp = 0 - temp
            ptr = ptr - 1
         END IF
         IF MID$(a$, ptr, 1) = "*" THEN
            temp = 32767
            DO UNTIL (MID$(a$, ptr, 1) <> "*")
               ptr = ptr - 1
            LOOP
         END IF
         yr(month, day) = temp
         month = month - 1
      END IF

      IF MID$(a$, ptr, 1) = "-" THEN
         DO UNTIL (MID$(a$, ptr, 1) <> "-")
            ptr = ptr - 1
         LOOP
         temp = 32766
         yr(month, day) = temp
         month = month - 1
      END IF

      IF MID$(a$, ptr, 1) = " " THEN ptr = ptr - 1
   LOOP

   month = 12
   ptr = length
   DO UNTIL (ptr <= 1) OR (month < 1)
      pwr = 1
      temp = 0
      IF (ASC(MID$(b$, ptr, 1)) >= ASC("0")) AND (ASC(MID$(b$, ptr, 1)) <= ASC("9")) OR (MID$(b$, ptr, 1) = "*") THEN
        
         DO UNTIL (ptr <= 1) OR (month < 1) OR (ASC(MID$(b$, ptr, 1)) < ASC("0")) OR (ASC(MID$(b$, ptr, 1)) > ASC("9"))
            temp = temp + pwr * (ASC(MID$(b$, ptr, 1)) - ASC("0"))
            pwr = pwr * 10
            ptr = ptr - 1
         LOOP
         IF MID$(b$, ptr, 1) = "-" THEN
            temp = 0 - temp
            ptr = ptr - 1
         END IF
         IF MID$(b$, ptr, 1) = "*" THEN
            temp = 32767
            DO UNTIL (MID$(b$, ptr, 1) <> "*")
               ptr = ptr - 1
            LOOP
         END IF
         yr(month, day) = (yr(month, day) + temp) / 2
         month = month - 1
      END IF
      IF MID$(b$, ptr, 1) = "-" THEN
         DO UNTIL (MID$(a$, ptr, 1) <> "-")
            ptr = ptr - 1
         LOOP
         temp = 32766
         yr(month, day) = temp
         month = month - 1
      END IF

      IF MID$(a$, ptr, 1) = " " THEN ptr = ptr - 1
      IF MID$(b$, ptr, 1) = " " THEN ptr = ptr - 1
   LOOP

   IF day = 31 THEN
      zl = 1
      'PRINT
      LOCATE 24, 2: PRINT "                                                                              ";
      LOCATE 24, 2: PRINT "Converting year "; year$;
      'PRINT "Compressing year "; year$; ". Please wait";
      FOR x = 1 TO 12
         PRINT ".";
         FOR y = 1 TO 31
            IF yr(x, y) = 32766 THEN
               monthavg = 0
               daysctr = 0
               FOR z = 1 TO 31
                  IF (yr(x, z) < 32766) THEN
                     monthavg = monthavg + yr(x, z)
                     daysctr = daysctr + 1
                  END IF
               NEXT z
               yr(x, y) = monthavg / daysctr
               'PRINT
               'PRINT "%Warning - missing data detected"
            END IF
            IF yr(x, y) <> 32767 THEN
               fin(zl) = yr(x, y)
               zl = zl + 1
               ctr = ctr + 1
            END IF
               
         NEXT y
      NEXT x
      blank = 0
      ctr = ctr - 1
      DO UNTIL ((ctr / 10) = INT(ctr / 10))
         fin(zl) = blank
         zl = zl + 1
         'PRINT ".";
         ctr = ctr + 1
      LOOP
      zm = 1
      zl = 1
      'PRINT
   '  LOCATE 23, 2: PRINT "                                                                              "
   '  LOCATE 23, 2: PRINT "Converting year "; year$;
      'PRINT "Converting year "; year$; ". Please wait";
      DO UNTIL (zl > 37)
         PRINT #3, " " + year$;
'        PRINT " " + year$;
        'PRINT ".";
         FOR zz = 1 TO 10
            PRINT #3, USING "####.#"; fin((10 * (zl - 1)) + zz);
'            PRINT USING "####.#"; fin((10 * (zl - 1)) + zz);
         NEXT zz
         PRINT #3, USING "#####"; zl
'         PRINT USING "#####"; zl
         zl = zl + 1
      LOOP
      'PRINT
   

   END IF

   day = day + 1
END IF
LOOP 'until the end of file is reached

'Close all opened files/ Program is finished
CLOSE #1
CLOSE #2
CLOSE #3
END

