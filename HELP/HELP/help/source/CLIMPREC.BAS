COLOR 15, 1
DIM yr$(12, 31), total(15)
blank$ = " 0.00"
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
         IF length >= 120 THEN
            year$ = MID$(a$, 1, 4)
            month = 1
            LOCATE 23, 2: PRINT "                                                                              "
            LOCATE 23, 2: PRINT "Converting year "; year$;
         END IF
         IF length < 120 THEN
             FOR x = 1 TO length
                 c$ = MID$(a$, x, 1)
                 day$ = "    "
                 IF MID$(a$, x, 1) = "." THEN day$ = MID$(a$, x - 2, 5): month = month + 1
                 IF MID$(a$, x, 1) = "e" THEN day$ = " 0.00": month = month + 1
                 IF MID$(a$, x, 1) = "*" THEN day$ = "n": x = x + 6: month = month + 1
                 IF MID$(a$, x, 1) = "-" THEN
                    day$ = "m": x = x + 6
                    month = month + 1
                    SOUND 500, .5
                   ' PRINT
                  '  COLOR 15
                    'PRINT "Warning";
                 '   COLOR 7
                    'PRINT " - Missing data on ";
                    'PRINT USING "##"; month - 1;
                    'PRINT "/";
                    'PRINT USING "##"; day;
                    'PRINT "/";
                    'PRINT year$
                 END IF
                 IF (c$ = ".") OR (c$ = "e") OR (c$ = "*") OR (c$ = "-") THEN
                    yr$(month - 1, day) = day$
                 END IF
             NEXT x
             month = 1
             PRINT ".";
            day = day + 1
         END IF
      END IF
   END IF
   missptr = 0
   threecycle = 1
  
   IF day = 32 THEN
      month = 1
     'PRINT
      'PRINT "Writing year "; year$; ". Please wait";
      INPUT #1, a$
      INPUT #1, a$
      length = LEN(a$)
      FOR x = 1 TO length
         c$ = MID$(a$, x, 1)
         day$ = "    "
         IF MID$(a$, x, 1) = "." THEN day$ = MID$(a$, x - 2, 5): month = month + 1
         IF (c$ = ".") THEN
            z$ = day$
            IF z$ = "" THEN z$ = "00.00"
            l1 = ASC(MID$(z$, 1, 1)) - ASC("0")
            l2 = ASC(MID$(z$, 2, 1)) - ASC("0")
            r1 = ASC(MID$(z$, 4, 1)) - ASC("0")
            r2 = ASC(MID$(z$, 5, 1)) - ASC("0")
            IF (l1 < 0) OR (l1 > 9) THEN l1 = 0
            IF (l2 < 0) OR (l2 > 9) THEN l2 = 0
            IF (r1 < 0) OR (r1 > 9) THEN r1 = 0
            IF (r2 < 0) OR (r2 > 9) THEN r2 = 0
            value = (10 * l1) + (l2) + (r1 * (1 / 10)) + (r2 * (1 / 100))
            total(month - 1) = value
            'PRINT month - 1; " ";
            'PRINT USING "##.##"; value;
         END IF
      NEXT x
' end obtaining yearly totals
' calculations and output done now


      day = 1
      month = 1
      INPUT #1, a$
      ptrctr = 1
      FOR x = 1 TO 12
         Missing = 0
         missptr = 0
       '  PRINT ".";
         FOR y = 1 TO 31
            IF (ptrctr = 1) AND (yr$(x, y) <> "n") THEN
               PRINT #2, "      " + year$;
               'PRINT "      " + year$;
            END IF
            IF (yr$(x, y) <> "n") AND (yr$(x, y) <> "m") THEN
               PRINT #2, yr$(x, y);
               'PRINT yr$(x, y);
               ptrctr = ptrctr + 1
            END IF
            IF (yr$(x, y) = "m") AND (missptr = 1) THEN
               IF ((threecycle / 3) = INT(threecycle / 3)) THEN
                  'PRINT USING "##.##"; missing;
                  PRINT #2, USING "##.##"; Missing;
                  ptrctr = ptrctr + 1
               END IF
               IF ((threecycle / 3) <> INT(threecycle / 3)) THEN
                  'PRINT USING "##.##"; 0;
                  PRINT #2, USING "##.##"; 0;
                  ptrctr = ptrctr + 1
               END IF
               threecycle = threecycle + 1
            END IF
            IF (yr$(x, y) = "m") AND (missptr = 0) THEN 'calculate comp
               ALL1 = 0
               ctr = 0
               missptr = 1
               FOR z = 1 TO 31
                 IF yr$(x, z) = "m" THEN ctr = ctr + 1
                 IF (yr$(x, z) <> "n") AND (yr$(x, z) <> "m") THEN
                    z$ = yr$(x, z)
                    l1 = ASC(MID$(z$, 1, 1)) - ASC("0")
                    l2 = ASC(MID$(z$, 2, 1)) - ASC("0")
                    r1 = ASC(MID$(z$, 4, 1)) - ASC("0")
                    r2 = ASC(MID$(z$, 5, 1)) - ASC("0")
                    IF (l1 < 0) OR (l1 > 9) THEN l1 = 0
                    IF (l2 < 0) OR (l2 > 9) THEN l2 = 0
                    IF (r1 < 0) OR (r1 > 9) THEN r1 = 0
                    IF (r2 < 0) OR (r2 > 9) THEN r2 = 0
                    value = (10 * l1) + (l2) + (r1 * (1 / 10)) + (r2 * (1 / 100))
                    ALL1 = ALL1 + value
                 END IF
               NEXT z
               oldall = ALL1
               ALL1 = total(x) - ALL1
               atot% = INT((ALL1 + .001) * 100)
               group% = INT((ctr / 3) + .8)
               Missing% = INT(atot% / group%)
               er% = atot% - (Missing% * group%)
               tot% = er% + Missing%
               tot = (tot% / 100) + .001
               Missing = (Missing% / 100) + .001
               'PRINT USING "##.##"; (tot);
               ptrctr = ptrctr + 1
               PRINT #2, USING "##.##"; tot;
               'PRINT oldall, ALL1
               'PRINT "ALL1 : "; ALL1; "  total : "; total(x), x, ctr
            END IF
            IF ptrctr = 11 THEN
               ptrctr = 1
               'PRINT USING "##########"; linectr
               PRINT #2, USING "##########"; linectr
               linectr = linectr + 1
            END IF
         NEXT y
      NEXT
   DO UNTIL ptrctr = 11
      ptrctr = ptrctr + 1
      'PRINT USING "##.##"; 0;
      PRINT #2, USING "##.##"; 0;
   LOOP
   'PRINT USING "##########"; linectr
   PRINT #2, USING "##########"; linectr
   linectr = 1
  ' PRINT
   END IF
LOOP
CLOSE #1
CLOSE #2

END

