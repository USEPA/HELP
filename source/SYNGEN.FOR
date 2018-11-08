C      ************************* SYNGEN *************************
C
      PROGRAM    SYNGEN
C
C     THIS ROUTINE PREPARES CLIMATOLOGICAL INPUT FILES FROM THE
C     SYNTHETIC CLIMATOLOGICAL DATA TAPE.
C     TAPE1 CONTAINS THE SYNTHETIC RAIN PARAMETERS, ALPHA AND BETA
C     DATA4 CONTAINS THE SELECTED DAILY PRECIPITATION VALUES.
C     DATA7 CONTAINS THE SELECTED AVERAGE DAILY TEMPERATURES.
C     DATA11 CONTAINS THE NAME OF THE SELECTED CITY,
C     THE EVAPORATIVE ZONE DEPTH, THE VEGETATION TYPE, MAX LAI,
C     PLANTING AND HARVESTING DATES.
C     DATA13 CONTAINS THE SELECTED SOLAR RADIATION VALUES.
C
      CHARACTER*1 DUMMY
      CHARACTER*4  KNAME (10)
      COMMON /BLK17/ TXM (366), TXS (366), TXM1 (366), TXS1 (366),
     1   TNM (366), TNS (366), RM0 (366), RS0 (366), RM1 (366),
     2   RS1 (366), RC (366), TCFMAX (12), TCFMIN (12)
      COMMON /BLK18/ PWW (12), PWD (12), TM (12), TO (12), ULAT
      DIMENSION RM (12), RO (12), RAD (370), HUM (4), RG (12), K (4),
     1   VALUE (12), ALPHA (12), BETA (12), NI (12), NII (12),
     2   PW (12), TG (12), RCF (12), RAIN (370), TMAX (370), TMIN (370)
      DATA NI/31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365/
      DATA NII/31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366/
      DATA K/2510, 7692, 2456, 3765/
C
C
C***********************************************************************
C*    ITYPE    =  FLAG FOR TYPE OF DATA TO BE GENERATED,               *
C*                     1 FOR RAINFALL IN INCHES                        *
C*                     2 FOR TEMPERATURE IN DEGREES FAHRENHEIT         *
C*                     3 FOR SOLAR RADIATION IN LANGLEYS               *
C*    IUNITS    =  FLAG FOR UNITS OF DATA TO BE GENERATED,             *
C*                     1 FOR IN INCHES, DEGREES FAHRENHEIT & LANGLEYS  *
C*                     2 FOR CM, DEGREES CELSIUS AND MJ/M2             *
C*    NYRS     =  NUMBER OF YEARS OF DATA TO BE GENERATED              *
C*    KNAME(L) =  NAME OF CITY FOR SOURCE OF WGEN COEFFICIENTS         *
C*    ALAT     =  DEFAULT LATITUDE OF SELECTED CITY                    *
C*    TXMD     =  MEAN OF DAILY MAX TEMP FOR ALL DRY DAYS, DEGREES F   *
C*    TXMW     =  MEAN OF DAILY MAX TEMP FOR ALL WET DAYS, DEGREES F   *
C*    TN       =  MEAN OF DAILY MIN TEMP FOR ALL DAYS, DEGREES F       *
C*    ATX      =  YEARLY AMPLITUDE OF DAILY MAX TEMP, DEGREES F        *
C*    ATN      =  YEARLY AMPLITUDE OF DAILY MIN TEMP, DEGREES F        *
C*    CVTX     =  COVARIANCE OF DAILY MAX TEMP, DEGREES F              *
C*    ACVTX    =  AMPLITUDE OF COVARIANCE OF DAILY MAX TEMP, DEGREES F *
C*    CVTN     =  COVARIANCE OF DAILY MIN TEMP, DEGREES F              *
C*    ACVTN    =  AMPLITUDE OF COVARIANCE OF DAILY MIN TEMP, DEGREES F *
C*    RMD      =  MEAN OF DAILY SOLAR RAD. FOR ALL DRY DAYS, LANGLEYS  *
C*    RMW      =  MEAN OF DAILY SOLAR RAD. FOR ALL WET DAYS, LANGLEYS  *
C*    AR       =  AMPLITUDE OF DAILY SOLAR RAD. FOR DRY DAYS, LANGLEYS *
C*    ULAT     =  USER-SPECIFIED LATITUDE OF SITE                      *
C*    PWW(I)   =  PROBABILITY OF A WET DAY FOLLOWING A WET DAY         *
C*    PWD(I)   =  PROBABILITY OF A WET DAY FOLLOWING A DRY DAY         *
C*    TO(I)    =  DEFAULT NORMAL MEAN MONTHLY TEMP FOR SELECTED CITY,  *
C*                     DEGREES FAHRENHEIT                              *
C*    TM(I)    =  USER-SPECIFIED NORMAL MEAN MONTHLY TEMP FOR          *
C*                     SELECTED CITY, DEGREES FAHRENHEIT               *
C*    IPL      =  DEFAULT PLANTING DATE, START OF GROWING, JULIAN DATE *
C*    IHV      =  DEFAULT HARVEST DATE, END OF GROWING, JULIAN DATE    *
C*    BLAI     =  DEFAULT MAXIMUM LEAF AREA INDEX                      *
C*    IBG      =  DEFAULT VALUE FOR EVAPORATIVE ZONE DEPTH OF BARE     *
C*                     GROUND AT SELECTED CITY, INCHES                 *
C*    IFG      =  DEFAULT VALUE FOR EVAPORATIVE ZONE DEPTH OF FAIR     *
C*                     STAND OF GRASS AT SELECTED CITY, INCHES         *
C*    IEG      =  DEFAULT VALUE FOR EVAP. ZONE DEPTH OF EXCELLENT      *
C*                     STAND OF GRASS AT SELECTED CITY, INCHES         *
C*    ALPHA(I) =  12 MONTHLY VALUES OF GAMMA DISTRIBUTION              *
C*                     SHAPE PARAMETER                                 *
C*    BETA(I)  =  12 MONTHLY VALUES OF GAMMA DISTRIBUTION              *
C*                    SCALE PARAMETER                                  *
C*    RO(I)    =  12 MONTHLY VALUES OF DEFAULT MEAN RAINFALL, INCHES   *
C*   RM(I)    =  12 MONTHLY VALUES OF ACTUAL MEAN RAINFALL, INCHES     *
C*    WIND     =  MEAN ANNUAL WIND SPEED, MPH                          *
C*    RHUM(I)  =  4 QUARTERLY VALUES OF MEAN RELATIVE HUMIDITY, %      *
C***********************************************************************
C
      OPEN(11,FILE='SYNGEN.TMP',STATUS='OLD')
      REWIND 11
      READ (11, 5000) ITYPE
      READ (11, 5000) IUNITS
      READ (11, 5010, END = 1200) (KNAME (L), L = 1, 10)
      READ (11, 5020) NYRS
      IF (ITYPE .EQ. 1) THEN
      IF(IUNITS.EQ.1)READ (11, 5030, END = 1200) (RM (I), I = 1, 12)
      IF(IUNITS.EQ.2)READ (11, 5070, END = 1200) (RM (I), I = 1, 12)
      END IF
      IF(ITYPE.EQ.2)READ (11, 5070, END = 1200) (TM (I), I = 1, 12)
      IF(ITYPE.EQ.3)READ (11, 5031) ULAT
      READ (11, 5040, END = 1200) ALAT, TXMD, TXMW, TN, ATX,
     1   ATN, CVTX, ACVTX, CVTN, ACVTN
      READ (11, 5050, END = 1200) RMD, RMW, AR
      READ (11, 5060, END = 1200) (PWW (I), I = 1, 12)
      READ (11, 5060, END = 1200) (PWD (I), I = 1, 12)
      READ (11, 5070, END = 1200) (TO (I), I = 1, 12)
      READ (11, 5080, END = 1200) IPL, IHV, BLAI, IBG, IFG, IEG
      READ (11, 5060, END = 1200) (ALPHA (I), I = 1, 12)
      READ (11, 5060, END = 1200) (BETA (I), I = 1, 12)
      READ (11, 5030, END = 1200) (RO (I), I = 1, 12)
      READ (11, 5090, END = 1200) WIND, (HUM (I), I = 1, 4)
 5000 FORMAT(I2)
 5010 FORMAT(10A4)
 5020 FORMAT(I4)
 5030 FORMAT(12F6.2)
 5031 FORMAT(F6.2)
 5040 FORMAT(6F7.2,4F7.3)
 5050 FORMAT(3F6.1)
 5060 FORMAT(12F6.3)
 5070 FORMAT(12F6.1)
 5080 FORMAT(2I4,F4.1,3I4)
 5090 FORMAT(5F5.1)
 1200 CLOSE (11)
      IH = 0
      IKIND = 2
      OPEN (4, FILE = 'DATA4.TMP', STATUS = 'UNKNOWN')
      REWIND 4
C
      IF (ITYPE .EQ. 1) THEN
         WRITE (4, 5000) IKIND
         WRITE (4, 5000) IUNITS
         WRITE (4, 5010) (KNAME (L), L = 1, 10)
         IF(IUNITS.EQ.1)WRITE (4, 5030) (RM (I), I = 1, 12)
         IF(IUNITS.EQ.2)WRITE (4, 5070) (RM (I), I = 1, 12)
         DO 1150 IM = 1, 12
            IF (IUNITS .EQ. 2) RM (IM) = RM (IM)/25.4
            PW (IM) = PWD (IM)/(1. - PWW (IM) + PWD (IM))
            NL = NI (IM)
            IF (IM .EQ. 1) GO TO 1160
            NF = NI (IM - 1) + 1
            GO TO 1170
 1160       CONTINUE
            NF = 1
 1170       CONTINUE
            ZN = NL - NF + 1
C*****CALCULATE MONTHLY RAINFALL CORRECTION FACTOR
            RG (IM) = ALPHA (IM)*BETA (IM)*ZN*PW (IM)
            RCF (IM) = RM (IM)/RG (IM)
 1150    CONTINUE
      END IF
C
      IF(ITYPE.EQ.2)THEN
         IF (TM(1) .GT. TM(7)) IH = 1         
         OPEN (7, FILE = 'DATA7.TMP', STATUS = 'UNKNOWN')
         REWIND 7
         WRITE (7, 5000) IKIND
         WRITE (7, 5000) IUNITS
         WRITE (7, 5010) (KNAME (L), L = 1, 10)
         WRITE (7, 5070) (TM (I), I = 1, 12)
         D1 = TXMD - TXMW
      DO J = 1, 366
         XJ = J
         DT = COS (.0172*(XJ - 200. + (IH * 183)))
         TXM (J) = TXMD + ATX*DT
         XCR1 = CVTX + ACVTX*DT
         IF (XCR1 .LT. 0.0) XCR1 = 0.06
         TXS (J) = TXM (J)*XCR1
         TXM1 (J) = TXM (J) - D1
         TXS1 (J) = TXM1 (J)*XCR1
         TNM (J) = TN + ATN*DT
         XCR2 = CVTN + ACVTN*DT
         IF (XCR2 .LT. 0.0) XCR2 = 0.06
         TNS (J) = TNM (J)*XCR2
      end do
      DO 2160 IM = 1, 12
         IF (IUNITS .EQ. 2) TM (IM) = (1.8 * TM (IM)) + 32.0
         TCFMAX (IM) = 0.0
         TCFMIN (IM) = 0.0
         PW (IM) = PWD (IM)/(1. - PWW (IM) + PWD (IM))
         S1 = 0.
         S2 = 0.
         S3 = 0.
         NL = NI (IM)
         IF (IM .EQ. 1) GO TO 2170
         NF = NI (IM - 1) + 1
         GO TO 2180
 2170    CONTINUE
         NF = 1
 2180    CONTINUE
         ZN = NL - NF + 1
         DO 2190 J = NF, NL
            S1 = S1 + TXM (J)/ZN
            S2 = S2 + TXM1 (J)/ZN
            S3 = S3 + TNM (J)/ZN
 2190    CONTINUE
C*****CALCULATE MONTHLY TEMP CORRECTION FACTOR
         TMD = (S1 + S3)/2.
         TMW = (S2 + S3)/2.
         TG (IM) = TMW*PW (IM) + TMD*(1 - PW (IM))
         TCFMAX (IM) = TM (IM) - TG (IM)
         TCFMIN (IM) = TCFMAX (IM)
 2160 CONTINUE
      ENDIF
C
      IF(ITYPE.EQ.3)THEN
         IF (ULAT .LT. 0.) IH = 1
         OPEN (13, FILE = 'DATA13.TMP', STATUS = 'UNKNOWN')
         REWIND 13
         WRITE (13, 5000) IKIND
         WRITE (13, 5000) IUNITS
         WRITE (13, 5010) (KNAME (L), L = 1, 10)
         WRITE (13, 5031) ULAT
      CVRD = 0.24
      ACVRD = - 0.08
      CVRW = 0.48
      ACVRW = - 0.13
      D2 = RMD - RMW
C***** CALCULATE MAXIMUM SOLAR RADIATION FOR EACH DAY
      XLAT = ABS(ULAT)*6.2832/360.
      DO 3130 I = 1, 366
         XI = I
         SD = 0.4102*SIN (0.0172*(XI - 80.25 + (IH * 183)))
         CH = - TAN (XLAT)*TAN (SD)
         IF (CH .GT. 1.0) H = 0.
         IF (CH .GT. 1.0) GO TO 3140
         IF (CH .LT. - 1.0) H = 3.1416
         IF (CH .LT. - 1.0) GO TO 3140
         H = ACOS (CH)
 3140    CONTINUE
         DD = 1.0 + 0.0335*SIN (0.0172*(XI + 88.2 + (IH * 183)))
         RC (I) = 889.2305*DD*((H*SIN (XLAT)*SIN (SD)) +
     1      (COS (XLAT)*COS (SD)*SIN (H)
     2      ))
         RC (I) = RC (I)*0.8
 3130 CONTINUE
      DO 3150 J = 1, 366
         XJ = J
         DR = COS (.0172*(XJ - 172. + (IH * 183)))
         RM0 (J) = RMD + AR*DR
         XCR3 = CVRD + ACVRD*DR
         IF (XCR3 .LT. 0.0) XCR3 = 0.06
         RS0 (J) = RM0 (J)*XCR3
         RM1 (J) = RM0 (J) - D2
         XCR4 = CVRW + ACVRW*DR
         IF (XCR4 .LT. 0.0) XCR4 = 0.06
         RS1 (J) = RM1 (J)*XCR4
 3150 CONTINUE
      END IF
c
C*******BEGINNING OF DAILY CALCULATIONS LOOP
c
      IDAYS = 365
      LEAP = 0
      DO 1180 IYR = 1, NYRS
         NYR = IYR
         LEAP = LEAP + 1
         IF(LEAP.EQ.4)IDAYS=366
         IF(LEAP.GT.4)LEAP= LEAP-4
         IF(ITYPE.EQ.1)CALL SYRN (PWW, PWD, ALPHA, BETA, RAIN,
     1     RCF, IDAYS, NI, NII, K)
         IF(ITYPE.GT.1) THEN
            IF(IYR.EQ.1) THEN
               READ(4,1181)DUMMY
               READ(4,1181)DUMMY
               READ(4,1181)DUMMY
               READ(4,1181)DUMMY
 1181          FORMAT(A1)
            END IF
            DO 1050 L = 1, 37
               LR1 = 10*L - 9
               LR2 = 10*L
               IF(L.EQ.1)READ(4,5500)NYR,(RAIN(L2),L2=LR1,LR2)
 5500          FORMAT(I10,10F5.0)
               IF(L.GT.1)READ(4,5510)(RAIN(L2),L2=LR1,LR2)
 5510          FORMAT(10X,10F5.0)
 1050       CONTINUE
            IDAYS=365
            IF(MOD(NYR,4).EQ.0)IDAYS=366
            CALL DAILY (RAIN, TMAX, TMIN, RAD, IDAYS, K)
         END IF
         DO 1190 I1 = 1, 37
            IR1 = 10*I1 - 9
            IR2 = 10*I1
            ICOUNT = I1
            IF(ITYPE.EQ.1)THEN
               IF(IUNITS.EQ.2)THEN
                  DO 1191 I2 = IR1, IR2
                     RAIN (I2) = RAIN (I2) * 25.4
 1191             CONTINUE
                  WRITE(4,6200)NYR,(RAIN(I2),I2=IR1,IR2),ICOUNT
               else
                  WRITE(4,6201)NYR,(RAIN(I2),I2=IR1,IR2),ICOUNT
               END IF
            END IF
 6200       FORMAT(I10,10F5.1,I10)
 6201       FORMAT(I10,10F5.2,I10)
            IF(ITYPE.EQ.2)THEN
               IF(IUNITS.EQ.2)THEN
                  DO 1192 I2 = IR1, IR2
                     TMAX (I2) = (TMAX (I2) - 32.0) / 1.8
                     TMIN (I2) = (TMIN (I2) - 32.0) / 1.8
 1192             CONTINUE
               END IF
               WRITE(7,6210) NYR,((TMAX (I) + TMIN (I))/2.,
     1         I = IR1, IR2), ICOUNT
            END IF
 6210       FORMAT(I5,10F6.1,I5)
            IF(ITYPE.EQ.3)THEN
               IF(IUNITS.EQ.2)THEN
                  DO 1193 I2 = IR1, IR2
                     RAD (I2) = RAD (I2) * 0.04186
 1193             CONTINUE
               END IF
        IF(IUNITS.EQ.1)WRITE(13,6210) NYR,(RAD(I), I=IR1,IR2),ICOUNT
        IF(IUNITS.EQ.2)WRITE(13,6220) NYR,(RAD(I), I=IR1,IR2),ICOUNT
 6220       FORMAT(I5,10F6.2,I5)
            END IF
 1190    CONTINUE
 1180 CONTINUE
      CLOSE (4)
      IF(ITYPE.EQ.2)CLOSE (7)
      IF(ITYPE.EQ.3)CLOSE (13)
C
      STOP
      END
C************************SYRN*******************
C
C*****THE FOLLOWING SUBROUTINE GENERATES DAILY PRECIPITATION FOR
C*****ONE YEAR.
      SUBROUTINE SYRN (PWW,PWD,ALPHA,BETA,RAIN,RCF,IDAYS,NI,NII,K)
      DIMENSION RAIN (370), PWW (12), PWD (12), ALPHA (12), BETA (12),
     1   NI (12), NII (12), RCF (12), K (4)
      SAVE IP
      DATA IP/0/
      IM = 1
      DO 1000 IDAY = 1, IDAYS
         IF (IDAYS .EQ. 366) GO TO 1010
         IF (IDAY .GT. NI (IM)) IM = IM + 1
         GO TO 1020
 1010    CONTINUE
         IF (IDAY .GT. NII (IM)) IM = IM + 1
 1020    CONTINUE
C*****DETERMINE WET OR DRY DAY USING MARKOV CHAIN MODEL
         CALL RANDN (RN,K)
         IF (IP - 0) 1030, 1030, 1040
 1030    CONTINUE
         IF (RN - PWD (IM)) 1050, 1050, 1060
 1060    CONTINUE
         IP = 0
         RAIN (IDAY) = 0.
         GO TO 1000
 1040    CONTINUE
         IF (RN - PWW (IM)) 1050, 1050, 1060
 1050    CONTINUE
         IP = 1
C*****DETERMINE RAINFALL AMOUNT FOR WET DAYS USING GAMMA DISTRIBUTION
         AA = 1./ALPHA (IM)
         AB = 1./(1. - ALPHA (IM))
         TR1 = EXP ( - 18.42/AA)
         TR2 = EXP ( - 18.42/AB)
 1070    CONTINUE
         CALL RANDN (RN1,K)
         CALL RANDN (RN2,K)
         IF (RN1 - TR1) 1080, 1080, 1090
 1080    CONTINUE
         S1 = 0.
         GO TO 1100
 1090    CONTINUE
         S1 = RN1**AA
 1100    CONTINUE
         IF (RN2 - TR2) 1110, 1110, 1120
 1110    CONTINUE
         S2 = 0.
         GO TO 1130
 1120    CONTINUE
         S2 = RN2**AB
 1130    CONTINUE
         S12 = S1 + S2
         IF (S12 - 1.) 1140, 1140, 1070
 1140    CONTINUE
         Z = S1/S12
         CALL RANDN (RN3,K)
         RAIN (IDAY) = - Z*ALOG (RN3)*BETA (IM)*RCF (IM)
 1000 CONTINUE
      END
C***********************DAILY*************************
C
C*****THE FOLLOWING SUBROUTINE GENERATED DAILY TEMPERATURES AND
C*****RADIATION FOR ONE YEAR.
C
      SUBROUTINE DAILY (RAIN, TMAX, TMIN, RAD, IDAYS, K)
      COMMON /BLK17/ TXM (366), TXS (366), TXM1 (366),
     1    TXS1 (366), TNM (366),
     2   TNS (366), RM0 (366), RS0 (366), RM1 (366), RS1 (366),
     3   RC (366), TCFMAX (12), TCFMIN (12)
      DIMENSION RAIN (370), TMAX (370), TMIN (370), RAD (370),
     1   A (3, 3), B (3, 3), K (4),
     2   XIM1 (3), E (3), R (3), X (3), RR (3), NI (12), NII (12)
      DATA A/0.567, 0.253, - 0.006, 0.086, 0.504, - 0.
     1   039, - 0.002, - 0.050, 0.244/
      DATA B/0.781, 0.328, 0.238, 0.0, 0.637, - 0.341, 0.0, 0.0, 0.873/
      DATA NI/31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365/
      DATA NII/31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366/
      DATA XIM1/0., 0., 0./
      DATA IP/0/
      IM = 1
      DO 1000 IDAY = 1, IDAYS
         IF (IDAYS .EQ. 366) GO TO 1010
         IF (IDAY .GT. NI (IM)) IM = IM + 1
         GO TO 1020
 1010    CONTINUE
         IF (IDAY .GT. NII (IM)) IM = IM + 1
 1020    CONTINUE
         IF (RAIN (IDAY)) 1030, 1030, 1040
 1030    CONTINUE
         IP = 0
         GO TO 1050
 1040    CONTINUE
         IP = 1
 1050    CONTINUE
         IF (IP - 1) 1060, 1070, 1070
C*****GENERATE TMAX, TMIN, AND RAD FOR IDAY
 1060    CONTINUE
         RM = RM0 (IDAY)
         RS = RS0 (IDAY)
         TXXM = TXM (IDAY)
         TXXS = TXS (IDAY)
         GO TO 1080
 1070    CONTINUE
         RM = RM1 (IDAY)
         RS = RS1 (IDAY)
         TXXM = TXM1 (IDAY)
         TXXS = TXS1 (IDAY)
 1080    CONTINUE
         DO 1090 KK = 1, 3
 1100       CONTINUE
            CALL RANDN (RN1,K)
            CALL RANDN (RN2,K)
            V = SQRT ( - 2.*ALOG (RN1))*COS (6.283185*RN2)
            IF (ABS (V) .GT. 2.5) GO TO 1100
            E (KK) = V
 1090    CONTINUE
         DO 1110 I = 1, 3
            R (I) = 0.
            RR (I) = 0.
 1110    CONTINUE
         DO 1120 I = 1, 3
            DO 1120 J = 1, 3
               R (I) = R (I) + B (I, J)*E (J)
               RR (I) = RR (I) + A (I, J)*XIM1 (J)
 1120    CONTINUE
         DO 1130 KK = 1, 3
            X (KK) = R (KK) + RR (KK)
            XIM1 (KK) = X (KK)
 1130    CONTINUE
         TMAX (IDAY) = X (1)*TXXS + TXXM
         TMIN (IDAY) = X (2)*TNS (IDAY) + TNM (IDAY)
         IF (TMIN (IDAY) .GT. TMAX (IDAY)) GO TO 1140
         GO TO 1150
 1140    CONTINUE
         TMM = TMAX (IDAY)
         TMAX (IDAY) = TMIN (IDAY)
         TMIN (IDAY) = TMM
 1150    CONTINUE
C*****TMAX(IDAY) IS GENERATED TMAX FOR IDAY
C*****TMIN(IDAY) IS GENERATED TMIN FOR IDAY
         TMAX (IDAY) = TMAX (IDAY) + TCFMAX (IM)
         TMIN (IDAY) = TMIN (IDAY) + TCFMIN (IM)
         RAD (IDAY) = X (3)*RS + RM
         RMIN = 0.2*RC (IDAY)
C*****RAD(IDAY) IS GENERATED RAD FOR IDAY
         IF (RAD (IDAY) .LT. RMIN) RAD (IDAY) = RMIN
         IF (RAD (IDAY) .GT. RC (IDAY)) RAD (IDAY) = RC (IDAY)
 1000 CONTINUE
      RETURN
      END
C********************** RANDN ************************
C
C*****THE FOLLOWING SUBROUTINE GENERATES A UNIFORM RANDOM NUMBER ON
C*****THE INTERVAL 0 - 1
      SUBROUTINE RANDN (YFL,K)
      DIMENSION K (4)
      K (4) = 3*K (4) + K (2)
      K (3) = 3*K (3) + K (1)
      K (2) = 3*K (2)
      K (1) = 3*K (1)
      I = K (1)/1000
      K (1) = K (1) - I*1000
      K (2) = K (2) + I
      I = K (2)/100
      K (2) = K (2) - 100*I
      K (3) = K (3) + I
      I = K (3)/1000
      K (3) = K (3) - I*1000
      K (4) = K (4) + I
      I = K (4)/100
      K (4) = K (4) - 100*I
      YFL = (((FLOAT (K (1))*.001 + FLOAT (K (2)))*.01 +
     1   FLOAT (K (3)))*.001 + FLOAT
     2   (K (4)))*.01
      RETURN
      END
