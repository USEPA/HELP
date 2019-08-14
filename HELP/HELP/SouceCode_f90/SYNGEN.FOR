!      ************************* SYNGEN *************************
!
      PROGRAM SYNGEN
      IMPLICIT NONE
!*--SYNGEN6
!*** Start of declarations inserted by SPAG
      REAL acvrd , acvrw , acvtn , acvtx , alat , alpha , ar , atn ,    &
         & atx , beta , blai , ch , cvrd , cvrw , cvtn , cvtx , d1 ,    &
         & d2 , dd , dr
      REAL dt , h , hum , pw , PWD , PWW , rad , rain , RC , rcf , rg , &
         & rm , RM0 , RM1 , rmd , rmw , ro , RS0 , RS1 , s1
      REAL s2 , s3 , sd , TCFmax , TCFmin , tg , TM , tmax , tmd ,      &
         & tmin , tmw , tn , TNM , TNS , TO , TXM , TXM1 , txmd , txmw ,&
         & TXS
      REAL TXS1 , ULAt , wind , xcr1 , xcr2 , xcr3 , xcr4 , xi , xj ,   &
         & xlat , zn
      INTEGER i , i1 , i2 , ibg , icount , idays , ieg , ifg , ih ,     &
            & ihv , ikind , im , ipl , ir1 , ir2 , itype , iunits ,     &
            & iyr , j , k
      INTEGER l , l2 , leap , lr1 , lr2 , nf , ni , nii , nl , nyr ,    &
            & nyrs
!*** End of declarations inserted by SPAG
!
!     THIS ROUTINE PREPARES CLIMATOLOGICAL INPUT FILES FROM THE
!     SYNTHETIC CLIMATOLOGICAL DATA TAPE.
!     TAPE1 CONTAINS THE SYNTHETIC RAIN PARAMETERS, ALPHA AND BETA
!     DATA4 CONTAINS THE SELECTED DAILY PRECIPITATION VALUES.
!     DATA7 CONTAINS THE SELECTED AVERAGE DAILY TEMPERATURES.
!     DATA11 CONTAINS THE NAME OF THE SELECTED CITY,
!     THE EVAPORATIVE ZONE DEPTH, THE VEGETATION TYPE, MAX LAI,
!     PLANTING AND HARVESTING DATES.
!     DATA13 CONTAINS THE SELECTED SOLAR RADIATION VALUES.
!
      CHARACTER*1 dummy
      CHARACTER*4 kname(10)
      COMMON /BLK17 / TXM(366) , TXS(366) , TXM1(366) , TXS1(366) ,     &
                    & TNM(366) , TNS(366) , RM0(366) , RS0(366) ,       &
                    & RM1(366) , RS1(366) , RC(366) , TCFmax(12) ,      &
                    & TCFmin(12)
      COMMON /BLK18 / PWW(12) , PWD(12) , TM(12) , TO(12) , ULAt
      DIMENSION rm(12) , ro(12) , rad(370) , hum(4) , rg(12) , k(4) ,   &
              & alpha(12) , beta(12) , ni(12) , nii(12) , pw(12) ,      &
              & tg(12) , rcf(12) , rain(370) , tmax(370) , tmin(370)
      DATA ni/31 , 59 , 90 , 120 , 151 , 181 , 212 , 243 , 273 , 304 ,  &
         & 334 , 365/
      DATA nii/31 , 60 , 91 , 121 , 152 , 182 , 213 , 244 , 274 , 305 , &
         & 335 , 366/
      DATA k/2510 , 7692 , 2456 , 3765/
!
!
!***********************************************************************
!*    ITYPE    =  FLAG FOR TYPE OF DATA TO BE GENERATED,               *
!*                     1 FOR RAINFALL IN INCHES                        *
!*                     2 FOR TEMPERATURE IN DEGREES FAHRENHEIT         *
!*                     3 FOR SOLAR RADIATION IN LANGLEYS               *
!*    IUNITS    =  FLAG FOR UNITS OF DATA TO BE GENERATED,             *
!*                     1 FOR IN INCHES, DEGREES FAHRENHEIT & LANGLEYS  *
!*                     2 FOR CM, DEGREES CELSIUS AND MJ/M2             *
!*    NYRS     =  NUMBER OF YEARS OF DATA TO BE GENERATED              *
!*    KNAME(L) =  NAME OF CITY FOR SOURCE OF WGEN COEFFICIENTS         *
!*    ALAT     =  DEFAULT LATITUDE OF SELECTED CITY                    *
!*    TXMD     =  MEAN OF DAILY MAX TEMP FOR ALL DRY DAYS, DEGREES F   *
!*    TXMW     =  MEAN OF DAILY MAX TEMP FOR ALL WET DAYS, DEGREES F   *
!*    TN       =  MEAN OF DAILY MIN TEMP FOR ALL DAYS, DEGREES F       *
!*    ATX      =  YEARLY AMPLITUDE OF DAILY MAX TEMP, DEGREES F        *
!*    ATN      =  YEARLY AMPLITUDE OF DAILY MIN TEMP, DEGREES F        *
!*    CVTX     =  COVARIANCE OF DAILY MAX TEMP, DEGREES F              *
!*    ACVTX    =  AMPLITUDE OF COVARIANCE OF DAILY MAX TEMP, DEGREES F *
!*    CVTN     =  COVARIANCE OF DAILY MIN TEMP, DEGREES F              *
!*    ACVTN    =  AMPLITUDE OF COVARIANCE OF DAILY MIN TEMP, DEGREES F *
!*    RMD      =  MEAN OF DAILY SOLAR RAD. FOR ALL DRY DAYS, LANGLEYS  *
!*    RMW      =  MEAN OF DAILY SOLAR RAD. FOR ALL WET DAYS, LANGLEYS  *
!*    AR       =  AMPLITUDE OF DAILY SOLAR RAD. FOR DRY DAYS, LANGLEYS *
!*    ULAT     =  USER-SPECIFIED LATITUDE OF SITE                      *
!*    PWW(I)   =  PROBABILITY OF A WET DAY FOLLOWING A WET DAY         *
!*    PWD(I)   =  PROBABILITY OF A WET DAY FOLLOWING A DRY DAY         *
!*    TO(I)    =  DEFAULT NORMAL MEAN MONTHLY TEMP FOR SELECTED CITY,  *
!*                     DEGREES FAHRENHEIT                              *
!*    TM(I)    =  USER-SPECIFIED NORMAL MEAN MONTHLY TEMP FOR          *
!*                     SELECTED CITY, DEGREES FAHRENHEIT               *
!*    IPL      =  DEFAULT PLANTING DATE, START OF GROWING, JULIAN DATE *
!*    IHV      =  DEFAULT HARVEST DATE, END OF GROWING, JULIAN DATE    *
!*    BLAI     =  DEFAULT MAXIMUM LEAF AREA INDEX                      *
!*    IBG      =  DEFAULT VALUE FOR EVAPORATIVE ZONE DEPTH OF BARE     *
!*                     GROUND AT SELECTED CITY, INCHES                 *
!*    IFG      =  DEFAULT VALUE FOR EVAPORATIVE ZONE DEPTH OF FAIR     *
!*                     STAND OF GRASS AT SELECTED CITY, INCHES         *
!*    IEG      =  DEFAULT VALUE FOR EVAP. ZONE DEPTH OF EXCELLENT      *
!*                     STAND OF GRASS AT SELECTED CITY, INCHES         *
!*    ALPHA(I) =  12 MONTHLY VALUES OF GAMMA DISTRIBUTION              *
!*                     SHAPE PARAMETER                                 *
!*    BETA(I)  =  12 MONTHLY VALUES OF GAMMA DISTRIBUTION              *
!*                    SCALE PARAMETER                                  *
!*    RO(I)    =  12 MONTHLY VALUES OF DEFAULT MEAN RAINFALL, INCHES   *
!*   RM(I)    =  12 MONTHLY VALUES OF ACTUAL MEAN RAINFALL, INCHES     *
!*    WIND     =  MEAN ANNUAL WIND SPEED, MPH                          *
!*    RHUM(I)  =  4 QUARTERLY VALUES OF MEAN RELATIVE HUMIDITY, %      *
!***********************************************************************
!
      OPEN (11,FILE='SYNGEN.TMP',STATUS='OLD')
      REWIND 11
      READ (11,99011) itype
      READ (11,99011) iunits
      READ (11,99012,END=100) (kname(l),l=1,10)
      READ (11,99001) nyrs
99001 FORMAT (I4)
      IF ( itype.EQ.1 ) THEN
         IF ( iunits.EQ.1 ) READ (11,99013,END=100) (rm(i),i=1,12)
         IF ( iunits.EQ.2 ) READ (11,99016,END=100) (rm(i),i=1,12)
      ENDIF
      IF ( itype.EQ.2 ) READ (11,99016,END=100) (TM(i),i=1,12)
      IF ( itype.EQ.3 ) READ (11,99014) ULAt
      READ (11,99002,END=100) alat , txmd , txmw , tn , atx , atn ,     &
                            & cvtx , acvtx , cvtn , acvtn
99002 FORMAT (6F7.2,4F7.3)
      READ (11,99003,END=100) rmd , rmw , ar
99003 FORMAT (3F6.1)
      READ (11,99015,END=100) (PWW(i),i=1,12)
      READ (11,99015,END=100) (PWD(i),i=1,12)
      READ (11,99016,END=100) (TO(i),i=1,12)
      READ (11,99004,END=100) ipl , ihv , blai , ibg , ifg , ieg
99004 FORMAT (2I4,F4.1,3I4)
      READ (11,99015,END=100) (alpha(i),i=1,12)
      READ (11,99015,END=100) (beta(i),i=1,12)
      READ (11,99013,END=100) (ro(i),i=1,12)
      READ (11,99005,END=100) wind , (hum(i),i=1,4)
99005 FORMAT (5F5.1)
 100  CLOSE (11)
      ih = 0
      ikind = 2
      OPEN (4,FILE='DATA4.TMP',STATUS='UNKNOWN')
      REWIND 4
!
      IF ( itype.EQ.1 ) THEN
         WRITE (4,99011) ikind
         WRITE (4,99011) iunits
         WRITE (4,99012) (kname(l),l=1,10)
         IF ( iunits.EQ.1 ) WRITE (4,99013) (rm(i),i=1,12)
         IF ( iunits.EQ.2 ) WRITE (4,99016) (rm(i),i=1,12)
         DO im = 1 , 12
            IF ( iunits.EQ.2 ) rm(im) = rm(im)/25.4
            pw(im) = PWD(im)/(1.-PWW(im)+PWD(im))
            nl = ni(im)
            IF ( im.EQ.1 ) THEN
               nf = 1
            ELSE
               nf = ni(im-1) + 1
            ENDIF
            zn = nl - nf + 1
!*****CALCULATE MONTHLY RAINFALL CORRECTION FACTOR
            rg(im) = alpha(im)*beta(im)*zn*pw(im)
            rcf(im) = rm(im)/rg(im)
         ENDDO
      ENDIF
!
      IF ( itype.EQ.2 ) THEN
         IF ( TM(1).GT.TM(7) ) ih = 1
         OPEN (7,FILE='DATA7.TMP',STATUS='UNKNOWN')
         REWIND 7
         WRITE (7,99011) ikind
         WRITE (7,99011) iunits
         WRITE (7,99012) (kname(l),l=1,10)
         WRITE (7,99016) (TM(i),i=1,12)
         d1 = txmd - txmw
         DO j = 1 , 366
            xj = j
            dt = COS(.0172*(xj-200.+(ih*183)))
            TXM(j) = txmd + atx*dt
            xcr1 = cvtx + acvtx*dt
            IF ( xcr1.LT.0.0 ) xcr1 = 0.06
            TXS(j) = TXM(j)*xcr1
            TXM1(j) = TXM(j) - d1
            TXS1(j) = TXM1(j)*xcr1
            TNM(j) = tn + atn*dt
            xcr2 = cvtn + acvtn*dt
            IF ( xcr2.LT.0.0 ) xcr2 = 0.06
            TNS(j) = TNM(j)*xcr2
         ENDDO
         DO im = 1 , 12
            IF ( iunits.EQ.2 ) TM(im) = (1.8*TM(im)) + 32.0
            TCFmax(im) = 0.0
            TCFmin(im) = 0.0
            pw(im) = PWD(im)/(1.-PWW(im)+PWD(im))
            s1 = 0.
            s2 = 0.
            s3 = 0.
            nl = ni(im)
            IF ( im.EQ.1 ) THEN
               nf = 1
            ELSE
               nf = ni(im-1) + 1
            ENDIF
            zn = nl - nf + 1
            DO j = nf , nl
               s1 = s1 + TXM(j)/zn
               s2 = s2 + TXM1(j)/zn
               s3 = s3 + TNM(j)/zn
            ENDDO
!*****CALCULATE MONTHLY TEMP CORRECTION FACTOR
            tmd = (s1+s3)/2.
            tmw = (s2+s3)/2.
            tg(im) = tmw*pw(im) + tmd*(1-pw(im))
            TCFmax(im) = TM(im) - tg(im)
            TCFmin(im) = TCFmax(im)
         ENDDO
      ENDIF
!
      IF ( itype.EQ.3 ) THEN
         IF ( ULAt.LT.0. ) ih = 1
         OPEN (13,FILE='DATA13.TMP',STATUS='UNKNOWN')
         REWIND 13
         WRITE (13,99011) ikind
         WRITE (13,99011) iunits
         WRITE (13,99012) (kname(l),l=1,10)
         WRITE (13,99014) ULAt
         cvrd = 0.24
         acvrd = -0.08
         cvrw = 0.48
         acvrw = -0.13
         d2 = rmd - rmw
!***** CALCULATE MAXIMUM SOLAR RADIATION FOR EACH DAY
         xlat = ABS(ULAt)*6.2832/360.
         DO i = 1 , 366
            xi = i
            sd = 0.4102*SIN(0.0172*(xi-80.25+(ih*183)))
            ch = -TAN(xlat)*TAN(sd)
            IF ( ch.GT.1.0 ) h = 0.
            IF ( ch.LE.1.0 ) THEN
               IF ( ch.LT.-1.0 ) h = 3.1416
               IF ( ch.GE.-1.0 ) h = ACOS(ch)
            ENDIF
            dd = 1.0 + 0.0335*SIN(0.0172*(xi+88.2+(ih*183)))
            RC(i) = 889.2305*dd*((h*SIN(xlat)*SIN(sd))+(COS(xlat)*COS(sd&
                  & )*SIN(h)))
            RC(i) = RC(i)*0.8
         ENDDO
         DO j = 1 , 366
            xj = j
            dr = COS(.0172*(xj-172.+(ih*183)))
            RM0(j) = rmd + ar*dr
            xcr3 = cvrd + acvrd*dr
            IF ( xcr3.LT.0.0 ) xcr3 = 0.06
            RS0(j) = RM0(j)*xcr3
            RM1(j) = RM0(j) - d2
            xcr4 = cvrw + acvrw*dr
            IF ( xcr4.LT.0.0 ) xcr4 = 0.06
            RS1(j) = RM1(j)*xcr4
         ENDDO
      ENDIF
!
!*******BEGINNING OF DAILY CALCULATIONS LOOP
!
      idays = 365
      leap = 0
      DO iyr = 1 , nyrs
         nyr = iyr
         leap = leap + 1
         IF ( leap.EQ.4 ) idays = 366
         IF ( leap.GT.4 ) leap = leap - 4
         IF ( itype.EQ.1 ) CALL SYRN(PWW,PWD,alpha,beta,rain,rcf,idays, &
                                   & ni,nii,k)
         IF ( itype.GT.1 ) THEN
            IF ( iyr.EQ.1 ) THEN
               READ (4,99017) dummy
               READ (4,99017) dummy
               READ (4,99017) dummy
               READ (4,99017) dummy
            ENDIF
            DO l = 1 , 37
               lr1 = 10*l - 9
               lr2 = 10*l
               IF ( l.EQ.1 ) READ (4,99006) nyr , (rain(l2),l2=lr1,lr2)
99006          FORMAT (I10,10F5.0)
               IF ( l.GT.1 ) READ (4,99007) (rain(l2),l2=lr1,lr2)
99007          FORMAT (10X,10F5.0)
            ENDDO
            idays = 365
            IF ( MOD(nyr,4).EQ.0 ) idays = 366
            CALL DAILY(rain,tmax,tmin,rad,idays,k)
         ENDIF
         DO i1 = 1 , 37
            ir1 = 10*i1 - 9
            ir2 = 10*i1
            icount = i1
            IF ( itype.EQ.1 ) THEN
               IF ( iunits.EQ.2 ) THEN
                  DO i2 = ir1 , ir2
                     rain(i2) = rain(i2)*25.4
                  ENDDO
                  WRITE (4,99008) nyr , (rain(i2),i2=ir1,ir2) , icount
99008             FORMAT (I10,10F5.1,I10)
               ELSE
                  WRITE (4,99009) nyr , (rain(i2),i2=ir1,ir2) , icount
99009             FORMAT (I10,10F5.2,I10)
               ENDIF
            ENDIF
            IF ( itype.EQ.2 ) THEN
               IF ( iunits.EQ.2 ) THEN
                  DO i2 = ir1 , ir2
                     tmax(i2) = (tmax(i2)-32.0)/1.8
                     tmin(i2) = (tmin(i2)-32.0)/1.8
                  ENDDO
               ENDIF
               WRITE (7,99018) nyr , ((tmax(i)+tmin(i))/2.,i=ir1,ir2) , &
                             & icount
            ENDIF
            IF ( itype.EQ.3 ) THEN
               IF ( iunits.EQ.2 ) THEN
                  DO i2 = ir1 , ir2
                     rad(i2) = rad(i2)*0.04186
                  ENDDO
               ENDIF
               IF ( iunits.EQ.1 ) WRITE (13,99018) nyr ,                &
                  & (rad(i),i=ir1,ir2) , icount
               IF ( iunits.EQ.2 ) WRITE (13,99010) nyr ,                &
                  & (rad(i),i=ir1,ir2) , icount
99010          FORMAT (I5,10F6.2,I5)
            ENDIF
         ENDDO
      ENDDO
      CLOSE (4)
      IF ( itype.EQ.2 ) CLOSE (7)
      IF ( itype.EQ.3 ) CLOSE (13)
99011 FORMAT (I2)
99012 FORMAT (10A4)
99013 FORMAT (12F6.2)
99014 FORMAT (F6.2)
99015 FORMAT (12F6.3)
99016 FORMAT (12F6.1)
99017 FORMAT (A1)
99018 FORMAT (I5,10F6.1,I5)
!
      END
!*==SYRN.spg  processed by SPAG 6.72Dc at 11:24 on 14 Aug 2019
!************************SYRN*******************
!
!*****THE FOLLOWING SUBROUTINE GENERATES DAILY PRECIPITATION FOR
!*****ONE YEAR.
      SUBROUTINE SYRN(Pww,Pwd,Alpha,Beta,Rain,Rcf,Idays,Ni,Nii,K)
      IMPLICIT NONE
!*--SYRN342
!*** Start of declarations inserted by SPAG
      REAL aa , ab , Alpha , Beta , Pwd , Pww , Rain , Rcf , rn , rn1 , &
         & rn2 , rn3 , s1 , s12 , s2 , tr1 , tr2 , z
      INTEGER iday , Idays , im , ip , K , Ni , Nii
!*** End of declarations inserted by SPAG
      DIMENSION Rain(370) , Pww(12) , Pwd(12) , Alpha(12) , Beta(12) ,  &
              & Ni(12) , Nii(12) , Rcf(12) , K(4)
      SAVE ip
      DATA ip/0/
      im = 1
      DO iday = 1 , Idays
         IF ( Idays.EQ.366 ) THEN
            IF ( iday.GT.Nii(im) ) im = im + 1
         ELSE
            IF ( iday.GT.Ni(im) ) im = im + 1
         ENDIF
!*****DETERMINE WET OR DRY DAY USING MARKOV CHAIN MODEL
         CALL RANDN(rn,K)
         IF ( ip.LE.0 ) THEN
            IF ( rn.LE.Pwd(im) ) GOTO 50
         ELSEIF ( rn.LE.Pww(im) ) THEN
            GOTO 50
         ENDIF
         ip = 0
         Rain(iday) = 0.
         GOTO 200
 50      ip = 1
!*****DETERMINE RAINFALL AMOUNT FOR WET DAYS USING GAMMA DISTRIBUTION
         aa = 1./Alpha(im)
         ab = 1./(1.-Alpha(im))
         tr1 = EXP(-18.42/aa)
         tr2 = EXP(-18.42/ab)
 100     CALL RANDN(rn1,K)
         CALL RANDN(rn2,K)
         IF ( rn1.LE.tr1 ) THEN
            s1 = 0.
         ELSE
            s1 = rn1**aa
         ENDIF
         IF ( rn2.LE.tr2 ) THEN
            s2 = 0.
         ELSE
            s2 = rn2**ab
         ENDIF
         s12 = s1 + s2
         IF ( s12.GT.1. ) GOTO 100
         z = s1/s12
         CALL RANDN(rn3,K)
         Rain(iday) = -z*ALOG(rn3)*Beta(im)*Rcf(im)
 200  ENDDO
      END
!*==DAILY.spg  processed by SPAG 6.72Dc at 11:24 on 14 Aug 2019
!***********************DAILY*************************
!
!*****THE FOLLOWING SUBROUTINE GENERATED DAILY TEMPERATURES AND
!*****RADIATION FOR ONE YEAR.
!
      SUBROUTINE DAILY(Rain,Tmax,Tmin,Rad,Idays,K)
      IMPLICIT NONE
!*--DAILY402
!*** Start of declarations inserted by SPAG
      REAL a , b , e , r , Rad , Rain , RC , rm , RM0 , RM1 , rmin ,    &
         & rn1 , rn2 , rr , rs , RS0 , RS1 , TCFmax , TCFmin , Tmax
      REAL Tmin , tmm , TNM , TNS , TXM , TXM1 , TXS , TXS1 , txxm ,    &
         & txxs , v , x , xim1
      INTEGER i , iday , Idays , im , ip , j , K , kk , ni , nii
!*** End of declarations inserted by SPAG
      COMMON /BLK17 / TXM(366) , TXS(366) , TXM1(366) , TXS1(366) ,     &
                    & TNM(366) , TNS(366) , RM0(366) , RS0(366) ,       &
                    & RM1(366) , RS1(366) , RC(366) , TCFmax(12) ,      &
                    & TCFmin(12)
      DIMENSION Rain(370) , Tmax(370) , Tmin(370) , Rad(370) , a(3,3) , &
              & b(3,3) , K(4) , xim1(3) , e(3) , r(3) , x(3) , rr(3) ,  &
              & ni(12) , nii(12)
      DATA a/0.567 , 0.253 , -0.006 , 0.086 , 0.504 , -0.039 , -0.002 , &
         & -0.050 , 0.244/
      DATA b/0.781 , 0.328 , 0.238 , 0.0 , 0.637 , -0.341 , 0.0 , 0.0 , &
         & 0.873/
      DATA ni/31 , 59 , 90 , 120 , 151 , 181 , 212 , 243 , 273 , 304 ,  &
         & 334 , 365/
      DATA nii/31 , 60 , 91 , 121 , 152 , 182 , 213 , 244 , 274 , 305 , &
         & 335 , 366/
      DATA xim1/0. , 0. , 0./
      DATA ip/0/
      im = 1
      DO iday = 1 , Idays
         IF ( Idays.EQ.366 ) THEN
            IF ( iday.GT.nii(im) ) im = im + 1
         ELSE
            IF ( iday.GT.ni(im) ) im = im + 1
         ENDIF
         IF ( Rain(iday).LE.0 ) THEN
            ip = 0
         ELSE
            ip = 1
         ENDIF
         IF ( ip.LT.1 ) THEN
!*****GENERATE TMAX, TMIN, AND RAD FOR IDAY
            rm = RM0(iday)
            rs = RS0(iday)
            txxm = TXM(iday)
            txxs = TXS(iday)
         ELSE
            rm = RM1(iday)
            rs = RS1(iday)
            txxm = TXM1(iday)
            txxs = TXS1(iday)
         ENDIF
         DO kk = 1 , 3
 20         CALL RANDN(rn1,K)
            CALL RANDN(rn2,K)
            v = SQRT(-2.*ALOG(rn1))*COS(6.283185*rn2)
            IF ( ABS(v).GT.2.5 ) GOTO 20
            e(kk) = v
         ENDDO
         DO i = 1 , 3
            r(i) = 0.
            rr(i) = 0.
         ENDDO
         DO i = 1 , 3
            DO j = 1 , 3
               r(i) = r(i) + b(i,j)*e(j)
               rr(i) = rr(i) + a(i,j)*xim1(j)
            ENDDO
         ENDDO
         DO kk = 1 , 3
            x(kk) = r(kk) + rr(kk)
            xim1(kk) = x(kk)
         ENDDO
         Tmax(iday) = x(1)*txxs + txxm
         Tmin(iday) = x(2)*TNS(iday) + TNM(iday)
         IF ( Tmin(iday).GT.Tmax(iday) ) THEN
            tmm = Tmax(iday)
            Tmax(iday) = Tmin(iday)
            Tmin(iday) = tmm
         ENDIF
!*****TMAX(IDAY) IS GENERATED TMAX FOR IDAY
!*****TMIN(IDAY) IS GENERATED TMIN FOR IDAY
         Tmax(iday) = Tmax(iday) + TCFmax(im)
         Tmin(iday) = Tmin(iday) + TCFmin(im)
         Rad(iday) = x(3)*rs + rm
         rmin = 0.2*RC(iday)
!*****RAD(IDAY) IS GENERATED RAD FOR IDAY
         IF ( Rad(iday).LT.rmin ) Rad(iday) = rmin
         IF ( Rad(iday).GT.RC(iday) ) Rad(iday) = RC(iday)
      ENDDO
      END
!*==RANDN.spg  processed by SPAG 6.72Dc at 11:24 on 14 Aug 2019
!********************** RANDN ************************
!
!*****THE FOLLOWING SUBROUTINE GENERATES A UNIFORM RANDOM NUMBER ON
!*****THE INTERVAL 0 - 1
      SUBROUTINE RANDN(Yfl,K)
      IMPLICIT NONE
!*--RANDN497
!*** Start of declarations inserted by SPAG
      INTEGER i , K
      REAL Yfl
!*** End of declarations inserted by SPAG
      DIMENSION K(4)
      K(4) = 3*K(4) + K(2)
      K(3) = 3*K(3) + K(1)
      K(2) = 3*K(2)
      K(1) = 3*K(1)
      i = K(1)/1000
      K(1) = K(1) - i*1000
      K(2) = K(2) + i
      i = K(2)/100
      K(2) = K(2) - 100*i
      K(3) = K(3) + i
      i = K(3)/1000
      K(3) = K(3) - i*1000
      K(4) = K(4) + i
      i = K(4)/100
      K(4) = K(4) - 100*i
      Yfl = (((FLOAT(K(1))*.001+FLOAT(K(2)))*.01+FLOAT(K(3)))           &
          & *.001+FLOAT(K(4)))*.01
      END
