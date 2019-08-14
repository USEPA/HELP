! *****************************************************************
      SUBROUTINE ACELL(Nrow,Nbstart,Ns,Txt,Nl,Nh,Nwrite)
      IMPLICIT NONE
!*--ACELL5
!*** Start of declarations inserted by SPAG
      INTEGER*2 i , iblnk , icol , N0 , N1 , N2 , N32 , N64 , N8 , na , &
              & nb , Nbstart , nc , nf , Nh , Nl , nltmp , Nrow , Ns ,  &
              & Nwrite
!*** End of declarations inserted by SPAG
      PARAMETER (N0=0,N1=1,N2=2,N8=8,N32=32,N64=64)
      CHARACTER Txt*80 , xlm*1(80) , blnk*1
      LOGICAL first
      first = .TRUE.
      iblnk = N32
      blnk = CHAR(N32)
      Nl = N0
      Nh = N0
      na = Nrow
      DO icol = N1 , Ns
         nb = Nbstart - N1 + icol
         nf = N8 + N32
         CALL UTLTY(nf,na,nb,Nl,Nh,nc,Txt)
         nf = N2 + N8 + N64
         CALL UTLTY(nf,na,nb,Nl,Nh,nc,Txt)
         xlm(icol) = CHAR(Nl)
      ENDDO
      IF ( Ns.LT.80 ) THEN
         DO icol = Ns + 1 , 80
            xlm(icol) = blnk
         ENDDO
      ENDIF
      icol = N1
 100  nf = N8 + 128
      nb = Nbstart + icol - N1
      CALL UTLTY(nf,na,nb,Nl,Nh,nc,Txt)
      IF ( Nwrite.GT.N0 ) THEN
         IF ( Nl.GE.33 .AND. Nl.LE.122 ) THEN
            nf = N2 + N8 + N64
            IF ( first ) THEN
               DO i = N1 , Ns
                  nb = Nbstart + i - N1
                  CALL UTLTY(nf,na,nb,iblnk,Nh,nc,Txt)
                  xlm(i) = blnk
               ENDDO
            ENDIF
            nb = Nbstart + icol - N1
            CALL UTLTY(nf,na,nb,Nl,Nh,nc,Txt)
            xlm(icol) = CHAR(Nl)
            icol = icol + N1
            IF ( icol.GT.Ns ) icol = Ns
         ELSEIF ( Nl.EQ.N8 .AND. Nh.EQ.14 ) THEN
            icol = icol - N1
            IF ( icol.LT.N1 ) THEN
               icol = N1
            ELSE
               DO i = icol , Ns
                  nf = N8 + N32
                  nb = Nbstart + i
                  CALL UTLTY(nf,na,nb,Nl,Nh,nc,Txt)
                  nf = N2 + N8 + N64
                  nb = nb - N1
                  CALL UTLTY(nf,na,nb,Nl,Nh,nc,Txt)
                  xlm(i) = CHAR(Nl)
               ENDDO
               xlm(Ns) = blnk
               nb = Nbstart + Ns - N1
               CALL UTLTY(nf,na,nb,iblnk,Nh,nc,Txt)
            ENDIF
         ELSEIF ( Nl.EQ.N0 .AND. Nh.EQ.77 ) THEN
            icol = icol + N1
            IF ( icol.GT.Ns ) icol = N1
         ELSEIF ( Nl.EQ.N0 .AND. Nh.EQ.75 ) THEN
            icol = icol - N1
            IF ( icol.LT.N1 ) icol = Ns
         ELSEIF ( Nl.EQ.N32 ) THEN
            icol = icol + N1
            IF ( icol.GT.Ns ) THEN
               icol = Ns
            ELSE
               DO i = Ns , icol , -N1
                  nf = N8 + N32
                  nb = Nbstart + i - N2
                  CALL UTLTY(nf,na,nb,Nl,Nh,nc,Txt)
                  nf = N2 + N8 + N64
                  nb = nb + N1
                  CALL UTLTY(nf,na,nb,Nl,Nh,nc,Txt)
                  xlm(i) = CHAR(Nl)
               ENDDO
               xlm(icol-N1) = blnk
               nb = icol + Nbstart - N2
               CALL UTLTY(nf,na,nb,iblnk,Nh,nc,Txt)
            ENDIF
         ENDIF
      ENDIF
      first = .FALSE.
      IF ( Nl.EQ.13 .OR. Nl.EQ.42 .OR.                                  &
         & (Nl.EQ.0 .AND. (Nh.EQ.79 .OR. Nh.EQ.72 .OR. Nh.EQ.73 .OR.    &
         & Nh.EQ.80 .OR. Nh.EQ.81 .OR. Nh.EQ.82 .OR. Nh.EQ.83 .OR.      &
         & Nh.EQ.71 .OR. Nh.EQ.1)) .OR. Nl.EQ.27 ) THEN
!
!        ... Change cell to normal video
!
         nf = 4 + 8 + 64
         DO icol = 1 , Ns
            nb = Nbstart - 1 + icol
            nltmp = ICHAR(xlm(icol))
            CALL UTLTY(nf,na,nb,nltmp,Nh,nc,Txt)
         ENDDO
         Txt = xlm(1)//xlm(2)//xlm(3)//xlm(4)//xlm(5)//xlm(6)//xlm(7)   &
             & //xlm(8)//xlm(9)//xlm(10)//xlm(11)//xlm(12)//xlm(13)     &
             & //xlm(14)//xlm(15)//xlm(16)//xlm(17)//xlm(18)//xlm(19)   &
             & //xlm(20)//xlm(21)//xlm(22)//xlm(23)//xlm(24)//xlm(25)   &
             & //xlm(26)//xlm(27)//xlm(28)//xlm(29)//xlm(30)//xlm(31)   &
             & //xlm(32)//xlm(33)//xlm(34)//xlm(35)//xlm(36)//xlm(37)   &
             & //xlm(38)//xlm(39)//xlm(40)//xlm(41)//xlm(42)//xlm(43)   &
             & //xlm(44)//xlm(45)//xlm(46)//xlm(47)//xlm(48)//xlm(49)   &
             & //xlm(50)//xlm(51)//xlm(52)//xlm(53)//xlm(54)//xlm(55)   &
             & //xlm(56)//xlm(57)//xlm(58)//xlm(59)//xlm(60)//xlm(61)   &
             & //xlm(62)//xlm(63)//xlm(64)//xlm(65)//xlm(66)//xlm(67)   &
             & //xlm(68)//xlm(69)//xlm(70)//xlm(71)//xlm(72)//xlm(73)   &
             & //xlm(74)//xlm(75)//xlm(76)//xlm(77)//xlm(78)//xlm(79)   &
             & //xlm(80)
         RETURN
      ENDIF
      GOTO 100
      END
!*==CELL.spg  processed by SPAG 6.72Dc at 11:14 on 14 Aug 2019
! *****************************************************************
      SUBROUTINE CELL(Na,Nbstart,Ns,Nl,Nh,Vari,No,Txt)
      IMPLICIT NONE
!*--CELL132
!*** Start of declarations inserted by SPAG
      INTEGER*2 icol , icol1 , itemp , j , N0 , N1 , N2 , N64 , N8 ,    &
              & Na , nb , Nbstart , nc , nf , Nh , Nl , nltmp , No , Ns
      REAL VAR1 , Vari
!*** End of declarations inserted by SPAG
      PARAMETER (N0=0,N1=1,N2=2,N8=8,N64=64)
      CHARACTER Txt*80
      CHARACTER*1 xlm(80)
      icol1 = Ns
      DO icol = N1 , Ns
         nb = Nbstart - N1 + icol
         nf = N8 + 32
         CALL UTLTY(nf,Na,nb,Nl,Nh,nc,Txt)
         nf = N2 + N64
         CALL UTLTY(nf,Na,nb,Nl,Nh,nc,Txt)
         xlm(icol) = CHAR(Nl)
      ENDDO
      DO icol = Ns , N1 , -N1
         IF ( ICHAR(xlm(icol)).LT.46 .OR. ICHAR(xlm(icol)).GT.57 )      &
            & GOTO 100
      ENDDO
      IF ( icol.LE.N1 .OR. icol.GT.Ns ) icol = N1
 100  icol = icol + N1
      IF ( icol.GT.Ns ) icol = Ns
 200  nf = N8 + 128
      nb = Nbstart - N1 + icol
      IF ( nb.GT.Nbstart-N1+Ns ) THEN
         nb = Nbstart - N1 + Ns
         icol = Ns
      ENDIF
      CALL UTLTY(nf,Na,nb,Nl,Nh,nc,Txt)
      IF ( (Nl.GE.48 .AND. Nl.LE.57) .OR. Nl.EQ.46 .OR. Nl.EQ.32 .OR.   &
         & Nl.EQ.43 .OR. Nl.EQ.45 .OR. Nl.EQ.69 .OR. Nl.EQ.101 ) THEN
         itemp = Nl
         IF ( icol.LT.Ns ) icol1 = icol
         IF ( icol.EQ.Ns ) THEN
            IF ( icol1.GT.Ns ) THEN
               DO j = N2 , Ns
                  xlm(j-N1) = xlm(j)
               ENDDO
               nf = N2 + N8 + N64
               DO icol = N1 , Ns - N1
                  Nl = ICHAR(xlm(icol))
                  nb = Nbstart - N1 + icol
                  CALL UTLTY(nf,Na,nb,Nl,Nh,nc,Txt)
               ENDDO
               icol = Ns
            ENDIF
         ENDIF
         Nl = itemp
         xlm(icol) = CHAR(Nl)
         nf = N2 + N8 + N64
         nb = Nbstart - N1 + icol
         CALL UTLTY(nf,Na,nb,Nl,Nh,nc,Txt)
         IF ( icol.LT.Ns ) icol = icol + N1
         icol1 = icol1 + N1
         GOTO 200
      ELSEIF ( Nl.EQ.N8 .AND. Nh.EQ.14 ) THEN
         Nl = ICHAR(' ')
         nf = N2 + N8 + N64
         CALL UTLTY(nf,Na,nb,Nl,Nh,nc,Txt)
         xlm(icol) = ' '
         icol = icol - N1
         IF ( icol.LT.N1 ) icol = N1
         GOTO 200
      ELSEIF ( Nl.EQ.N0 .AND. Nh.EQ.77 ) THEN
         icol = icol + N1
         IF ( icol.GT.Ns ) icol = N1
         GOTO 200
      ELSEIF ( Nl.EQ.N0 .AND. Nh.EQ.75 ) THEN
         icol = icol - N1
         IF ( icol.LT.N1 ) icol = Ns
         GOTO 200
      ELSEIF ( Nl.EQ.13 .OR. Nl.EQ.9 .OR. Nl.EQ.42 .OR.                 &
             & (Nl.EQ.27 .AND. Nh.EQ.1) .OR.                            &
             & (Nl.EQ.N0 .AND. (Nh.GE.71 .AND. Nh.LE.83) .OR.           &
             & Nh.EQ.15 .OR. Nh.EQ.115 .OR. Nh.EQ.116) ) THEN
         Txt = ' '
!
!        ... Change cell to normal video
!
         nf = 4 + 8 + 64
         DO icol = 1 , Ns
            nb = Nbstart - 1 + icol
            nltmp = ICHAR(xlm(icol))
            CALL UTLTY(nf,Na,nb,nltmp,Nh,nc,Txt)
         ENDDO
         DO icol = N1 , Ns
            Txt(icol:icol) = xlm(icol)
         ENDDO
         Vari = VAR1(Txt)
      ELSE
         GOTO 200
      ENDIF
      END
!*==VAR1.spg  processed by SPAG 6.72Dc at 11:14 on 14 Aug 2019
! *****************************************************************
      FUNCTION VAR1(Var)
      IMPLICIT NONE
!*--VAR1232
!*** Start of declarations inserted by SPAG
      REAL rnum , sign , VAR1
!*** End of declarations inserted by SPAG
      PARAMETER (N0=0,N1=1,N10=10)
      INTEGER*2 N0 , N1 , N10 , nexp , k , i , ib , ie , ix , ixs , j , &
              & ip
      CHARACTER Var*80 , minus*1 , plus*1 , period*1 , blk*1 , num(N10) &
              & *1 , e*1 , ee*1
      DATA num , minus , plus , period , blk , e , ee/'0' , '1' , '2' , &
          &'3' , '4' , '5' , '6' , '7' , '8' , '9' , '-' , '+' , '.' ,  &
          &' ' , 'E' , 'e'/
      sign = 1.
      rnum = 0.
      nexp = N0
      k = N1
      DO i = N1 , 80
         IF ( Var(i:i).NE.blk ) GOTO 100
         k = k + N1
      ENDDO
      VAR1 = rnum
      RETURN
 100  ib = k
      DO i = ib , 80
         IF ( Var(i:i).EQ.blk ) GOTO 200
         k = k + N1
      ENDDO
 200  ie = k - N1
      IF ( Var(ib:ib).NE.minus ) THEN
         IF ( Var(ib:ib).EQ.plus ) ib = ib + N1
      ELSE
         ib = ib + N1
         sign = -sign
      ENDIF
      DO i = ie , ib , -N1
         IF ( Var(i:i).EQ.e .OR. Var(i:i).EQ.ee ) GOTO 300
      ENDDO
      GOTO 500
 300  ixs = N1
      ix = i
      i = i + N1
      IF ( Var(i:i).EQ.minus ) THEN
         ixs = -ixs
         i = i + N1
      ENDIF
      IF ( Var(i:i).EQ.plus ) i = i + N1
      DO j = ie , i , -N1
         DO k = N1 , N10
            IF ( Var(j:j).EQ.num(k) ) GOTO 400
         ENDDO
         nexp = nexp + (k-N1)*N10**(ie-j)
 400  ENDDO
      nexp = nexp*ixs
      ie = ix - N1
 500  DO i = ib , ie
         IF ( Var(i:i).EQ.period ) GOTO 600
      ENDDO
      ie = i
 600  ip = i
      k = N1
      DO i = ib , ie
         DO j = N1 , N10
            IF ( Var(i:i).EQ.num(j) ) GOTO 650
         ENDDO
         k = N0
         GOTO 700
 650     rnum = rnum + FLOAT(j-N1)*10.**(ip-i-k)
 700  ENDDO
      VAR1 = rnum*sign
      IF ( nexp.NE.N0 ) VAR1 = sign*rnum*10.**nexp
      END
!*==AGAIN.spg  processed by SPAG 6.72Dc at 11:14 on 14 Aug 2019
! *****************************************************************
      SUBROUTINE AGAIN(Iagain)
!     *************************************
!     * This subroutine allows the user   *
!     * to decide whether or not he wants *
!     * to enter another year of data.    *
!     *************************************
!
!     COMMON /FILENM/ FNAME
!     CHARACTER FNAME*40
      IMPLICIT NONE
!*--AGAIN315
!*** Start of declarations inserted by SPAG
      INTEGER*2 Iagain , ianswer , na , nb , nbstart , nc , nf , nh , nl
      REAL txt
!*** End of declarations inserted by SPAG
      Iagain = 0
!
!     ... Clear screen and write yes-no prompt
!
      nf = 8 + 256
      na = 9
      nb = 12
      CALL UTLTY(nf,na,nb,nl,nh,nc,txt)
      WRITE (6,99001)
99001 FORMAT (                                                          &
     &'               ���������������'',/,  &
     &                                       'Doyouwishtoenteranother   &
     &''',/,                                                          ' &
    & yearofdatatothisdataset'YesNo                                     &
     &''',/,                                                          ''&
     &'',/,                                                          '''&
     &',/,                                                          ''�&
     &�')
      na = 12
      nbstart = 49
      CALL YESORNO(ianswer,na,nbstart,nl,nh)
      IF ( ianswer.EQ.1 ) Iagain = 1
      END
      ! *****************************************************************
            SUBROUTINE APPEND(Iagain)
      !     ***************************************
      !     * This subroutine allows the user     *
      !     * to decide whether or not he wants   *
      !     * to append data to existing edit file*
      !     ***************************************
      !
      !     COMMON /FILENM/ FNAME
      !     CHARACTER FNAME*40
            IMPLICIT NONE
      !*--APPEND13
      !*** Start of declarations inserted by SPAG
            INTEGER*2 Iagain , ianswer , na , nb , nbstart , nc , nf , nh , nl
            REAL txt
      !*** End of declarations inserted by SPAG
            Iagain = 0
      !
      !     ... Clear screen and write yes-no prompt
      !
            nf = 8 + 256
            na = 9
            nb = 12
            CALL UTLTY(nf,na,nb,nl,nh,nc,txt)
            WRITE (6,99001)
      99001 FORMAT (                                                          &
           &'               ���������������'',/,  &
           &                                       'Datafilealreadyexistsin   &
           &''',/,                                                          ' &
          & theeditfile.DoyouwishYesNo                                        &
           &''',/,                                                          ' &
          & toappenddatatoit'                                                 &
           &''',/,                                                          ''&
           &'',/,                                                          ''�&
           &��')
            na = 12
            nbstart = 49
            CALL YESORNO(ianswer,na,nbstart,nl,nh)
            IF ( ianswer.EQ.1 ) Iagain = 1
            END
      !*==KEEPER.spg  processed by SPAG 6.72Dc at 11:15 on 14 Aug 2019
      ! *****************************************************************
            SUBROUTINE KEEPER(Iagain,Iwhich)
      !     *************************************
      !     * This subroutine allows the user      *
      !     * to decide whether or not to keep     *
      !     * leap year data with last day missing *
      !     *************************************
      !
            IMPLICIT NONE
      !*--KEEPER52
      !*** Start of declarations inserted by SPAG
            INTEGER*2 Iagain , ianswer , Iwhich , na , nb , nbstart , nc ,    &
                    & nf , nh , nl
            REAL txt
      !*** End of declarations inserted by SPAG
            Iagain = 0
      !
      !     ... Clear screen and write yes-no prompt
      !
            nf = 8 + 256
            na = 9
            nb = 12
            CALL UTLTY(nf,na,nb,nl,nh,nc,txt)
            IF ( Iwhich.EQ.1 ) THEN
               WRITE (6,99001)
      99001    FORMAT (                                                       &
           &'               ���������������'',/,  &
           &                                       'Datasethasonevaluemissing &
           &''',/,                                                          ' &
          & forleapyear.Doyouwant                                             &
           &''',/,                                                          ' &
          & toaddthisyearofdatatoYesNo                                        &
           &''',/,                                                          ' &
          & thedataset'                                                       &
           &''',/,                                                          ''&
           &'',/,                                                          ''�&
           &��')
            ELSE
               WRITE (6,99002)
      99002    FORMAT (                                                       &
           &'               ���������������'',/,  &
           &                                       'Datasethasextradata       &
          & .Doyouwish                                                        &
           &''',/,                                                          ' &
          & tousethefirst365(or366)                                           &
           &''',/,                                                          ' &
          & datavalues'ExtradatawillYesNo                                     &
           &''',/,                                                          ' &
          & beignored.                                                        &
           &''',/,                                                          ''&
           &'',/,                                                          ''�&
           &��')
            ENDIF
            na = 13
            nbstart = 49
            CALL YESORNO(ianswer,na,nbstart,nl,nh)
            IF ( ianswer.EQ.1 ) Iagain = 1
            END
      !*==BOXER2.spg  processed by SPAG 6.72Dc at 11:15 on 14 Aug 2019
      !
      !
      !
      !
      !
      !
            SUBROUTINE BOXER2(Lr,Lc,Ln,Lw,Nf1,Nl,Nh,Icap)
      !
      !     ***************************************************************
      !     *** This subroutine draws a box around the current pointer  ***
      !     *** location.                                               ***
      !     ***                                                         ***
      !     *** LR = Top row of the box                                 ***
      !     *** LC = Left column of the box                             ***
      !     *** LN = Number of lines of text                            ***
      !     *** LW = Width (in spaces) of the box                       ***
      !     *** NF1 = Designation whether to draw a normal box (NF1=4), ***
      !     ***       reverse video box (NF1=2), or erase box (NF1=0)   ***
      !     *** NL = ASCII value of the key accepted from the keyboard  ***
      !     *** NH = Second byte of the ASCII value in NL               ***
      !     *** ICAP = If ICAP < 0, the key capture is disabled         ***
      !     ***************************************************************
      !
            IMPLICIT NONE
      !*--BOXER2126
      !*** Start of declarations inserted by SPAG
            INTEGER*2 i , Icap , Lc , Ln , Lr , Lw , na , nb , nc , nf , Nf1 ,&
                    & nfr , Nh , Nl
      !*** End of declarations inserted by SPAG
            CHARACTER txt*80 , txt1*80 , txt2*80 , txt3*1
      !
      !     ...Initialize TXT
      !
            DO i = 1 , 80
               txt(i:i) = ' '
            ENDDO
      !
      !     ...Initialize box type
      !
            IF ( Nf1.GE.1 ) THEN
               txt1(1:1) = CHAR(218)
               txt1(Lw:Lw) = CHAR(191)
               txt2(1:1) = CHAR(192)
               txt2(Lw:Lw) = CHAR(217)
               txt3 = CHAR(179)
               DO i = 2 , Lw - 1
                  txt1(i:i) = CHAR(196)
                  txt2(i:i) = CHAR(196)
               ENDDO
            ELSE
               txt3 = ' '
               DO i = 1 , Lw
                  txt1(i:i) = ' '
                  txt2(i:i) = ' '
               ENDDO
            ENDIF
            nfr = Nf1
            IF ( nfr.EQ.0 ) nfr = 4
      !
      !     ...Draw top bar
      !
            nf = nfr + 8 + 64
            na = Lr
            DO i = 1 , Lw
               nb = i + Lc - 1
               Nl = ICHAR(txt1(i:i))
               CALL UTLTY(nf,na,nb,Nl,Nh,nc,txt)
            ENDDO
      !
      !     ...Draw side bars
      !
            Nl = ICHAR(txt3)
            nf = nfr + 8 + 64
            DO i = Lr , Ln + Lr
               na = i + 1
               nb = Lc
               CALL UTLTY(nf,na,nb,Nl,Nh,nc,txt)
               nb = Lc + Lw - 1
               CALL UTLTY(nf,na,nb,Nl,Nh,nc,txt)
            ENDDO
      !
      !     ...Draw bottom bar
      !
            nf = nfr + 8 + 64
            na = Lr + Ln + 1
            DO i = 1 , Lw
               nb = i + Lc - 1
               Nl = ICHAR(txt2(i:i))
               CALL UTLTY(nf,na,nb,Nl,Nh,nc,txt)
            ENDDO
      !
      !     ...Capture a key and return it to the calling subroutine
      !
            IF ( Nf1.GE.1 .AND. Icap.GE.0 ) THEN
               nf = 128
               CALL UTLTY(nf,na,nb,Nl,Nh,nc,txt)
            ENDIF
      !
            END
      !*==YESORNO.spg  processed by SPAG 6.72Dc at 11:15 on 14 Aug 2019
      !
      !
      !
            SUBROUTINE YESORNO(Ianswer,Na,Nbstart,Nl,Nh)
      !
            IMPLICIT NONE
      !*--YESORNO208
      !*** Start of declarations inserted by SPAG
            INTEGER*2 Ianswer , icap , ln , lr , lw , Na , nb , Nbstart ,     &
                    & nf1 , Nh , nh1 , Nl , nl1
      !*** End of declarations inserted by SPAG
      !
      !     ...Draw reverse video box around the CURRENT selection
      !
      !     ##??##
      !      IANSWER = 1
       100  icap = 1
            ln = 1
            lr = Na - 1
            nf1 = 2
            nb = Nbstart + 6
            lw = 4
            IF ( Ianswer.GT.0 ) THEN
               ln = 1
               lw = 5
               nb = Nbstart
            ENDIF
            CALL BOXER2(lr,nb,ln,lw,nf1,Nl,Nh,icap)
            nl1 = Nl
            nh1 = Nh
      !
      !     ...Check for right or left keys
      !
            IF ( Nl.EQ.0 .AND. (Nh.EQ.75 .OR. Nh.EQ.77) ) THEN
               nf1 = 0
               CALL BOXER2(lr,nb,ln,lw,nf1,Nl,Nh,icap)
               IF ( Ianswer.GT.0 ) THEN
                  Ianswer = 0
               ELSE
                  Ianswer = 1
               ENDIF
               GOTO 100
      !
      !     ...Check for 'Y' or 'y'
      !
            ELSEIF ( Nl.EQ.121 .OR. Nl.EQ.89 ) THEN
               nf1 = 0
               CALL BOXER2(lr,nb,ln,lw,nf1,Nl,Nh,icap)
               Ianswer = 1
               nb = Nbstart
               lw = 5
               icap = -1
               nf1 = 4
               CALL BOXER2(lr,nb,ln,lw,nf1,Nl,Nh,icap)
      !
      !     ...Check for 'N' or 'n' or Esc
      !
            ELSEIF ( Nl.EQ.27 .OR. Nl.EQ.110 .OR. Nl.EQ.78 ) THEN
               nf1 = 0
               CALL BOXER2(lr,nb,ln,lw,nf1,Nl,Nh,icap)
               Ianswer = 0
               nb = Nbstart + 6
               lw = 4
               icap = -1
               nf1 = 4
               CALL BOXER2(lr,nb,ln,lw,nf1,Nl,Nh,icap)
      !
      !     ...... Check for an ARROW DOWN or UP key or the ENTER key
      !
            ELSEIF ( Nl.EQ.13 .OR. (Nl.EQ.0 .AND. (Nh.EQ.80 .OR. Nh.EQ.72)) ) &
                   & THEN
      !
      !     ...Draw a normal video box at the current location
      !
               icap = -1
               nf1 = 4
               CALL BOXER2(lr,nb,ln,lw,nf1,Nl,Nh,icap)
      !
            ENDIF
            Nl = nl1
            Nh = nh1
      !
            END
      !*==SCANIT.spg  processed by SPAG 6.72Dc at 11:15 on 14 Aug 2019
      ! *****************************************************************
      !      ******************** SCANIT *************************
      !
      !
      !    SUBROUTINE SCANIT IS USED TO PICK OFF
      !    1UMERIC INPUT FROM THE USER'S ASCII FILE.  DATA MAY
      !    BE INTEGER, F-FORMAT OR E-FORMAT.  10 VALUES ARE
      !    RETURNED AFTER EACH CALL.
      !
            SUBROUTINE SCANIT(No,Value,M7,Klm,Ncol)
            IMPLICIT NONE
      !*--SCANIT297
      !*** Start of declarations inserted by SPAG
            REAL dkt , sgn , Value
            INTEGER i , k , k7 , kk , kpt , M7 , n , Ncol , No
      !*** End of declarations inserted by SPAG
            CHARACTER*1 Klm(*) , num(11) , ipoint , iplus , minus , icomp
            CHARACTER*1 uppere , lowere
            INTEGER esgn , exp
            DIMENSION Value(366)
            DATA uppere , lowere/'E' , 'e'/
            DATA ipoint , iplus , minus/'.' , '+' , '-'/
            DATA num/'0' , '1' , '2' , '3' , '4' , '5' , '6' , '7' , '8' ,    &
               & '9' , '0'/
            k7 = M7 + 1
            DO i = 1 , 366
               Value(i) = 0.
            ENDDO
            Ncol = 1
            n = 1
            kpt = 0
      !
      !  Exp is for e-type exponent.
      !
            exp = 0
       100  IF ( Klm(Ncol).NE.minus ) THEN
               IF ( Klm(Ncol).NE.iplus ) THEN
                  IF ( Klm(Ncol).NE.ipoint ) THEN
                     k = 0
                     icomp = num(1)
       110           IF ( Klm(Ncol).EQ.icomp ) THEN
                        sgn = 1.
                        Value(n) = k
                        GOTO 200
                     ELSE
                        k = k + 1
                        icomp = num(k+1)
                        IF ( k.LT.10 ) GOTO 110
                        Ncol = Ncol + 1
                        GOTO 400
                     ENDIF
                  ELSE
                     kpt = 1
                  ENDIF
               ENDIF
               sgn = 1.
            ELSE
               sgn = -1.
            ENDIF
            Value(n) = 0.
       200  Ncol = Ncol + 1
            IF ( Ncol.GE.k7 ) GOTO 600
      !      IF (KLM (NCOL) .NE. IPOINT) GO TO 1180
            IF ( Klm(Ncol).EQ.ipoint ) THEN
               kpt = 1
               GOTO 200
      !
      !     check for "E" exponent
      !
            ELSEIF ( (Klm(Ncol).NE.uppere) .AND. (Klm(Ncol).NE.lowere) ) THEN
               k = 0
               icomp = num(1)
       250     IF ( Klm(Ncol).EQ.icomp ) THEN
                  IF ( kpt.NE.0 ) THEN
                     Value(n) = Value(n) + k*10.**(-kpt)
                     kpt = kpt + 1
                  ELSE
                     Value(n) = Value(n)*10. + k
                  ENDIF
                  GOTO 200
               ELSE
                  k = k + 1
                  icomp = num(k+1)
                  IF ( k.GE.10 ) GOTO 600
                  GOTO 250
               ENDIF
            ELSE
      !
      !     found an "e" type number
      !     set sign of exponent to +1
      !
               esgn = 1
      !
      !     dkt counts # of digits in exponent
      !
               dkt = 0
      !
      !     move past "e"
      !
               Ncol = Ncol + 1
               IF ( Klm(Ncol).EQ.minus ) THEN
                  esgn = -1.
                  Ncol = Ncol + 1
                  exp = 0
               ELSEIF ( Klm(Ncol).NE.iplus ) THEN
                  exp = 0
               ELSE
                  esgn = 1.
      !
      !     move past sign
      !
                  Ncol = Ncol + 1
                  exp = 0
               ENDIF
      !
      !     Check to make sure there is more data to scan.
      !
       300     IF ( Ncol.GE.k7 ) GOTO 600
      !
      !     kk is loop control for compares.
      !
               kk = 0
               icomp = num(1)
       350     IF ( Klm(Ncol).EQ.icomp ) THEN
      !
      !     found a digit for exponent.
      !
                  dkt = dkt + 1
                  IF ( dkt.EQ.1 ) exp = exp + kk
                  IF ( dkt.GT.1 ) exp = exp*10 + kk
                  Ncol = Ncol + 1
                  GOTO 300
               ELSE
                  kk = kk + 1
                  icomp = num(kk+1)
                  IF ( kk.LT.10 ) GOTO 350
      !
      !     fix exponent of number
      !
                  exp = exp*esgn
      !
      !     go fix number now
      !
                  GOTO 600
               ENDIF
            ENDIF
       400  IF ( Ncol.LT.k7 ) GOTO 100
       500  No = n - 1
            RETURN
       600  IF ( exp.NE.0 ) Value(n) = Value(n)*(10.**exp)
            Value(n) = Value(n)*sgn
            exp = 0
            n = n + 1
            kpt = 0
            IF ( n.LE.366 ) GOTO 400
            GOTO 500
            END
      !*==TOUPPER.spg  processed by SPAG 6.72Dc at 11:15 on 14 Aug 2019
      ! *****************************************************************
            SUBROUTINE TOUPPER(Letters,Inum_char)
            IMPLICIT NONE
      !*--TOUPPER447
      !*** Start of declarations inserted by SPAG
            INTEGER*2 i , Inum_char , k
      !*** End of declarations inserted by SPAG
            CHARACTER Letters*80

            DO i = 1 , Inum_char
               k = ICHAR(Letters(i:i))
               IF ( k.GE.97 .AND. k.LE.122 ) THEN
                  k = k - 32
                  Letters(i:i) = CHAR(k)
               ENDIF
            ENDDO
            END
