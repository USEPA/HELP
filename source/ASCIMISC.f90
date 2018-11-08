! *****************************************************************
SUBROUTINE ACELL (NROW, NBSTART, NS, TXT, NL, NH, NWRITE)
    IMPLICIT INTEGER(kind=2) (I - N)
    PARAMETER (N0 = 0, N1 = 1, N2 = 2, N8 = 8, N32 = 32, N64 = 64)
    CHARACTER TXT*80, XLM*1 (80), BLNK*1
    LOGICAL FIRST
    FIRST = .TRUE.
    IBLNK = N32
    BLNK = CHAR (N32)
    NL = N0
    NH = N0
    NA = NROW
    DO ICOL = N1, NS
        NB = NBSTART - N1 + ICOL
        NF = N8 + N32
        CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
        NF = N2 + N8 + N64
        CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
        XLM (ICOL) = CHAR (NL)
    end do
    IF (NS < 80) THEN
        DO 1010 ICOL = NS + 1, 80
            XLM (ICOL) = BLNK
        1010    CONTINUE
    END IF
    ICOL = N1
    1020 CONTINUE
    NF = N8 + 128
    NB = NBSTART + ICOL - N1
    CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
    IF (NWRITE > N0) THEN
        IF (NL >= 33 .and. NL <= 122) THEN
            NF = N2 + N8 + N64
            IF (FIRST) THEN
                DO 1030 I = N1, NS
                    NB = NBSTART + I - N1
                    CALL UTLTY (NF, NA, NB, IBLNK, NH, NC, TXT)
                    XLM (I) = BLNK
                1030          CONTINUE
            END IF
            NB = NBSTART + ICOL - N1
            CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
            XLM (ICOL) = CHAR (NL)
            ICOL = ICOL + N1
            IF (ICOL > NS) ICOL = NS
        ELSE IF (NL == N8 .and. NH == 14) THEN
            ICOL = ICOL - N1
            IF (ICOL < N1) THEN
                ICOL = N1
            ELSE
                DO 1040 I = ICOL, NS
                    NF = N8 + N32
                    NB = NBSTART + I
                    CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
                    NF = N2 + N8 + N64
                    NB = NB - N1
                    CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
                    XLM (I) = CHAR (NL)
                1040          CONTINUE
                XLM (NS) = BLNK
                NB = NBSTART + NS - N1
                CALL UTLTY (NF, NA, NB, IBLNK, NH, NC, TXT)
            END IF
        ELSE IF (NL == N0 .and. NH == 77) THEN
            ICOL = ICOL + N1
            IF (ICOL > NS) ICOL = N1
        ELSE IF (NL == N0 .and. NH == 75) THEN
            ICOL = ICOL - N1
            IF (ICOL < N1) ICOL = NS
        ELSE IF (NL == N32) THEN
            ICOL = ICOL + N1
            IF (ICOL > NS) THEN
                ICOL = NS
            ELSE
                DO 1050 I = NS, ICOL, - N1
                    NF = N8 + N32
                    NB = NBSTART + I - N2
                    CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
                    NF = N2 + N8 + N64
                    NB = NB + N1
                    CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
                    XLM (I) = CHAR (NL)
                1050          CONTINUE
                XLM (ICOL - N1) = BLNK
                NB = ICOL + NBSTART - N2
                CALL UTLTY (NF, NA, NB, IBLNK, NH, NC, TXT)
            END IF
        END IF
    END IF
    FIRST = .FALSE.
    IF (NL == 13 .or. NL == 42 .or. (NL == 0 .and. (NH&
            == 79 .or. NH == 72 .or.&
            NH == 73 .or. NH == 80 .or. NH == 81 .or.&
            NH == 82 .or. NH == 83&
            .or. NH == 71 .or. NH == 1)) .or. NL == 27) THEN
        !
        !        ... Change cell to normal video
        !
        NF = 4 + 8 + 64
        DO 1060 ICOL = 1, NS
            NB = NBSTART - 1 + ICOL
            NLTMP = ICHAR (XLM (ICOL))
            CALL UTLTY (NF, NA, NB, NLTMP, NH, NC, TXT)
        1060    CONTINUE
        TXT = XLM (1) // XLM (2) // XLM (3) // XLM (4) // XLM&
                (5) // XLM (6) // XLM (7) // &
                XLM (8) // XLM (9) // XLM (10) // XLM (11) // XLM (12) // XLM (13) // &
                XLM (14) // XLM (15) // XLM (16) // XLM (17) // XLM (18) // XLM (19) // &
                XLM (20) // XLM (21) // XLM (22) // XLM (23) // XLM (24) // XLM (25) // &
                XLM (26) // XLM (27) // XLM (28) // XLM (29) // XLM (30) // XLM (31) // &
                XLM (32) // XLM (33) // XLM (34) // XLM (35) // XLM (36) // XLM (37) // &
                XLM (38) // XLM (39) // XLM (40) // XLM (41) // XLM (42) // XLM (43) // &
                XLM (44) // XLM (45) // XLM (46) // XLM (47) // XLM (48) // XLM (49) // &
                XLM (50) // XLM (51) // XLM (52) // XLM (53) // XLM (54) // XLM (55) // &
                XLM (56) // XLM (57) // XLM (58) // XLM (59) // XLM (60) // XLM (61) // &
                XLM (62) // XLM (63) // XLM (64) // XLM (65) // XLM (66) // XLM (67) // &
                XLM (68) // XLM (69) // XLM (70) // XLM (71) // XLM (72) // XLM (73) // &
                XLM (74) // XLM (75) // XLM (76) // XLM (77) // XLM (78) // XLM (79) // &
                XLM (80)
        RETURN
    END IF
    GO TO 1020
END
! *****************************************************************
SUBROUTINE CELL (NA, NBSTART, NS, NL, NH, VARI, NO, TXT)
    IMPLICIT INTEGER(kind=2) (I - N)
    PARAMETER (N0 = 0, N1 = 1, N2 = 2, N8 = 8, N64 = 64)
    CHARACTER TXT*80
    CHARACTER*1 XLM (80)
    ICOL1 = NS
    DO 1000 ICOL = N1, NS
        NB = NBSTART - N1 + ICOL
        NF = N8 + 32
        CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
        NF = N2 + N64
        CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
        XLM (ICOL) = CHAR (NL)
    1000 CONTINUE
    DO 1010 ICOL = NS, N1, - N1
        IF (ICHAR (XLM (ICOL)) < 46 .OR. ICHAR (XLM&
                (ICOL)) > 57) GO TO 1020
    1010 CONTINUE
    IF (ICOL <= N1 .OR. ICOL > NS) ICOL = N1
    1020 CONTINUE
    ICOL = ICOL + N1
    IF (ICOL > NS) ICOL = NS
    1030 CONTINUE
    NF = N8 + 128
    NB = NBSTART - N1 + ICOL
    IF (NB > NBSTART - N1 + NS) THEN
        NB = NBSTART - N1 + NS
        ICOL = NS
    END IF
    CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
    IF ((NL >= 48 .AND. NL <= 57) .OR. NL == 46 .OR. NL == 32&
            .OR. NL == 43 .OR. NL == 45 .OR. NL ==&
            69 .OR. NL == 101) THEN
        ITEMP = NL
        IF (ICOL < NS) ICOL1 = ICOL
        IF (ICOL == NS) THEN
            IF (ICOL1 <= NS) GO TO 1040
            DO 1050 J = N2, NS
                XLM (J - N1) = XLM (J)
            1050       CONTINUE
            NF = N2 + N8 + N64
            DO 1060 ICOL = N1, NS - N1
                NL = ICHAR (XLM (ICOL))
                NB = NBSTART - N1 + ICOL
                CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
            1060       CONTINUE
            ICOL = NS
        END IF
        1040    CONTINUE
        NL = ITEMP
        XLM (ICOL) = CHAR (NL)
        NF = N2 + N8 + N64
        NB = NBSTART - N1 + ICOL
        CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
        IF (ICOL < NS) ICOL = ICOL + N1
        ICOL1 = ICOL1 + N1
        GO TO 1030
    ELSE IF (NL == N8 .AND. NH == 14) THEN
        NL = ICHAR (' ')
        NF = N2 + N8 + N64
        CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
        XLM (ICOL) = ' '
        ICOL = ICOL - N1
        IF (ICOL < N1) ICOL = N1
        GO TO 1030
    ELSE IF (NL == N0 .AND. NH == 77) THEN
        ICOL = ICOL + N1
        IF (ICOL > NS) ICOL = N1
        GO TO 1030
    ELSE IF (NL == N0 .AND. NH == 75) THEN
        ICOL = ICOL - N1
        IF (ICOL < N1) ICOL = NS
        GO TO 1030
    ELSE IF (NL == 13 .or. NL == 9 .or. NL == 42&
            .or. (NL == 27 .AND. NH&
                    == 1) .or. (NL == N0 .AND. (NH >=&
            71 .and. NH <= 83) .or.&
            NH == 15 .or. NH == 115 .or. NH == 116)) THEN
        TXT = ' '
        !
        !        ... Change cell to normal video
        !
        NF = 4 + 8 + 64
        DO 1070 ICOL = 1, NS
            NB = NBSTART - 1 + ICOL
            NLTMP = ICHAR (XLM (ICOL))
            CALL UTLTY (NF, NA, NB, NLTMP, NH, NC, TXT)
        1070    CONTINUE
        DO 1080 ICOL = N1, NS
            TXT (ICOL:ICOL) = XLM (ICOL)
        1080    CONTINUE
        VARI = VAR1 (TXT)
    ELSE
        GO TO 1030
    END IF
    RETURN
END
! *****************************************************************
FUNCTION VAR1 (VAR)
    PARAMETER (N0 = 0, N1 = 1, N10 = 10)
    INTEGER(kind=2) N0, N1, N10, NEXP, K, I, IB, IE, IX, IXS, J, IP
    CHARACTER VAR*80, MINUS*1, PLUS*1, PERIOD*1, &
            BLK*1, NUM (N10)*1, E*1, &
            Ee*1
    DATA NUM, MINUS, PLUS, PERIOD, BLK, E, Ee/'0', &
            '1', '2', '3', '4', '5', '6', &
            '7', '8', '9', '-', '+', '.', ' ', 'E', 'e'/
    SIGN = 1.
    RNUM = 0.
    NEXP = N0
    K = N1
    DO 1000 I = N1, 80
        IF (VAR (I:I) /= BLK) GO TO 1010
        K = K + N1
    1000 CONTINUE
    VAR1 = RNUM
    RETURN
    1010 CONTINUE
    IB = K
    DO 1020 I = IB, 80
        IF (VAR (I:I) == BLK) GO TO 1030
        K = K + N1
    1020 CONTINUE
    1030 CONTINUE
    IE = K - N1
    IF (VAR (IB:IB) /= MINUS) GO TO 1040
    IB = IB + N1
    SIGN = - SIGN
    GO TO 1050
    1040 CONTINUE
    IF (VAR (IB:IB) == PLUS) IB = IB + N1
    1050 CONTINUE
    DO 1060 I = IE, IB, - N1
        IF (VAR (I:I) == E .OR. VAR (I:I) == Ee) GO TO 1070
    1060 CONTINUE
    GO TO 1080
    1070 CONTINUE
    IXS = N1
    IX = I
    I = I + N1
    IF (VAR (I:I) /= MINUS) GO TO 1090
    IXS = - IXS
    I = I + N1
    1090 CONTINUE
    IF (VAR (I:I) == PLUS) I = I + N1
    DO 1100 J = IE, I, - N1
        DO 1110 K = N1, N10
            IF (VAR (J:J) == NUM (K)) GO TO 1100
        1110    CONTINUE
        NEXP = NEXP + (K - N1) * N10**(IE - J)
    1100 CONTINUE
    NEXP = NEXP * IXS
    IE = IX - N1
    1080 CONTINUE
    DO 1120 I = IB, IE
        IF (VAR (I:I) == PERIOD) GO TO 1130
    1120 CONTINUE
    IE = I
    1130 CONTINUE
    IP = I
    K = N1
    DO 1140 I = IB, IE
        DO 1150 J = N1, N10
            IF (VAR (I:I) == NUM (J)) GO TO 1160
        1150    CONTINUE
        K = N0
        GO TO 1140
        1160    CONTINUE
        RNUM = RNUM + FLOAT (J - N1) * 10.**(IP - I - K)
    1140 CONTINUE
    VAR1 = RNUM * SIGN
    IF (NEXP /= N0) VAR1 = SIGN * RNUM * 10.**NEXP
    RETURN
END
! *****************************************************************
SUBROUTINE AGAIN (iagain)
    !     *************************************
    !     * This subroutine allows the user   *
    !     * to decide whether or not he wants *
    !     * to enter another year of data.    *
    !     *************************************
    !
    !     COMMON /FILENM/ FNAME
    !     CHARACTER FNAME*40
    IMPLICIT INTEGER(kind=2) (I - N)
    iagain = 0
    !
    !     ... Clear screen and write yes-no prompt
    !
    NF = 8 + 256
    NA = 9
    NB = 12
    CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
    WRITE (6, 6000)
    6000 FORMAT('               ����������������������������������������', &
            '������ͻ', /, &
            '               �   Do you wish  to enter another              ', &
            '�', /, &
            '               �   year of data to this data set?  Yes   No   ', &
            '�', /, &
            '               �                                              ', &
            '�', /, &
            '               �                                              ', &
            '�', /, &
            '               �����������������������������������������������', &
            '�')
    NA = 12
    NBSTART = 49
    CALL YESORNO (IANSWER, NA, NBSTART, NL, NH)
    IF (IANSWER == 1) iagain = 1
    RETURN
END
! *****************************************************************
SUBROUTINE APPEND (iagain)
    !     ***************************************
    !     * This subroutine allows the user     *
    !     * to decide whether or not he wants   *
    !     * to append data to existing edit file*
    !     ***************************************
    !
    !     COMMON /FILENM/ FNAME
    !     CHARACTER FNAME*40
    IMPLICIT INTEGER(kind=2) (I - N)
    iagain = 0
    !
    !     ... Clear screen and write yes-no prompt
    !
    NF = 8 + 256
    NA = 9
    NB = 12
    CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
    WRITE (6, 6000)
    6000 FORMAT('               ����������������������������������������', &
            '������ͻ', /, &
            '               �   Data file already exists in                ', &
            '�', /, &
            '               �   the edit file.  Do you wish     Yes   No   ', &
            '�', /, &
            '               �   to append data to it?                      ', &
            '�', /, &
            '               �                                              ', &
            '�', /, &
            '               �����������������������������������������������', &
            '�')
    NA = 12
    NBSTART = 49
    CALL YESORNO (IANSWER, NA, NBSTART, NL, NH)
    IF (IANSWER == 1) iagain = 1
    RETURN
END
! *****************************************************************
SUBROUTINE keeper (iagain, IWHICH)
    !     *************************************
    !     * This subroutine allows the user      *
    !     * to decide whether or not to keep     *
    !     * leap year data with last day missing *
    !     *************************************
    !
    IMPLICIT INTEGER(kind=2) (I - N)
    iagain = 0
    !
    !     ... Clear screen and write yes-no prompt
    !
    NF = 8 + 256
    NA = 9
    NB = 12
    CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
    IF (IWHICH == 1) THEN
        WRITE (6, 6000)
    ELSE
        WRITE (6, 6010)
    END IF
    6000 FORMAT('               ����������������������������������������', &
            '������ͻ', /, &
            '               �   Data set has one value missing             ', &
            '�', /, &
            '               �   for leap year.  Do you want                ', &
            '�', /, &
            '               �   to add this year of data to     Yes   No   ', &
            '�', /, &
            '               �   the data set?                              ', &
            '�', /, &
            '               �                                              ', &
            '�', /, &
            '               �����������������������������������������������', &
            '�')
    6010 FORMAT('               ����������������������������������������', &
            '������ͻ', /, &
            '               �   Data set has extra data.  Do you wish      ', &
            '�', /, &
            '               �   to use the first 365 (or 366)              ', &
            '�', /, &
            '               �   data values?  Extra data will   Yes   No   ', &
            '�', /, &
            '               �   be ignored.                                ', &
            '�', /, &
            '               �                                              ', &
            '�', /, &
            '               �����������������������������������������������', &
            '�')
    NA = 13
    NBSTART = 49
    CALL YESORNO (IANSWER, NA, NBSTART, NL, NH)
    IF (IANSWER == 1) iagain = 1
    RETURN
END
!
!
!
!
!
!
SUBROUTINE BOXER2 (LR, LC, LN, LW, NF1, NL, NH, ICAP)
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
    IMPLICIT INTEGER(kind=2) (I - N)
    CHARACTER TXT*80, TXT1*80, TXT2*80, TXT3*1
    !
    !     ...Initialize TXT
    !
    DO 1000 I = 1, 80
        TXT (I:I) = ' '
    1000 CONTINUE
    !
    !     ...Initialize box type
    !
    IF (NF1 >= 1) THEN
        TXT1 (1:1) = CHAR (218)
        TXT1 (LW:LW) = CHAR (191)
        TXT2 (1:1) = CHAR (192)
        TXT2 (LW:LW) = CHAR (217)
        TXT3 = CHAR (179)
        DO 1010 I = 2, LW - 1
            TXT1 (I:I) = CHAR (196)
            TXT2 (I:I) = CHAR (196)
        1010    CONTINUE
    ELSE
        TXT3 = ' '
        DO 1020 I = 1, LW
            TXT1 (I:I) = ' '
            TXT2 (I:I) = ' '
        1020    CONTINUE
    END IF
    NFR = NF1
    IF (NFR == 0) NFR = 4
    !
    !     ...Draw top bar
    !
    NF = NFR + 8 + 64
    NA = LR
    DO 1030 I = 1, LW
        NB = I + LC - 1
        NL = ICHAR (TXT1 (I:I))
        CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
    1030 CONTINUE
    !
    !     ...Draw side bars
    !
    NL = ICHAR (TXT3)
    NF = NFR + 8 + 64
    DO 1040 I = LR, LN + LR
        NA = I + 1
        NB = LC
        CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
        NB = LC + LW - 1
        CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
    1040 CONTINUE
    !
    !     ...Draw bottom bar
    !
    NF = NFR + 8 + 64
    NA = LR + LN + 1
    DO 1050 I = 1, LW
        NB = I + LC - 1
        NL = ICHAR (TXT2 (I:I))
        CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
    1050 CONTINUE
    !
    !     ...Capture a key and return it to the calling subroutine
    !
    IF (NF1 >= 1 .and. ICAP >= 0) THEN
        NF = 128
        CALL UTLTY (NF, NA, NB, NL, NH, NC, TXT)
    END IF
    !
    RETURN
END
!
!
!
SUBROUTINE YESORNO (IANSWER, NA, NBSTART, NL, NH)
    !
    IMPLICIT INTEGER(kind=2) (I - N)
    CHARACTER*80 TXT
    !
    !     ...Draw reverse video box around the CURRENT selection
    !
    !     ##??##
    !      IANSWER = 1
    1000 CONTINUE
    ICAP = 1
    LN = 1
    LR = NA - 1
    NF1 = 2
    NB = NBSTART + 6
    LW = 4
    IF (IANSWER > 0) THEN
        LN = 1
        LW = 5
        NB = NBSTART
    END IF
    CALL BOXER2 (LR, NB, LN, LW, NF1, NL, NH, ICAP)
    NL1 = NL
    NH1 = NH
    !
    !     ...Check for right or left keys
    !
    IF (NL == 0 .and. (NH == 75 .or. NH == 77)) THEN
        NF1 = 0
        CALL BOXER2 (LR, NB, LN, LW, NF1, NL, NH, ICAP)
        IF (IANSWER > 0) THEN
            IANSWER = 0
        ELSE
            IANSWER = 1
        END IF
        GO TO 1000
        !
        !     ...Check for 'Y' or 'y'
        !
    ELSE IF (NL == 121 .or. NL == 89) THEN
        NF1 = 0
        CALL BOXER2 (LR, NB, LN, LW, NF1, NL, NH, ICAP)
        IANSWER = 1
        NB = NBSTART
        LW = 5
        ICAP = - 1
        NF1 = 4
        CALL BOXER2 (LR, NB, LN, LW, NF1, NL, NH, ICAP)
        !
        !     ...Check for 'N' or 'n' or Esc
        !
    ELSE IF (NL == 27 .OR. NL == 110 .or. NL == 78) THEN
        NF1 = 0
        CALL BOXER2 (LR, NB, LN, LW, NF1, NL, NH, ICAP)
        IANSWER = 0
        NB = NBSTART + 6
        LW = 4
        ICAP = - 1
        NF1 = 4
        CALL BOXER2 (LR, NB, LN, LW, NF1, NL, NH, ICAP)
        !
        !     ...... Check for an ARROW DOWN or UP key or the ENTER key
        !
    ELSE IF (NL == 13 .or. (NL == 0 .and. (NH == 80&
            .or. NH == 72))) THEN
        !
        !     ...Draw a normal video box at the current location
        !
        ICAP = - 1
        NF1 = 4
        CALL BOXER2 (LR, NB, LN, LW, NF1, NL, NH, ICAP)
        !
    END IF
    NL = NL1
    NH = NH1
    RETURN
    !
END
! *****************************************************************
!      ******************** SCANIT *************************
!
!
!    SUBROUTINE SCANIT IS USED TO PICK OFF
!    1UMERIC INPUT FROM THE USER'S ASCII FILE.  DATA MAY
!    BE INTEGER, F-FORMAT OR E-FORMAT.  10 VALUES ARE
!    RETURNED AFTER EACH CALL.
!
SUBROUTINE SCANIT (NO, VALUE, M7, KLM, NCOL)
    CHARACTER*1 KLM (*), NUM (11), IPOINT, IPLUS, MINUS, ICOMP
    character*1 uppere, lowere
    integer esgn, exp
    DIMENSION VALUE (366)
    data uppere, lowere/'E', 'e'/
    DATA IPOINT, IPLUS, MINUS/'.', '+', '-'/
    DATA NUM/'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'/
    K7 = M7 + 1
    DO 1000 I = 1, 366
        VALUE (I) = 0.
    1000 CONTINUE
    NCOL = 1
    N = 1
    KPT = 0
    !
    !  Exp is for e-type exponent.
    !
    exp = 0
    1010 CONTINUE
    IF (KLM (NCOL) /= MINUS) GO TO 1020
    CONTINUE
    SGN = - 1.
    GO TO 1040
    1020 CONTINUE
    IF (KLM (NCOL) /= IPLUS) GO TO 1050
    1060 CONTINUE
    SGN = 1.
    1040 CONTINUE
    VALUE (N) = 0.
    GO TO 1070
    1050 CONTINUE
    IF (KLM (NCOL) /= IPOINT) GO TO 1080
    1090 CONTINUE
    KPT = 1
    GO TO 1060
    1080 CONTINUE
    K = 0
    ICOMP = NUM (1)
    1100 CONTINUE
    IF (KLM (NCOL) == ICOMP) GO TO 1110
    1120 CONTINUE
    K = K + 1
    ICOMP = NUM (K + 1)
    IF (K - 10) 1100, 1130, 1130
    1130 CONTINUE
    NCOL = NCOL + 1
    1140 CONTINUE
    IF (NCOL - K7) 1010, 1150, 1150
    1150 CONTINUE
    NO = N - 1
    RETURN
    1110 CONTINUE
    SGN = 1.
    VALUE (N) = K
    1070 CONTINUE
    NCOL = NCOL + 1
    IF (NCOL - K7) 1160, 1170, 1170
    1160 CONTINUE
    !      IF (KLM (NCOL) .NE. IPOINT) GO TO 1180
    IF (KLM (NCOL) /= IPOINT) GO TO 1180
    1190 CONTINUE
    KPT = 1
    GO TO 1070
    !
    !     check for "E" exponent
    !
    1180 CONTINUE
    if ((klm (ncol) /= uppere) .and. (klm (ncol)&
            /= lowere)) go to 1200
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
    ncol = ncol + 1
    IF (KLM (NCOL) /= MINUS) GO TO 1210
    1220 CONTINUE
    esgn = - 1.
    GO TO 1230
    1210 CONTINUE
    IF (KLM (NCOL) /= IPLUS) GO TO 1240
    1250 CONTINUE
    esgn = 1.
    1230 CONTINUE
    !
    !     move past sign
    !
    ncol = ncol + 1
    1240 CONTINUE
    exp = 0
    1260 CONTINUE
    !
    !     Check to make sure there is more data to scan.
    !
    if (ncol >= k7) go to 1170
    !
    !     kk is loop control for compares.
    !
    Kk = 0
    ICOMP = NUM (1)
    1270 CONTINUE
    IF (KLM (NCOL) == ICOMP) GO TO 1280
    1290 CONTINUE
    Kk = Kk + 1
    ICOMP = NUM (Kk + 1)
    IF (Kk - 10) 1270, 1300, 1300
    !
    !     found a digit for exponent.
    !
    1280 CONTINUE
    dkt = dkt + 1
    if (dkt == 1) exp = exp + kk
    if (dkt > 1) exp = exp * 10 + kk
    ncol = ncol + 1
    go to 1260
    1300 continue
    !
    !     fix exponent of number
    !
    exp = exp * esgn
    !
    !     go fix number now
    !
    go to 1170
    1200 CONTINUE
    K = 0
    ICOMP = NUM (1)
    1310 CONTINUE
    IF (KLM (NCOL) == ICOMP) GO TO 1320
    1330 CONTINUE
    K = K + 1
    ICOMP = NUM (K + 1)
    IF (K - 10) 1310, 1170, 1170
    1170 CONTINUE
    if (exp /= 0) value (n) = value (n) * (10.**exp)
    VALUE (N) = VALUE (N) * SGN
    exp = 0
    N = N + 1
    KPT = 0
    IF (N > 366) GO TO 1150
    GO TO 1140
    1320 CONTINUE
    IF (KPT) 1340, 1350, 1340
    1350 CONTINUE
    VALUE (N) = VALUE (N) * 10. + K
    GO TO 1070
    1340 CONTINUE
    VALUE (N) = VALUE (N) + K * 10.**(- KPT)
    KPT = KPT + 1
    GO TO 1070
END
! *****************************************************************
SUBROUTINE TOUPPER (LETTERS, INUM_CHAR)
    IMPLICIT INTEGER(kind=2) (I - N)
    CHARACTER LETTERS*80

    DO 101 I = 1, INUM_CHAR
        K = ICHAR(LETTERS(I:I))
        IF (K>=97 .AND. K<=122) THEN
            K = K - 32
            LETTERS(I:I) = CHAR(K)
        END IF
    101   CONTINUE
END
