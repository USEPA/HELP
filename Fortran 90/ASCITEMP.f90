PROGRAM SCANT
    !
    !
    !********************************************************************
    !    THIS ROUTINE CONVERTS AN ASCII FILE TO A FORMATTED FILE
    !    CONTAINING DAILY VALUES.  EACH LINE OF OUTPUT CONSISTS OF
    !    THE YEAR,  TEN DATA VALUES, AND THE LINE NUMBER.
    !********************************************************************

    INTEGER(kind=2) IFLAG, NL, NH
    CHARACTER TXT*80, INFILE*60, CITY*20, STATE*20
    CHARACTER CITY2*20, STATE2*20, OUTFILE*12
    LOGICAL LVAL
    REAL INDATA(370), RVALUE
    DATA TXT/'&
            '/

    !
    !  INITIALIZE VARIBLES
    !
    INFILE = TXT(1:60)
    CITY = TXT(1:20)
    STATE = TXT(1:20)
    CITY2 = TXT(1:20)
    STATE2 = TXT(1:20)
    LVAL = .FALSE.
    ICONVERT = 0
    !
    ! READ INPUT PARAMETERS - CITY, STATE, AND DATA UNITS.
    !
    INQUIRE(FILE = 'SCAN.TMP', EXIST = LVAL)
    IF (.NOT. LVAL) THEN
        CALL UTLTY (256, 1, 1, NL, NH, 1, ' ')
        CALL UTLTY (8 + 16, 2, 16, NL, NH, 43, '<<< SCAN.TMP INPUT FILE DOES NO&
                T EXIST! >>>')
        CALL UTLTY (8, 6, 1, NL, NH, 1, ' ')
        WRITE (6, 41)
        41       FORMAT(&
                15x, '����������������������������������������������ͻ', /, &
                15x, '�                                              �', /, &
                15x, '�         Enter the following information:     �', /, &
                15x, '�                                              �', /, &
                15x, '�        CITY  :                               �', /, &
                15x, '�        STATE :                               �', /, &
                15x, '�        UNITS (1=U.S.  2=METRIC) :            �', /, &
                15x, '�                                              �', /, &
                15x, '����������������������������������������������ͼ')
        CALL ACELL (11, 33, 20, CITY, NL, NH, 1)
        CALL ACELL (12, 33, 20, STATE, NL, NH, 1)
        50       CALL CELL (13, 52, 4, NL, NH, RVALUE, IFLAG, TXT)
        IF (RVALUE /= 1.0 .AND. RVALUE /= 2.0) GOTO 50
        IUNIT = RVALUE
    ELSE
        OPEN (20, FILE = 'SCAN.TMP', STATUS = 'OLD')
        READ (20, 100, END = 120) CITY
        READ (20, 100, END = 120) STATE
        100      FORMAT(A20)
        READ (20, 105, END = 120) IUNIT
        105      FORMAT(I1)
        120      CONTINUE
        CLOSE (20)
    ENDIF
    CALL TOUPPER(CITY, 20)
    CALL TOUPPER(STATE, 20)
    IUNIT2 = IUNIT

    !
    !       SEE IF THEY WANT TO ADD DATA TO EXISTING FILE
    !
    OUTFILE(1:9) = 'DATA7.TMP'
    INQUIRE(FILE = OUTFILE, EXIST = LVAL)
    IF (LVAL) THEN
        CALL APPEND (IFLAG)
        IF (IFLAG == 1) THEN
            OPEN (10, FILE = OUTFILE, STATUS = 'OLD')
            READ(10, 105) ITMP
            READ(10, 105) IUNIT2
            READ(10, 135) CITY2, STATE2
            135         FORMAT(A20, A20)
            CALL TOUPPER(CITY2, 20)
            CALL TOUPPER(STATE2, 20)
            IF(CITY /= CITY2 .OR. STATE /= STATE2) THEN
                CALL UTLTY (8 + 256, 10, 10, NL, NH, 1, ' ')
                WRITE(6, 145) CITY2, STATE2, CITY, STATE
                145            FORMAT(5x, 'CITY AND STATE IN MASTER FILE  < ', A20, A20, '>'&
                        , /, 5x, '******* DOES NOT MATCH *******', /, 5x, 'CITY AND STATE IN INP&
                        UT FILE   < ', A20, A20, '>')
                CALL UTLTY (8 + 18 + 128, 18, 25, NL, NH, 30, 'PRESS ANY KEY TO EXI&
                        T PROGRAM!')
                GOTO 999
            ENDIF
            IF (IUNIT /= IUNIT2) THEN
                ICONVERT = IUNIT2
                CALL UTLTY (256, 1, 1, NL, NH, 1, ' ')
                CALL UTLTY (8 + 16, 10, 12, NL, NH, 56, ' DATA WILL BE CONVERTED &
                        TO UNITS PRESENT IN MASTER FILE.')
                IF(ICONVERT == 1) THEN
                    CALL UTLTY (8 + 16, 11, 27, NL, NH, 27, '(FROM METRIC TO U.S. &
                            UNITS)')
                ELSE
                    CALL UTLTY (8 + 16, 11, 27, NL, NH, 27, '(FROM U.S. TO METRIC &
                            UNITS)')
                ENDIF
                CALL UTLTY (8 + 16 + 128, 14, 27, NL, NH, 26, 'PRESS ANY KEY TO CON&
                        TINUE!')
                GOTO 180
            ENDIF
            GOTO 180
        ENDIF
    ENDIF

    !
    !  CREATE NEW MASTER FILE
    !

    CALL ERASE(OUTFILE(1:12), *151)
    151   OPEN (10, FILE = OUTFILE, STATUS = 'NEW')
    WRITE(10, 161) 7, IUNIT, CITY, STATE, '  '
    161   FORMAT(I1/I1/A20, A20/A2)
    GOTO 200

    180   CLOSE(10)
    OPEN (10, FILE = OUTFILE, STATUS = 'OLD', ACCESS = 'APPEND')

    !
    !       WRITE FIRST SCREEN TO INPUT FILE NAME THE USER WISHES TO USE AS
    !       INPUT - FILE IS ASSUMED TO BE ASCII FILE AND WILL BE OUTPUT AS
    !       FORMATTED FILE CALLED "DATA?.TMP". ? = (4=rain 7=temp 13=srad)
    !

    200   CALL UTLTY (8 + 256, 6, 1, NL, NH, 1, ' ')
    WRITE (6, 241) CHAR(39)
    241   FORMAT(&
            15x, '����������������������������������������������ͻ', /, &
            15x, '�                                              �', /, &
            15x, '�    Enter the name of the data set for the    �', /, &
            15x, '�             current year', A1, 's data.             �', /, &
            15x, '�                                              �', /, &
            15x, '�      Temperature input values should         �')
    IF (IUNIT == 1) WRITE (6, 261)
    261      FORMAT(&
            15x, '�             be in fahrenheit.                �')
    IF (IUNIT == 2) WRITE (6, 271)
    271      FORMAT(&
            15x, '�              be in celsius.                  �')
    WRITE (*, 281)
    281   FORMAT(&
            15x, '�                                              �', /, &
            15x, '����������������������������������������������ͼ', /, &
            15x, '          Enter ESC to exit this session. ')

    CALL ACELL (14, 18, 40, INFILE, NL, NH, 1)
    !
    !   CHECK FOR ESC TO EXIT
    !
    IF (NL == 27) GO TO 999
    INQUIRE (FILE = INFILE, EXIST = LVAL)
    IF(.NOT. LVAL) THEN
        CALL UTLTY (256, 1, 1, NL, NH, 1, ' ')
        CALL UTLTY (8 + 16, 10, 20, NL, NH, 39, '<<< INPUT DATA FILE DOES NOT E&
                XIST! >>>')
        CALL UTLTY (8 + 16 + 128, 15, 27, NL, NH, 26, 'PRESS ANY KEY TO CONTINUE!&
                ')
        GO TO 200
    ENDIF

    300   CALL UTLTY (8 + 256, 7, 1, NL, NH, 1, ' ')
    WRITE (6, 341)
    341   FORMAT(&
            15x, '����������������������������������������������ͻ', /, &
            15x, '�                                              �', /, &
            15x, '�    Enter the year of the temperature data    �', /, &
            15x, '�     contained in this file being added.      �', /, &
            15x, '�                                              �', /, &
            15x, '�                                              �', /, &
            15x, '����������������������������������������������ͼ')

    CALL CELL (13, 18, 4, NL, NH, RVALUE, IFLAG, TXT)
    IF(RVALUE == 0) GOTO 300
    IYEAR = (RVALUE + 0.001)

    !
    !     Check for leap year.
    !

    IF (((IYEAR / 4) * 4) /= IYEAR) THEN
        INUMDAYS = 365
    ELSE
        INUMDAYS = 366
    ENDIF

    !
    !     INITIALIZE ARRAY AND READ INPUT FILE
    !

    DO I = 1, INUMDAYS
        INDATA(I) = 9999.0
    end do
    DO I = INUMDAYS + 1, 370
        INDATA(I) = 0.0
    end do
    OPEN(11, FILE = INFILE, STATUS = 'OLD')
    READ(11, *, END = 410) (INDATA(I), I = 1, INUMDAYS + 1)
    CLOSE(11)

    !
    !  TO MUCH DATA IN INPUT FILE
    !

    CALL KEEPER(IFLAG, 2)
    IF(IFLAG == 0) GOTO 200
    INDATA(INUMDAYS + 1) = 0.0
    GOTO 500

    !
    !  CHECK IF NOT ENOUGH DATA
    !

    410   CLOSE(11)
    DO I = 1, INUMDAYS
        IF(INDATA(I) == 9999) GOTO 420
    end do
    GOTO 500

    !
    !  NOT ENOUGH DATA IN INPUT FILE
    !

    420   CALL UTLTY (256, 1, 1, NL, NH, 1, ' ')
    CALL UTLTY (8 + 16, 8, 23, NL, NH, 36, 'NOT ENOUGH DATA FOUND IN INPUT FIL&
            E!')
    WRITE(6, 446) I - 1, INUMDAYS
    446   FORMAT(23x, '-------------------------------------', /, 23x, 'TOTAL NU&
            MBER OF VALUES READ  ---> ', I4, /, 23x, 'TOTAL NUMBER OF DAYS IN YEAR&
            ---> ', I4)
    CALL UTLTY (8 + 16 + 128, 16, 27, NL, NH, 26, 'PRESS ANY KEY TO CONTINUE!')
    GOTO 200

    !
    !  CHECK IF DATA NEEDS UNITS CONVERTED
    !

    500   IF(ICONVERT == 1) THEN
        DO I = 1, INUMDAYS
            INDATA(I) = INDATA(I) * 1.8 + 32.0
        end do
    ELSE IF(ICONVERT == 2) THEN
        DO I = 1, INUMDAYS
            INDATA(I) = (INDATA(I) - 32.0) / 1.8
        end do
    ENDIF

    !
    !  WRITE DATA TO MASTER FILE FORMATTED
    !

    IF (IUNIT2 == 1) THEN
        DO I = 1, INUMDAYS, 10
            WRITE(10, 546) IYEAR, (INDATA(J), J = I, I + 9), (J / 10)
            546         FORMAT(I5, 10F6.1, I5)
        end do
    ELSE
        DO I = 1, INUMDAYS, 10
            WRITE(10, 566) IYEAR, (INDATA(J), J = I, I + 9), (J / 10)
            566         FORMAT(I5, 10F6.1, I5)
        end do
    ENDIF
    GOTO 200
    999   STOP
END
