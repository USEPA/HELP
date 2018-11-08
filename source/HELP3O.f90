!
!******************************MAIN*************************
!                         OUTPUT ROUTINE
!
!    DIRECTS THE RUNNING OF THE SIMULATION AND PRINTING OF OUTPUT,
!    AND PERFORMS THE ACCOUNTING ON THE RESULTS.
!
PROGRAM HELP3O
    !
    PARAMETER (MXYR = 100)
    PARAMETER (MAXMO = 12)
    !
    DOUBLE PRECISION BALY, BALT, DRIN, SWULY, DUMMY1, DUMMY2
    !    1  QDRN1, QDRN2, QDRN3, QDRN4, QDRN5,
    !    2  QPRCY1, QPRCY2, QPRCY3, QPRCY4, QPRCY5, QPRCY6, QLATY1,
    !    3  QLATY2, QLATY3, QLATY4, QLATY5, DUMMY1, DUMMY2,
    !    4  THY1, THY2, THY3, THY4, THY5, THY6
    !
    DOUBLE PRECISION PRC1, PRC2, PRC3, PRC4, PRC5, PRC6, DRN1, DRN2, &
            DRN3, DRN4, DRN5, HED1, HED2, HED3, HED4, HED5, &
            RCR1, RCR2, RCR3, RCR4, RCR5, RCRI
    !
    DOUBLE PRECISION DSWUL, DTHICK, DRCUL, DBUBUL, &
            DWPUL, DLAMUL, DRSUL, DUL, DFCUL, DSUBIN, DCHG, DRCRS
    !
    INTEGER(kind=2) NAAA, NBBB, NCCC, NFFF, NHHH, NLLL
    CHARACTER*1 ISTAR(2), ASTAR, SSTAR
    CHARACTER*60 F4, F7, F13, F11, F10, F8, TITLE
    !
    COMMON /BLK0/ F4, F7, F13, F11, F10, F8, TITLE
    COMMON /BLK1/ IT4, IT7, IT13, IU4, IU7, IU13, IU11, IU10, IU8, &
            LMYR, IOD, IOM, IOA, IOS
    COMMON /BLK3/ PORO(20), FC(20), WP(20), RC(20), SW(20), &
            RS(20), XLAMBD(20), BUB(20), THICK(20), SLOPE(20), &
            XLENG(20), CON(20), SUBIN(20), RECIR(20), PHOLE(20), &
            DEFEC(20), TRANS(20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK4/ LAYER(21), LAYR(20), IPQ(20), ISOIL(20), LAY, &
            LAYSEG(67, 2), LSEG(67), LSEGS(20, 2), LRIN(20), LSIN(20)
    COMMON /BLK5/ AREA, FRUNOF, CN2, OCN2, SSLOPE, SLENG, SMX
    COMMON /BLK6/ ULAI, WIND, RH(366), OSNO, ULAT
    COMMON /BLK7/ STHICK(67), UL(67), FCUL(67), WPUL(67), SWUL(67), &
            RCUL(67), RSUL(67), XLAMBU(67), CONUL(67), BUBUL(67), &
            SUBINS(67), SWULI(67)
    COMMON /BLK8/ PRE(370), TMPF(366), RAD(366)
    COMMON /BLK9/ PINF, ES1T, SMELT, GMRO, ADDRUN, OLDSNO, SNO, WF(7), &
            SSNO
    COMMON /BLK10/ ETT, ESS, EP, ES, ET (67)
    COMMON /BLK11/ ETO, XLAI, STAGE1, CONA, RAIN, RUN, ABST, EAJ, TS2
    COMMON /BLK12/ PRC1M (12), PRC2M (12), PRC3M (12), &
            PRC4M (12), PRC5M (12), PRC6M (12), DRN1M (12), &
            DRN2M (12), DRN3M (12), DRN4M (12), DRN5M (12), &
            PREM (12), RUNM (12), ETM (12), HED1M (12), &
            HED2M (12), HED3M (12), HED4M (12), HED5M (12), &
            RCRIM (20, 12), SUBINM (20, 12), RCR1M (12), &
            RCR2M (12), RCR3M (12), RCR4M (12), RCR5M (12)
    COMMON /BLK13/ PRC1A, PRC2A, PRC3A, PRC4A, PRC5A, PRC6A, &
            DRN1A, DRN2A, DRN3A, DRN4A, DRN5A, BAL, PREA, RUNA, ETA, &
            STOR, RCR1A, RCR2A, RCR3A, RCR4A, RCR5A, RCRIA(20), HED1A, &
            HED2A, HED3A, HED4A, HED5A, OSWULE, PSWULE, SUBINA(20)
    COMMON /BLK14/ PPRC1, PPRC2, PPRC3, PPRC4, PPRC5, PPRC6, &
            PDRN1, PDRN2, PDRN3, PDRN4, PDRN5, PPRE, PRUN, PSNO, PSW, DSW, &
            PHED1, PHED2, PHED3, PHED4, PHED5, PRCR1, PRCR2, PRCR3, &
            PRCR4, PRCR5, PRCRI(20)
    COMMON /BLK15/ PRC1, PRC2, PRC3, PRC4, PRC5, PRC6, DRN1, DRN2, &
            DRN3, DRN4, DRN5, HED1, HED2, HED3, HED4, HED5, &
            RCR1, RCR2, RCR3, RCR4, RCR5, RCRI(20)
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK17/ NSEG1, NSEG2, NSEG3, NSEG4, NSEG5, NSEG6, NSEG
    COMMON /BLK19/ IPL, IHV, IPRE, IRUN, ITSOIL, IVEG
    COMMON /BLK22/ PRC1M2 (12), PRC2M2 (12), PRC3M2 (12), &
            PRC4M2 (12), PRC5M2 (12), PRC6M2 (12), DRN1M2 (12), &
            DRN2M2 (12), DRN3M2 (12), DRN4M2 (12), DRN5M2 (12), &
            PREM2 (12), RUNM2 (12), ETM2 (12), HED1M2 (12), &
            HED2M2 (12), HED3M2 (12), HED4M2 (12), HED5M2 (12), &
            RCRIM2 (20, 12), RCR1M2 (12), RCR2M2 (12), &
            RCR3M2 (12), RCR4M2 (12), RCR5M2 (12)
    COMMON /BLK23/ PRC1M1 (12), PRC2M1 (12), PRC3M1 (12), &
            PRC4M1 (12), PRC5M1 (12), PRC6M1 (12), DRN1M1 (12), &
            DRN2M1 (12), DRN3M1 (12), DRN4M1 (12), DRN5M1 (12), &
            PREM1 (12), RUNM1 (12), ETM1 (12), HED1M1 (12), &
            HED2M1 (12), HED3M1 (12), HED4M1 (12), HED5M1 (12), &
            RCRIM1 (20, 12), RCR1M1 (12), RCR2M1 (12), &
            RCR3M1 (12), RCR4M1 (12), RCR5M1 (12)
    COMMON /BLK24/ PRC1A2, PRC2A2, PRC3A2, PRC4A2, PRC5A2, PRC6A2, &
            DRN1A2, DRN2A2, DRN3A2, DRN4A2, DRN5A2, PREA2, RUNA2, ETA2, &
            STOR2, RCR1A2, RCR2A2, RCR3A2, RCR4A2, RCR5A2, RCRIA2(20), &
            HED1A2, HED2A2, HED3A2, HED4A2, HED5A2
    COMMON /BLK25/ PRC1A1, PRC2A1, PRC3A1, PRC4A1, PRC5A1, PRC6A1, &
            DRN1A1, DRN2A1, DRN3A1, DRN4A1, DRN5A1, PREA1, RUNA1, ETA1, &
            STOR1, RCR1A1, RCR2A1, RCR3A1, RCR4A1, RCR5A1, RCRIA1(20), &
            HED1A1, HED2A1, HED3A1, HED4A1, HED5A1
    COMMON /BLK27/ IFREZ, IDFS, IFCNT, KCNT, MXKCNT
    COMMON /BLK28/ WE, XNEGHS, XLIQW, TINDEX, STORGE, EXLAG(4), TWE
    COMMON /BLK31/ DRCUL (67), DBUBUL (67), DRSUL (67), DLAMUL (67), &
            DUL(67), DFCUL(67), DWPUL(67), DSWUL(67), DTHICK(67), &
            DSUBIN(67), DCHG(67), DRCRS(67)
    COMMON /BLK32/ SHED1(12), SHED2(12), SHED3(12), SHED4(12), &
            SHED5(12)
    COMMON /BLK33/ JYEAR(MXYR)
    COMMON /BLK35/ NSEG1B, NSEG2B, NSEG3B, NSEG4B, NSEG5B, &
            NSEG1D, NSEG2D, NSEG3D, NSEG4D, NSEG5D
    !
    DIMENSION  BALY(67), BALT(67), SWULL(20), HED1S(31), HED2S(31), &
            HED3S(31), HED4S(31), HED5S(31), DRIN(68), SWULY(67)
    DATA ISTAR/'*', ' '/
    !
    WF(1) = 0.085
    WF(2) = 0.334
    WF(3) = 0.252
    WF(4) = 0.151
    WF(5) = 0.091
    WF(6) = 0.054
    WF(7) = 0.033
    OPEN (1, FILE = 'OUTPARAM.DAT', STATUS = 'OLD')
    REWIND 1
    READ (1, 5000) F4
    READ (1, 5000) F7
    READ (1, 5000) F13
    READ (1, 5000) F11
    READ (1, 5000) F10
    READ (1, 5000) F8
    5000 FORMAT(A60)
    READ (1, 5010) IU8
    5010 FORMAT(I2)
    READ (1, 5020) LMYR
    5020 FORMAT(I4)
    READ (1, 5030) IOD
    READ (1, 5030) IOM
    READ (1, 5030) IOA
    READ (1, 5030) IOS
    5030 FORMAT(I1)
    CLOSE (1)
    !
    !    LMYR IS THE NUMBER OF YEARS OF SIMULATION.
    !
    !    SETS CONTROLS FOR PRINTING OUTPUT:
    !       IOD=1 IF DAILY OUTPUT IS DESIRED.
    !       IOM=1 IF MONTHLY OUTPUT IS DESIRED.
    !       IOA=1 IF ANNUAL OUTPUT IS DESIRED.
    !       IOS=1 IF SUMMARY OUTPUT IS DESIRED.
    !
    OPEN (8, FILE = F8, STATUS = 'UNKNOWN')
    !
    !    READS CLIMATIC INPUT AND SOIL CHARACTERISTICS AND DESIGN
    !    INFORMATION.
    !
    CALL READIN
    !
    !    SETS CONTROLS FOR SUBPROFILES AND OUTPUT.
    !
    CALL CNTRLD
    !
    !    ASSIGNS THICKNESSES AND SOIL CHARACTERISTICS TO SEGMENTS.
    !
    CALL SGMNT
    !
    !    COMPUTES MAXIMUM STORAGE RETENTION PARMETER.
    !
    C2 = CN2 * CN2
    C3 = CN2 * C2
    C4 = CN2 * C3
    CN1 = 0.3750701 * CN2 + 2.756779E-03 * C2 - 1.638951E-05 * C3 + &
            5.142644E-07 * C4
    SMXN = (1000. / CN1) - 10.0
    !
    !   SMX FOR FROZEN SOIL CONDITIONS
    !
    CN22 = 95.0
    IF (CN2 >= 80) CN22 = 98.0
    C22 = CN22 * CN22
    C33 = CN22 * C22
    C44 = CN22 * C33
    CN11 = 0.3750701 * CN22 + 2.756779E-03 * C22 - 1.638951E-05 * C33 + &
            5.142644E-07 * C44
    SMXF = (1000. / CN11) - 10.
    !
    !    COMPUTES SOIL CHARACTERISTICS OF THE EVAPORATIVE ZONE.
    !
    ULE = 0.0
    DO J = 1, 7
        ULE = ULE + UL(J)
    end do
    !
    !    COMPUTES SOIL EVAPORATION COEFFICIENT.
    !
    CALL ETCOEF
    !
    !    INITIALIZES ACCOUNTING VARIABLES.
    !
    SWULE = 0.0
    DO J = 1, 7
        SWULE = SWULE + SWUL(J)
    end do
    OSWULE = 0.0
    DO J = 1, NSEG
        OSWULE = OSWULE + SWUL(J)
        DSWUL(J) = SWUL(J)
        SWULI(J) = SWUL(J)
    end do
    NSEG1B = 1
    NSEG1E = NSEG1
    NSEG1D = NSEG1E
    IF (LSEG(NSEG1) > 2) NSEG1D = NSEG1E - 1
    LAY1B = 1
    LAY1E = LP(1)
    IF (NSEG2 > 0) THEN
        NSEG2B = NSEG1 + 1
        NSEG2E = NSEG1 + NSEG2
        NSEG2D = NSEG2E
        IF (LSEG(NSEG2E) > 2) NSEG2D = NSEG2E - 1
        LAY2B = LP(1) + 1
        LAY2E = LP(2)
    END IF
    IF (NSEG3 > 0) THEN
        NSEG3B = NSEG2E + 1
        NSEG3E = NSEG2E + NSEG3
        NSEG3D = NSEG3E
        IF (LSEG(NSEG3E) > 2) NSEG3D = NSEG3E - 1
        LAY3B = LP(2) + 1
        LAY3E = LP(3)
    END IF
    IF (NSEG4 > 0) THEN
        NSEG4B = NSEG3E + 1
        NSEG4E = NSEG3E + NSEG4
        NSEG4D = NSEG4E
        IF (LSEG(NSEG4E) > 2) NSEG4D = NSEG4E - 1
        LAY4B = LP(3) + 1
        LAY4E = LP(4)
    END IF
    IF (NSEG5 > 0) THEN
        NSEG5B = NSEG4E + 1
        NSEG5E = NSEG4E + NSEG5
        NSEG5D = NSEG5E
        IF (LSEG(NSEG5E) > 2) NSEG5D = NSEG5E - 1
        LAY5B = LP(4) + 1
        LAY5E = LP(5)
    END IF
    IF (NSEG6 > 0) THEN
        NSEG6B = NSEG5E + 1
        NSEG6E = NSEG
        NSEG6D = NSEG
        LAY6B = LP(5) + 1
        LAY6E = LP(6)
    END IF
    DO J = 1, 67
        BALT(J) = 0.0D0
        BALY(J) = 0.0D0
        ET(J) = 0.0
    end do
    IODAY = 0
    RUN = 0.0
    ES1T = 0.0
    TS2 = 0.0
    DRN1 = 0.0D0
    DRN2 = 0.0D0
    DRN3 = 0.0D0
    DRN4 = 0.0D0
    DRN5 = 0.0D0
    PRC1 = 0.0D0
    PRC2 = 0.0D0
    PRC3 = 0.0D0
    PRC4 = 0.0D0
    PRC5 = 0.0D0
    PRC6 = 0.0D0
    HED1 = 0.0D0
    HED2 = 0.0D0
    HED3 = 0.0D0
    HED4 = 0.0D0
    HED5 = 0.0D0
    !    COMPILER CORRECTIONS BELOW
    RCR1 = 0.0D0
    RCR2 = 0.0D0
    RCR3 = 0.0D0
    RCR4 = 0.0D0
    RCR5 = 0.0D0
    !     COMPILER CORRECTIONS ABOVE
    ADDRUN = 0.0
    EXWAT2 = 0.0
    EXWAT3 = 0.0
    EXWAT4 = 0.0
    EXWAT5 = 0.0
    EXWAT6 = 0.0
    !     QDRN1 = 0.0D0
    !     QDRN2 = 0.0D0
    !     QDRN3 = 0.0D0
    !     QDRN4 = 0.0D0
    !     QDRN5 = 0.0D0
    !     QPRCY1 = 0.0D0
    !     QPRCY2 = 0.0D0
    !     QPRCY3 = 0.0D0
    !     QPRCY4 = 0.0D0
    !     QPRCY5 = 0.0D0
    !     QPRCY6 = 0.0D0
    !     QLATY1 = 0.0D0
    !     QLATY2 = 0.0D0
    !     QLATY3 = 0.0D0
    !     QLATY4 = 0.0D0
    !     QLATY5 = 0.0D0
    !     THY1 = 0.0D0
    !     THY2 = 0.0D0
    !     THY3 = 0.0D0
    !     THY4 = 0.0D0
    !     THY5 = 0.0D0
    !     THY6 = 0.0D0
    CALL INITIA
    IDA = 0
    ND = 365
    1060 CONTINUE
    !
    !  INITIALIZE COEFFICIENTS FOR THE HYDRO-17 SNOWMELT CALCULATIONS
    !
    CALL SNOCOEF
    !
    !    INITIALIZES TIME STEPS AND RESETS VALUES AFTER INITIALIZATION YEAR
    !
    CALL SETUPS (SWULL, BALT, BALY, EXWAT2, EXWAT3, EXWAT4, &
            EXWAT5, EXWAT6)
    !
    !    PRINTS SOIL CHARACTERISTICS AND DESIGN INFORMATION.
    !
    IF (IPRE >= 2) CALL OUTDAT (SWULL)
    !
    !   INITIALIZING VARIABLES
    !
    OSWULE = 0.0
    DO J = 1, NSEG
        OSWULE = OSWULE + DSWUL(J)
    end do
    SWULE = 0.0
    DO J = 1, 7
        SWULE = SWULE + SWUL(J)
    end do
    PSW = SWULE / EDEPTH
    DSW = SWULE / EDEPTH
    IMO = 0
    IDA = 1
    PRC1A1 = 0.0
    PRC2A1 = 0.0
    PRC3A1 = 0.0
    PRC4A1 = 0.0
    PRC5A1 = 0.0
    PRC6A1 = 0.0
    DRN1A1 = 0.0
    DRN2A1 = 0.0
    DRN3A1 = 0.0
    DRN4A1 = 0.0
    DRN5A1 = 0.0
    RCR1A1 = 0.0
    RCR2A1 = 0.0
    RCR3A1 = 0.0
    RCR4A1 = 0.0
    RCR5A1 = 0.0
    HED1A1 = 0.0
    HED2A1 = 0.0
    HED3A1 = 0.0
    HED4A1 = 0.0
    HED5A1 = 0.0
    RUNA1 = 0.0
    PREA1 = 0.0
    ETA1 = 0.0
    STOR1 = 0.0
    PRC1A2 = 0.0
    PRC2A2 = 0.0
    PRC3A2 = 0.0
    PRC4A2 = 0.0
    PRC5A2 = 0.0
    PRC6A2 = 0.0
    DRN1A2 = 0.0
    DRN2A2 = 0.0
    DRN3A2 = 0.0
    DRN4A2 = 0.0
    DRN5A2 = 0.0
    RCR1A2 = 0.0
    RCR2A2 = 0.0
    RCR3A2 = 0.0
    RCR4A2 = 0.0
    RCR5A2 = 0.0
    HED1A2 = 0.0
    HED2A2 = 0.0
    HED3A2 = 0.0
    HED4A2 = 0.0
    HED5A2 = 0.0
    RUNA2 = 0.0
    PREA2 = 0.0
    ETA2 = 0.0
    STOR2 = 0.0
    DO K = 1, 20
        RCRIA(K) = 0.0
        RCRIA1(K) = 0.0
        RCRIA2(K) = 0.0
        SUBINA(K) = 0.0
    end do
    PPRC1 = 0.0
    PPRC2 = 0.0
    PPRC3 = 0.0
    PPRC4 = 0.0
    PPRC5 = 0.0
    PPRC6 = 0.0
    PDRN1 = 0.0
    PDRN2 = 0.0
    PDRN3 = 0.0
    PDRN4 = 0.0
    PDRN5 = 0.0
    PRCR1 = 0.0
    PRCR2 = 0.0
    PRCR3 = 0.0
    PRCR4 = 0.0
    PRCR5 = 0.0
    PRUN = 0.0
    PPRE = 0.0
    PSNO = 0.0
    PHED1 = 0.0
    PHED2 = 0.0
    PHED3 = 0.0
    PHED4 = 0.0
    PHED5 = 0.0
    DO I = 1, MAXMO
        PREM1(I) = 0.0
        RUNM1(I) = 0.0
        ETM1(I) = 0.0
        DRN1M1(I) = 0.0
        DRN2M1(I) = 0.0
        DRN3M1(I) = 0.0
        DRN4M1(I) = 0.0
        DRN5M1(I) = 0.0
        RCR1M1(I) = 0.0
        RCR2M1(I) = 0.0
        RCR3M1(I) = 0.0
        RCR4M1(I) = 0.0
        RCR5M1(I) = 0.0
        HED1M1(I) = 0.0
        HED2M1(I) = 0.0
        HED3M1(I) = 0.0
        HED4M1(I) = 0.0
        HED5M1(I) = 0.0
        PRC1M1(I) = 0.0
        PRC2M1(I) = 0.0
        PRC3M1(I) = 0.0
        PRC4M1(I) = 0.0
        PRC5M1(I) = 0.0
        PRC6M1(I) = 0.0
        DO K = 1, 20
            RCRIM1(K, I) = 0.0
            RCRIM(K, I) = 0.0
            SUBINM(K, I) = 0.0
            RCRIM2(K, I) = 0.0
        end do
        PREM2(I) = 0.0
        RUNM2(I) = 0.0
        ETM2(I) = 0.0
        DRN1M2(I) = 0.0
        DRN2M2(I) = 0.0
        DRN3M2(I) = 0.0
        DRN4M2(I) = 0.0
        DRN5M2(I) = 0.0
        RCR1M2(I) = 0.0
        RCR2M2(I) = 0.0
        RCR3M2(I) = 0.0
        RCR4M2(I) = 0.0
        RCR5M2(I) = 0.0
        HED1M2(I) = 0.0
        HED2M2(I) = 0.0
        HED3M2(I) = 0.0
        HED4M2(I) = 0.0
        HED5M2(I) = 0.0
        PRC1M2(I) = 0.0
        PRC2M2(I) = 0.0
        PRC3M2(I) = 0.0
        PRC4M2(I) = 0.0
        PRC5M2(I) = 0.0
        PRC6M2(I) = 0.0
    end do
    !
    !    START YEARLY LOOP FOR SIMULATION. IYR IS THE NUMBER
    !    OF THE YEAR.
    !
    DO IYR = 1, LMYR
        PRC1A = 0.0
        PRC2A = 0.0
        PRC3A = 0.0
        PRC4A = 0.0
        PRC5A = 0.0
        PRC6A = 0.0
        DRN1A = 0.0
        DRN2A = 0.0
        DRN3A = 0.0
        DRN4A = 0.0
        DRN5A = 0.0
        RCR1A = 0.0
        RCR2A = 0.0
        RCR3A = 0.0
        RCR4A = 0.0
        RCR5A = 0.0
        HED1A = 0.0
        HED2A = 0.0
        HED3A = 0.0
        HED4A = 0.0
        HED5A = 0.0
        RUNA = 0.0
        PREA = 0.0
        ETA = 0.0
        BAL = 0.0
        STOR = 0.0
        DO I = 1, 12
            HED1M(I) = 0.0
            HED2M(I) = 0.0
            HED3M(I) = 0.0
            HED4M(I) = 0.0
            HED5M(I) = 0.0
            PRC1M(I) = 0.0
            PRC2M(I) = 0.0
            PRC3M(I) = 0.0
            PRC4M(I) = 0.0
            PRC5M(I) = 0.0
            PRC6M(I) = 0.0
            RCR1M(I) = 0.0
            RCR2M(I) = 0.0
            RCR3M(I) = 0.0
            RCR4M(I) = 0.0
            RCR5M(I) = 0.0
            DO K = 1, 20
                RCRIM(K, I) = 0.0
            end do
            DRN1M(I) = 0.0
            DRN2M(I) = 0.0
            DRN3M(I) = 0.0
            DRN4M(I) = 0.0
            DRN5M(I) = 0.0
            RUNM(I) = 0.0
            PREM(I) = 0.0
            ETM(I) = 0.0
        end do
        DO K = 1, 20
            RCRIA(K) = 0.0
        end do
        !
        !    IMO IS THE NUMBER OF THE MONTH.
        !    MO1 IS THE NUMBER OF THE MONTH OF THE YEAR.
        !
        IDAMO = 0
        MO1 = 1
        IMO = 0
        IMO = IMO + 1
        !
        !    READS AND PRINTS WEATHER DATA FOR YEAR OF SIMULATION.
        !
        CALL READCD (NYEAR, ND, NT)
        JYEAR(IYR) = NYEAR
        NFFF = 256 + 8
        NAAA = 0
        NBBB = 0
        CALL UTLTY(NFFF, NAAA, NBBB, NLLL, NHHH, NCCC, TXT)
        IF (IPRE < 2) THEN
            WRITE (*, 6000)
            6000     FORMAT(/////////&
                    12X, ' PERFORMING CALCULATIONS FOR INITIALIZATION.'/&
                    12X, ' (MAY TAKE UP TO  60 MIN  ON AN 8088 CPU WITH AN 8087,'/&
                    22X, ' UP TO  15 MIN  ON AN 80286 CPU WITH AN 80287, '/&
                    22X, ' UP TO   3 MIN  ON AN 80386 CPU WITH AN 80387, OR '/&
                    22X, ' UP TO   1 MIN  ON AN 80486 CPU.     '//)
        ELSE
            WRITE (*, 6010) JYEAR(IYR)
            6010     FORMAT(/////////&
                    12X, ' PERFORMING CALCULATIONS FOR YEAR', I5, '.'/&
                    12X, ' (MAY TAKE UP TO  60 MIN  ON AN 8088 CPU WITH AN 8087,'/&
                    22X, ' UP TO  15 MIN  ON AN 80286 CPU WITH AN 80287, '/&
                    22X, ' UP TO   3 MIN  ON AN 80386 CPU WITH AN 80387, OR '/&
                    22X, ' UP TO   1 MIN  ON AN 80486 CPU.     '//)
        END IF
        !
        !
        !    START DAILY LOOP FOR SIMULATION.
        !    IDA IS THE DAY OF THE YEAR OF SIMULATION.
        !
        DO IDA = 1, ND
            !
            !    COMPUTES THE MONTH OF YEAR FOR THE DAY OF SIMULATION, MO
            !    1OMPARES IT WITH YESTERDAY'S MONTH, MO1 AND INCREMENTS
            !    IMO IF DIFFERENT.
            !
            !
            !
            IDAMO = IDAMO + 1
            MO = MONTH (IDA, NT)
            IF (MO /= MO1) IMO = IMO + 1
            IF (MO /= MO1) IDAMO = 1
            MO1 = MO
            PREM(IMO) = PREM(IMO) + PRE(IDA)
            PREA = PREA + PRE(IDA)
            IF (PRE(IDA) > PPRE) PPRE = PRE(IDA)
            !
            !    DETERMINES IF SOIL IS FROZEN
            !
            CALL FRZCHK
            !
            !    SSTAR INDICATES FROZEN SOIL
            !
            SSTAR = ISTAR(2)
            IF (IFREZ == 1) SSTAR = ISTAR(1)
            !
            !    COMPUTES SNOWFALL AND SNOWMELT AND ADJUSTS RAIN
            !    ACCORDINGLY.
            !
            CALL SNOW (IT)
            IF (SNO > PSNO) PSNO = SNO
            !
            !    ASTAR INDICATES FREEZING TEMPERATURES.
            !
            ASTAR = ISTAR(IT)
            !
            !    COMPUTES RUNOFF.
            !
            SMX = SMXN
            IF (IFREZ > 0) SMX = SMXF
            CALL RUNOFF
            !
            !   COMPUTES SOIL TEMPERATURE AT BOTTOM OF EACH SEGMENT
            !
            CALL SOLT
            !
            !    COMPUTES SURFACE AND SOIL EVAPORATION AND POTENTIAL PLANT
            !    TRANSPIRATION.
            !
            CALL EVAPOT
            DO J = 1, 19
                IF (DABS (BALT(J)) < 1.E-08) BALT(J) = 0.0
            end do
            !
            !    COMPUTES DAILY LEAF AREA INDEX , PLANT TRANSPIRATION
            !    WHEN SOIL WATER CONTENT LIMITS
            !    PLANT TRANSPIRATION AND DISTRIBUTES EVAPOTRANSPIRATION
            !    AMONG TOP SEGMENTS.
            !
            CALL CRPMOD
            !
            !    COMPUTES LATERAL AND VERTICAL WATER ROUTING IN THE TOP
            !    SUBPROFILE.
            !
            ADDRUN = 0.0
            NPROF = 1
            CALL DRAIN (PINF, DRN1, PRC1, HED1, BALY, BALT, ADDRUN, &
                    NPROF, NSEG1B, NSEG1E, LAY1B, LAY1E, DRIN, SWULY)
            !
            !    ACCOUNTING IS PERFORMED.
            !
            IF (HED1 > PHED1) PHED1 = HED1
            SWULE = 0.0
            EXET = 0.0
            DO J = 1, 7
                IF ((DSWUL(J) + (BALT(J) / 2.0D0)) < DWPUL(J)) THEN
                    EXET2 = EXET + DWPUL(J) - DSWUL(J) - (BALT(J) / 2.0D0)
                    IF (EXET <= ETT) THEN
                        DSWUL(J) = DWPUL(J) - (BALT(J) / 2.0D0)
                        SWULY(J) = DSWUL(J)
                        EXET = EXET2
                    END IF
                END IF
                SWULE = SWULE + DSWUL(J) + BALT(J) / 2.0D0
            end do
            SWE = SWULE / EDEPTH
            IF (SWE < DSW) DSW = SWE
            IF (SWE > PSW) PSW = SWE
            !
            !    EXCESS WATER ADDED TO THE TOP SEGMENT IS DIVERTED TO RUNOFF.
            !
            RUN = RUN + ADDRUN * FRUNOF
            ADDRUN = ADDRUN * (1.0 - FRUNOF)
            IF (SNO > 0.0) THEN
                SNO = SNO + ADDRUN
                TWE = TWE + ADDRUN * 25.4
                XLIQW = XLIQW + ADDRUN * 25.4
                ADDRUN = 0.0
            END IF
            DO J = 1, NSEG1D
                SWUL(J) = DSWUL(J)
            end do
            !
            !     PERFORM ACCOUNTING OF MONTHLY AND ANNUAL TOTALS AND
            !     PEAK VALUES FOR THE TOP SUBPROFILE.
            !
            RUNM(IMO) = RUNM(IMO) + RUN
            RUNA = RUNA + RUN
            IF (RUN > PRUN) PRUN = RUN
            ETT = ETT - EXET
            ETM(IMO) = ETM(IMO) + ETT
            ETA = ETA + ETT
            HED1S(IDAMO) = HED1
            HED1M(IMO) = HED1M(IMO) + HED1
            IF(LD(1) > 0) THEN
                RCR1 = RECIR(LD(1)) * DRN1 / 100.0
                DRN1 = DRN1 - RCR1
                RCR1M(IMO) = RCR1M(IMO) + RCR1
                RCR1A = RCR1A + RCR1
                IF (RCR1 > PRCR1) PRCR1 = RCR1
            END IF
            DRN1M(IMO) = DRN1M(IMO) + DRN1
            DRN1A = DRN1A + DRN1
            IF (DRN1 > PDRN1) PDRN1 = DRN1
            IF (PRC1 > PPRC1) PPRC1 = PRC1
            PRC1M(IMO) = PRC1M(IMO) + PRC1
            PRC1A = PRC1A + PRC1
            FIN2 = PRC1 + EXWAT2
            IF (NSEG2 <= 0) GO TO 1150
            !
            !     SETS INFLOW EQUAL TO PERCOLATION FROM TOP SUBPROFILE.
            !
            !
            !     CONTROLS SIMULATION OF SECOND SUBPROFILE FROM THE TOP.
            !
            !     COMPUTES LATERAL AND VERTICAL WATER ROUTING
            !     IN THE SECOND SUBPROFILE FROM THE TOP.
            !
            NPROF = 2
            EXWAT2 = 0.0

            CALL DRAIN (FIN2, DRN2, PRC2, HED2, BALY, BALT, EXWAT2, &
                    NPROF, NSEG2B, NSEG2E, LAY2B, LAY2E, DRIN, SWULY)
            IF (HED2 > PHED2) PHED2 = HED2
            !
            !     COMPUTES SOIL WATER CONTENT IN THE
            !     SECOND SUBPROFILE FROM THE TOP.
            !
            DO J = NSEG2B, NSEG2D
                SWUL(J) = DSWUL(J)
            end do
            !
            !     PERFORM ACCOUNTING OF MONTHLY AND ANNUAL TOTALS AND
            !     PEAK VALUES FOR THE SECOND SUBPROFILE FROM THE TOP.
            !
            HED2S(IDAMO) = HED2
            HED2M(IMO) = HED2M(IMO) + HED2
            IF(LD(2) > 0) THEN
                RCR2 = RECIR(LD(2)) * DRN2 / 100.0
                DRN2 = DRN2 - RCR2
                RCR2M(IMO) = RCR2M(IMO) + RCR2
                RCR2A = RCR2A + RCR2
                IF (RCR2 > PRCR2) PRCR2 = RCR2
            END IF
            DRN2M(IMO) = DRN2M(IMO) + DRN2
            DRN2A = DRN2A + DRN2
            IF (DRN2 > PDRN2) PDRN2 = DRN2
            PRC2M(IMO) = PRC2M(IMO) + PRC2
            PRC2A = PRC2A + PRC2
            IF (PRC2 > PPRC2) PPRC2 = PRC2
            FIN3 = PRC2 + EXWAT3
            IF (NSEG3 <= 0) GO TO 1150
            !
            !     SETS INFLOW EQUAL TO PERCOLATION FROM
            !     SECOND SUBPROFILE FROM THE TOP.
            !
            !     CONTROLS SIMULATION OF THIRD SUBPROFILE FROM THE TOP.
            !
            !
            !     COMPUTES LATERAL AND VERTICAL WATER ROUTING IN
            !     THE THIRD SUBPROFILE FROM THE TOP.
            !
            NPROF = 3
            EXWAT3 = 0.0
            CALL DRAIN (FIN3, DRN3, PRC3, HED3, BALY, BALT, EXWAT3, &
                    NPROF, NSEG3B, NSEG3E, LAY3B, LAY3E, DRIN, SWULY)
            IF (HED3 > PHED3) PHED3 = HED3
            DO J = NSEG3B, NSEG3D
                SWUL(J) = DSWUL(J)
            end do
            !
            !     PERFORM ACCOUNTING OF MONTHLY AND ANNUAL TOTALS AND
            !     PEAK VALUES FOR THE THIRD SUBPROFILE FROM THE TOP.
            !
            HED3S(IDAMO) = HED3
            HED3M(IMO) = HED3M(IMO) + HED3
            IF(LD(3) > 0) THEN
                RCR3 = RECIR(LD(3)) * DRN3 / 100.0
                DRN3 = DRN3 - RCR3
                RCR3M(IMO) = RCR3M(IMO) + RCR3
                RCR3A = RCR3A + RCR3
                IF (RCR3 > PRCR3) PRCR3 = RCR3
            END IF
            DRN3M(IMO) = DRN3M(IMO) + DRN3
            DRN3A = DRN3A + DRN3
            IF (DRN3 > PDRN3) PDRN3 = DRN3
            PRC3M(IMO) = PRC3M(IMO) + PRC3
            PRC3A = PRC3A + PRC3
            IF (PRC3 > PPRC3) PPRC3 = PRC3
            FIN4 = PRC3 + EXWAT4
            IF (NSEG4 <= 0) GO TO 1150
            !
            !     SETS INFLOW EQUAL TO PERCOLATION FROM
            !     THIRD SUBPROFILE FROM THE TOP.
            !
            !
            !     CONTROLS SIMULATION OF FOURTH SUBPROFILE FROM THE TOP.
            !
            !
            !     COMPUTES LATERAL AND VERTICAL WATER ROUTING IN
            !     THE FOURTH SUBPROFILE FROM THE TOP.
            !
            NPROF = 4
            EXWAT4 = 0.0
            CALL DRAIN (FIN4, DRN4, PRC4, HED4, BALY, BALT, EXWAT4, &
                    NPROF, NSEG4B, NSEG4E, LAY4B, LAY4E, DRIN, SWULY)
            IF (HED4 > PHED4) PHED4 = HED4
            !
            !     COMPUTES SOIL WATER CONTENT IN THE THIRD
            !     SUBPROFILE FROM THE TOP.
            !
            DO J = NSEG4B, NSEG4D
                SWUL(J) = DSWUL(J)
            end do
            !
            !     PERFORM ACCOUNTING OF MONTHLY AND ANNUAL TOTALS AND
            !     PEAK VALUES FOR THE THIRD SUBPROFILE FROM THE TOP.
            !
            HED4S(IDAMO) = HED4
            HED4M(IMO) = HED4M(IMO) + HED4
            IF(LD(4) > 0) THEN
                RCR4 = RECIR(LD(4)) * DRN4 / 100.0
                DRN4 = DRN4 - RCR4
                RCR4M(IMO) = RCR4M(IMO) + RCR4
                RCR4A = RCR4A + RCR4
                IF (RCR4 > PRCR4) PRCR4 = RCR4
            END IF
            DRN4M(IMO) = DRN4M(IMO) + DRN4
            DRN4A = DRN4A + DRN4
            IF (DRN4 > PDRN4) PDRN4 = DRN4
            PRC4M(IMO) = PRC4M(IMO) + PRC4
            PRC4A = PRC4A + PRC4
            IF (PRC4 > PPRC4) PPRC4 = PRC4
            FIN5 = PRC4 + EXWAT5
            IF (NSEG5 <= 0) GO TO 1150
            !
            !     SETS INFLOW EQUAL TO PERCOLATION FROM
            !     FOURTH SUBPROFILE FROM THE TOP.
            !
            !     CONTROLS SIMULATION OF FIFTH SUBPROFILE FROM THE TOP.
            !
            !
            !     COMPUTES LATERAL AND VERTICAL WATER ROUTING IN
            !     THE FIFTH SUBPROFILE FROM THE TOP.
            !
            NPROF = 5
            EXWAT5 = 0.0
            CALL DRAIN (FIN5, DRN5, PRC5, HED5, BALY, BALT, EXWAT5, &
                    NPROF, NSEG5B, NSEG5E, LAY5B, LAY5E, DRIN, SWULY)
            IF (HED5 > PHED5) PHED5 = HED5
            DO J = NSEG5B, NSEG5D
                SWUL(J) = DSWUL(J)
            end do
            !
            !     PERFORM ACCOUNTING OF MONTHLY AND ANNUAL TOTALS AND
            !     PEAK VALUES FOR THE THIRD SUBPROFILE FROM THE TOP.
            !
            HED5S(IDAMO) = HED5
            HED5M(IMO) = HED5M(IMO) + HED5
            IF(LD(5) > 0) THEN
                RCR5 = RECIR(LD(5)) * DRN5 / 100.0
                DRN5 = DRN5 - RCR5
                RCR5M(IMO) = RCR5M(IMO) + RCR5
                RCR5A = RCR5A + RCR5
                IF (RCR5 > PRCR5) PRCR5 = RCR5
            END IF
            DRN5M(IMO) = DRN5M(IMO) + DRN5
            DRN5A = DRN5A + DRN5
            IF (DRN5 > PDRN5) PDRN5 = DRN5
            PRC5M(IMO) = PRC5M(IMO) + PRC5
            PRC5A = PRC5A + PRC5
            IF (PRC5 > PPRC5) PPRC5 = PRC5
            FIN6 = PRC5 + EXWAT6
            IF (NSEG6 <= 0) GO TO 1150
            !
            !     SETS INFLOW EQUAL TO PERCOLATION FROM
            !     SECOND SUBPROFILE FROM THE TOP.
            !
            !     CONTROLS SIMULATION OF THIRD SUBPROFILE FROM THE TOP.
            !
            !
            !     COMPUTES LATERAL AND VERTICAL WATER ROUTING IN
            !     THE THIRD SUBPROFILE FROM THE TOP.
            !
            NPROF = 6
            EXWAT6 = 0.0
            CALL DRAIN (FIN6, DUMMY1, PRC6, DUMMY2, BALY, BALT, EXWAT6, &
                    NPROF, NSEG6B, NSEG6E, LAY6B, LAY6E, DRIN, SWULY)
            DO J = NSEG6B, NSEG6D
                SWUL(J) = DSWUL(J)
            end do
            !
            !     PERFORM ACCOUNTING OF MONTHLY AND ANNUAL TOTALS AND
            !     PEAK VALUES FOR THE THIRD SUBPROFILE FROM THE TOP.
            !
            PRC6M(IMO) = PRC6M(IMO) + PRC6
            PRC6A = PRC6A + PRC6
            IF (PRC6 > PPRC6) PPRC6 = PRC6
            !
            !     CALLS SUBROUTINE OUTDAY IF DAILY OUTPUT IS DESIRED.
            !
            1150     CONTINUE
            CALL RECIRC (IMO)
            IF (IOD == 1 .AND. IPRE >= 2) CALL OUTDAY (SWE, &
                    IODAY, ASTAR, SSTAR, NVAR)
            !
            !     END OF DAILY LOOP.
            !
            IF (IDA == ND) GO TO 1210
            IDAP1 = IDA + 1
            MO = MONTH (IDAP1, NT)
            IF (MO == MO1) GO TO 1100
            1210     CONTINUE
            HD1M2 = 0.0
            HD2M2 = 0.0
            HD3M2 = 0.0
            HD4M2 = 0.0
            HD5M2 = 0.0
            DO I = 1, IDAMO
                HD1M2 = HD1M2 + (HED1S(I) * HED1S(I))
                HD2M2 = HD2M2 + (HED2S(I) * HED2S(I))
                HD3M2 = HD3M2 + (HED3S(I) * HED3S(I))
                HD4M2 = HD4M2 + (HED4S(I) * HED4S(I))
                HD5M2 = HD5M2 + (HED5S(I) * HED5S(I))
            end do
            HD1M2 = ((IDAMO * HD1M2) - (HED1M(IMO) * HED1M(IMO))) / &
                    (IDAMO * (IDAMO - 1))
            HD2M2 = ((IDAMO * HD2M2) - (HED2M(IMO) * HED2M(IMO))) / &
                    (IDAMO * (IDAMO - 1))
            HD3M2 = ((IDAMO * HD3M2) - (HED3M(IMO) * HED3M(IMO))) / &
                    (IDAMO * (IDAMO - 1))
            HD4M2 = ((IDAMO * HD4M2) - (HED4M(IMO) * HED4M(IMO))) / &
                    (IDAMO * (IDAMO - 1))
            HD5M2 = ((IDAMO * HD5M2) - (HED5M(IMO) * HED5M(IMO))) / &
                    (IDAMO * (IDAMO - 1))
            HED1M(IMO) = HED1M(IMO) / IDAMO
            HED2M(IMO) = HED2M(IMO) / IDAMO
            HED3M(IMO) = HED3M(IMO) / IDAMO
            HED4M(IMO) = HED4M(IMO) / IDAMO
            HED5M(IMO) = HED5M(IMO) / IDAMO
            SHED1(MO1) = 0.0
            SHED2(MO1) = 0.0
            SHED3(MO1) = 0.0
            SHED4(MO1) = 0.0
            SHED5(MO1) = 0.0
            IF (HD1M2 > 0.0) SHED1(MO1) = HD1M2**0.5
            IF (HD2M2 > 0.0) SHED2(MO1) = HD2M2**0.5
            IF (HD3M2 > 0.0) SHED3(MO1) = HD3M2**0.5
            IF (HD4M2 > 0.0) SHED4(MO1) = HD4M2**0.5
            IF (HD5M2 > 0.0) SHED5(MO1) = HD5M2**0.5
        1100   CONTINUE
        end do
        PSWULE = EXWAT2 + EXWAT3 + EXWAT4 + EXWAT5 + EXWAT6
        !
        !     COMPUTES TOTAL SOIL WATER STORAGE.
        !
        SSNO = ADDRUN + SNO
        !        XLIQW = XLIQW + ADDRUN*25.4
        !        ADDRUN = 0.0
        DO J = 1, NSEG
            PSWULE = PSWULE + DSWUL(J) + (BALT(J) / 2.0D0)
        end do
        !
        !     COMPUTES YEARLY WATER BUDGET BALANCE CHECK.
        !
        SUBINY = SUBINF * FLOAT(ND)
        STOR = PSWULE - OSWULE + SSNO - OLDSNO
        BAL = PREA - RUNA - ETA - STOR&
                - DRN1A - DRN2A - DRN3A - DRN4A&
                - DRN5A + SUBINY
        IF (NSEG6 > 0) THEN
            BAL = BAL - PRC6A
        ELSE IF (NSEG5 > 0) THEN
            BAL = BAL - PRC5A
        ELSE IF (NSEG4 > 0) THEN
            BAL = BAL - PRC4A
        ELSE IF (NSEG3 > 0) THEN
            BAL = BAL - PRC3A
        ELSE IF (NSEG2 > 0) THEN
            BAL = BAL - PRC2A
        ELSE
            BAL = BAL - PRC1A
        END IF
        !
        !     CALLS SUBROUTINE OUTMO IF MONTHLY OUTPUT IS DESIRED.
        !
        IF (IPRE >= 2 .AND. (IOM == 1 .OR. IOS == 1))&
                CALL OUTMO
        !
        !     CALLS SUBROUTINE OUTYR TO PRINT ANNUAL TOTALS.
        !
        IF (IPRE >= 2 .AND. (IOA == 1 .OR. IOS == 1))&
                CALL OUTYR
        !
        !     INITIALIZES VARIABLES FOR THE NEXT YEAR.
        !
        OSWULE = PSWULE
        OLDSNO = SSNO
        !
        !     END OF YEARLY LOOP.
        !
        IF (IPRE < 2) THEN
            IPRE = IPRE + 2
            DO INSEG = 1, NSEG
                DSWUL(INSEG) = DSWUL(INSEG) + DCHG(INSEG) / 2.D0
                SWUL(INSEG) = DSWUL(INSEG)
                DCHG(INSEG) = 0.0D0
                BALT(INSEG) = 0.0D0
                BALY(INSEG) = 0.0D0
            end do
            GO TO 1060
        END IF
    end do
    REWIND 4
    REWIND 7
    REWIND 11
    REWIND 13
    CLOSE (4)
    CLOSE (7)
    CLOSE (11)
    CLOSE (13)
    !    COMPILER CORRECTIONS BELOW
    !     Do subprofiles exist? (NSEGnB not initialized, use NSEGn)
    IF (NSEG2 > 0) DSWUL(NSEG2B) = DSWUL(NSEG2B) + EXWAT2
    IF (NSEG3 > 0) DSWUL(NSEG3B) = DSWUL(NSEG3B) + EXWAT3
    IF (NSEG4 > 0) DSWUL(NSEG4B) = DSWUL(NSEG4B) + EXWAT4
    IF (NSEG5 > 0) DSWUL(NSEG5B) = DSWUL(NSEG5B) + EXWAT5
    IF (NSEG6 > 0) DSWUL(NSEG6B) = DSWUL(NSEG6B) + EXWAT6
    !     COMPILER CORRECTIONS ABOVE
    EXWAT2 = 0.0
    EXWAT3 = 0.0
    EXWAT4 = 0.0
    EXWAT5 = 0.0
    EXWAT6 = 0.0
    !
    !     CALLS SUBROUTINE OUTAV TO PRINT AVERAGE
    !     RESULTS FOR THE SIMULATION PERIOD.
    !
    IF (IOS == 1) CALL OUTAV
    !
    !     CALLS SUBROUTINE OUTPEK TO PRINT PEAK
    !     RESULTS FOR THE SIMULATION PERIOD.
    !
    IF (IOS == 1) CALL OUTPEK
    CALL OUTSW
    !
    CLOSE (8)
    !
    !
    STOP
END
!
!
!        *********************** LEAP ************************
!
!
!     INTEGER FUNCTION SUBPROGRAM LEAP DETERMINES
!     WHETHER A YEAR IS A LEAP YEAR
!
INTEGER FUNCTION LEAP (NYEAR, NT)
    !
    !     IF DIVISIBLE BY FOUR, NYEAR IS A LEAP YEAR AND NT IS SET
    !     TO 1; ELSE, NYEAR IS NOT A LEAP YEAR AND NT = 0.
    !
    IF (MOD (NYEAR, 4) /= 0) THEN
        LEAP = 365
        NT = 0
    ELSE
        LEAP = 366
        NT = 1
    END IF
    RETURN
END
!
!
!      ************************* MONTH *************************
!
!
!     THIS INTEGER FUNCTION SUBPROGRAM DETERMINES THE
!     NUMBER OF THE MONTH FOR A GIVEN JULIAN DATE AND
!     A FLAG FOR A LEAP YEAR. FLAG EQUALS 1 FOR A LEAP
!     YEAR AND 0 FOR NON LEAP YEAR.
!
INTEGER FUNCTION MONTH (JDAY, NT)
    DIMENSION ICAL (12)
    !
    !     CAL(12) ARE THE JULIAN DATES OF THE LAST DAY
    !     OF EACH MONTH OF A LEAP YEAR.
    !
    DATA ICAL/31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365/
    JTMP = JDAY
    !
    !     ADJUSTS JULIAN DATE FOR JULIAN DATES AFTER JANUARY.
    !
    IF (JDAY > ICAL (2)) JTMP = JDAY - NT
    !
    !     COMPARES ADJUSTED JULIAN DATE WITH ARRAY OF JULIAN DATES
    !     FOR THE LAST DAY OF EACH MONTH TO DETERMINE MONTH.
    !
    DO I = 1, 12
        IF (JTMP <= ICAL (I)) GO TO 1010
    end do
    I = 1
    1010 CONTINUE
    MONTH = I
    !
    !     RETURNS NUMBER OF THE MONTH.
    !
    RETURN
END
!
!      ******************** SCAN *************************
!
!
!    SUBROUTINE SCAN IS USED TO READ ALL UNFORMATTED
!    NUMERIC INPUT FROM THE USER. 74 COLUMNS MAY BE USED
!    FOR A LINE OF INPUT CONTAINING UP TO 10 VALUES.
!
SUBROUTINE SCAN (NO, VALUE, M7, KLM)
    CHARACTER*1 KLM (74), NUM (11), IPOINT, IPLUS, MINUS, ICOMP
    DIMENSION VALUE (12)
    DATA IPOINT, IPLUS, MINUS/'.', '+', '-'/
    DATA NUM/'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'/
    K7 = M7 + 1
    DO I = 1, 10
        VALUE (I) = 0.
    end do
    NCOL = 1
    N = 1
    KPT = 0
    1010 CONTINUE
    IF (KLM (NCOL) /= MINUS) GO TO 1020
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
    KPT = 1
    GO TO 1060
    1080 CONTINUE
    K = 0
    ICOMP = NUM (1)
    1100 CONTINUE
    IF (KLM (NCOL) == ICOMP) GO TO 1110
    CONTINUE
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
    IF (KLM (NCOL) /= IPOINT) GO TO 1180
    1190 CONTINUE
    KPT = 1
    GO TO 1070
    1180 CONTINUE
    K = 0
    ICOMP = NUM (1)
    1200 CONTINUE
    IF (KLM (NCOL) == ICOMP) GO TO 1210
    1220 CONTINUE
    K = K + 1
    ICOMP = NUM (K + 1)
    IF (K - 10) 1200, 1170, 1170
    1170 CONTINUE
    VALUE (N) = VALUE (N) * SGN
    N = N + 1
    KPT = 0
    GO TO 1140
    1210 CONTINUE
    IF (KPT) 1230, 1240, 1230
    1240 CONTINUE
    VALUE (N) = VALUE (N) * 10. + K
    GO TO 1070
    1230 CONTINUE
    VALUE (N) = VALUE (N) + K * 10.**(- KPT)
    KPT = KPT + 1
    GO TO 1070
END
!
!
!
!      ************************* READCD *************************
!
!    SUBROUTINE READCD READS A YEAR OF WEATHER DATA AT THE START
!    OF EACH YEAR OF SIMULATION.
!
SUBROUTINE READCD (NYEAR, ND, NT)
    COMMON /BLK1/ IT4, IT7, IT13, IU4, IU7, IU13, IU11, IU10, IU8, &
            LMYR, IOD, IOM, IOA, IOS
    COMMON /BLK8/ PRE(370), TMPF(366), RAD(366)
    !
    !    READS PRECIPITATION DATA.
    !
    DO K = 1, 37
        J = 10 * K
        I = J - 9
        READ (4, 5000) NYEAR, (PRE (N), N = I, J)
        5000   FORMAT(I10, 10F5.2)
    end do
    !
    !    DETERMINES IF THE YEAR IS A LEAP YEAR.
    !
    ND = LEAP (NYEAR, NT)
    !
    !    READS DAILY TEMPERATURE VALUES AND DAILY RADIATION VALUES.
    !    TEMPERATURE IN DEG F, SOLAR RADIATION VALUES IN LANGLEYS PER DAY.
    !
    DO K = 1, 37
        J = 10 * K
        I = J - 9
        IF (J > 366) J = 366
        READ (7, 5010) MYEAR, (TMPF (N), N = I, J)
        IF (IU13 == 1) READ (13, 5010) LYEAR, (RAD (N), N = I, J)
        IF (IU13 /= 1) READ (13, 5011) LYEAR, (RAD (N), N = I, J)
        5010    FORMAT(I5, 10F6.1)
        5011    FORMAT(I5, 10F6.0)
    end do
    IF (MYEAR /= NYEAR) WRITE (*, 6000) MYEAR, NYEAR
    IF (MYEAR /= NYEAR) WRITE (8, 6000) MYEAR, NYEAR
    6000 FORMAT(1X/' WARNING:  TEMPERATURE FOR YEAR', I5, ' USED ', &
            'WITH PRECIPITATION FOR YEAR', I5/)
    IF (LYEAR /= NYEAR) WRITE (*, 6010) LYEAR, NYEAR
    IF (LYEAR /= NYEAR) WRITE (8, 6010) LYEAR, NYEAR
    6010 FORMAT(1X/' WARNING:  SOLAR RADIATION FOR YEAR', I5, ' USED ', &
            'WITH PRECIPITATION FOR YEAR', I5/)
    IF (IU4 /= 1) THEN
        DO N = 1, 366
            PRE(N) = PRE(N) / 25.4
        end do
    END IF
    IF (IU7 /= 1) THEN
        DO N = 1, 366
            TMPF(N) = (TMPF(N) * 1.8) + 32.0
        end do
    END IF
    IF (IU13 /= 1) THEN
        DO N = 1, 366
            RAD(N) = RAD(N) * 23.89
        end do
    END IF
    RETURN
END
!
!
!      ************************* READIN *************************
!
!  SUBROUTINE READIN READS ET AND SOIL DESIGN DATA, AND WEATHER HEADERS
!
SUBROUTINE READIN
    !
    CHARACTER*40 CITY4, CITY7, CITY13, CITY11
    CHARACTER*60 F4, F7, F13, F11, F10, F8, TITLE
    COMMON /BLK0/ F4, F7, F13, F11, F10, F8, TITLE
    COMMON /BLK1/ IT4, IT7, IT13, IU4, IU7, IU13, IU11, IU10, IU8, &
            LMYR, IOD, IOM, IOA, IOS
    COMMON /BLK2/ CITY4, CITY7, CITY13, CITY11
    COMMON /BLK3/ PORO (20), FC (20), WP (20), RC (20), SW (20), &
            RS (20), XLAMBD (20), BUB (20), THICK (20), SLOPE (20), &
            XLENG (20), CON (20), SUBIN (20), RECIR (20), PHOLE (20), &
            DEFEC (20), TRANS (20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN(20), LSIN(20)
    COMMON /BLK5/ AREA, FRUNOF, CN2, OCN2, SSLOPE, SLENG, SMX
    COMMON /BLK6/ ULAI, WIND, RH (366), OSNO, ULAT
    COMMON /BLK18/ RM (12), TM (12), RLAT
    COMMON /BLK19/ IPL, IHV, IPRE, IRUN, ITSOIL, IVEG
    COMMON /BLK26/ TMPSUM, TMPAVG, TAVG(30)
    COMMON /BLK27/ IFREZ, IDFS, IFCNT, KCNT, MXKCNT
    !
    DIMENSION DRAD(31)
    !
    !
    !    READS STATE AND CITY , MAX LAI, PLANTING AND HARVESTING DATES,
    !    PROBABILITIES, AND MEAN MONTHLY TEMPERATURES IN DEGREES
    !    FAHRENHEIT FROM *.D11.
    !
    IERR = 0
    OPEN (4, FILE = F4, STATUS = 'OLD', ERR = 1000)
    1010 CONTINUE
    OPEN (7, FILE = F7, STATUS = 'OLD', ERR = 1020)
    1030 CONTINUE
    OPEN (13, FILE = F13, STATUS = 'OLD', ERR = 1040)
    1050 CONTINUE
    OPEN (11, FILE = F11, STATUS = 'OLD', ERR = 1060)
    !
    1070 CONTINUE
    IF (IERR > 0) THEN
        WRITE (*, 6000)
        6000    FORMAT (1X/' CORRECT THE DOS PATH AND FILE NAMES BEFORE ', &
                'RESTARTING.'/)
        CLOSE (4)
        CLOSE (7)
        CLOSE (13)
        CLOSE (11)
        STOP
    END IF
    !
    REWIND (4)
    REWIND (7)
    REWIND (13)
    REWIND (11)
    READ (4, 5040) IT4, IU4, CITY4, (RM (I), I = 1, 12)
    READ (7, 5040) IT7, IU7, CITY7, (TM (I), I = 1, 12)
    READ (13, 5040) IT13, IU13, CITY13, RLAT
    READ (11, 5050) IU11, CITY11
    READ (11, 5060) ULAT, IPL, IHV, ULAI, EDEPTH, WIND, HUM1, HUM2, &
            HUM3, HUM4
    5040 FORMAT (I2/I2/A40/12F6.2)
    5050 FORMAT (I2/A40)
    5060 FORMAT (F10.2, I4, I4, F7.0, F8.0, 5F5.0)
    CLOSE (11)
    !
    IF (IU7 == 1) THEN
        DO I = 1, 12
            TM (I) = (TM (I) - 32.0) / 1.8
        end do
    END IF
    IF (IU11 /= 1) EDEPTH = EDEPTH / 2.54
    IF (IU11 /= 1) WIND = WIND / 1.609
    DO I = 1, 91
        RH (I) = HUM1
    end do
    DO I = 92, 182
        RH (I) = HUM2
    end do
    DO I = 183, 274
        RH (I) = HUM3
    end do
    DO I = 275, 366
        RH (I) = HUM4
    end do
    !
    !   READS DATA10
    !
    OCN2 = 0.
    SSLOPE = 0.
    SLENG = 0.
    IVEG = 0
    ITSOIL = 0
    LAY = 0
    !    COMPILER CORRECTIONS BELOW
    DO I = 1, 20
        LAYR (I) = 0
    end do
    !     COMPILER CORRECTIONS ABOVE
    OPEN (10, FILE = F10, STATUS = 'OLD', ERR = 1120)
    REWIND 10
    READ (10, 5070) TITLE
    5070 FORMAT(A60)
    READ (10, 5080) IU10, IPRE, OSNO, AREA, FRUNOF, IRUN
    5080 FORMAT(I2, I2, 2F10.0, F6.0, I2)
    IF (IRUN == 1) READ (10, 5090) CN2
    IF (IRUN == 2) READ (10, 5100) OCN2, SSLOPE, SLENG, CN2
    IF (IRUN == 3) READ (10, 5110) SSLOPE, SLENG, ITSOIL, IVEG, CN2
    5090 FORMAT(F7.0)
    5100 FORMAT(4F7.0)
    5110 FORMAT(2F7.0, 2I3, F7.0)
    !
    DO J = 1, 20
        READ (10, 5120, END = 1140) LAYER (J), THICK (J), ISOIL (J), &
                PORO (J), FC (J), WP (J), SW (J), RC (J)
        LAY = LAY + 1
        READ (10, 5130, END = 1140) XLENG (J), SLOPE (J), RECIR (J), &
                LAYR (J), SUBIN (J), PHOLE (J), DEFEC (J), IPQ (J), TRANS (J)
    end do
    5120 FORMAT(I2, F7.0, I4, 4F6.0, F16.0)
    5130 FORMAT(F7.0, 2F6.0, I3, F13.0, 2F7.0, I2, G14.6)
    1140 CONTINUE
    !
    CLOSE (10)
    !
    !   DETERMINES THE BROOKS-COREY PARAMETERS AND EVAPORATION COEFFICIENT
    !
    DO J = 1, LAY
        IF (LAYER (J) == 4) THEN
            PORO (J) = .04
            FC (J) = .03
            WP (J) = .02
            SW (J) = .04
        END IF
        IF (WP(J) < 0.04) THEN
            RS(J) = 0.6 * WP(J)
        ELSE
            RS (J) = 0.014 + 0.253 * WP (J)
        END IF
        IF (WP (J) < 0.02) RS (J) = 0.75 * WP (J)
        IF (WP (J) <= 0.0) WP (J) = 0.001
        IF (FC (J) <= WP(J)) FC(J) = WP(J) + 0.001
        IF (PORO (J) <= FC(J)) PORO(J) = FC(J) + 0.001
        XLAMBD (J) = ALOG ((FC (J) - RS (J)) / (WP (J) - RS&
                (J))) / ALOG (45.)
        RELFC = (FC (J) - RS (J)) / (PORO (J) - RS (J))
        BUB(J) = (RELFC**(1. / XLAMBD (J))) * 1033.4 / 3.
        IF (BUB(J) > 103.34) BUB(J) = 103.34
        CON (J) = 2.44 + (1485216. * RC (J) * ((BUB(J) / 103.34)**&
                ((3. * XLAMBD (J)) + 2.)))
        IF (RELFC < 0.20) CON (J) = 3.30
        IF (RC (J) < 0.000005) CON (J) = 3.30
        IF (CON (J) < 3.30) CON (J) = 3.30
        IF (CON (J) > 5.50) CON (J) = 5.50
        IF (LAYER (J) == 4) THEN
            PORO (J) = .0
            FC (J) = .0
            WP (J) = .0
            SW (J) = .0
        END IF
    end do
    !
    !     INITIALIZE FROZEN SOIL MODEL PARAMETERS
    !
    IFREZ = 0
    IFCNT = 0
    TMPAVG = 33.0
    TMPSUM = 990.0
    RADTOT = 0.0
    DO I = 1, 30
        TAVG(I) = 33.0
    end do
    DO I = 1, 100
        IF (ULAT >= 0.0) THEN
            READ(13, 5400, END = 1410) (DRAD(N), N = 1, 31)
        ELSE
            READ(13, 5410, END = 1410) (DRAD(N), N = 1, 31)
            READ(13, 5420, END = 1410)
        END IF
        DO N = 1, 31
            RADTOT = RADTOT + DRAD(N)
        end do
        NYRS = I
    end do
    5400 FORMAT(33(/), 29X, 6F6.1/2(5X, 10F6.1/), 5X, 5F6.1)
    5410 FORMAT(15(/), 11X, 9F6.1/2(5X, 10F6.1/), 5X, 2F6.1)
    5420 FORMAT(17(/))
    1410 IF (NYRS > 1) THEN
        RADAVG = RADTOT / NYRS / 31
    ELSE
        RADAVG = RADTOT / 31
    END IF
    IF (IU13 /= 1) RADAVG = RADAVG * 23.89
    IDFS = 35.4 - (0.154 * RADAVG)
    IF (IDFS < 1) IDFS = 1
    MXKCNT = (IDFS + 2) / 3
    !
    REWIND 13
    READ (13, 5040) IT13, IU13, CITY13, RLAT
    !
    RETURN
    !
    1000 CONTINUE
    IERR = IERR + 1
    WRITE (*, 6010) F4
    6010 FORMAT (1X/' PRECIPITATION DATA FILE NOT FOUND!!!  '/1X, A60/)
    GO TO 1010
    1020 CONTINUE
    IERR = IERR + 1
    WRITE (*, 6020) F7
    6020 FORMAT (1X/' TEMPERATURE DATA FILE NOT FOUND!!!  '/1X, A60/)
    GO TO 1030
    1040 CONTINUE
    IERR = IERR + 1
    WRITE (*, 6030) F13
    6030 FORMAT (1X/' SOLAR RADIATION DATA FILE NOT FOUND!!!  '/1X, A60/)
    GO TO 1050
    1060 CONTINUE
    IERR = IERR + 1
    WRITE (*, 6040) F11
    6040 FORMAT (1X/' EVAPOTRANSPIRATION DATA FILE NOT FOUND!!!'/1X, A60/)
    GO TO 1070
    1120 CONTINUE
    IERR = IERR + 1
    WRITE (*, 6050) F10
    6050 FORMAT (1X/' SOIL AND DESIGN DATA FILE NOT FOUND!!!  '/1X, A60/)
    GO TO 1070
END
!
!
!      ************************** SGMNT *************************
!
!
!    SUBROUTINE SGMNT BREAKS THE LAYERS INTO MODELING SEGMENTS AND
!       ASSIGNS THICKNESS, AND PROPERTIES TO THE SEGMENTS.
!
SUBROUTINE SGMNT
    !
    DOUBLE PRECISION DSWUL, DTHICK, DRCUL, DBUBUL, &
            DWPUL, DLAMUL, DRSUL, DUL, DFCUL, DSUBIN, DCHG, DRCRS
    COMMON /BLK1/ IT4, IT7, IT13, IU4, IU7, IU13, IU11, IU10, IU8, &
            LMYR, IOD, IOM, IOA, IOS
    COMMON /BLK3/ PORO(20), FC(20), WP(20), RC(20), SW(20), &
            RS(20), XLAMBD(20), BUB(20), THICK(20), SLOPE(20), &
            XLENG(20), CON(20), SUBIN(20), RECIR(20), PHOLE(20), &
            DEFEC(20), TRANS(20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN(20), LSIN(20)
    COMMON /BLK5/ AREA, FRUNOF, CN2, OCN2, SSLOPE, SLENG, SMX
    COMMON /BLK6/ ULAI, WIND, RH (366), OSNO, ULAT
    COMMON /BLK7/ STHICK(67), UL(67), FCUL(67), WPUL(67), SWUL(67), &
            RCUL(67), RSUL(67), XLAMBU(67), CONUL(67), BUBUL(67), &
            SUBINS(67), SWULI(67)
    COMMON /BLK9/ PINF, ES1T, SMELT, GMRO, ADDRUN, OLDSNO, SNO, WF(7), &
            SSNO
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK17/ NSEG1, NSEG2, NSEG3, NSEG4, NSEG5, NSEG6, NSEG
    COMMON /BLK19/ IPL, IHV, IPRE, IRUN, ITSOIL, IVEG
    COMMON /BLK31/ DRCUL (67), DBUBUL (67), DRSUL (67), DLAMUL (67), &
            DUL(67), DFCUL(67), DWPUL(67), DSWUL(67), DTHICK(67), &
            DSUBIN(67), DCHG(67), DRCRS(67)
    DIMENSION RCULS(67)
    !
    !   INITIALIZES THE NUMBER OF SEGMENTS IN EACH SUBPROFILE AND PROPERTIES
    !
    !
    NSEG1 = 0
    NSEG2 = 0
    NSEG3 = 0
    NSEG4 = 0
    NSEG5 = 0
    NSEG6 = 0
    DO N = 1, 5
        LCASE(N) = 0
    end do
    THICK1 = 0.0
    CORECT = 1.0
    IF (ISOIL(1)<=34 .AND. ISOIL(1)>=1)THEN
        CORECT = 1.0 + 0.5966 * ULAI + 0.132659 * ULAI**2 + &
                0.1123454 * ULAI**3 - 0.04777627 * ULAI**4 + &
                0.004325035 * ULAI**5
        IF (CORECT > 5.0) CORECT = 5.0
    END IF
    DO J = 1, 67
        STHICK(J) = 0.0
    end do
    !
    !    INITIALIZES THE MOISTURE CONTENTS OF THE LAYERS TO NEAR STEADY-
    !    STATE WHEN USER REQUESTS PROGRAM TO INITIALIZE SOIL MOISTURES.
    !    1W OF FML's = 0.0  AND  SW OF BARRIER LINER = POROSITY
    !
    !    ALSO SUMS TOTAL ANNUAL SUBSURFACE INFLOW  (SUBINF)
    !
    SUBINF = 0.0
    DO K = 1, LAY
        LSEGS(K, 1) = 0
        LSEGS(K, 2) = 0
        SUBIN(K) = SUBIN(K) / 365.0
        SUBINF = SUBINF + SUBIN(K)
        IF (IPRE == 0 .OR. IPRE==2) SW(K) = FC(K)
        IF (LAYER(K) == 4) SW(K) = 0.0
        IF (LAYER(K) == 3) SW(K) = PORO(K)
    end do
    FRUNOF = FRUNOF / 100.0
    IF (IU10 == 2) OSNO = OSNO / 25.4
    OLDSNO = OSNO
    IF (IU10 == 2) THEN
        SLENG = SLENG * 3.281
        SUBINF = SUBINF / 25.4
        DO J = 1, LAY
            DEFEC(J) = DEFEC(J) / 2.471
            PHOLE(J) = PHOLE(J) / 2.471
            THICK(J) = THICK(J) / 2.54
            XLENG(J) = XLENG(J) * 3.281
            SUBIN(J) = SUBIN(J) / 25.4
            !         TRANS(J) = TRANS(J) / 2.54 / 2.54
        end do
    END IF
    !
    !    COMPUTES DEPTH TO SURFACE OF TOP LINER TO LIMIT EVAPORATIVE DEPTH
    !
    DO J = 1, LP(1)
        IF (LAYER(J) == 3 .OR. LAYER(J) == 4) GO TO 1110
        THICK1 = THICK1 + THICK(J)
    end do
    1110 CONTINUE
    IF (THICK1 < EDEPTH) EDEPTH = THICK1
    !
    !    SETS THE EVAPORATIVE ZONE TO SEVEN SEGMENTS AND ASSIGNS
    !    THE THICKNESS OF THE TOP SEVEN SEGMENTS.
    !
    STHICK(1) = EDEPTH / 36.0
    STHICK(2) = 5.0 * EDEPTH / 36.0
    TTHICK = STHICK(1) + STHICK(2)
    DO J = 3, 6
        STHICK(J) = EDEPTH / 6.0
        TTHICK = TTHICK + STHICK(J)
    end do
    STHICK(7) = EDEPTH - TTHICK
    !
    !    LAYSEG(#, 1) = # OF TOP LAYER IN SEGMENT & SOIL LAYER PROPERTIES
    !    LAYSEG(#, 2) = # OF BOTTOM LAYER IN SEGMENT OR LAYER # OF AN FML
    !
    J = 1
    THICK1 = THICK(J)
    THICK2 = 0.0
    DO K = 1, 7
        THICK2 = THICK2 + STHICK(K)
        LAYSEG(K, 1) = J
        1140   CONTINUE
        IF (THICK2 <= THICK1) LAYSEG(K, 2) = J
        IF (THICK2 > THICK1) THEN
            J = J + 1
            THICK1 = THICK1 + THICK(J)
            GO TO 1140
        END IF
    end do
    !
    !    SEGMENT ABOVE LINER LIMITED TO 18 INCHES
    !
    !    LAYERS BELOW EVAPORATIVE ZONE LIMITED TO 3 SEGMENTS
    !
    !    LAYERS GREATER THAN 36 INCHES ASSIGNED 3 SEGMENTS; TOP SEGMENT &
    !      BOTTOM SEGMENT ARE EACH 12 INCHES
    !
    !    THICKNESS OF TOP SEGMENT OF LAYERS HAVING MORE THAN 1 SEGMENT IS
    !      ASSIGNED TO EQUAL 12 INCHES
    !
    !    LAYERS LESS THAN 18 INCHES ASSIGNED 1 SEGMENT
    !
    !
    !    ASSIGNS THE THICKNESS OF THE REMAINING SEGMENTS IN THE
    !      TOP SUBPROFILE
    !
    NSEGS = 7
    THICK1 = 0.0
    THICK2 = EDEPTH + 0.0002
    DO J = 1, LP(1)
        LAYSEG(NSEGS + 1, 2) = J
        THICK1 = THICK1 + THICK(J)
        IF (THICK1 > (EDEPTH + 0.0001)) THEN
            THICKL = THICK1 - THICK2
            IF (LAYER(J) == 3) THEN
                STHICK(NSEGS + 1) = THICKL
                LAYSEG(NSEGS + 1, 1) = J
                LSEG(NSEGS + 1) = LAYER(J)
                IF (J /= LP(1)) LSEG(NSEGS + 1) = 5
                IF (LAYER(J - 1) == 4) LSEG(NSEGS + 1) = 4
                NSEGS = NSEGS + 1
            ELSE IF (LAYER(J) == 4) THEN
                IF (J == LP(1)) THEN
                    IF (LAYER(J - 1) /= 3) THEN
                        IF ((J + 1) <= LAY) THEN
                            IF (RC(J + 1) < RC(J - 1)) THEN
                                IF (THICK(J + 1) > 18.) THEN
                                    STHICK(NSEGS + 1) = 12.
                                ELSE
                                    STHICK(NSEGS + 1) = THICK(J + 1)
                                END IF
                                LSEG(NSEGS + 1) = 6
                                LAYSEG(NSEGS + 1, 1) = J + 1
                            ELSE
                                STHICK(NSEGS + 1) = STHICK(NSEGS)
                                LSEG(NSEGS + 1) = 7
                                LAYSEG(NSEGS + 1, 1) = J - 1
                            END IF
                        ELSE
                            STHICK(NSEGS + 1) = STHICK(NSEGS)
                            LSEG(NSEGS + 1) = 7
                            LAYSEG(NSEGS + 1, 1) = J - 1
                        END IF
                        NSEGS = NSEGS + 1
                    END IF
                END IF
            ELSE IF (THICKL > 36.) THEN
                STHICK(NSEGS + 1) = 12.
                STHICK(NSEGS + 2) = THICKL - 12. - 12.
                STHICK(NSEGS + 3) = 12.
                LAYSEG(NSEGS + 1, 1) = J
                LAYSEG(NSEGS + 1, 2) = J
                LAYSEG(NSEGS + 2, 1) = J
                LAYSEG(NSEGS + 2, 2) = J
                LAYSEG(NSEGS + 3, 1) = J
                LAYSEG(NSEGS + 3, 2) = J
                LSEG(NSEGS + 1) = LAYER(J)
                LSEG(NSEGS + 2) = LAYER(J)
                LSEG(NSEGS + 3) = LAYER(J)
                NSEGS = NSEGS + 3
            ELSE IF (THICKL > 18.) THEN
                STHICK(NSEGS + 1) = 12.
                STHICK(NSEGS + 2) = THICKL - 12.
                LAYSEG(NSEGS + 1, 1) = J
                LAYSEG(NSEGS + 1, 2) = J
                LAYSEG(NSEGS + 2, 1) = J
                LAYSEG(NSEGS + 2, 2) = J
                LSEG(NSEGS + 1) = LAYER(J)
                LSEG(NSEGS + 2) = LAYER(J)
                NSEGS = NSEGS + 2
            ELSE
                STHICK(NSEGS + 1) = THICKL
                LAYSEG(NSEGS + 1, 1) = J
                LAYSEG(NSEGS + 1, 2) = J
                LSEG(NSEGS + 1) = LAYER(J)
                NSEGS = NSEGS + 1
            END IF
            THICK2 = THICK1
        END IF
    end do
    NSEG1 = NSEGS
    IF (LAY <= LP(1)) GO TO 1160
    DO J = (LP(1) + 1), LP(2)
        LAYSEG(NSEGS + 1, 2) = J
        IF (LAYER(J) == 3) THEN
            STHICK(NSEGS + 1) = THICK(J)
            LAYSEG(NSEGS + 1, 1) = J
            LSEG(NSEGS + 1) = LAYER(J)
            IF (J /= LP(2)) LSEG(NSEGS + 1) = 5
            IF (LAYER(J - 1) == 4) LSEG(NSEGS + 1) = 4
            NSEGS = NSEGS + 1
        ELSE IF (LAYER(J) == 4) THEN
            IF (J == LP(2)) THEN
                IF (LAYER(J - 1) /= 3) THEN
                    IF ((J + 1) <= LAY) THEN
                        IF (RC(J + 1) < RC(J - 1)) THEN
                            IF (THICK(J + 1) > 18.) THEN
                                STHICK(NSEGS + 1) = 12.
                            ELSE
                                STHICK(NSEGS + 1) = THICK(J + 1)
                            END IF
                            LSEG(NSEGS + 1) = 6
                            LAYSEG(NSEGS + 1, 1) = J + 1
                        ELSE
                            STHICK(NSEGS + 1) = STHICK(NSEGS)
                            LSEG(NSEGS + 1) = 7
                            LAYSEG(NSEGS + 1, 1) = J - 1
                        END IF
                    ELSE
                        STHICK(NSEGS + 1) = STHICK(NSEGS)
                        LSEG(NSEGS + 1) = 7
                        LAYSEG(NSEGS + 1, 1) = J - 1
                    END IF
                    NSEGS = NSEGS + 1
                END IF
            END IF
        ELSE IF (THICK(J) > 36.) THEN
            STHICK(NSEGS + 1) = 12.
            STHICK(NSEGS + 2) = THICK(J) - 12. - 12.
            STHICK(NSEGS + 3) = 12.
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LAYSEG(NSEGS + 2, 1) = J
            LAYSEG(NSEGS + 2, 2) = J
            LAYSEG(NSEGS + 3, 1) = J
            LAYSEG(NSEGS + 3, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            LSEG(NSEGS + 2) = LAYER(J)
            LSEG(NSEGS + 3) = LAYER(J)
            NSEGS = NSEGS + 3
        ELSE IF (THICK(J) > 18.) THEN
            STHICK(NSEGS + 1) = 12.
            STHICK(NSEGS + 2) = THICK(J) - 12.
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LAYSEG(NSEGS + 2, 1) = J
            LAYSEG(NSEGS + 2, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            LSEG(NSEGS + 2) = LAYER(J)
            NSEGS = NSEGS + 2
        ELSE
            STHICK(NSEGS + 1) = THICK(J)
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            NSEGS = NSEGS + 1
        END IF
    end do
    NSEG2 = NSEGS - NSEG1
    IF (LAY <= LP(2)) GO TO 1160
    DO J = (LP(2) + 1), LP(3)
        LAYSEG(NSEGS + 1, 2) = J
        IF (LAYER(J) == 3) THEN
            STHICK(NSEGS + 1) = THICK(J)
            LAYSEG(NSEGS + 1, 1) = J
            LSEG(NSEGS + 1) = LAYER(J)
            IF (J /= LP(3)) LSEG(NSEGS + 1) = 5
            IF (LAYER(J - 1) == 4) LSEG(NSEGS + 1) = 4
            NSEGS = NSEGS + 1
        ELSE IF (LAYER(J) == 4) THEN
            IF (J == LP(3)) THEN
                IF (LAYER(J - 1) /= 3) THEN
                    IF ((J + 1) <= LAY) THEN
                        IF (RC(J + 1) < RC(J - 1)) THEN
                            IF (THICK(J + 1) > 18.) THEN
                                STHICK(NSEGS + 1) = 12.
                            ELSE
                                STHICK(NSEGS + 1) = THICK(J + 1)
                            END IF
                            LSEG(NSEGS + 1) = 6
                            LAYSEG(NSEGS + 1, 1) = J + 1
                        ELSE
                            STHICK(NSEGS + 1) = STHICK(NSEGS)
                            LSEG(NSEGS + 1) = 7
                            LAYSEG(NSEGS + 1, 1) = J - 1
                        END IF
                    ELSE
                        STHICK(NSEGS + 1) = STHICK(NSEGS)
                        LSEG(NSEGS + 1) = 7
                        LAYSEG(NSEGS + 1, 1) = J - 1
                    END IF
                    NSEGS = NSEGS + 1
                END IF
            END IF
        ELSE IF (THICK(J) > 36.) THEN
            STHICK(NSEGS + 1) = 12.
            STHICK(NSEGS + 2) = THICK(J) - 12. - 12.
            STHICK(NSEGS + 3) = 12.
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LAYSEG(NSEGS + 2, 1) = J
            LAYSEG(NSEGS + 2, 2) = J
            LAYSEG(NSEGS + 3, 1) = J
            LAYSEG(NSEGS + 3, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            LSEG(NSEGS + 2) = LAYER(J)
            LSEG(NSEGS + 3) = LAYER(J)
            NSEGS = NSEGS + 3
        ELSE IF (THICK(J) > 18.) THEN
            STHICK(NSEGS + 1) = 12.
            STHICK(NSEGS + 2) = THICK(J) - 12.
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LAYSEG(NSEGS + 2, 1) = J
            LAYSEG(NSEGS + 2, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            LSEG(NSEGS + 2) = LAYER(J)
            NSEGS = NSEGS + 2
        ELSE
            STHICK(NSEGS + 1) = THICK(J)
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            NSEGS = NSEGS + 1
        END IF
    end do
    NSEG3 = NSEGS - NSEG1 - NSEG2
    IF (LAY <= LP(3)) GO TO 1160
    DO J = (LP(3) + 1), LP(4)
        LAYSEG(NSEGS + 1, 2) = J
        IF (LAYER(J) == 3) THEN
            STHICK(NSEGS + 1) = THICK(J)
            LAYSEG(NSEGS + 1, 1) = J
            LSEG(NSEGS + 1) = LAYER(J)
            IF (J /= LP(4)) LSEG(NSEGS + 1) = 5
            IF (LAYER(J - 1) == 4) LSEG(NSEGS + 1) = 4
            NSEGS = NSEGS + 1
        ELSE IF (LAYER(J) == 4) THEN
            IF (J == LP(4)) THEN
                IF (LAYER(J - 1) /= 3) THEN
                    IF ((J + 1) <= LAY) THEN
                        IF (RC(J + 1) < RC(J - 1)) THEN
                            IF (THICK(J + 1) > 18.) THEN
                                STHICK(NSEGS + 1) = 12.
                            ELSE
                                STHICK(NSEGS + 1) = THICK(J + 1)
                            END IF
                            LSEG(NSEGS + 1) = 6
                            LAYSEG(NSEGS + 1, 1) = J + 1
                        ELSE
                            STHICK(NSEGS + 1) = STHICK(NSEGS)
                            LSEG(NSEGS + 1) = 7
                            LAYSEG(NSEGS + 1, 1) = J - 1
                        END IF
                    ELSE
                        STHICK(NSEGS + 1) = STHICK(NSEGS)
                        LSEG(NSEGS + 1) = 7
                        LAYSEG(NSEGS + 1, 1) = J - 1
                    END IF
                    NSEGS = NSEGS + 1
                END IF
            END IF
        ELSE IF (THICK(J) > 36.) THEN
            STHICK(NSEGS + 1) = 12.
            STHICK(NSEGS + 2) = THICK(J) - 12. - 12.
            STHICK(NSEGS + 3) = 12.
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LAYSEG(NSEGS + 2, 1) = J
            LAYSEG(NSEGS + 2, 2) = J
            LAYSEG(NSEGS + 3, 1) = J
            LAYSEG(NSEGS + 3, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            LSEG(NSEGS + 2) = LAYER(J)
            LSEG(NSEGS + 3) = LAYER(J)
            NSEGS = NSEGS + 3
        ELSE IF (THICK(J) > 18.) THEN
            STHICK(NSEGS + 1) = 12.
            STHICK(NSEGS + 2) = THICK(J) - 12.
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LAYSEG(NSEGS + 2, 1) = J
            LAYSEG(NSEGS + 2, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            LSEG(NSEGS + 2) = LAYER(J)
            NSEGS = NSEGS + 2
        ELSE
            STHICK(NSEGS + 1) = THICK(J)
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            NSEGS = NSEGS + 1
        END IF
    end do
    NSEG4 = NSEGS - NSEG1 - NSEG2 - NSEG3
    IF (LAY <= LP(4)) GO TO 1160
    DO J = (LP(4) + 1), LP(5)
        LAYSEG(NSEGS + 1, 2) = J
        IF (LAYER(J) == 3) THEN
            STHICK(NSEGS + 1) = THICK(J)
            LAYSEG(NSEGS + 1, 1) = J
            LSEG(NSEGS + 1) = LAYER(J)
            IF (J /= LP(5)) LSEG(NSEGS + 1) = 5
            IF (LAYER(J - 1) == 4) LSEG(NSEGS + 1) = 4
            NSEGS = NSEGS + 1
        ELSE IF (LAYER(J) == 4) THEN
            IF (J == LP(5)) THEN
                IF (LAYER(J - 1) /= 3) THEN
                    IF ((J + 1) <= LAY) THEN
                        IF (RC(J + 1) < RC(J - 1)) THEN
                            IF (THICK(J + 1) > 18.) THEN
                                STHICK(NSEGS + 1) = 12.
                            ELSE
                                STHICK(NSEGS + 1) = THICK(J + 1)
                            END IF
                            LSEG(NSEGS + 1) = 6
                            LAYSEG(NSEGS + 1, 1) = J + 1
                        ELSE
                            STHICK(NSEGS + 1) = STHICK(NSEGS)
                            LSEG(NSEGS + 1) = 7
                            LAYSEG(NSEGS + 1, 1) = J - 1
                        END IF
                    ELSE
                        STHICK(NSEGS + 1) = STHICK(NSEGS)
                        LSEG(NSEGS + 1) = 7
                        LAYSEG(NSEGS + 1, 1) = J - 1
                    END IF
                    NSEGS = NSEGS + 1
                END IF
            END IF
        ELSE IF (THICK(J) > 36.) THEN
            STHICK(NSEGS + 1) = 12.
            STHICK(NSEGS + 2) = THICK(J) - 12. - 12.
            STHICK(NSEGS + 3) = 12.
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LAYSEG(NSEGS + 2, 1) = J
            LAYSEG(NSEGS + 2, 2) = J
            LAYSEG(NSEGS + 3, 1) = J
            LAYSEG(NSEGS + 3, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            LSEG(NSEGS + 2) = LAYER(J)
            LSEG(NSEGS + 3) = LAYER(J)
            NSEGS = NSEGS + 3
        ELSE IF (THICK(J) > 18.) THEN
            STHICK(NSEGS + 1) = 12.
            STHICK(NSEGS + 2) = THICK(J) - 12.
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LAYSEG(NSEGS + 2, 1) = J
            LAYSEG(NSEGS + 2, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            LSEG(NSEGS + 2) = LAYER(J)
            NSEGS = NSEGS + 2
        ELSE
            STHICK(NSEGS + 1) = THICK(J)
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            NSEGS = NSEGS + 1
        END IF
    end do
    NSEG5 = NSEGS - NSEG1 - NSEG2 - NSEG3 - NSEG4
    IF (LAY <= LP(5)) GO TO 1160
    DO J = (LP(5) + 1), LP(6)
        IF (THICK(J) > 36.) THEN
            STHICK(NSEGS + 1) = 12.
            STHICK(NSEGS + 2) = THICK(J) - 12. - 12.
            STHICK(NSEGS + 3) = 12.
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LAYSEG(NSEGS + 2, 1) = J
            LAYSEG(NSEGS + 2, 2) = J
            LAYSEG(NSEGS + 3, 1) = J
            LAYSEG(NSEGS + 3, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            LSEG(NSEGS + 2) = LAYER(J)
            LSEG(NSEGS + 3) = LAYER(J)
            NSEGS = NSEGS + 3
        ELSE IF (THICK(J) > 18.) THEN
            STHICK(NSEGS + 1) = 12.
            STHICK(NSEGS + 2) = THICK(J) - 12.
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LAYSEG(NSEGS + 2, 1) = J
            LAYSEG(NSEGS + 2, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            LSEG(NSEGS + 2) = LAYER(J)
            NSEGS = NSEGS + 2
        ELSE
            STHICK(NSEGS + 1) = THICK(J)
            LAYSEG(NSEGS + 1, 1) = J
            LAYSEG(NSEGS + 1, 2) = J
            LSEG(NSEGS + 1) = LAYER(J)
            NSEGS = NSEGS + 1
        END IF
    end do
    1160 CONTINUE
    !
    NSEG6 = NSEGS - NSEG1 - NSEG2 - NSEG3 - NSEG4 - NSEG5
    NSEG = NSEGS
    !
    !    COMPUTES THE EFFECTIVE SOIL CHARACTERISTICS OF THE SEGMENTS,
    !    AVERAGED BY THICKNESS OF A SEGMENT IN MULTIPLE LAYERS.
    !    CORRECTS SEGMENTS 5 TO 7 OF THE TOP LAYER OF DEFAULT SOIL FOR
    !    VEGETATION EFFECTS ON HYDRAULIC CONDUCTIVITY.
    !
    DEPSEG = 0.0
    DEPTHS = 0.0
    DEPTHL = THICK(1)
    K = 1
    LSEGS(K, 1) = 1
    RC1 = RC(1)
    RC(1) = RC(1) * CORECT
    DO J = 1, 7
        UL(J) = 0.0
        WPUL(J) = 0.0
        FCUL(J) = 0.0
        CONUL(J) = 0.0
        RSUL(J) = 0.0
        XLAMBU(J) = 0.0
        SWUL(J) = 0.0
        RCULS(J) = 0.0
        BUBUL(J) = 0.0
        SUBINS(J) = 0.0
        IF (J == 5) RC(1) = RC1
        IF (DEPSEG >= DEPTHL) THEN
            K = K + 1
            LSEGS(K, 1) = J
            DEPTHL = DEPTHL + THICK(K)
        END IF
        DEPTHS = DEPTHS + STHICK(J)
        1230   CONTINUE
        LSEG(J) = LAYER(K)
        IF (DEPTHS <= DEPTHL) GO TO 1240
        DSEG = DEPTHL - DEPSEG
        IF(DSEG>0.0001) LSEGS(K, 2) = J
        DEPSEG = DEPTHL
        UL(J) = PORO(K) * DSEG + UL(J)
        FCUL(J) = FC(K) * DSEG + FCUL(J)
        WPUL(J) = WP(K) * DSEG + WPUL(J)
        CONUL(J) = CON(K) * DSEG + CONUL(J)
        BUBUL(J) = BUB(K) * DSEG + BUBUL(J)
        RCULS(J) = RC(K) * DSEG + RCULS(J)
        RSUL(J) = RS(K) * DSEG + RSUL(J)
        XLAMBU(J) = XLAMBD(K) * DSEG + XLAMBU(J)
        SUBINS(J) = SUBIN(K) * DSEG / THICK(K) + SUBINS(J)
        SWUL(J) = SW(K) * DSEG + SWUL(J)
        K = K + 1
        LSEGS(K, 1) = J
        DEPTHL = DEPTHL + THICK(K)
        GO TO 1230
        1240   CONTINUE
        DSEG = DEPTHS - DEPSEG
        IF(DSEG>0.0001) LSEGS(K, 2) = J
        UL(J) = PORO(K) * DSEG + UL(J)
        FCUL(J) = FC(K) * DSEG + FCUL(J)
        WPUL(J) = WP(K) * DSEG + WPUL(J)
        CONUL(J) = CON(K) * DSEG + CONUL(J)
        BUBUL(J) = BUB(K) * DSEG + BUBUL(J)
        RCULS(J) = RC(K) * DSEG + RCULS(J)
        RSUL(J) = RS(K) * DSEG + RSUL(J)
        XLAMBU(J) = XLAMBD(K) * DSEG + XLAMBU(J)
        SUBINS(J) = SUBIN(K) * DSEG / THICK(K) + SUBINS(J)
        SWUL(J) = SW(K) * DSEG + SWUL(J)
        DEPSEG = DEPTHS
    end do
    IF (NSEG > 7) THEN
        K = LAYSEG(7, 2)
        DO J = 8, NSEG
            IF (LAYSEG(J, 2) /= K) THEN
                K = LAYSEG(J, 2)
                LSEGS(K, 1) = J
            END IF
            LSEGS(K, 2) = J
            UL(J) = PORO(LAYSEG(J, 1)) * STHICK(J)
            FCUL(J) = FC(LAYSEG(J, 1)) * STHICK(J)
            WPUL(J) = WP(LAYSEG(J, 1)) * STHICK(J)
            CONUL(J) = CON(LAYSEG(J, 1)) * STHICK(J)
            BUBUL(J) = BUB(LAYSEG(J, 1)) * STHICK(J)
            RCULS(J) = RC(LAYSEG(J, 1)) * STHICK(J)
            RSUL(J) = RS(LAYSEG(J, 1)) * STHICK(J)
            XLAMBU(J) = XLAMBD(LAYSEG(J, 1)) * STHICK(J)
            IF (LSEG(J) < 3) THEN
                SUBINS(J) = SUBIN(LAYSEG(J, 2)) * STHICK(J) / THICK(LAYSEG(J, 2))
            ELSE
                SUBINS(J) = SUBIN(LAYSEG(J, 2))
            END IF
            IF (LSEG(J) == 4) THEN
                SUBINS(J) = SUBINS(J) + SUBIN(LAYSEG(J, 2) - 1)
            END IF
            SWUL(J) = SW(LAYSEG(J, 1)) * STHICK(J)
        end do
    END IF
    !
    !    CONVERTS THE HYDRAULIC CONDUCTIVITY FROM CM/SEC TO IN/DAY.
    !
    DO J = 1, NSEG
        RCUL(J) = (RCULS(J) * 24. * 1417.3) / STHICK(J)
        XLAMBU(J) = XLAMBU(J) / STHICK(J)
        BUBUL(J) = BUBUL(J) / STHICK(J)
        CONUL(J) = CONUL(J) / STHICK(J)
        DRCUL(J) = RCUL(J)
        DUL(J) = UL(J)
        DRSUL(J) = RSUL(J)
        DLAMUL(J) = XLAMBU(J)
        DSUBIN(J) = SUBINS(J)
        DBUBUL(J) = BUBUL(J)
        DWPUL(J) = WPUL(J)
        DTHICK(J) = STHICK(J)
        DSWUL(J) = SWUL(J)
        DFCUL(J) = FCUL(J)
    end do
    DO K = 1, LAY
        IF (LAYR(K) > 0) THEN
            IF (LAYER(LAYR(K)) <= 2) THEN
                DO II = 1, 5
                    IF (K == LD(II)) THEN
                        TTHICK = 0.0
                        DO J = LSEGS(LAYR(K), 1), LSEGS(LAYR(K), 2)
                            TTHICK = TTHICK + STHICK(J)
                        end do
                    END IF
                end do
            END IF
        END IF
    end do
    N = 1
    NSEGE = NSEG1
    IF (LP(1) == LAY) THEN
        DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(1))
        SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(1))
        DSUBIN(NSEGE) = 0.0D0
        SUBINS(NSEGE) = 0.0
        IF ((LP(1) - 1) == 3 .OR. (LP(1) - 1) == 4) THEN
            DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(1) - 1)
            SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(1) - 1)
        END IF
    ELSE IF (LSEG(NSEGE) == 5) THEN
        DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(1) - 1)
        SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(1) - 1)
        DSUBIN(NSEGE) = SUBIN (LP(1))
        SUBINS(NSEGE) = SUBIN (LP(1))
    END IF
    IF (LSEG(NSEGE) > 3) THEN
        IF (LSEG(NSEGE) == 4) LCASE(N) = 3
        IF (LSEG(NSEGE) == 5) LCASE(N) = 4
        IF (LSEG(NSEGE) == 6) THEN
            IF (RC(LAYSEG(NSEGE, 1)) > 0.0999) THEN
                LCASE(N) = 1
            ELSE IF (RC(LAYSEG(NSEGE, 1)) <= 0.0000999) THEN
                LCASE(N) = 3
            ELSE
                LCASE(N) = 2
            END IF
        END IF
        IF (LSEG(NSEGE) == 7) THEN
            IF (RC(LAYSEG(NSEGE, 1)) > 0.0999) THEN
                LCASE(N) = 1
            ELSE IF (RC(LAYSEG(NSEGE, 1)) <= 0.0000999) THEN
                LCASE(N) = 4
            ELSE
                LCASE(N) = 2
            END IF
        END IF
        IF (LPQ(N) == 6) THEN
            IF (LCASE(N) == 2 .OR. LCASE(N) == 3) THEN
                LCASE(N) = 5
            ELSE IF (LCASE(N) == 4) THEN
                LCASE(N) = 6
            END IF
        END IF
    END IF
    N = 2
    IF (LP(N) > 0) THEN
        NSEGE = NSEGE + NSEG2
        IF (LP(2) == LAY) THEN
            DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(2))
            SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(2))
            DSUBIN(NSEGE) = 0.0D0
            SUBINS(NSEGE) = 0.0
            IF ((LP(2) - 1) == 3 .OR. (LP(2) - 1) == 4) THEN
                DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(2) - 1)
                SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(2) - 1)
            END IF
        ELSE IF (LSEG(NSEGE) == 5) THEN
            DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(2) - 1)
            SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(2) - 1)
            DSUBIN(NSEGE) = SUBIN (LP(2))
            SUBINS(NSEGE) = SUBIN (LP(2))
        END IF
        IF (LSEG(NSEGE) > 3) THEN
            IF (LSEG(NSEGE) == 4) LCASE(N) = 3
            IF (LSEG(NSEGE) == 5) LCASE(N) = 4
            IF (LSEG(NSEGE) == 6) THEN
                IF (RC(LAYSEG(NSEGE, 1)) > 0.0999) THEN
                    LCASE(N) = 1
                ELSE IF (RC(LAYSEG(NSEGE, 1)) <= 0.0000999) THEN
                    LCASE(N) = 3
                ELSE
                    LCASE(N) = 2
                END IF
            END IF
            IF (LSEG(NSEGE) == 7) THEN
                IF (RC(LAYSEG(NSEGE, 1)) > 0.099) THEN
                    LCASE(N) = 1
                ELSE IF (RC(LAYSEG(NSEGE, 1)) <= 0.0000999) THEN
                    LCASE(N) = 4
                ELSE
                    LCASE(N) = 2
                END IF
            END IF
            IF (LPQ(N) == 6) THEN
                IF (LCASE(N) == 2 .OR. LCASE(N) == 3) THEN
                    LCASE(N) = 5
                ELSE IF (LCASE(N) == 4) THEN
                    LCASE(N) = 6
                END IF
            END IF
        END IF
    END IF
    N = 3
    IF (LP(N) > 0) THEN
        NSEGE = NSEGE + NSEG3
        IF (LP(3) == LAY) THEN
            DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(3))
            SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(3))
            DSUBIN(NSEGE) = 0.0D0
            SUBINS(NSEGE) = 0.0
            IF ((LP(3) - 1) == 3 .OR. (LP(3) - 1) == 4) THEN
                DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(3) - 1)
                SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(3) - 1)
            END IF
        ELSE IF (LSEG(NSEGE) == 5) THEN
            DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(3) - 1)
            SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(3) - 1)
            DSUBIN(NSEGE) = SUBIN (LP(3))
            SUBINS(NSEGE) = SUBIN (LP(3))
        END IF
        IF (LSEG(NSEGE) > 3) THEN
            IF (LSEG(NSEGE) == 4) LCASE(N) = 3
            IF (LSEG(NSEGE) == 5) LCASE(N) = 4
            IF (LSEG(NSEGE) == 6) THEN
                IF (RC(LAYSEG(NSEGE, 1)) > 0.0999) THEN
                    LCASE(N) = 1
                ELSE IF (RC(LAYSEG(NSEGE, 1)) <= 0.0000999) THEN
                    LCASE(N) = 3
                ELSE
                    LCASE(N) = 2
                END IF
            END IF
            IF (LSEG(NSEGE) == 7) THEN
                IF (RC(LAYSEG(NSEGE, 1)) > 0.099) THEN
                    LCASE(N) = 1
                ELSE IF (RC(LAYSEG(NSEGE, 1)) <= 0.0000999) THEN
                    LCASE(N) = 4
                ELSE
                    LCASE(N) = 2
                END IF
            END IF
            IF (LPQ(N) == 6) THEN
                IF (LCASE(N) == 2 .OR. LCASE(N) == 3) THEN
                    LCASE(N) = 5
                ELSE IF (LCASE(N) == 4) THEN
                    LCASE(N) = 6
                END IF
            END IF
        END IF
    END IF
    N = 4
    IF (LP(N) > 0) THEN
        NSEGE = NSEGE + NSEG4
        IF (LP(4) == LAY) THEN
            DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(4))
            SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(4))
            DSUBIN(NSEGE) = 0.0D0
            SUBINS(NSEGE) = 0.0
            IF ((LP(4) - 1) == 3 .OR. (LP(4) - 1) == 4) THEN
                DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(4) - 1)
                SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(4) - 1)
            END IF
        ELSE IF (LSEG(NSEGE) == 5) THEN
            DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(4) - 1)
            SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(4) - 1)
            DSUBIN(NSEGE) = SUBIN (LP(4))
            SUBINS(NSEGE) = SUBIN (LP(4))
        END IF
        IF (LSEG(NSEGE) > 3) THEN
            IF (LSEG(NSEGE) == 4) LCASE(N) = 3
            IF (LSEG(NSEGE) == 5) LCASE(N) = 4
            IF (LSEG(NSEGE) == 6) THEN
                IF (RC(LAYSEG(NSEGE, 1)) > 0.0999) THEN
                    LCASE(N) = 1
                ELSE IF (RC(LAYSEG(NSEGE, 1)) <= 0.0000999) THEN
                    LCASE(N) = 3
                ELSE
                    LCASE(N) = 2
                END IF
            END IF
            IF (LSEG(NSEGE) == 7) THEN
                IF (RC(LAYSEG(NSEGE, 1)) > 0.099) THEN
                    LCASE(N) = 1
                ELSE IF (RC(LAYSEG(NSEGE, 1)) <= 0.0000999) THEN
                    LCASE(N) = 4
                ELSE
                    LCASE(N) = 2
                END IF
            END IF
            IF (LPQ(N) == 6) THEN
                IF (LCASE(N) == 2 .OR. LCASE(N) == 3) THEN
                    LCASE(N) = 5
                ELSE IF (LCASE(N) == 4) THEN
                    LCASE(N) = 6
                END IF
            END IF
        END IF
    END IF
    N = 5
    IF (LP(N) > 0) THEN
        NSEGE = NSEGE + NSEG5
        IF (LP(5) == LAY) THEN
            DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(5))
            SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(5))
            DSUBIN(NSEGE) = 0.0D0
            SUBINS(NSEGE) = 0.0
            IF ((LP(5) - 1) == 3 .OR. (LP(5) - 1) == 4) THEN
                DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(5) - 1)
                SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(5) - 1)
            END IF
        ELSE IF (LSEG(NSEGE) == 5) THEN
            DSUBIN(NSEGE - 1) = DSUBIN(NSEGE - 1) + SUBIN (LP(5) - 1)
            SUBINS(NSEGE - 1) = SUBINS(NSEGE - 1) + SUBIN (LP(5) - 1)
            DSUBIN(NSEGE) = SUBIN (LP(5))
            SUBINS(NSEGE) = SUBIN (LP(5))
        END IF
        IF (LSEG(NSEGE) > 3) THEN
            IF (LSEG(NSEGE) == 4) LCASE(N) = 3
            IF (LSEG(NSEGE) == 5) LCASE(N) = 4
            IF (LSEG(NSEGE) == 6) THEN
                IF (RC(LAYSEG(NSEGE, 1)) > 0.0999) THEN
                    LCASE(N) = 1
                ELSE IF (RC(LAYSEG(NSEGE, 1)) <= 0.0000999) THEN
                    LCASE(N) = 3
                ELSE
                    LCASE(N) = 2
                END IF
            END IF
            IF (LSEG(NSEGE) == 7) THEN
                IF (RC(LAYSEG(NSEGE, 1)) > 0.099) THEN
                    LCASE(N) = 1
                ELSE IF (RC(LAYSEG(NSEGE, 1)) <= 0.0000999) THEN
                    LCASE(N) = 4
                ELSE
                    LCASE(N) = 2
                END IF
            END IF
            IF (LPQ(N) == 6) THEN
                IF (LCASE(N) == 2 .OR. LCASE(N) == 3) THEN
                    LCASE(N) = 5
                ELSE IF (LCASE(N) == 4) THEN
                    LCASE(N) = 6
                END IF
            END IF
        END IF
    END IF
    !
    RCES = 0.0
    DO J = 1, 7
        RCES = RCES + WF(J) * RCUL(J)
    end do
    RCES = RCES / 34016.
    RCES = -1.0 * ALOG10 (RCES)
    SEDMX = 4.606745 * (1.595155**RCES)
    IF (SEDMX < 18.) SEDMX = 18.
    IF (SEDMX > 48.) SEDMX = 48.
    IF (EDEPTH > SEDMX) THEN
        SEDEP = SEDMX
    ELSE
        SEDEP = EDEPTH
    END IF
    RETURN
END
!
!
!      ************************* CNTRLD *************************
!
SUBROUTINE CNTRLD
    !
    !     SUBROUTINE CNTRLD DETERMINES THE DRAINAGE AND
    !     PERCOLATION LAYERS FOR OUTPUT.
    !
    !
    COMMON /BLK3/ PORO(20), FC(20), WP(20), RC(20), SW(20), &
            RS(20), XLAMBD(20), BUB(20), THICK(20), SLOPE(20), &
            XLENG(20), CON(20), SUBIN(20), RECIR(20), PHOLE(20), &
            DEFEC(20), TRANS(20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN (20), LSIN (20)
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    !
    NL = 0
    DO I = 1, 5
        LP(I) = 0
        LPQ(I) = 0
        LD(I) = 0
    end do
    LP(6) = 0
    DO I = 1, LAY
        LRIN (I) = 0
        LSIN (I) = 0
        IF (LAYER (I) == 3 .or. LAYER (I) == 4) THEN
            IF (LAYER (I - 1) /= 3 .AND. LAYER (I - 1) /= 4)&
                    NL = NL + 1
            IF (NL >= 1) THEN
                LP(NL) = I
                IF (LAYER (I) == 4) THEN
                    IF (IPQ (I) < 1 .OR. IPQ (I) > 6) IPQ (I) = 4
                    LPQ (NL) = IPQ (I)
                END IF
            END IF
        END IF
    end do
    DO I = 1, 5
        IF (LP(I) >= 2) THEN
            IF (LAYER (LP(I) - 1) == 2) THEN
                LD(I) = LP(I) - 1
            ELSE IF (LP(I) > 2) THEN
                IF (LAYER (LP(I) - 2) == 2) LD(I) = LP(I) - 2
            END IF
        END IF
    end do
    IF (LAYER (LAY) /= 3 .AND. LAYER (LAY) /= 4) THEN
        IF (NL == 0) LP(1) = LAY
        IF (NL == 1 .and. LAY > LP(1)) LP(2) = LAY
        IF (NL == 2 .and. LAY > LP(2)) LP(3) = LAY
        IF (NL == 3 .and. LAY > LP(3)) LP(4) = LAY
        IF (NL == 4 .and. LAY > LP(4)) LP(5) = LAY
        IF (NL == 5 .and. LAY > LP(5)) LP(6) = LAY
    END IF
    DO N = 1, 5
        !    COMPILER CORRECTIONS BELOW
        IF (LD(N) /= 0 .and. layr(ld(n)) /= 0) THEN
            IF (LAYER(LAYR(LD(N))) > 2) THEN
                LRIN(LAYR(LD(N))) = 0
                RECIR(LAYR(LD(N))) = 0.0
            else IF (RECIR(LD(N)) > 0.0) then
                LRIN(LAYR(LD(N))) = 1
            END IF
        END IF
        !     COMPILER CORRECTIONS ABOVE
    end do
    DO N = 1, LAY
        IF (SUBIN(N) > 0.0) LSIN(N) = 1
    end do
    RETURN
END
!
!   ***************************** INITIA ************************
!
!     SUBROUTINE INITIA INITIALIZES VARIABLES USED IN SUBROUTINES
!     SOLT AND CRPMOD
!
SUBROUTINE INITIA
    !
    COMMON /BLK6/ ULAI, WIND, RH (366), OSNO, ULAT
    COMMON /BLK7/ STHICK(67), UL(67), FCUL(67), WPUL(67), SWUL(67), &
            RCUL(67), RSUL(67), XLAMBU(67), CONUL(67), BUBUL(67), &
            SUBINS(67), SWULI(67)
    COMMON /BLK11/ ETO, XLAI, STAGE1, CONA, RAIN, RUN, ABST, EAJ, TS2
    COMMON /BLK18/ RM (12), TM (12), RLAT
    COMMON /BLK19/ IPL, IHV, IPRE, IRUN, ITSOIL, IVEG
    COMMON /BLK20/ TSEG (7), Z (7), FCS (7), ST (7)
    COMMON /BLK21/ TB, TO, TS, AVT, AMP, ABD, PHU, GS, CV, &
            RSD, DM, WFT (12), DD, DLAI
    !     DIMENSION TC(12)
    !******SETTING BASE TEMP, TB=0 DEG. C; AND OPTIMAL TEMP, TO=20 DEG. C.
    TB = 5.
    TO = 20.
    !******COMPUTING AVERAGE YEARLY TEMP, AVT; AMPLITUDE, AMP; AND WFT
    AVT = 0.
    TBB = 0.
    TS = 200.
    DO I = 1, 12
        IF (TM (I) > TBB) TBB = TM (I)
        IF (TM (I) < TS) TS = TM (I)
        AVT = AVT + TM (I)
        !        WFT (I) = PWD (I)/(1. - PWW (I) + PWD (I))
        WFT (I) = 0.2 / (1. - 0.4 + 0.2)
    end do
    !.....DEFINING AVT & AMP IN DEG. C.
    AVT = AVT / 12.
    AMP = (TBB - TS) / 2.0
    !******CHECKING PLANTING AND HARVEST DATES (IN JULIAN DATES)
    !      TO COMPUTE PHU - POTENTIAL HEAT UNITS
    CALL DATFIT(12, TM, A1, A2, A3)
    PHUX = 0.
    IF (IPL >= IHV) THEN
        PHU = AHU (IPL, 365, A1, A2, A3)
        PHUX = PHU
        PHU = PHU + AHU (1, IHV, A1, A2, A3)
    ELSE
        PHU = AHU (IPL, IHV, A1, A2, A3)
    END IF
    IF (PHU < 30.) PHU = 30.
    !******INITIALIZING VARIABLES
    XLAI = (PHUX / PHU) * ULAI
    DLAI = XLAI
    GS = PHUX / PHU
    RSD = 8000. * ULAI / 5.
    CV = RSD
    DM = XLAI * 100.
    ABD = 0.
    SWS = 0.
    DO I = 1, 7
        Z (I) = 0.0
    end do
    DO I = 1, 7
        !
        !    ACCUMULATING THICKNESSES OF EACH LAYER
        !      Z  = DEPTH FROM SURFACE TO BOTTOM OF SEGMENT I
        !
        IF (I == 1) THEN
            ZSEG = 0.
        ELSE
            ZSEG = Z (I - 1)
        END IF
        Z (I) = ZSEG + STHICK (I) * 25.4
        !*****DETERMINE DRAINAGE COEFS FOR EACH SOIL SEGMENT.
        XZ = UL (I) * 25.4 * 10.
        ABD = ABD + XZ
        FCS (I) = (FCUL (I) - WPUL (I)) * 25.4
        ST (I) = (SWUL (I) - WPUL (I)) * 25.4
        SWS = SWS + ST (I)
        ST(I) = ST (I) / 25.4
    end do
    !
    !  **** DETERMINING DD *****
    !
    ABD = ABD / (10. * Z (7))
    F = ABD / (ABD + 686. * EXP (- 5.63 * ABD))
    DP = 1000. + 2500. * F
    WW = .356 - .144 * ABD
    B = ALOG (500. / DP)
    WC = SWS / (WW * Z (7))
    F = EXP (B * ((1. - WC) / (1. + WC))**2)
    DD = F * DP
    RETURN
END
!
!
!   ************************** AHU *******************************
!
FUNCTION AHU (M, K, A1, A2, A3)
    !
    !     THIS SUBROUTINE ACCUMULATES HEAT UNITS AND RADIATION TO CALCULATE
    !     THE POTENTIAL HEAT UNITS FOR EACH CROP BEFORE DAILY SIMULATION BE
    !
    AHU = 0.
    DO L = M, K
        TA = COMPUT (A1, A2, A3, L, 366) - 5.
        IF (TA > 0.0) AHU = AHU + TA
    end do
    RETURN
END
!
!
!
!      ************************** DATFIT *************************
!
SUBROUTINE DATFIT(M, D, AC, A, B)
    REAL D(12)
    PI = 3.14159
    SUMD = 0.0
    AM = FLOAT(M)
    DO I = 1, M
        SUMD = SUMD + D(I)
    end do
    AC = SUMD / AM
    AN = 1.0
    SUMA = 0.0
    SUMB = 0.0
    DO I = 1, M
        TI = FLOAT(I) - 0.5
        TH = 2.0 * PI * AN * TI
        FCOS = COS(TH / AM)
        FSIN = SIN(TH / AM)
        SUMA = SUMA + D(I) * FCOS
        SUMB = SUMB + D(I) * FSIN
    end do
    A = 2.0 / AM * SUMA
    B = 2.0 / AM * SUMB
    RETURN
END
FUNCTION COMPUT(AC, A, B, I, N)
    AI = FLOAT(I) - 0.5
    AN = FLOAT(N)
    ANG = 6.283185 * AI / AN
    COMPUT = AC + A * COS(ANG) + B * SIN(ANG)
    RETURN
END
!
!
!
!
!
!
!  *****************************  SETUPS  *****************************
!
SUBROUTINE SETUPS(SWULL, BALT, BALY, EXWAT2, EXWAT3, &
        EXWAT4, EXWAT5, EXWAT6)
    !
    !     THIS SUBROUTINE SETS THE NUMBER OF TIME STEPS IN A DAY TO INSURE
    !     THAT DRAINAGE LAYERS ARE NOT SATURATED IN A SINGLE TIME STEP
    !     UNLESS THE IMPINGEMENT RATE IS GREATER THAN THE MAXIMUM LATERAL
    !     DRAINAGE RATE.
    !
    !     THIS SUBROUTINE ALSO INITIALIZES CONDITIONS FOLLOWING THE YEAR
    !     OF INITIALIZATION FOR VEGETATION, FROZEN SOIL AND SNOW.
    !
    DOUBLE PRECISION DSWUL, DTHICK, DRCUL, DBUBUL, DWPUL, DLAMUL, &
            DRSUL, DUL, DFCUL, DSUBIN, DCHG, DRCRS, BALY, BALT
    !
    COMMON /BLK3/ PORO (20), FC (20), WP (20), RC (20), SW (20), &
            RS (20), XLAMBD (20), BUB (20), THICK (20), SLOPE (20), &
            XLENG (20), CON (20), SUBIN (20), RECIR (20), PHOLE (20), &
            DEFEC (20), TRANS (20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN(20), LSIN(20)
    COMMON /BLK6/ ULAI, WIND, RH(366), OSNO, ULAT
    COMMON /BLK7/ STHICK(67), UL(67), FCUL(67), WPUL(67), SWUL(67), &
            RCUL(67), RSUL(67), XLAMBU(67), CONUL(67), BUBUL(67), &
            SUBINS(67), SWULI(67)
    COMMON /BLK9/ PINF, ES1T, SMELT, GMRO, ADDRUN, OLDSNO, SNO, WF(7), &
            SSNO
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK17/ NSEG1, NSEG2, NSEG3, NSEG4, NSEG5, NSEG6, NSEG
    COMMON /BLK19/ IPL, IHV, IPRE, IRUN, ITSOIL, IVEG
    COMMON /BLK31/ DRCUL (67), DBUBUL (67), DRSUL (67), DLAMUL (67), &
            DUL(67), DFCUL(67), DWPUL(67), DSWUL(67), DTHICK(67), &
            DSUBIN(67), DCHG(67), DRCRS(67)
    !
    DIMENSION SWULL(20), BALT(67), BALY(67)
    !
    IF (IPRE <= 1) THEN
        DO N = 1, 6
            NSTEP(N) = 4
        end do
        !    COMPILER CORRECTIONS BELOW
        !       RTOP is not initialized if the first subprofile has no LDL
        RTOP = RC (1)
        DO I = 2, LAY
            IF (LAYER (I) > 2) GOTO 1006
            IF (RC (I) < RTOP) RTOP = RC (I)
        end do
        1006   CONTINUE
        RTOP = 2.0 * RTOP
        IF (RTOP > 0.0005) RTOP = 0.0005
        !     COMPILER CORRECTIONS ABOVE
        IF (LD(1) > 1) THEN
            !    COMPILER CORRECTIONS BELOW
            !         RTOP = RC (1)
            !         DO 1010 I = 2, LD(1)
            !           IF (RC(I) .LT. RTOP) RTOP = RC(I)
            !1010     CONTINUE
            !         RTOP = 2.0 * RTOP
            !         IF (RTOP .GT. 0.0005) RTOP = 0.0005
            !     COMPILER CORRECTIONS ABOVE
            IF (XLENG(LD(1)) == 0.0) XLENG(LD(1)) = 1.
            IF (SLOPE(LD(1)) == 0.0) SLOPE(LD(1)) = 0.00001
            RDR = RC(LD(1)) * THICK(LD(1)) * SLOPE(LD(1)) / &
                    (1200. * XLENG(LD(1)))
            IF (RDR > RTOP) THEN
                DT = (PORO(LD(1)) - FC(LD(1))) * THICK (LD(1)) * 2.54 / &
                        (RTOP * 86400.0)
                NSTEP(1) = (INT(1.0 / DT) + 1) * 2
                IF (NSTEP(1) < 4) NSTEP(1) = 4
            ELSE
                DT = (PORO(LD(1)) - FC(LD(1))) * THICK (LD(1)) * 2.54 / &
                        (RDR * 86400.0)
                NSTEP(1) = (INT(1.0 / DT) + 1) * 2
                IF (NSTEP(1) < 4) NSTEP(1) = 4
            END IF
        END IF
        DO N = 2, 5
            IF (LP(N) > 1) THEN
                LPP1 = LP(N - 1) + 1
                LPM1 = LP(N - 1) - 1
                IF (LAYER(LPM1) == 3) RTOP2 = RC(LPM1)
                IF (LAYER(LP(N - 1)) == 3) RTOP2 = RC(LP(1))
                IF (LAYER(LPM1) == 4) RTOP2 = RTOP2 * 0.001
                IF (LAYER(LP(N - 1)) == 4) THEN
                    IF (LAYER(LPM1) == 3) THEN
                        RTOP2 = RTOP2 * 0.004
                    ELSE IF (RC(LPM1) < RC(LPP1)) THEN
                        RTOP2 = 0.004 * RC(LPM1)
                        IF (RTOP2 > 0.000005) RTOP2 = 0.000005
                    ELSE
                        RTOP2 = 0.004 * RC(LPP1)
                        IF (RTOP2 > 0.000005) RTOP2 = 0.000005
                    END IF
                END IF
                RTOP2 = RTOP2 * 2.0
                IF (RTOP2 < RTOP) RTOP = RTOP2
                IF (LD(N) > 1) THEN
                    DO I = LP(N - 1) + 1, LD(N)
                        IF (RC(I) < RTOP) RTOP = RC(I)
                    end do
                ELSE
                    DO I = LP(N - 1) + 1, LP(N)
                        IF (RC(I) < RTOP) RTOP = RC(I)
                    end do
                END IF
                IF (LD(N) > 1) THEN
                    IF (XLENG(LD(N)) == 0.0) XLENG(LD(N)) = 1.
                    IF (SLOPE(LD(N)) == 0.0) SLOPE(LD(N)) = 0.00001
                    RDR = RC(LD(N)) * THICK(LD(N)) * SLOPE(LD(N)) / &
                            (1200. * XLENG(LD(N)))
                    IF (RDR > RTOP) THEN
                        DT = (PORO(LD(N)) - FC(LD(N))) * THICK (LD(N)) * 2.54 / &
                                (RTOP * 86400.0)
                        NSTEP(N) = (INT(1.0 / DT) + 1) * 2
                    ELSE
                        DT = (PORO(LD(N)) - FC(LD(N))) * THICK (LD(N)) * 2.54 / &
                                (RDR * 86400.0)
                        NSTEP(N) = (INT(1.0 / DT) + 1) * 2
                    END IF
                END IF
            END IF
        end do
        !
        DO N = 1, 6
            IF (NSTEP(N) < 4) THEN
                NSTEP(N) = 4
            ELSE IF (NSTEP(N) > 48 .AND. N > 1) THEN
                NSTEP(N) = 48
            ELSE IF (NSTEP(N) > 288) THEN
                NSTEP(N) = 288
            END IF
        end do
    ELSE IF (IPRE == 3 .OR. IPRE == 2) THEN
        ADDRUN = 0.0
        EXWAT2 = 0.0
        EXWAT3 = 0.0
        EXWAT4 = 0.0
        EXWAT5 = 0.0
        EXWAT6 = 0.0
        IF (IPRE ==3) THEN
            DO J = 1, NSEG
                SWUL(J) = SWULI(J)
                DSWUL(J) = SWULI(J)
                DCHG(J) = 0.0D0
                BALT(J) = 0.0D0
                BALY(J) = 0.0D0
            end do
        ELSE IF (IPRE == 2) THEN
            !
            !    COMPUTE INITIAL MOISTURES AT START OF SIMULATION;
            !    RESET RATES AS NEEDED
            !
            !        END IF
            DO K = 1, 20
                SWULL(K) = 0.0
            end do
            J = 1
            OTHCK1 = 0.0
            THICK1 = STHICK(J)
            THICK2 = 0.0
            SWULJ = DSWUL(J) + (DCHG(J) / 2.0D0)
            DO K = 1, LP(1)
                IF (LAYER(K) > 2) GO TO 1150
                THICK2 = THICK2 + THICK(K)
                1130       CONTINUE
                IF (LAYSEG(J, 2) < K) GO TO 1140
                IF (THICK1 > THICK2) THEN
                    DSEG = THICK2 - OTHCK1
                    OTHCK1 = THICK2
                    SWULL(K) = SWULL(K) + ((DSEG * SWULJ) / STHICK(J))
                    GO TO 1120
                ELSE
                    SWULL(K) = SWULL(K) + (SWULJ * (THICK1 - OTHCK1) / STHICK(J))
                END IF
                1140       J = J + 1
                IF (J > NSEG1) GO TO 1150
                IF (LSEG(J) > 2) GO TO 1150
                SWULJ = DSWUL(J) + (DCHG(J) / 2.0D0)
                OTHCK1 = THICK1
                THICK1 = THICK1 + DTHICK(J)
                IF (LAYSEG(J, 1) > K) GO TO 1120
                GO TO 1130
            1120     CONTINUE
            end do
            1150     CONTINUE
            IF (NSEG > NSEG1) THEN
                DO J = NSEG1 + 1, NSEG
                    IF (LSEG(J) <= 2) THEN
                        SWULL(LAYSEG(J, 2)) = SWULL(LAYSEG(J, 2)) + DSWUL(J)&
                                + (DCHG(J) / 2.0D0)
                    END IF
                end do
            END IF
            DO K = 1, LAY
                IF (LAYER(K) > 2) SWULL(K) = SW(K) * THICK(K)
            end do
            DO J = 1, NSEG
                DCHG(J) = 0.0D0
                BALT(J) = 0.0D0
                BALY(J) = 0.0D0
            end do
        END IF
    END IF
    !
    RETURN
END
!
!    ************************* RUNOFF *************************
!
!
!    SUBROUTINE RUNOFF
!
SUBROUTINE RUNOFF
    !
    COMMON /BLK5/ AREA, FRUNOF, CN2, OCN2, SSLOPE, SLENG, SMX
    COMMON /BLK7/ STHICK(67), UL(67), FCUL(67), WPUL(67), SWUL(67), &
            RCUL (67), RSUL (67), XLAMBU (67), CONUL (67), BUBUL (67), &
            SUBINS (67), SWULI (67)
    COMMON /BLK9/ PINF, ES1T, SMELT, GMRO, ADDRUN, OLDSNO, SNO, WF(7), &
            SSNO
    COMMON /BLK11/ ETO, XLAI, STAGE1, CONA, RAIN, RUN, ABST, EAJ, TS2
    IF (RAIN > 0.0) THEN
        WTSM = 0.0
        !
        !    COMPUTES DEPTH-WEIGHTED EFFECTIVE SOIL WATER CONTENT
        !    IN VOL/VOL.
        !
        DO I = 1, 7
            SW = SWUL (I)
            SWAMCI = (FCUL (I) + WPUL (I)) / 2.0
            IF (SWUL (I) > UL (I)) SW = UL (I)
            IF (SWUL (I) < SWAMCI) SW = SWAMCI
            WTSM = WTSM + (WF (I) * (SW - SWAMCI) / (UL (I) - SWAMCI))
        end do
        !
        !    COMPUTES STORAGE RETENTION PARAMTER.
        !
        S = SMX * (1.0 - WTSM)
        !
        !    COMPUTES INITIAL ABSTRACTION.
        !
        IF (S < 0.0) S = 0.0
        AB = 0.2 * S
        IF (RAIN > AB) THEN
            !
            !    COMPUTES RUNOFF.
            !
            RUN = ((RAIN - AB)**2) / (RAIN + 0.8 * S)
            RUN = RUN * FRUNOF
        ELSE
            RUN = 0.0
        END IF
    ELSE
        RUN = 0.0
    END IF
    RETURN
END
!
!
!   **************************   ETCHK   *************************
!     SUBROUTINE ETCHK CORRECTS THE PLANT TRANSPIRATION FOR LIMITING
!     SOIL MOISTURE  AND  COMPUTES ET FROM EACH SEGMENT
!
SUBROUTINE ETCHK (WS)
    !
    DOUBLE PRECISION  DRCUL, DBUBUL, DRSUL, DLAMUL, &
            DUL, DFCUL, DWPUL, DSWUL, DTHICK, &
            DSUBIN, DCHG, DRCRS
    !
    COMMON /BLK3/ PORO (20), FC (20), WP (20), RC (20), SW (20), &
            RS (20), XLAMBD (20), BUB (20), THICK (20), SLOPE (20), &
            XLENG (20), CON (20), SUBIN (20), RECIR (20), PHOLE (20), &
            DEFEC (20), TRANS (20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK7/ STHICK(67), UL(67), FCUL(67), WPUL(67), SWUL(67), &
            RCUL (67), RSUL (67), XLAMBU (67), CONUL (67), BUBUL (67), &
            SUBINS (67), SWULI (67)
    COMMON /BLK9/ PINF, ES1T, SMELT, GMRO, ADDRUN, OLDSNO, SNO, WF(7), &
            SSNO
    COMMON /BLK10/ ETT, ESS, EP, ES, ET (67)
    COMMON /BLK20/ TSEG (7), Z (7), FCS (7), ST (7)
    COMMON /BLK31/ DRCUL (67), DBUBUL (67), DRSUL (67), DLAMUL (67), &
            DUL(67), DFCUL(67), DWPUL(67), DSWUL(67), DTHICK(67), &
            DSUBIN(67), DCHG(67), DRCRS(67)
    !
    DIMENSION  ESJ(7), ULL(7), EPJ(7), AW(7), XP(7)
    !
    !    COMPUTES THE DEPTH WEIGHTED PLANT AVAILABLE WATER CAPACITY
    !     IN VOL/VOL.
    !
    TET = ES + EP
    IF (TET <= 0.0) THEN
        ETT = ESS
        WS = 0.
        DO J = 1, 7
            ET (J) = 0.0
        end do
        RETURN
    END IF
    !
    !     DISTRIBUTES SOIL EVAPORATION TO SEGMENTS IN THE TOP 18 INCHES
    !     OF SOIL PROFILE; WATER IS EXTRACTED FROM TOP DOWN.
    !
    !    XES = UNSATISFIED SOIL EVAPORATIVE DEMAND; ADDED TO DEMAND ON LOWER
    !           SEGMENT OR POSSIBLY UNSATISFIED
    !    SEDEP = SOIL EVAPORATIVE ZONE DEPTH
    !    XPINF = POSSIBLE EXCESS INFILTRATION INTO SEGMENT, REMOVED BY SOIL
    !             EVAPORATION IN EXCESS OF WEIGHTING FACTOR
    !    ESJO =  WEIGHTED AVERAGED SOIL EVAPORATION FROM A SEGMENT BASED ON
    !             EXPONENTIAL DISTRIBUTION OF SOIL MOISTURES
    !
    ESX = ES
    XES = 0.0
    THCK = 0.0
    OTHCK = 0.0
    !
    !   D1INF = POSSIBLE DRAINAGE INTO SEGMENT FROM TODAY'S INFILTRATION
    !
    DRINF = PINF
    DO J = 1, 7
        XP (J) = 0.0
        ESJ (J) = 0.0
        ULL (J) = FCUL (J) - WPUL (J)
        ST (J) = SWUL (J) + (DCHG (J) / 2.) - WPUL (J) + DRINF
        IF (ST (J) < 0.0) ST (J) = 0.0
        IF (ST (J) > UL (J)) THEN
            XPINF = ST (J) - UL (J)
            ST (J) = UL (J)
        ELSE
            XPINF = 0.0
        END IF
        IF (OTHCK < SEDEP) THEN
            IF (XPINF > RCUL (J)) THEN
                XPINF = XPINF - RCUL (J)
                IF (XPINF < ESX) THEN
                    ESX = ESX - XPINF
                    ESJ (J) = XPINF
                    XP (J) = XPINF
                ELSE
                    ESJ (J) = ESX
                    XP (J) = ESX
                    ESX = 0.0
                END IF
                DRINF = RCUL (J)
            ELSE
                DRINF = XPINF
            END IF
            IF (ESX > 0.0) THEN
                THCK = THCK + STHICK (J)
                ESJO = WFS (OTHCK, THCK, SEDEP) * ES + XES
                IF (ESJO > ESX) ESJO = ESX
                IF (THCK <= SEDEP) THEN
                    IF (ST (J) < ESJO) THEN
                        XES = ESJO - ST (J)
                        ESX = ESX - ST (J)
                        ESJ (J) = ST (J) + ESJ (J)
                    ELSE
                        XES = 0.0
                        ESX = ESX - ESJO
                        ESJ (J) = ESJO + ESJ (J)
                    END IF
                ELSE
                    RTO = ST (J) * (SEDEP - OTHCK) / (STHICK (J))
                    IF (RTO < ESJO) THEN
                        XES = ESJO - RTO
                        ESX = ESX - RTO
                        ESJ (J) = RTO + ESJ (J)
                        ES = ES - ESX
                        ES1T = ES1T - ESX
                        IF (ES1T <= 0.0) ES1T = 0.0
                        ESX = 0.0
                    ELSE
                        XES = 0.0
                        ESX = ESX - ESJO
                        ESJ (J) = ESJO + ESJ (J)
                        ES = ES - ESX
                        ES1T = ES1T - ESX
                        IF (ES1T <= 0.0) ES1T = 0.0
                        ESX = 0.0
                    END IF
                END IF
            END IF
            OTHCK = THCK
        END IF
    end do
    !
    !     COMPUTES THE RATIO OF ACTUAL OR LIMITED PLANT TRANSPIRATION
    !     TO POTENTIAL PLANT TRANSPIRATION.
    !
    !     COMPUTES ACTUAL EVAPOTRANSPIRATION FROM EACH SEGMENT AND,
    !
    !     DEMAND IS DISTRIBUTED TO LOWER SEGMENTS UNTIL DRY.
    !
    DO J = 1, 7
        EPJ (J) = 0.0
        AW (J) = ST (J) - ESJ (J) + XP (J)
        IF (AW (J) < 0.0) AW (J) = 0.0
    end do
    IF (EP > 0.0) THEN
        SUM = 0.0
        EPX = 0.0
        EPXX = 0.0
        DO J = 1, 7
            EPXX = EP * WF (J)
            EPXX = SUM + EPXX
            EPJ (J) = EPXX
            IF (AW (J) > ULL (J)) THEN
                UL4 = (AW (J) - ULL (J)) + ULL (J) / 4.
            ELSE
                UL4 = ULL (J) / 4.
            END IF
            IF (UL4 < EPXX) THEN
                IF (UL4 <= AW (J)) THEN
                    EPJ (J) = UL4
                ELSE
                    EPJ (J) = AW (J)
                END IF
            ELSE
                IF (EPXX < AW (J)) THEN
                    EPJ (J) = EPXX
                ELSE
                    EPJ (J) = AW (J)
                END IF
            END IF
            EPX = EPX + EPJ (J)
            SUM = EPXX - EPJ (J)
        end do
        EP = EPX
    END IF
    ETT = ESS
    DO J = 1, 7
        ET (J) = ESJ (J) + EPJ (J)
        ETT = ETT + ET (J)
    end do
    WS = (ETT - ESS) / TET
    RETURN
END
!
!
!   ************************** WFS *******************************
!
FUNCTION WFS (D1, D2, D3)
    !
    !     THIS FUNCTION SUBROUTINE CALCULATES THE WEIGHTING FACTOR FOR
    !     DISTRIBUTING SOIL EVAPORATION TO THE SEGMENTS TO ASSURE A PROPER
    !     SOIL MOISTURE PROFILE FOR EVAPORATION FROM BARE GROUND AND TO
    !     IMPROVE THE WEIGHTED MOISTURE CONTENT FOR ADJUSTING THE DAILY
    !     CURVE NUMBER.
    !
    !     D1 = DEPTH TO TOP OF SEGMENT
    !     D2 = DEPTH TO BOTTOM OF SEGMENT
    !     D3 = MAXIMUM DEPTH OF SOIL EVAPORATION
    !
    !     AWF IS A FUNCTION THAT CALCULATES THE ACCUMULATIVE WEIGHTING
    !     FACTOR FOR SOIL EVAPORATION FROM SURFACE TO DEPTH D WHERE D IS
    !     RATIO OF THE DEPTH TO THE MAXIMUM DEPTH OF SOIL EVAPORATION
    !
    AWF (D) = 4.759177E-4 + 3.14529 * D - 4.319952 * D * D&
            + 3.089057 * D * D * D - 0.9150572 * D * D * D * D
    IF (D1 < D3) THEN
        R1 = D1 / D3
    ELSE
        R1 = 1.0
    END IF
    IF (D2 < D3) THEN
        R2 = D2 / D3
    ELSE
        R2 = 1.0
    END IF
    IF (R1 < R2) THEN
        WFS = AWF (R2) - AWF (R1)
    ELSE
        WFS = 0.0
    END IF
    RETURN
END
!
!
!      ************************* ETCOEF ************************
!
!
!     SUBROUTINE ETCOEF COMPUTES COEFICIENTS FOR EVAPOTRANSPIRATION.
!
SUBROUTINE ETCOEF
    !
    COMMON /BLK7/ STHICK(67), UL(67), FCUL(67), WPUL(67), SWUL(67), &
            RCUL (67), RSUL (67), XLAMBU (67), CONUL (67), BUBUL (67), &
            SUBINS (67), SWULI (67)
    COMMON /BLK9/ PINF, ES1T, SMELT, GMRO, ADDRUN, OLDSNO, SNO, WF(7), &
            SSNO
    COMMON /BLK11/ ETO, XLAI, STAGE1, CONA, RAIN, RUN, ABST, EAJ, TS2
    !
    !     COMPUTES THE EFFECTIVE DEPTH-WEIGHTED
    !     EVAPORATION(TRANSMISSIVITY) COEFFICIENT.
    !
    CONA = 0.0
    DO J = 1, 7
        CONA = CONA + (WF (J) * CONUL (J))
    end do
    IF (CONA < 3.3) CONA = 3.3
    IF (CONA > 5.1) CONA = 5.1
    !
    !     COMPUTES THE UPPER LIMIT OF STAGE ONE SOIL EVAPORATION.
    !
    STAGE1 = (23. * (CONA - 3.0)**0.42) / 25.4
    RETURN
END
!
!      ************************* EVAPOT *************************
!
!
!     SUBROUTINE EVAPOT COMPUTES DAILY SURFACE EVAPORATION,
!     SOIL EVAPORATION AND POTENTIAL PLANT TRANSPIRATION.
!
SUBROUTINE EVAPOT
    !
    COMMON /BLK6/ ULAI, WIND, RH (366), OSNO, ULAT
    COMMON /BLK8/ PRE (370), TMPF (366), RAD (366)
    COMMON /BLK9/ PINF, ES1T, SMELT, GMRO, ADDRUN, OLDSNO, SNO, WF(7), &
            SSNO
    COMMON /BLK10/ ETT, ESS, EP, ES, ET (67)
    COMMON /BLK11/ ETO, XLAI, STAGE1, CONA, RAIN, RUN, ABST, EAJ, TS2
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK21/ TB, TO, TS, AVT, AMP, ABD, PHU, GS, CV, &
            RSD, DM, WFT (12), DD, DLAI
    COMMON /BLK27/ IFREZ, IDFS, IFCNT, KCNT, MXKCNT
    COMMON /BLK28/ WE, XNEGHS, XLIQW, TINDEX, STORGE, EXLAG(4), TWE
    !
    !
    !
    !  INITIALIZE VARIABLES
    !
    RAIN = RAIN - SMELT
    ESNO = 0.0
    EABS = 0.0
    ESS = 0.0
    ESM = 0.0
    EGM = 0.0
    EMELT = 0.0
    EADD = 0.0
    EP = 0.0
    ES = 0.0
    ES1 = 0.0
    ES2 = 0.0
    ABST = 0.0
    TA = (TMPF(IDA) - 32.) * 0.55556
    !
    !  CALL SUBROUTINE WHICH COMPUTES POTENTIAL EVAPOTRANSPIRATION
    !
    CALL POTET (ETO1, ETO2)
    !
    !     COMPUTE SURFACE EVAPORATION
    !
    TDEST = TA
    IF (PRE(IDA) == 0.0) TDEST = ((RH(IDA) / 100.)**(1. / 8.))&
            * (112. + (0.9 * TA))&
            - 112. + (0.1 * TA)
    !
    !  IF THERE IS SNOW ON THE GROUND AND THE ESTIMATED DEWPOINT
    !  TEMPERATURE IS GREATER THAN TEMPERATURE OF THE SNOW -
    !  NO EVAPORATION WILL OCCUR
    !
    IF (SNO>0.0 .AND. TDEST>=TINDEX) THEN
        PINF = RAIN + SMELT + ADDRUN + GMRO - RUN
        IF (PINF < 0.0) THEN
            RUN = RUN + PINF
            IF (RUN < 0.0) RUN = 0.0
        END IF
        ES1T = ES1T - PINF
        IF (ES1T <= 0.0) ES1T = 0.0
        RETURN
    ENDIF
    !
    !  IF THERE IS SNOW ON THE GROUND AND THE ESTIMATED DEWPOINT
    !  TEMPERATURE IS LESS THAN TEMPERATURE OF THE SNOW -
    !  EVAPORATIVE DEMAND IS APPLIED TO THE SNOWMELT AND TO THE SNOW
    !
    ETA = ETO
    IF (SNO>0.0 .OR. SMELT>0.0) THEN
        IF (SMELT > 0.0) THEN
            !  ENERGY NOT AVAILABLE FOR ET DUE TO  MELTING & WARMING
            ESM = ESM + (0.1367 * SMELT) + (0.0240 * SMELT)
            ETA = ETO - ESM
            IF (ESM > ETO) THEN
                ETA = 0.0
                ESM = ETO
            END IF
            EMELT = ETA
            IF (EMELT > SMELT) THEN
                EMELT = SMELT
                ETA = ETA - SMELT
                SMELT = 0.0
            ELSE
                ETA = 0.0
                SMELT = SMELT - EMELT
            END IF
        ELSE
            EMELT = 0.0
            ETA = ETO
        ENDIF
        ESNO = 0.8615 * ETA
        IF (ESNO >= SNO) THEN
            ESNO = SNO
            SNO = 0.0
            ESM = (ESNO * (0.1367 + 0.0240)) + ESM
            ETA = ETA - (ESNO / 0.8615)
            !
            !   SNOW GONE - SET ALL CARRYOVER TO NO SNOW CONDITIONS
            !
            WE = 0.
            XNEGHS = 0.0
            TINDEX = 0.0
            DO N = 1, 4
                EXLAG(N) = 0.0
            end do
            TEX = 0.
            XLIQW = 0.
            STORGE = 0.
        ELSE
            PRSNO = SNO
            SNO = SNO - ESNO
            ESM = (ESNO * (0.1367 + 0.0240)) + ESM
            ETA = 0.0
            !
            !  ADJUST SNOW PARAMETERS ACCORDING TO EVAPORATED AMOUNT
            !
            WE = SNO / PRSNO * WE
            XNEGHS = SNO / PRSNO * XNEGHS
            XLIQW = SNO / PRSNO * XLIQW
            STORGE = SNO / PRSNO * STORGE
            TEX = 0.0
            DO J = 1, 4
                EXLAG(J) = SNO / PRSNO * EXLAG(J)
                TEX = TEX + EXLAG(J)
            end do
            TWE = WE + XLIQW + STORGE + TEX
        ENDIF
    ELSE
        !
        !   IF THERE IS NO SNOW ON THE GROUND, SURFACE EVAPORATION
        !   IS EQUAL TO ANY INTERCEPTION
        !
        IF (RAIN > 0.0) THEN
            ABSMAX = 0.05
            IF (CV < 14000.) ABSMAX = 0.05 * (CV + .1) / 14000.
            IF ((RAIN / ABSMAX) < 88.)THEN
                ABST = ABSMAX * (1 - EXP (- RAIN / ABSMAX))
            ELSE
                ABST = ABSMAX
            END IF
        END IF
        EABS = ETA
        IF (EABS >= ABST) THEN
            EABS = ABST
            ETA = ETA - ABST
        ELSE
            ABST = EABS
            ETA = 0.0
        ENDIF
    END IF
    IF (ADDRUN > 0.0) THEN
        EADD = ETA
        IF (EADD > ADDRUN) THEN
            EADD = ADDRUN
            ETA = ETA - EADD
            ADDRUN = 0.0
        ELSE
            ADDRUN = ADDRUN - EADD
            ETA = 0.0
        END IF
    END IF
    IF (GMRO > 0.0) THEN
        EGM = ETA
        IF (EGM > GMRO) THEN
            EGM = GMRO
            ETA = ETA - EGM
            GMRO = 0.0
        ELSE
            GMRO = GMRO - EGM
            ETA = 0.0
        END IF
    END IF
    PINF = RAIN + SMELT + ADDRUN + GMRO - RUN - ABST
    IF (PINF < 0.0) THEN
        RUN = RUN + PINF
        PINF = 0.0
        IF (RUN < 0.0) THEN
            ABST = ABST + RUN
            RUN = 0.0
            EABS = ABST
        END IF
    END IF
    ESS = EMELT + ESNO + EABS + EADD + EGM
    !
    !    IF THE AVAILABLE ENERGY FOR EVAPOTRANSPIRATION HAS BEEN EXHAUSTED
    !      BY THE SURFACE EVAPORATION, RETURN; NO SOIL EVAPORATION OR
    !      PLANT TRANSPIRATION IS POSSIBLE.
    !
    IF (ETA <= 0.0 .OR. (ETO - ESS - ESM) <= 0.0) THEN
        ES1T = ES1T - PINF
        IF (ES1T <= 0.0) ES1T = 0.0
        RETURN
    END IF
    !
    !    COMPUTE SOIL EVAPORATION AND PLANT TRANSPIRATION
    !    SOIL EVAPORATION AND PLANT TRANSPIRATION ARE NOT
    !    PERMITTED IF THE TEMPERATURE IS BELOW 23 DEGREES F
    !    OR IF THE SOIL IS CONSIDERED TO BE FROZEN
    !
    IF (TMPF (IDA) >= 23.0 .AND. IFREZ /= 1) THEN
        !
        !     EXCESS ET DEMAND (ETA) IS EXERTED ON SOIL EVAPORATION AFTER
        !     DETERMINING THE SURFACE EVAPORATION; STAGE 1 IS IF ES1T IS
        !     LESS THAN THE STAGE 1 LIMIT; ELSE, STAGE 2 EVAPORATION OCCURS.
        !
        ETO2X = (1.0 - (7.143E-05 * CV)) * ETO2
        IF (ETO2X < 0.0) ETO2X = 0.0
        ESO = EAJ * (ETO1 + ETO2X)
        IF (ESO > ETA) ESO = ETA
        IF (ESO < 0.0) ESO = 0.0
        IF (ES1T <= STAGE1) THEN
            ES1 = ETA
            IF (ES1 >= ESO) THEN
                ES1 = ESO
                ETA = ETA - ESO
            ELSE IF (ES1 <= 0.0) THEN
                ES1 = 0.0
                ETA = 0.0
            ELSE
                ETA = ETA - ES1
            END IF
            ES1T = ES1T + ES1 - PINF
            IF (ES1T <= 0.0) ES1T = 0.0
            IF (ES1T <= STAGE1) TS2 = 0.0
            ES = ES1
        ELSE
            !
            !     COMPUTES STAGE 2 SOIL EVAPORATION.
            !
            TS2 = TS2 + 1.0
            ES2 = CONA * (TS2**0.5 - (TS2 - 1.0)**0.5) / 25.4
            IF (ES2 >= ESO) THEN
                ES2 = ESO
                ETA = 0.0
            ELSE IF (ES2 <= 0.0) THEN
                ES2 = 0.0
            ELSE
                ETA = ETA - ES2
            END IF
            ES1T = ES1T + ES2 - PINF
            IF (ES1T <= 0.0) ES1T = 0.0
            ES = ES2
        END IF
        !
        !     COMPUTES POTENTIAL PLANT TRANSPIRATION.
        !
        EP = ETO * XLAI / 3.0
        IF (EP > ETA) THEN
            EP = ETA
            ETA = 0.0
        ELSE IF (EP < 0.0) THEN
            EP = 0.0
        ELSE
            ETA = ETA - EP
        END IF
        IF (EP >= (ETO - ESM - ESS - ES)) EP = ETO - ESM - ES - ESS
        IF (EP < 0.0) THEN
            ES = ES + EP
            EP = 0.0
        END IF
    ELSE
        !
        !     NO EVAPOTRANSPIRATION OCCURS; ONLY SURFACE
        !     EVAPORATION OCCURRED.
        !
        ES1T = ES1T - PINF
        IF (ES1T <= 0.0) ES1T = 0.0
    END IF
    RETURN
END
!
!      ************************* POTET **************************
!
!     SUBROUTINE POTET COMPUTES THE DAILY POTENTIAL
!     EVAPOTRANSPIRATION VALUES FOR A YEAR OF SIMULATION.
!
SUBROUTINE POTET (ETO1, ETO2)
    !
    COMMON /BLK6/ ULAI, WIND, RH (366), OSNO, ULAT
    COMMON /BLK8/ PRE (370), TMPF (366), RAD (366)
    COMMON /BLK9/ PINF, ES1T, SMELT, GMRO, ADDRUN, OLDSNO, SNO, WF(7), &
            SSNO
    COMMON /BLK11/ ETO, XLAI, STAGE1, CONA, RAIN, RUN, ABST, EAJ, TS2
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK21/ TB, TO, TS, AVT, AMP, ABD, PHU, GS, CV, &
            RSD, DM, WFT (12), DD, DLAI
    !
    DATA SALB /0.23/
    !     DATA G /0.68/
    !     DATA CEJ /- 2.9E-5/
    !
    !
    !  ***********************************************************
    !
    !     PRIESTLY TAYLOR ALGORITHM
    !     COMPUTES SLOPE OF VAPOR PRESSURE CURVE, A, AND NET
    !     SOLAR RADIATION, H, AND POTENTIAL EVAPOTRANSPIRATION, ETO.
    !
    !     A = (5304./(TK*TK))*EXP (21.255 - (5304./TK))
    !     H = (1 - ALB)*RAD (IDA)/58.3
    !     ETO = 1.28*A*H/((A + G)*25.4)
    !     IF (ETO .LT. 0.0) ETO = 0.0
    !     RETURN
    !     END
    !
    !
    !***************************************************************
    !
    !     PENMAN  METHOD
    !     COMPUTES SLOPE OF VAPOR PRESSURE CURVE (DELTA), NET SOLAR
    !     RADIATION (RN), SATURATION VAPOR PRESSURE (SVP),
    !     ACTUAL VAPOR PRESSURE (AVP),
    !
    IF (CV > 40000.) CV = 40000.
    EAJ = EXP (-0.000029 * (CV + .1))
    IF (SNO <= 5. / 25.4) THEN
        ALB = SALB
        IF (XLAI > 0.0) ALB = .23 * (1. - EAJ) + SALB * EAJ
    ELSE
        ALB = .6
    END IF
    !
    !     CONVERTS TEMPERATURE VALUE FROM DEGREES
    !     FAHRENHEIT TO DEGREES KELVIN.
    !
    TK = ((TMPF (IDA) - 32.0) * 5.0 / 9.0) + 273.0
    !
    TC = TK - 273.
    SVP = 33.8639 * ((0.00738 * TC + 0.8072)**8. - 0.000019 * &
            ABS(1.8 * TC + 48.) + 0.001316)
    AVP = (RH(IDA) / 100.) * SVP
    DELTA = 33.8639 * (0.05904 * (0.00738 * TC + 0.8072)**7. - &
            0.0000342)
    RBO = 11.71E-8 * (0.39 - 0.05 * AVP**0.5) * TK**4.
    !
    !     DETERMINE RSO FROM WGEN ROUTINE
    !
    XLAT = ULAT * 6.2832 / 360.
    XI = IDA
    SD = 0.4102 * SIN(0.0172 * (XI - 80.25))
    CH = - TAN(XLAT) * TAN(SD)
    IF (CH <= 1.0 .AND. CH >= -1.0) THEN
        H = ACOS (CH)
    ELSE IF (CH > 1.0) THEN
        H = 0.0
    ELSE IF (CH < - 1.0) THEN
        H = 3.1416
    END IF
    DDD = 1.0 + 0.0335 * SIN(0.0172 * (XI + 88.2))
    RSO = 889.2305 * DDD * ((H * SIN(XLAT) * SIN(SD)) + (COS(SD) * COS(XLAT)&
            * SIN(H)))
    RSO = RSO * 0.8
    !
    !     CALCULATION OF LONGWAVE RADIATION LOSS FROM RSO, RAD, AND RBO
    !
    IF (RH (IDA) < 50) THEN
        ACOEF = 1.2
        BCOEF = - 0.2
    ELSE
        IF (RH (IDA) < 75) THEN
            ACOEF = 1.1
            BCOEF = - 0.1
        ELSE
            ACOEF = 1.0
            BCOEF = 0.0
        END IF
    END IF
    RB = (ACOEF * RAD(IDA) / RSO + BCOEF) * RBO
    !
    !     CALCULATION OF NET RADIATION
    !
    RN = (1 - ALB) * RAD(IDA) - RB
    IF (RN < 0.) RN = 0.
    !
    !   PENMAN EQUATION:  TERM1 = RADIATION TERM;  TERM2 = HUM-WIND TERM
    !
    WINDF = 1.0 + 0.2394 * WIND
    TERM1 = (DELTA / (DELTA + 0.68)) * RN
    TERM2 = (0.68 / (0.68 + DELTA)) * 15.36 * WINDF * (SVP - AVP)
    ETO = (TERM1 + TERM2) / (58.3 * 25.4)
    ETO1 = TERM1 / (58.3 * 25.4)
    ETO2 = TERM2 / (58.3 * 25.4)
    IF (ETO < 0.0) ETO = 0.0
    RETURN
END
!
!  *********************** CRPMOD ************************
!
SUBROUTINE CRPMOD
    !
    !     THIS SUBROUTINE PREDICTS DAILY POTENTIAL GROWTH OF TOTAL PLANT
    !     BIOMASS AND ROOTS AND CALCULATES LEAF AREA INDEX.  INCORPORATES
    !     RESIDUE FOR TILLAGE FUNCTIONS AND DECAYS RESIDUE ON GROUND
    !     SURFACE.  CALLS SUBROUTINE ETCHK AND ADJUSTS DAILY DRY MATTER TO
    !     WATER STRESS CALCULATED IN ETCHK.
    !
    COMMON /BLK6/ ULAI, WIND, RH (366), OSNO, ULAT
    COMMON /BLK8/ PRE (370), TMPF (366), RAD (366)
    COMMON /BLK11/ ETO, XLAI, STAGE1, CONA, RAIN, RUN, ABST, EAJ, TS2
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK19/ IPL, IHV, IPRE, IRUN, ITSOIL, IVEG
    COMMON /BLK20/ TSEG (7), Z (7), FCS (7), ST (7)
    COMMON /BLK21/ TB, TO, TS, AVT, AMP, ABD, PHU, GS, CV, &
            RSD, DM, WFT (12), DD, DLAI
    !
    SUT = ST (7) * 25.4 / FCS (7)
    CDG = (.9 * TSEG (7) / (TSEG (7) + EXP (9.93 - (.312 * TSEG (7))))) + .1
    DECR = .05 * AMIN1 (CDG, SUT)
    RSD = RSD * (1. - DECR)
    IF(IPL==0.AND.IDA==30)GS = 0.
    IF(IPL==367.AND.IDA==210)GS = 0.
    IF(IPL==0.AND.IHV==367)DM = DM * (1. - DECR)
    IF(IPL==367.AND.IHV==0)DM = DM * (1. - DECR)
    CV = .8 * DM + RSD
    IF (IPL >= IHV) GO TO 1000
    IF (IDA - IPL) 1010, 1000, 1020
    1010 CONTINUE
    CALL ETCHK (WS)
    RETURN
    1000 CONTINUE
    IF (IDA - IPL) 1020, 1030, 1040
    1030 CONTINUE
    GS = 0.
    DM = 0.
    GO TO 1040
    1020 CONTINUE
    IF (IDA - IHV) 1040, 1050, 1010
    1050 CONTINUE
    RSD = (0.53 * DM + RSD)
    DM = 0.
    XLAI = 0.
    CALL ETCHK (WS)
    GO TO 1060
    1040 CONTINUE
    CALL ETCHK (WS)
    TMPC = (TMPF (IDA) - 32.) / 1.8
    TGX = TMPC - TB
    IF (TGX > 0.) THEN
        CALL TSTR (TGX, TMPC)
    ELSE
        TS = 0.
    END IF
    DDM = .5 * RAD (IDA) * (1. - EXP ((-.65) * (XLAI + .05)))
    REG = AMIN1 (WS, TS)
    DM = DM + DDM * REG
    IF(DM<=0.0)DM = 0.0
    !   CORRECTION BELOW
    IF (TMPC > TB) GS = GS + (TMPC - TB) / PHU
    !     GS = GS + TMPC/PHU
    IF (GS > .75) THEN
        XLAI = 16. * DLAI * (1. - GS) * (1. - GS)
    ELSE
        WLV = .8 * DM
        F = WLV / (WLV + 5512 * EXP ((-0.000608) * WLV))
        XLAI = ULAI * F
        DLAI = XLAI
    END IF
    1060 IF (XLAI < 0.0) XLAI = 0.0
    RETURN
END
!
! ****************************** SOLT **************************
!
SUBROUTINE SOLT
    !     THIS SUBROUTINE ESTIMATES DAILY AVERAGE TEMPERATURE AT THE BOTTOM
    !     OF EACH SOIL LAYER
    !
    COMMON /BLK8/ PRE (370), TMPF (366), RAD (366)
    COMMON /BLK9/ PINF, ES1T, SMELT, GMRO, ADDRUN, OLDSNO, SNO, WF(7), &
            SSNO
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK20/ TSEG (7), Z (7), FCS (7), ST (7)
    COMMON /BLK21/ TB, TO, TS, AVT, AMP, ABD, PHU, GS, CV, &
            RSD, DM, WFT (12), DD, DLAI
    !
    !*****DETERMINING BCV
    !
    IF(CV<0.0)CV = 0.0
    IF(CV>20000.)CV = 20000.
    BCV = CV / (CV + EXP (7.563 - (0.0001297 * CV)))
    IF ((SNO * 25.4) > 120.) THEN
        BCV = AMAX1 (1., BCV)
    ELSE IF (SNO > 0.0) THEN
        XX = (SNO * 25.4) / (SNO * 25.4 + EXP (6.055 - (.3002 * SNO * 25.4)))
        BCV = AMAX1 (XX, BCV)
    END IF
    !
    XI = IDA
    ALX = (XI - 200.) / 58.13
    TMPC = (TMPF (IDA) - 32.) / 1.8
    IF (RAD (IDA) == 0.) THEN
        ST0 = WFT (MO) * 5. + TMPC
    ELSE
        ST0 = WFT (MO) * 5. + TMPC - 5.
    END IF
    XX = BCV * TO + (1. - BCV) * ST0
    DT = XX - (AVT + AMP * COS (ALX))
    DO K = 1, 7
        ZD = - Z (K) / DD
        IF(ABS(ZD)<37.)THEN
            TSEG (K) = AVT + (AMP * COS (ALX + ZD) + DT) * EXP (ZD)
        ELSE
            TSEG(K) = AVT
        END IF
    end do
    RETURN
END
!
! *************************** TSTR **************************
!
!      SUBROUTINE TSTR APPLIES A TEMPERATURE STRESS FACTOR TO CROPS
!      IF THE DAILY MAXIMUM TEMPERATURE IS MORE THEN 15 DEG. C. BELOW
!      THE AVERAGE ANNUAL TEMPERATURE
!
SUBROUTINE TSTR (TGX, TMPC)
    !
    COMMON /BLK21/ TB, TO, TS, AVT, AMP, ABD, PHU, GS, CV, &
            RSD, DM, WFT (12), DD, DLAI
    !
    IF (TMPC > TO) TGX = 2. * TO - TB - TMPC
    RTO = ((TO - TMPC) / TGX)**2
    IF (RTO > 200.) THEN
        TS = 0.
    ELSE
        TS = EXP (- 0.1054 * RTO)
    END IF
    IF ((TMPC - 5.) <= (AVT - 15.)) TS = 0.
    RETURN
END
!
!
! ************************* FRZCHK *******************************
!
!   SUBROUTINE FRZCHK DETERMINES THE OCCURENCE OF FROZEN SOIL
!      MODIFIED FROM A CREAMS APPROACH
!
SUBROUTINE FRZCHK
    !
    COMMON /BLK8/ PRE (370), TMPF (366), RAD (366)
    COMMON /BLK9/ PINF, ES1T, SMELT, GMRO, ADDRUN, OLDSNO, SNO, WF(7), &
            SSNO
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK26/ TMPSUM, TMPAVG, TAVG(30)
    COMMON /BLK27/ IFREZ, IDFS, IFCNT, KCNT, MXKCNT
    !
    !    UPDATE AVERAGE TEMPERATURE OF PREVIOUS 30 DAYS
    !
    TMPSUM = TMPSUM - TAVG(30) + TMPF(IDA)
    TMPAVG = TMPSUM / 30
    DO I = 1, 29
        J = 30 - I
        TAVG(J + 1) = TAVG(J)
    end do
    TAVG(1) = TMPF(IDA)
    !
    IF (TMPF(IDA) < 32.0 .AND. KCNT > 0) KCNT = KCNT - 1
    IF (TMPF(IDA) >= 32.0 .AND. KCNT < MXKCNT) KCNT = KCNT + 1
    !
    IF (IFREZ == 0) THEN
        IF (TMPAVG < 32.0 .AND. KCNT == 0) THEN
            IFREZ = 1
            MXKCNT = IDFS
        END IF
    ELSE
        IF (KCNT >= IDFS .AND. SNO <= 0.0) THEN
            IFREZ = 0
            MXKCNT = (IDFS + 2) / 3
            KCNT = MXKCNT
        END IF
    END IF
    RETURN
END
!
!
!
!      ************************* SNOCOEF ***********************
!
!     SUBROUTINE SNOCOEF COMPUTES COEFFICIENTS FOR SNOWMELT CALC
!
SUBROUTINE SNOCOEF
    !
    COMMON /BLK6/ ULAI, WIND, RH (366), OSNO, ULAT
    COMMON /BLK9/ PINF, ES1T, SMELT, GMRO, ADDRUN, OLDSNO, SNO, WF(7), &
            SSNO
    COMMON /BLK19/ IPL, IHV, IPRE, IRUN, ITSOIL, IVEG
    COMMON /BLK28/ WE, XNEGHS, XLIQW, TINDEX, STORGE, EXLAG(4), TWE
    COMMON /BLK30/ XMFMAX, XMFMIN, XNMF, TIPM, UADJ, &
            TBASE, PXTEMP, PLWHC, GM, PLQW, SFNEW, RFMIN
    !
    !   INPUT OF CONSTANTS
    !    XMFMAX = MAXIMUM NONRAIN MELT FACTOR, mm/day-deg C
    !    XMFMIN = MINIMUM NONRAIN MELT FACTOR, mm/day-deg C
    !    XNMF = MAXIMUM NEGATIVE MELT FACTOR, mm/day-deg C
    !    TIPM = ANTECEDANT PACK TEMPERATURE WEIGHTING FACTOR
    !    TBASE = BASE TEMPERATURE FOR MELTING (CELCIUS)
    !    PXTEMP = SNOW/RAIN THRESHOLD TEMPERATURE
    !    PLWHC = PROPORTION LIQUID WATER HOLDING CAPACITY FOR RIPE SNOW PACK
    !    GM = CONSTANT GROUND MELT, mm/day
    !    PLQW = INITIAL PROPORTION OF LIQUID WATER IN SNOW WATER EQUIVALENT
    !    UADJ = WIND FUNCTION ADJUSTMENT FACTOR, mm/day-mb
    !
    IF (IPRE < 2) THEN
        XMFMAX = 5.2
        XMFMIN = 2.0
        XNMF = 0.6
        TIPM = 0.3
        TBASE = 0.0
        PXTEMP = 0.0
        PLWHC = 0.01
        GM = 0.5
        PLQW = 0.01
        UADJ = 0.262 * (1.0 + 0.24 * WIND)
        !        PA = 1000.
        !
        !       IF SNOWFALL EXCEEDS SNEW/HR--TINDEX = AIR TEMPERATURE
        !
        SNEW = 1.5
        !
        !       IF RAIN EXCEEDS RMIN/HR--USE RAIN-ON-SNOW MELT EQUATION
        !
        RMIN = 0.25
        !
        SFNEW = SNEW * 24.
        RFMIN = RMIN * 24.
        !
        !       INITIALIZATION OF VARIABLES FOR SNOW SUBROUTINE
        !
        TWE = 0.0
        TEX = 0.0
        WE = 0.0
        XNEGHS = 0.0
        XLIQW = 0.0
        TINDEX = 0.0
        STORGE = 0.0
        DO N = 1, 4
            EXLAG (N) = 0.0
        end do
        IF (OSNO > 0 .AND. IPRE == 1) THEN
            !
            !        CASE OF SOME INITIAL SNOW COVER
            !
            XLIQW = (OSNO * 25.4) * PLQW
            WE = (OSNO * 25.4) - XLIQW
            SNO = OSNO
        ELSE
            SNO = 0.0
        END IF
        OLDSNO = OSNO
    ELSE IF (IPRE == 3) THEN
        IF (OSNO > 0.0) THEN
            IF (OLDSNO > 0.0) THEN
                WE = WE * OSNO / OLDSNO
                XNEGHS = XNEGHS * OSNO / OLDSNO
                XLIQW = XLIQW * OSNO / OLDSNO
                STORGE = STORGE * OSNO / OLDSNO
                TEX = 0.0
                DO I = 1, 4
                    EXLAG(I) = EXLAG(I) * OSNO / OLDSNO
                    TEX = TEX + EXLAG(I)
                end do
            ELSE
                TWE = 0.0
                TEX = 0.0
                WE = 0.0
                XNEGHS = 0.0
                XLIQW = 0.0
                TINDEX = 0.0
                STORGE = 0.0
                DO N = 1, 4
                    EXLAG (N) = 0.0
                end do
                XLIQW = (OSNO * 25.4) * PLQW
                WE = (OSNO * 25.4) - XLIQW
            END IF
        ELSE
            TWE = 0.0
            TEX = 0.0
            WE = 0.0
            XNEGHS = 0.0
            XLIQW = 0.0
            TINDEX = 0.0
            STORGE = 0.0
            DO N = 1, 4
                EXLAG (N) = 0.0
            end do
        END IF
        SNO = OSNO
        OLDSNO = OSNO
    ELSE IF (IPRE == 2) THEN
        OSNO = OLDSNO
        SNO = OLDSNO
    END IF
    !
    RETURN
END
!
!
!      ************************* SNOW *************************
!
SUBROUTINE SNOW (IT)
    !
    !     SUBROUTINE SNOW DETERMINES IF DAILY PRECIPITATION IS
    !     SNOW OR RAIN;  COMPUTES SNOW ACCUMULATION AND MELT.
    !
    !     ADOPTED FROM THE NWS HYDRO-17 CODE WHICH WAS INITIALLY
    !     WRITTEN BY ERIC ANDERSON   MAY 1980
    !.......................................
    !
    !     COMMON BLOCKS
    PARAMETER (MXYR = 100)
    !
    COMMON /BLK6/ ULAI, WIND, RH(366), OSNO, ULAT
    COMMON /BLK8/ PRE (370), TMPF (366), RAD (366)
    COMMON /BLK9/ PINF, ES1T, SMELT, GMRO, ADDRUN, OLDSNO, SNO, WF(7), &
            SSNO
    COMMON /BLK11/ ETO, XLAI, STAGE1, CONA, RAIN, RUN, ABST, EAJ, TS2
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK18/ RM (12), TM (12), RLAT
    COMMON /BLK27/ IFREZ, IDFS, IFCNT, KCNT, MXKCNT
    COMMON /BLK28/ WE, XNEGHS, XLIQW, TINDEX, STORGE, EXLAG(4), TWE
    COMMON /BLK30/ XMFMAX, XMFMIN, XNMF, TIPM, UADJ, &
            TBASE, PXTEMP, PLWHC, GM, PLQW, SFNEW, RFMIN
    COMMON /BLK33/ JYEAR (MXYR)
    !
    !  INITIAL VALUES
    !
    TA = (TMPF(IDA) - 32.) * 0.55556
    PXI = PRE(IDA) * 25.4
    !.......................................
    !  INITIALIZE DAILY VARIABLES AT 0.
    !
    SFALL = 0.0
    CNHSPX = 0.0
    CNHS = 0.0
    RAIN = 0.0
    ROS = 0.0
    RAINM = 0.0
    GMRO = 0.
    XMELT = 0.
    SMELT = 0.
    GMWLOS = 0.
    GMSLOS = 0.
    !.......................................
    !     DETERMINE FORM OF PRECIP. AND ACCUMULATE SNOW COVER IF SNOW.
    !
    IF(TA<PXTEMP) THEN
        !
        !     ACCUMULATE SNOWFALL
        !
        XLIQW = XLIQW + ADDRUN * 25.4
        SNO = SNO + ADDRUN
        TWE = TWE + ADDRUN * 25.4
        ADDRUN = 0.0
        TS = TA
        IF(TS > 0.0) TS = 0.0
        SFALL = PXI
        IT = 1
        WE = WE + SFALL
        IF(WE == 0.0)GO TO 380
        CNHSPX = -TS * SFALL / 160.0
        IF(SFALL>SFNEW) TINDEX = TS
    ELSE
        !
        !     RAINFALL AND RAIN MELT.
        !
        RAIN = PXI
        IT = 2
        IF(WE==0.0) GO TO 380
    END IF
    TR = TA
    IF(TR<0.0) TR = 0.0
    RAINM = 0.0125 * RAIN * TR
    !.......................................
    !     MELT AT GROUND-SNOW INTERFACE
    !
    GMC = GM
    IF(IFREZ==1)GMC = 0.0
    IF(WE<=GMC) THEN
        GMRO = WE + XLIQW
        GO TO 350
    END IF
    GMWLOS = (GMC / WE) * XLIQW
    GMSLOS = GMC
    ! ....................................................................
    !
    !    COMPUTE SURFACE ENERGY EXCHANGE FOR THE COMPUTATIONAL PERIOD BASED
    !        ON 100 PERCENT COVER AND NON-RAIN CONDITIONS -
    !
    !    ADJUST IDA SO THAT MAR 22 EQUALS DAY 1 for NORTHERN HEMISPHERE
    !    ADJUST IDA SO THAT SEP 21 EQUALS DAY 1 for SOUTHERN HEMISPHERE
    !
    ND = LEAP(JYEAR(IYR), NT)
    IF (ULAT >= 0.0) THEN
        IDANG = 80
        IF (ND==366) IDANG = 81
    ELSE
        IDANG = 263
        IF (ND==366) IDANG = 264
    END IF
    IDN = IDA - IDANG
    IF (IDN<=0) IDN = IDN + ND
    DIFF = (XMFMAX - XMFMIN)
    DAYN = IDN
    !.......................................
    !     MELT FACTOR VARIATION FOR LATITUDE ABOVE 60 DEGREES
    !
    IF(IDN>=275) THEN
        X = (DAYN - 275.0) / (458.0 - 275.0)
    ELSE IF(IDN>=92) THEN
        X = (275.0 - DAYN) / (275.0 - 92.0)
    ELSE
        X = (91.0 + DAYN) / 183.0
    END IF
    XX = (SIN(DAYN * 2.0 * 3.1416 / 366.0) * 0.5) + 0.5
    IF(X<=0.48) THEN
        ADJMF = 0.0
    ELSE IF (X>=0.70) THEN
        ADJMF = 1.0
    ELSE
        ADJMF = (X - 0.48) / (0.70 - 0.48)
    END IF
    RMF60 = (XX * ADJMF * DIFF) + XMFMIN
    XMF = RMF60
    !.......................................
    !     MELT FACTOR VARIATION FOR LATITUDE BELOW 60 DEGREES AND 50 DEGREES
    !
    IF (ABS(ULAT) < 60.0) THEN
        RMF50 = (SIN(DAYN * 2.0 * 3.1416 / 366.0) * DIFF * 0.5) + (XMFMAX + XMFMIN) * 0.5
        XMF = RMF50
        IF (ABS(ULAT) > 50.0) THEN
            RINT = (ABS(ULAT) - 50.) / 10.
            !  CORRECTION BELOW
            XMF = RMF50 + RINT * (RMF60 - RMF50)
        END IF
    END IF
    RATIO = XMF / XMFMAX
    !.......................................
    !     COMPUTE MELT AND NEGATIVE HEAT EXCHANGE INDEX TEMPERATURES.
    !
    TMX = TA - TBASE
    IF(TMX<0.0) TMX = 0.0
    TSUR = TA
    IF (TSUR>0.0) TSUR = 0.0
    TNMX = TINDEX - TSUR
    !.......................................
    !     NEGATIVE HEAT EXCHANGE
    !
    XNMRATE = RATIO * XNMF
    CNHS = XNMRATE * TNMX
    !
    !     UPDATE TINDEX
    !
    TINDEX = TINDEX + TIPM * (TA - TINDEX)
    IF(TINDEX>0.0) TINDEX = 0.0
    !.......................................
    !     SURFACE MELT.
    !
    IF(TMX>0.0) THEN
        XMELT = XMF * TMX
    END IF
    !.......................................
    !     DETERMINE MELT FOR THE TIME INTERVAL - SURFACE ENERGY EXCHANGE
    !        IS UNIFORM DURING THE COMPUTATIONAL PERIOD.
    !
    IF(RAIN<=RFMIN) THEN
        !
        !     NON-RAIN OR LIGHT DIZZLE INTERVAL
        !
        XMELT = XMELT + RAINM
    ELSE
        !
        !     RAIN INTERVAL.
        !
        SVP1 = (0.00738 * TA + 0.8072)**8.
        SVP2 = 0.000019 * ABS(1.8 * TA + 48.)
        SVP = 33.8639 * (SVP1 - SVP2 + 0.001316)
        !
        !     ASSUME 90 PERCENT RELATIVE HUMIDITY DURING RAIN-ON-SNOW
        !
        AVP = 0.9 * SVP
        TK = (TA + 273.)
        QN = (11.71E-8 * TK**4) / 7.97 - 81.61
        QE = 8.5 * (AVP - 6.11) * UADJ
        BR = 0.68 * TA / (AVP - 6.11)
        QH = BR * QE
        XMELT = QN + QE + QH + RAINM
        IF(XMELT<0.0) XMELT = 0.0
    END IF
    ROS = RAIN
    RAIN = 0.
    !.......................................
    !    CHECK CNHS.
    !
    IF((CNHS + XNEGHS)<0.0) CNHS = -1.0 * XNEGHS
    !.......................................
    !     ADJUST WE FOR SURFACE AND GROUND MELT
    !     GROUND MELT
    !
    WE = WE - GMSLOS
    XLIQW = XLIQW - GMWLOS
    GMRO = GMSLOS + GMWLOS
    !
    !     SURFACE MELT
    IF(XMELT>0.0) THEN
        IF(XMELT>=WE) THEN
            XMELT = WE + XLIQW
            GO TO 350
        ELSE
            WE = WE - XMELT
        END IF
    END IF
    !.......................................
    !     PERFORM HEAT AND WATER BALANCE FOR THE SNOW COVER.
    !
    WATER = XMELT + ROS
    HEAT = CNHS + CNHSPX
    XLIQWMX = PLWHC * WE
    XNEGHS = XNEGHS + HEAT
    !
    !     TEMPERATURE OF SNOW CAN NOT BE BELOW-52.8 DEGC
    !
    IF(XNEGHS<0.0) XNEGHS = 0.0
    IF(XNEGHS>0.33 * WE) XNEGHS = 0.33 * WE
    IF((WATER + XLIQW)>=(XLIQWMX + XNEGHS)) THEN
        !
        !     EXCESS WATER EXISTS
        !
        EXCESS = WATER + XLIQW - XLIQWMX - XNEGHS
        XLIQW = XLIQWMX
        WE = WE + XNEGHS
        XNEGHS = 0.0
    ELSE IF(WATER>=XNEGHS) THEN
        !
        !     WATER EXCEEDS XNEGHS - LIQUID WATER CONTENT IS INCREASED.
        !
        XLIQW = XLIQW + WATER - XNEGHS
        WE = WE + XNEGHS
        XNEGHS = 0.0
        EXCESS = 0.0
    ELSE
        !
        !     ALL WATER IS REFROZEN IN THE SNOW COVER.
        !
        WE = WE + WATER
        XNEGHS = XNEGHS - WATER
        EXCESS = 0.0
    END IF
    !
    !     IF NO NEGATIVE HEAT - TINDEX MUST BE 0.0.
    !
    IF(XNEGHS==0.0) TINDEX = 0.0
    !.......................................
    !     ROUTE EXCESS WATER THROUGH THE SNOW COVER.
    !
    PACKRO = 0.0
    FIT = 24
    !
    !     LAG EXCESS WATER FIRST - FUNCTION OF EXCESS AND WE.
    !
    IF(EXCESS/=0.0) THEN
        IF(EXCESS>=0.1 .AND. WE>=1.0) THEN
            !
            !     COMPUTE LAG IN HOURS AND PRORATE EXCESS.
            !
            N = ((EXCESS * 4.0)**0.3) + 0.5
            IF(N==0) N = 1
            FN = N
            DO I = 1, N
                FI = I
                TERM = 0.03 * WE * FN / (EXCESS * (FI - 0.5))
                IF(TERM>50.0) TERM = 50.0
                FLAG = 5.33 * (1.0 - EXP(-TERM))
                L2 = (FLAG + FIT) / FIT + 1.0
                L1 = L2 - 1
                ENDL1 = L1 * 24
                POR2 = (FLAG + FIT - ENDL1) / FIT
                POR1 = 1.0 - POR2
                EXLAG(L2) = EXLAG(L2) + POR2 * EXCESS / FN
                EXLAG(L1) = EXLAG(L1) + POR1 * EXCESS / FN
            end do
        ELSE
            !
            !     EXCESS OR WE SMALL, THUS NO LAG.
            !
            EXLAG(1) = EXLAG(1) + EXCESS
        END IF
    END IF
    !.......................................
    !     ATTENUATE LAGGED EXCESS WATER - FUNCTION OF STORGE AND WE.
    !
    IF((STORGE + EXLAG(1))/=0.0) THEN
        IF((STORGE + EXLAG(1))<0.1) THEN
            !
            !     NO ATTENUATION
            !
            PACKRO = STORGE + EXLAG(1)
            STORGE = 0.0
        ELSE
            !
            !     EFFECT OF ATTENUATION COMPUTED USING A ONE-HOUR TIME STEP.
            !
            EL = EXLAG(1) / FIT
            ELS = EL / 25.4
            WES = WE / 25.4
            TERM = 500.0 * ELS / (WES**1.3)
            IF(TERM>50.0) TERM = 50.0
            R1 = 1.0 / (5.0 * EXP(-TERM) + 1.0)
            DO I = 1, 24
                OS = (STORGE + EL) * R1
                PACKRO = PACKRO + OS
                STORGE = STORGE + EL - OS
            end do
            IF(STORGE<=0.001) THEN
                PACKRO = PACKRO + STORGE
                STORGE = 0.0
            END IF
        END IF
    END IF
    !
    !     DOWNSHIFT WATER IN EXLAG().
    290 DO I = 2, 4
        EXLAG(I - 1) = EXLAG(I)
    end do
    EXLAG(4) = 0.0
    !.......................................
    !     SET SURFACE MELT EQUAL TO SNOW COVER OUTFLOW
    !
    SMELT = PACKRO
    GO TO 380
    !.......................................
    !     SNOW GONE - SET ALL CARRYOVER TO NO SNOW CONDITIONS.
    !
    350 TEX = 0.0
    DO N = 1, 4
        TEX = TEX + EXLAG(N)
    end do
    SMELT = XMELT + TEX + STORGE + ROS
    WE = 0.0
    XNEGHS = 0.0
    XLIQW = 0.0
    TINDEX = 0.0
    STORGE = 0.0
    DO N = 1, 4
        EXLAG(N) = 0.0
    end do
    !
    !     END OF COMPUTATIONS
    !
    380 TEX = 0.0
    DO N = 1, 4
        TEX = TEX + EXLAG(N)
    end do
    TWE = WE + XLIQW + STORGE + TEX
    !
    !  CONVERSION OF DAILY VALUES TO ENGLISH UNITS
    !
    GMRO = GMRO / 25.4
    SMELT = SMELT / 25.4
    RAIN = RAIN / 25.4 + SMELT
    !
    !   OUTPUT ANALYSIS WRITE STATEMENTS
    !
    SNO = TWE / 25.4
    RETURN
END
!
!
!
!     ************************ DRAIN ************************
!
!
!     SUBROUTINE DRAIN CONTROLS THE COMPUTATION OF VERTICAL
!     WATER ROUTING, LATERAL SUBSURFACE DRAINAGE AND
!     PERCOLATION FOR A SUBPROFILE.
!
SUBROUTINE DRAIN (FIN, DRN, PRC, HED, BALY, BALT, EXTRA, &
        NPROF, NSEGB, NSEGE, LAYB, LAYE, DRIN, SWULY)
    !
    DOUBLE PRECISION DSWUL, DTHICK, DRCUL, DBUBUL, DWPUL, &
            DLAMUL, DRSUL, DUL, DFCUL, DSUBIN, DCHG, DRCRS, DSUB, DRCR
    !
    DOUBLE PRECISION DT, F, E, HED, THY, PRC, DRN, &
            QLAT, QPERC, DRIN, QPERCY, QLATY, &
            EXCESS, BALY, BALT, SWULY, DRCULS, DTHICS, ELKS
    !
    COMMON /BLK3/ PORO (20), FC (20), WP (20), RC (20), SW (20), &
            RS (20), XLAMBD (20), BUB (20), THICK (20), SLOPE (20), &
            XLENG (20), CON (20), SUBIN (20), RECIR (20), PHOLE (20), &
            DEFEC (20), TRANS (20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN(20), LSIN(20)
    COMMON /BLK7/ STHICK(67), UL(67), FCUL(67), WPUL(67), SWUL(67), &
            RCUL(67), RSUL(67), XLAMBU(67), CONUL(67), BUBUL(67), &
            SUBINS(67), SWULI(67)
    COMMON /BLK10/ ETT, ESS, EP, ES, ET (67)
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK17/ NSEG1, NSEG2, NSEG3, NSEG4, NSEG5, NSEG6, NSEG
    COMMON /BLK31/ DRCUL (67), DBUBUL (67), DRSUL (67), DLAMUL (67), &
            DUL(67), DFCUL(67), DWPUL(67), DSWUL(67), DTHICK(67), &
            DSUBIN(67), DCHG(67), DRCRS(67)
    !
    DIMENSION  SWULY (67), E (67), BALY (67), BALT (67), DRIN (68)&
            , DSUB(67), DRCR(67)
    !
    DTHICS = DTHICK (NSEGE)
    DRCULS = DRCUL (NSEGE)
    IBAR = 0
    LAT = 0
    NSEGL = NSEGE
    !
    !     IBAR=1 IF THERE IS A BARRIER LAYER, NSEGL IS THE NUMBER OF
    !     THE LOWER NON-BARRIER SEGMENT IN THE SUBPROFILE, AND NSEGE
    !     IS THE BOTTOM SEGMENT OF THE SUBPROFILE.
    !
    IF (LSEG (NSEGE) == 3) IBAR = 1
    IF (LSEG (NSEGE) >= 4) IBAR = 2
    IF (LAYB == LAYE) IBAR = 0
    IF (IBAR >= 1) THEN
        NSEGL = NSEGE - 1
        IF (LD(NPROF) > 0) LAT = 1
        IF (LAT == 1) LAYD = LAYSEG(NSEGL, 2)
    END IF
    !
    !     LAT=1 IF THERE IS A LATERAL DRAINAGE LAYER.
    !
    !     DT IS THE MODELING PERIOD AND F AND E ARE INFILTRATION
    !     AND EVAPOTRANSPIRATION OCCURRING DURING THE PERIOD.
    !
    DT = 1.0D0 / NSTEP(NPROF)
    F = FIN * DT
    DO J = NSEGB, NSEGE
        SWULY(J) = DSWUL (J)
        E(J) = ET(J) * DT
        DSUB(J) = DSUBIN(J) * DT
        DRCR(J) = DRCRS(J) * DT
    end do
    EXTRA = 0.0
    EXCESS = 0.0D0
    HED = 0.0D0
    PRC = 0.0D0
    DRN = 0.0D0
    IF (NSEG == NSEGE) THEN
        IF (IBAR == 1) THEN
            IF (SUBIN(LP(NPROF)) > 0.0) IBAR = IBAR + 3
        ELSE IF (IBAR == 2) THEN
            IF (SUBIN(LP(NPROF)) > 0.0 .OR.&
                    SUBIN(LP(NPROF) - 1) > 0.0) IBAR = IBAR + 3
        ELSE IF (DSUBIN(NSEGE) > 0.0D0) THEN
            IBAR = IBAR + 3
        END IF
    END IF
    DO K = 1, NSTEP(NPROF)
        DRIN (NSEGB) = F + EXCESS
        !       TH = THY
        EXCESS = 0.0D0
        IF (IBAR==0 .OR. IBAR==3) THEN
            QLAT = 0.0D0
            QLATY = 0.0D0
            !
            !     AVROUT COMPUTES VERTICAL WATER ROUTING.
            !
            CALL AVROUT (DRIN, E, DSUB, DRCR, BALY, BALT, SWULY, DT, IBAR, &
                    LAT, NSEGB, NSEGL, NSEGE, NPROF, QPERCY, QLATY, QPERC, QLAT)
            QPERC = DRIN (NSEGE + 1) / DT
            !          QDRN = QPERC
            CALL PROFIL (EXCESS, NSEGB, NSEGE, SWULY, BALT, DRIN, BALY)
            EXTRA = EXCESS
            PRC = PRC + DRIN(NSEGE + 1)
            DRN = 0.0D0
        ELSE
            !
            !     ESTIMATE DRAINAGE RATES
            !
            !
            !     HEAD COMPUTES THE DEPTH OF SATURATION ABOVE THE
            !     TOP OF THE BARRIER SOIL LINER.
            !
            CALL HEAD (THY, NSEGB, NSEGL, BALY, SWULY)
            QPERCY = 0.0D0
            QLATY = 0.0D0
            IF (THY > 0.0D0) THEN
                IF (IBAR == 1) QPERCY = DRCULS * (THY + DTHICS) / DTHICS
                IF (IBAR == 2) CALL FML (THY, QPERCY, NSEGE, NPROF)
                IF (LAT == 1) THEN
                    CALL LATKS (THY, NSEGB, NSEGL, ELKS)
                    CALL LATFLO (LAYD, THY, ELKS, QLATY)
                END IF
            END IF
            !          QDRN = QPERCY + QLATY
            !
            !     AVROUT COMPUTES VERTICAL WATER ROUTING.
            !
            CALL AVROUT (DRIN, E, DSUB, DRCR, BALY, BALT, SWULY, DT, IBAR, &
                    LAT, NSEGB, NSEGL, NSEGE, NPROF, QPERCY, QLATY, QPERC, QLAT)
            PRC = PRC + DRIN(NSEGE + 1)
            DRN = DRN + DRIN(NSEGL + 1) - DRIN(NSEGE + 1) + DSUB(NSEGE)
            !
            !     PROFIL DISTRIBUTES WATER BACK UP THE SUBPROFILE
            !     WHEN A SEGMENT IS SUPERSATURATED.
            !
            !          QDRN = QPERC + QLAT
            CALL PROFIL (EXCESS, NSEGB, NSEGL, SWULY, BALT, DRIN, BALY)
            EXTRA = EXCESS
            !
        END IF
        !
        DO J = NSEGB, NSEGL
            BALY (J) = BALT (J)
            SWULY (J) = DSWUL (J)
        end do
        IF (IBAR /= 0 .AND. IBAR /= 3) HED = HED + THY * DT
        !
    end do
    DO J = NSEGB, NSEGL
        DCHG (J) = BALT (J)
    end do
    RETURN
END
!
!
!       ************************* AVROUT *************************
!
!
!     SUBROUTINE AVROUT COMPUTES VERTICAL WATER ROUTING OR
!     DRAINAGE THROUGH THE SEGMENTS ABOVE THE BARRIER
!     LAYER OF A SUBPROFILE.
!
SUBROUTINE AVROUT (DRIN, E, DSUB, DRCR, BALY, BALT, SWULY, DT, &
        IBAR, LAT, NSEGB, NSEGL, NSEGE, NPROF, &
        QPERCY, QLATY, QPERC, QLAT)
    !
    DOUBLE PRECISION SWULY, DRNMX, DRMX, DSUB, DRCR, BALT, QLATY, &
            BALY, GRVWAT, DRNMAX, DRIN, DT, E, X, F, FDER, DX, QPERCY, &
            A, B, C, X0, FX0, FX0DER, QPERC, QLAT, &
            LOW, HIGH, SWMAX, SWMIN, TOLER
    !
    DOUBLE PRECISION DSWUL, DTHICK, DRCUL, DBUBUL, DWPUL, &
            DLAMUL, DRSUL, DUL, DFCUL, DSUBIN, DCHG, DRCRS
    !
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN(20), LSIN(20)
    COMMON /BLK10/ ETT, ESS, EP, ES, ET (67)
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK17/ NSEG1, NSEG2, NSEG3, NSEG4, NSEG5, NSEG6, NSEG
    COMMON /BLK27/ IFREZ, IDFS, IFCNT, KCNT, MXKCNT
    COMMON /BLK31/ DRCUL (67), DBUBUL (67), DRSUL (67), DLAMUL (67), &
            DUL(67), DFCUL(67), DWPUL(67), DSWUL(67), DTHICK(67), &
            DSUBIN(67), DCHG(67), DRCRS(67)
    DIMENSION  SWULY (67), E (67), BALY (67), BALT (67), DRIN (68), &
            DSUB(67), DRCR(67)

    !
    !       DEFINE STATEMENT FUNCTIONS
    !
    F (X) = A - (B * (X**C)) - (2 * X)
    FDER (X) = -(B * C * (X**(C - 1))) - 2
    !
    IFLAG = 0
    DO 1030 J = NSEGB, NSEGL
        !
        !     DRNMAX IS THE TOTAL DRAINABLE WATER IN A SEGMENT
        !     TODAY AND IS EQUAL TO THE SOIL WATER CONTENT AT THE
        !     BEGINNING OF THE DAY(SWULY+BALY/2) PLUS DRAINAGE INTO
        !     THE SEGMENT(DRIN) MINUS EVAPOTRANSPIRATION(E) AND THE
        !     LOWEST SOIL WATER CONTENT FOR DRAINAGE(SWLIM).
        !
        IF (J <= 7) THEN
            SWLIM = DWPUL(J)
            IF (DRIN(J) == 0.0D0 .AND. J < NSEGL) THEN
                RELMIN = (DWPUL(J + 1) - DRSUL(J + 1)) / &
                        (DUL(J + 1) - DRSUL(J + 1))
                RELSW = (SWULY(J + 1) + (BALY(J + 1) / 2.D0) - DRSUL(J + 1)) / &
                        (DUL(J + 1) - DRSUL(J + 1))
                IF (RELSW < RELMIN) RELSW = RELMIN
                PSIJP1 = DBUBUL(J + 1) * (RELSW ** (-1.0 / DLAMUL(J + 1)))
                IF (PSIJP1 < DBUBUL(J)) PSIJP1 = DBUBUL(J)
                RELSWL = (DBUBUL(J) / PSIJP1) ** DLAMUL(J)
                SWLIM = (RELSWL * (DUL(J) - DRSUL(J))) + DRSUL(J)
                IF (SWLIM < DWPUL(J)) SWLIM = DWPUL(J)
                IF (SWLIM > DFCUL(J)) SWLIM = DFCUL(J)
            END IF
            IF (J == NSEGL) SWLIM = DFCUL(J)
            !         IF (J .EQ. NSEG) SWLIM = DWPUL(J)
            IF (J == NSEG) SWLIM = DFCUL(J)
            IF (J == NSEG .AND. DRIN(J) > 0.0D0) SWLIM = DWPUL(J)
            IF (IFREZ == 1) SWLIM = DUL(J)
        ELSE IF (J == NSEGL .AND. J /= NSEG) THEN
            SWLIM = DFCUL(J)
        ELSE IF (J == NSEG .AND. DRIN(J) > 0.0D0) THEN
            SWLIM = DWPUL(J)
        ELSE IF (J == NSEG .AND. DRIN(J) <= 0.0D0) THEN
            SWLIM = DFCUL(J)
            !       ELSE IF (J .EQ. NSEG) THEN
            !         SWLIM = DWPUL(J)
        ELSE
            RELMIN = (DWPUL(J + 1) - DRSUL(J + 1)) / &
                    (DUL(J + 1) - DRSUL(J + 1))
            RELSW = (SWULY(J + 1) + (BALY(J + 1) / 2.D0) - DRSUL(J + 1)) / &
                    (DUL(J + 1) - DRSUL(J + 1))
            IF (RELSW < RELMIN) RELSW = RELMIN
            PSIJP1 = DBUBUL(J + 1) * (RELSW ** (-1.0 / DLAMUL(J + 1)))
            IF (PSIJP1 < DBUBUL(J)) PSIJP1 = DBUBUL(J)
            RELSWL = (DBUBUL(J) / PSIJP1) ** DLAMUL(J)
            SWLIM = (RELSWL * (DUL(J) - DRSUL(J))) + DRSUL(J)
            IF (SWLIM < DWPUL(J)) SWLIM = DWPUL(J)
            IF (SWLIM > DFCUL(J)) SWLIM = DFCUL(J)
        END IF
        DRNMAX = SWULY(J) + (BALY(J) / 2.0D0) + DRIN(J) + DSUB(J) + &
                DRCR(J) - E (J) - SWLIM
        IF (J == NSEGL) THEN
            IF (NSEGE == NSEG) THEN
                IF (IBAR == 3) DRNMAX = 0.0D0
                IF (IBAR > 3) DRNMAX = DRNMAX + DSUB(NSEGE)
                IF (IBAR >= 3) THEN
                    QPERC = 0.0D0
                    DRIN(NSEGE + 1) = 0.0D0
                    IF (LAT == 0) THEN
                        QLAT = 0.0D0
                        DRIN(NSEGL + 1) = 0.0D0
                        GO TO 1100
                    END IF
                END IF
            END IF
            IF (IBAR /= 0 .AND. IBAR /= 3) THEN
                GRVWAT = DRNMAX + SWLIM - DFCUL(J)
                IF (GRVWAT <= 0.0D0) THEN
                    GRVWAT = 0.0D0
                    QLAT = 0.0D0
                    QPERC = 0.0D0
                    DRIN(NSEGE + 1) = 0.0D0
                    DRIN(NSEGL + 1) = 0.0D0
                    IF (IBAR<3) DRIN(NSEGE + 1) = DRIN(NSEGE + 1) + DSUB(NSEGE)
                    GO TO 1100
                END IF
                IF (GRVWAT >= ((QPERCY + QLATY) * DT)) THEN
                    DRIN(NSEGL + 1) = (QPERCY + QLATY) * DT
                    DRIN(NSEGE + 1) = QPERCY * DT
                    QPERC = QPERCY
                    QLAT = QLATY
                ELSE IF (LAT == 0) THEN
                    QLAT = 0.0D0
                    QPERC = GRVWAT / DT
                    DRIN(NSEGL + 1) = GRVWAT
                    DRIN(NSEGE + 1) = GRVWAT
                ELSE
                    DRIN(NSEGL + 1) = GRVWAT
                    QLAT = (GRVWAT / DT) * QLATY / (QPERCY + QLATY)
                    QPERC = (GRVWAT / DT) * QPERCY / (QPERCY + QLATY)
                    DRIN(NSEGE + 1) = QPERC * DT
                    QPERC = GRVWAT / DT
                END IF
                IF (IBAR<3) DRIN(NSEGE + 1) = DRIN(NSEGE + 1) + DSUB(NSEGE)
                GO TO 1100
            END IF
        END IF
        IF (DRNMAX < 0.0D0) DRNMAX = 0.0D0
        !
        !     LOW IS MINIMUM UNSATURATED DRAINAGE RATE (UNRESTRICTED)
        !
        LOW = DRCUL (J) * DT * ((SWLIM - DRSUL (J)) / (DUL (J)&
                - DRSUL (J)))**(3.D0 + (2.0D0 / DLAMUL (J)))
        IF (LOW > DRNMAX) THEN
            DRIN(J + 1) = DRNMAX
            GO TO 1070
        END IF
        !
        !     SWMAX IS MOISTURE AT END OF PERIOD IF NO DRAINAGE OCCURRED
        !
        SWMAX = SWULY(J) + (0.5D0 * BALY (J)) + DSUB(J) + DRCR(J) + &
                DRIN (J) - E (J)
        IF (J==NSEGL .AND. NSEGE == NSEG
            . AND. IBAR .GT. 3)&
                    SWMAX = SWMAX + DSUB(NSEGE)
            IF (SWMAX > DUL(J)) SWMAX = DUL(J)
            IF (SWMAX <= SWLIM) THEN
                DRIN(J + 1) = 0.0D0
                GO TO 1070
            END IF
            !
            !     HIGH IS MAXIMUM UNSATURATED DRAINAGE RATE (UNRESTRICTED)
            !
            HIGH = DRCUL (J) * DT * ((SWMAX - DRSUL (J)) / (DUL (J)&
                    - DRSUL (J)))**(3.D0 + (2.0D0 / DLAMUL (J)))
            IF(HIGH>DRNMAX)HIGH = DRNMAX
            SWMIN = SWMAX - HIGH
            IF (SWMIN < SWLIM) SWMIN = SWLIM
            LOW = DRCUL (J) * DT * ((SWMIN - DRSUL (J)) / (DUL (J)&
                    - DRSUL (J)))**(3.D0 + (2.0D0 / DLAMUL (J)))
            IF (LOW > DRNMAX) LOW = DRNMAX
            SWMAX = SWULY(J) + (0.5D0 * BALY (J)) + DSUB(J) + DRCR(J) + &
                    DRIN (J) - E (J) - LOW
            IF (J==NSEGL .AND. NSEGE == NSEG
                . AND. IBAR .GT. 3)&
                        SWMAX = SWMAX + DSUB(NSEGE)
                IF (SWMAX > DUL(J)) SWMAX = DUL(J)
                IF (SWMAX <= SWMIN) THEN
                    DRIN(J + 1) = DRNMAX
                    GO TO 1070
                END IF
                !
                !     SOLVE FOR DRAINAGE OUT OF SEGMENT J BY NEWTON METHOD
                !
                A = 2.D0 * (SWULY(J) - DRSUL (J)) + BALY(J) + DRIN(J) + DSUB(J)&
                        + DRCR(J) - E(J)
                IF (J==NSEGL .AND. NSEGE == NSEG
                    . AND. IBAR .GT. 3)&
                            A = A + (2.0D0 * DSUB(NSEGE))
                    C = (3.D0 + (2.D0 / DLAMUL (J)))
                    B = (DRCUL (J) * DT) * ((1.D0 / (DUL(J) - DRSUL(J)))**C)
                    !
                    TOLER = 0.003D0 * (DUL(J) - DRSUL(J))
                    X0 = (SWMAX + SWMIN) / 2.D0
                    !
                    1060   CONTINUE
                    DO ITER = 1, 100
                        FX0 = F(X0)
                        IF (FX0 <= (0.3 * TOLER)) GO TO 1061
                        FX0DER = FDER(X0)
                        IF (ABS(FX0DER) < 1.0D-03) GO TO 1061
                        DX = FX0 / FX0DER
                        X0 = X0 - DX
                        IF(DABS(DX) < TOLER) GO TO 1061
                    end do
                    IF (IFLAG == 0) WRITE(8, 1063) NSEGL
                    1063   FORMAT(1X, 'UNSATURATED DRAINAGE ABOVE SEGMENT ', I2, ' DID NOT', &
                            ' CONVERGE.')
                    IFLAG = 1
                    !
                    1061   DSWUL(J) = X0
                    IF(DSWUL(J) > SWMAX) DSWUL(J) = SWMAX
                    IF(DSWUL(J) < SWMIN) DSWUL(J) = SWMIN
                    DRIN(J + 1) = 2.D0 * (SWULY(J) - DSWUL(J)) + BALY(J) + &
                            DRIN(J) + DRCR(J) + DSUB(J) - E(J)
                    IF (NSEGE == NSEG
                        . AND. IBAR .GT. 3) DRIN(J+1) = &
                                DRIN(J+1) + DSUB(NSEGE)
                        1070   CONTINUE
                        IF (LAYSEG(J, 2) < LD(NPROF)) THEN
                            IF (DRIN(J + 1) > (DRCUL(J) * DT)) DRIN(J + 1) = DRCUL(J) * DT
                        END IF
                        IF (DRIN(J + 1) > DRNMAX) DRIN(J + 1) = DRNMAX
                        IF (DRIN(J + 1) < 0.0D0) DRIN(J + 1) = 0.0D0
                        IF (J < NSEGL .AND. LAYSEG(J + 1, 2) < LD(NPROF)) THEN
                            IF (DRIN(J + 1) > (DRCUL(J + 1) * DT)) THEN
                                SW = (SWULY(J) + (BALY(J) / 2.D0) - DRSUL(J)) / &
                                        (DUL(J) - DRSUL(J))
                                TRATIO = DTHICK(J) / DTHICK(J + 1)
                                DRMX = DUL(J + 1) - SWULY(J + 1) - (BALY(J + 1) / 2.D0) + &
                                        (DRCUL(J + 1) * DT) + E(J + 1) - DSUB(J + 1) - DRCR(J + 1)
                                IF (DRMX > DRNMAX) DRMX = DRNMAX
                                DRNMX = DRCUL(J + 1) * DT * (1 + (SW * TRATIO))
                                IF (DRNMX > DRMX) DRNMX = DRMX
                                IF (DRNMX > DRNMAX) DRNMX = DRNMAX
                                IF (DRIN(J + 1) > DRNMX) DRIN(J + 1) = DRNMX
                                IF (DRIN(J + 1) < 0.0D0) DRIN(J + 1) = 0.0D0
                            END IF
                        END IF
                        !
                        1030 CONTINUE
                        !
                        1100 CONTINUE
                        DO J = NSEGB, NSEGL
                            IF (J == NSEGE .AND. IBAR == 3) DRIN(J + 1) = 0.0D0
                            BALT(J) = DRIN(J) + DSUB(J) + DRCR(J) - DRIN(J + 1) - E(J)
                            IF (J == NSEGL .AND. IBAR > 3) THEN
                                BALT(J) = BALT(J) + DSUB(NSEGE)
                            END IF
                            DSWUL(J) = SWULY(J) + ((BALY(J) + BALT(J)) / 2.D0)
                        end do
                        RETURN
END
!
!       ************************* PROFIL *************************
!
!     SUBROUTINE PROFIL DISTRIBUTES WATER IN A SUBPROFILE
!     FROM SUPERSATURATED SEGMENTS TO THE SEGMENTS
!     DIRECTLY ABOVE THE SUPERSATURATED SEGMENTS.
!
SUBROUTINE PROFIL (EXCESS, NSEGB, NSEGL, SWULY, BALT, DRIN, BALY)
    !
    DOUBLE PRECISION EXCESS, S, BALT, SWULY, BALY, EXCE, DRIN
    DOUBLE PRECISION DSWUL, DTHICK, DRCUL, DBUBUL, DSUBIN, DCHG, &
            DWPUL, DLAMUL, DRSUL, DUL, DFCUL, DRCRS
    !
    COMMON /BLK31/ DRCUL (67), DBUBUL (67), DRSUL (67), DLAMUL (67), &
            DUL(67), DFCUL(67), DWPUL(67), DSWUL(67), DTHICK(67), &
            DSUBIN(67), DCHG(67), DRCRS(67)
    !
    DIMENSION BALT (67), SWULY (67), BALY (67), DRIN (68)
    !
    EXCESS = 0.0D0
    DO J = NSEGB, NSEGL
        K = NSEGL + NSEGB - J
        !
        !     EXCESS IS THE EXCESS WATER(ABOVE SATURATION) IN THE SEGMENT
        !     DIRECTLY BELOW THE SEGMENT BEING EVALUATED.
        !
        S = DSWUL (K) + EXCESS + (BALT (K) / 2.0D0)
        EXCE = EXCESS
        IF (EXCE < 0.0D0) EXCE = 0.0D0
        EXCESS = S - DUL (K)
        IF (EXCESS > 0.0D0) GO TO 1010
        EXCESS = 0.0D0
        BALT (K) = BALT (K) + EXCE
        DSWUL (K) = SWULY (K) + ((BALY (K) + BALT (K)) / 2.0D0)
        GO TO 1000
        !
        !     THE DRAINAGE AND CHANGE OF STORAGE ARE RECOMPUTED
        !     TO ACCOUNT FOR THE REDISTRIBUTION OF SOIL WATER.
        !
        1010    CONTINUE
        BALT (K) = DUL (K) - SWULY (K) - (BALY (K) / 2.0D0)
        DSWUL (K) = DUL (K) - (BALT (K) / 2.0D0)
        DRIN (K) = DRIN (K) - EXCESS
    1000 CONTINUE
    end do
    !
    RETURN
END
!
!       ************************ HEAD ************************
!
!
!     SUBROUTINE HEAD COMPUTES THE GRAVITATIONAL HEAD ON OR THE
!     DEPTH OF SATURATION ABOVE THE TOP OF THE BARRIER SOIL LINER.
!
SUBROUTINE HEAD (TH, NSEGB, NSEGL, BALY, SWULY)
    !
    DOUBLE PRECISION  TH, BALY, XHEAD, SWULY
    DOUBLE PRECISION DSWUL, DTHICK, DRCUL, DBUBUL, DSUBIN, DCHG, &
            DWPUL, DLAMUL, DRSUL, DUL, DFCUL, DRCRS
    !
    COMMON /BLK31/ DRCUL (67), DBUBUL (67), DRSUL (67), DLAMUL (67), &
            DUL(67), DFCUL(67), DWPUL(67), DSWUL(67), DTHICK(67), &
            DSUBIN(67), DCHG(67), DRCRS(67)
    !
    DIMENSION  SWULY(67), BALY(67)
    !
    XHEAD = 0.0D0
    TH = 0.0D0
    !
    !     COMPUTES HEAD FROM THE BOTTOM TO THE TOP UNTIL A SEGMENT
    !     IS REACHED THAT IS NOT SATURATED AT END OF TIME PERIOD.
    !
    DO J = NSEGB, NSEGL
        !
        K = NSEGL + NSEGB - J
        IF ((SWULY(K) + (BALY(K) / 2.0D0)) <= DFCUL (K)) GO TO 1010
        XHEAD = DTHICK(K) * (SWULY(K) + (BALY(K) / 2.0D0) - DFCUL(K)) / &
                (DUL (K) - DFCUL (K))
        !
        IF (XHEAD > DTHICK (K)) XHEAD = DTHICK (K) + &
                SWULY (K) + (BALY(K) / 2.0D0) - DUL (K)
        TH = TH + XHEAD
        XHEAD = XHEAD - DTHICK(K)
        IF ((XHEAD + 0.00005D0) < 0.0D0) GO TO 1010
    end do
    RETURN
    !
    1010 CONTINUE
    RETURN
END
!
!
!    ************************* LATKS *************************
!
!
!     SUBROUTINE LATKS COMPUTES EFFECTIVE LATERAL
!     HYDRAULIC CONDUCTIVITY.
!
SUBROUTINE LATKS (TH, NSEGB, NSEGL, ELKS)
    !
    DOUBLE PRECISION ELKS, TH, H
    DOUBLE PRECISION DSWUL, DTHICK, DRCUL, DBUBUL, DSUBIN, DCHG, &
            DWPUL, DLAMUL, DRSUL, DUL, DFCUL, DRCRS
    !
    COMMON /BLK31/ DRCUL (67), DBUBUL (67), DRSUL (67), DLAMUL (67), &
            DUL(67), DFCUL(67), DWPUL(67), DSWUL(67), DTHICK(67), &
            DSUBIN(67), DCHG(67), DRCRS(67)
    !
    ELKS = 0.0D0
    IF (TH <= 0.0D0) GO TO 1000
    H = TH
    DO J = NSEGB, NSEGL
        K = NSEGL + NSEGB - J
        IF (H <= 0.0D0) GO TO 1000
        IF (H > DTHICK (K)) GO TO 1020
        ELKS = ELKS + DRCUL (K) * H / TH
        GO TO 1000
        1020    CONTINUE
        ELKS = ELKS + DRCUL (K) * DTHICK (K) / TH
        H = H - DTHICK (K)
    end do
    1000 CONTINUE
    RETURN
END
!
!      ************************* LATFLO ************************
!
!     SUBROUTINE LATFLO COMPUTES THE LATERAL DRAINAGE
!     AND PERCOLATION FROM THE LATERAL DRAINAGE LAYER.
!
SUBROUTINE LATFLO (LAYD, TH, ELKS, DRN)
    !
    DOUBLE PRECISION DRN, TH, ELKS
    DOUBLE PRECISION DSLOPE, DXLENG, ALPHA, YSTAR, QSTAR
    DOUBLE PRECISION PI, A, B, C, D, F, TOLER, X, EPS
    !     DOUBLE PRECISION DSWUL, DTHICK, DRCUL, DBUBUL, DSUBIN, DCHG,
    !    1   DWPUL, DLAMUL, DRSUL, DUL, DFCUL, DRCRS
    !
    COMMON /BLK3/ PORO (20), FC (20), WP (20), RC (20), SW (20), &
            RS (20), XLAMBD (20), BUB (20), THICK (20), SLOPE (20), &
            XLENG (20), CON (20), SUBIN (20), RECIR (20), PHOLE (20), &
            DEFEC (20), TRANS (20), EDEPTH, SUBINF, SEDEP, CORECT
    !     COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY,
    !    1  LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN(20), LSIN(20)
    !     COMMON /BLK31/ DRCUL (67), DBUBUL (67), DRSUL (67), DLAMUL (67),
    !    1  DUL(67), DFCUL(67), DWPUL(67), DSWUL(67), DTHICK(67),
    !    2  DSUBIN(67), DCHG(67), DRCRS(67)
    !
    !     DEFINE STATEMENT FUNCTION
    !
    F (X) = (A * (B**((X / C)**D)))
    !
    !     ASSIGN VALUES FOR TOLERANCE AND PI
    !
    DATA EPS/0.030D0/
    DATA PI/3.141592654D0/
    !
    DSLOPE = SLOPE (LAYD) / 100.D0
    DXLENG = XLENG (LAYD) * 12.D0
    IF(DXLENG == 0.0D0) DXLENG = 1.D0
    IF (TH > 0.0D0) GO TO 1000
    !
    !     NO DRAINAGE AND PERCOLATION IF THE HEAD IS ZERO.
    !
    DRN = 0.0D0
    RETURN
    !
    !     COMPUTE LATERAL DRAINAGE RATE BY REGULA FALSI METHOD
    !
    !     DEFINE VARIABLES IN DIMENSIONLESS FORM
    !
    1000 CONTINUE
    ITER = 1
    ALPHA = DATAN (DSLOPE)
    YSTAR = TH / DXLENG
    QSTAR = 2.0 * YSTAR * DSIN(ALPHA) * DCOS(ALPHA)
    !
    IF (ALPHA <= 0.) THEN
        QSTAR = ((4.D0 / PI) * YSTAR)**2
        DRN = QSTAR * ELKS
        RETURN
    ENDIF
    !
    A = PI / 4.0D0 / DCOS (ALPHA)
    B = 2.0D0 * DSQRT (0.4D0) / PI
    C = 0.4D0 * (DSIN (ALPHA))**2
    D = 0.5D0 / DLOG (B)
    !
    IF (YSTAR < (0.2 * DSLOPE)) THEN
        DRN = QSTAR * ELKS
        RETURN
    ENDIF
    !
    !
    1010 QSTARN = (YSTAR * YSTAR) / (F(QSTAR) * F(QSTAR))
    TOLER = 2.0 * DABS(QSTARN - QSTAR) / (QSTARN + QSTAR)
    IF (EPS < TOLER .AND. ITER < 10) THEN
        ITER = ITER + 1
        QSTAR = (QSTAR + QSTARN) / 2.0D0
        GO TO 1010
    ENDIF
    !
    DRN = (QSTAR + QSTARN) * ELKS / 2.0D0
    !
    RETURN
END
!
!
!
!     ************************   MOUND    ************************
!
!
!     SUBROUTINE  MOUND  COMPUTES THE MAXIMUM HEAD USING THE
!         MCENROE'S EQUATIONS
!
!
SUBROUTINE MOUND (QDMAX, ELKS, LAYD, PMHED, PLEN)
    !
    DOUBLE PRECISION ELKS
    !
    COMMON /BLK1/ IT4, IT7, IT13, IU4, IU7, IU13, IU11, IU10, IU8, &
            LMYR, IOD, IOM, IOA, IOS
    COMMON /BLK3/ PORO (20), FC (20), WP (20), RC (20), SW (20), &
            RS (20), XLAMBD (20), BUB (20), THICK (20), SLOPE (20), &
            XLENG (20), CON (20), SUBIN (20), RECIR (20), PHOLE (20), &
            DEFEC (20), TRANS (20), EDEPTH, SUBINF, SEDEP, CORECT
    !
    IF (QDMAX <= 0.0) THEN
        PMHED = 0.0
        PLEN = 0.0
        RETURN
    END IF
    XLEN = XLENG(LAYD) * 12.0
    IF (IU8 /= 1) XLEN = XLEN * 25.4
    TANALP = SLOPE(LAYD) / 100.0
    SINALP = SIN(ATAN(TANALP))
    IF (SINALP < 0.004) THEN
        !
        !     ORIGINAL MOUND MODEL BY MOORE
        !
        IF (IU8 /= 1) QDMAX = QDMAX / 25.4
        CMAX = QDMAX / ELKS
        IF (CMAX > 1.E-8) THEN
            PMHED = XLEN * SQRT(CMAX) * ((TANALP * TANALP / CMAX) + 1.0&
                    - ((TANALP / CMAX) * SQRT((TANALP * TANALP) + CMAX)))
        ELSE
            PMHED = XLEN * SQRT(CMAX)
        END IF
        QPMAX = ELKS * PMHED * TANALP / XLEN
        IF (QPMAX < QDMAX) THEN
            XL = 1. - (QPMAX / QDMAX)
            PLEN = XLEN * XL / 12.
            IF (IU8 /= 1) PLEN = XLEN * XL / 1000.0
        ELSE
            PLEN = 0.0
        END IF
        RETURN
    END IF
    !
    !     McENROE'S EQUATIONS
    !
    S = TANALP
    IF (IU8 /= 1) QDMAX = QDMAX / 25.4
    RMAX = QDMAX / ELKS / SINALP / SINALP
    IF (RMAX < 1. / 4.) THEN
        A = SQRT(1. - (4. * RMAX))
        Y1 = SQRT (RMAX - (RMAX * S) + ((RMAX * S) * (RMAX * S)))
        Y2 = (1. - A - 2 * RMAX) * (1 + A - 2 * (RMAX * S))
        Y3 = (1. + A - 2 * RMAX) * (1 - A - 2 * (RMAX * S))
        IF ((Y2 / Y3)> 0.0) THEN
            YMAX = Y1 * ((Y2 / Y3)**(1. / (2 * A)))
        ELSE
            PMHED = QDMAX * XLEN / ELKS / TANALP
            YMAX = PMHED / XLEN / S
        END IF
        PMHED = YMAX * XLEN * S
        QPMAX = ELKS * PMHED * TANALP / XLEN
        IF (QPMAX < QDMAX) THEN
            XL = 1. - (QPMAX / QDMAX)
            PLEN = XLEN * XL / 12.
            IF (IU8 /= 1) PLEN = XLEN * XL / 1000.0
        ELSE
            PLEN = 0.0
        END IF
        RETURN
    END IF
    IF (RMAX == 1. / 4.) THEN
        !   CORRECTION BELOW
        Y1 = (RMAX * (1. - (2 * RMAX * S))) / (1. - (2 * RMAX))
        Y2 = 2 * RMAX * (S - 1.) / ((1. - (2 * RMAX * S)) * (1. - (2 * RMAX)))
        YMAX = Y1 * EXP(Y2)
        PMHED = YMAX * XLEN * S
        QPMAX = ELKS * PMHED * TANALP / XLEN
        IF (QPMAX < QDMAX) THEN
            XL = 1. - (QPMAX / QDMAX)
            PLEN = XLEN * XL / 12.
            IF (IU8 /= 1) PLEN = XLEN * XL / 1000.0
        ELSE
            PLEN = 0.0
        END IF
        RETURN
    END IF
    IF (RMAX > 1. / 4.) THEN
        B = SQRT((4. * RMAX) - 1.)
        Y1 = SQRT (RMAX - (RMAX * S) + ((RMAX * S) * (RMAX * S)))
        Y2 = (1. / B) * ATAN(((2 * RMAX * S) - 1.) / B)
        Y3 = (1. / B) * ATAN(((2 * RMAX) - 1.) / B)
        YMAX = Y1 * EXP(Y2 - Y3)
        PMHED = YMAX * XLEN * S
        QPMAX = ELKS * PMHED * TANALP / XLEN
        IF (QPMAX < QDMAX) THEN
            XL = 1. - (QPMAX / QDMAX)
            PLEN = XLEN * XL / 12.
            IF (IU8 /= 1) PLEN = XLEN * XL / 1000.0
        ELSE
            PLEN = 0.0
        END IF
        RETURN
    END IF

    !
    !     REVISED MOUND MODEL BY GIROUD
    !
    !     PMHED = XLEN * (SQRT( (CMAX + (TANALP * TANALP)))
    !    1  - TANALP)
    !
    RETURN
END
!
!
!     ************************   FML    ************************
!
!
!     SUBROUTINE  FML   COMPUTES THE LEAKAGE RATE THROUGH INTACT
!     AND DAMAGED GEOMEMBRANE AND COMPOSITE LINERS
!
!     REQUIRED INPUT: LAYER TYPE, LAYER HYDRAULIC CONDUCTIVITY (CM/SEC),
!                     LAYER PLACEMENT QUALITY, GEOMEMBRANE THICKNESS (IN.),
!                     AND NUMBER OF DEFECT PER GEOMEMBRANE ACRE
!
SUBROUTINE FML(TH, QPERC, NSEGE, NPROF)
    !
    DOUBLE PRECISION DSWUL, DTHICK, DRCUL, DBUBUL, DWPUL, &
            DLAMUL, DRSUL, DUL, DFCUL, DSUBIN, DCHG, RDEFEC, &
            RPHOLE, HDEFEC, HPHOLE, DRCRS
    !
    DOUBLE PRECISION  TH, QPERC, QVAPOR
    !
    COMMON /BLK3/ PORO (20), FC (20), WP (20), RC (20), SW (20), &
            RS (20), XLAMBD (20), BUB (20), THICK (20), SLOPE (20), &
            XLENG (20), CON (20), SUBIN (20), RECIR (20), PHOLE (20), &
            DEFEC (20), TRANS (20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN(20), LSIN(20)
    COMMON /BLK7/ STHICK(67), UL(67), FCUL(67), WPUL(67), SWUL(67), &
            RCUL(67), RSUL(67), XLAMBU(67), CONUL(67), BUBUL(67), &
            SUBINS(67), SWULI(67)
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK17/ NSEG1, NSEG2, NSEG3, NSEG4, NSEG5, NSEG6, NSEG
    COMMON /BLK31/ DRCUL (67), DBUBUL (67), DRSUL (67), DLAMUL (67), &
            DUL(67), DFCUL(67), DWPUL(67), DSWUL(67), DTHICK(67), &
            DSUBIN(67), DCHG(67), DRCRS(67)
    !
    QVAPOR = 0.0D0
    QPHOLE = 0.0D0
    QDEFEC = 0.0D0
    !
    !     WATER VAPOR TRANSMISSION THROUGH INTACT GEOMEMBRANES
    !     (USED IN ALL SIX DESIGN CASES)
    !
    IF (LAYER(LP(NPROF)) == 4) LINER = LP(NPROF)
    IF (LAYER(LP(NPROF) - 1) == 4) LINER = LP(NPROF) - 1
    IF (TH > THICK(LINER)) THEN
        QVAPOR = 34016.D0 * RC(LINER) * TH / THICK(LINER)
    ELSE IF (LCASE(NPROF) == 3 .OR. LCASE(NPROF) == 5) THEN
        IF (TH > 0.0D0) THEN
            QVAPOR = 34016.D0 * RC(LINER)
        ELSE
            QVAPOR = 0.0D0
        END IF
    ELSE
        QVAPOR = 34016.D0 * RC(LINER)
    END IF
    !
    !     LEAKAGE THROUGH PINHOLES AND DEFECTS FOR SIX GEOMEMBRANE/SOIL LINER
    !     DESIGN CASES WITH DIFFERING LEVELS OF CONTACT
    !
    !     ASSUMED VALUES:
    !
    !     PINHOLE SIZE: DIAMETER = 0.001 M , AREA = 7.84E-7 SQM
    !     DEFECT SIZE: DIAMETER = 0.0113 M , AREA = 0.0001 SQM (20 MM X 5 MM)
    !
    !     HIGH K LAYER: K > 1 CM/SEC (COARSE GRAVEL OR GEONETS)
    !     MED. K LAYER: 1E-3 CM/SEC < K < 1E-1 CM/SEC (FINE GRAVEL OR SAND)
    !     LOW  K LAYER: 1E-8 CM/SEC < K < 1E-4 CM/SEC (SILT OR CLAY)
    !
    !     HEAD ON LINER MUST BE GREATER THAN 0.0
    !
    IF (TH > 0.0D0) THEN
        !
        !       CASE 1 - HIGH-K/GEOMEM/HIGH-K
        !
        IF (LCASE(NPROF) == 1) THEN
            !
            QPHOLE = PHOLE(LINER) * 0.000177 * TH / THICK(LINER)
            !
            QDEFEC = DEFEC(LINER) * 0.0356 * DSQRT(TH)
            !
        END IF
        !
        !       CASE 2 - MED-K/GEOMEM/HIGH-K  OR  HIGH-K/GEOMEM/MED-K  OR
        !                MED-K/GEOMEM/MED-K
        !
        IF (LCASE(NPROF) == 2) THEN
            !
            !         PERFECT CONTACT
            !
            IF (LPQ(NPROF) == 1) THEN
                !
                QPHOLE = PHOLE(LINER) * 0.000671 * TH * RC(LAYSEG(NSEGE, 1))
                !
                QDEFEC = DEFEC(LINER) * 0.00758 * TH * RC(LAYSEG(NSEGE, 1))
                !
            END IF
            !
            !         EXCELLENT CONTACT
            !
            IF (LPQ(NPROF) > 1 .AND. LPQ(NPROF) < 5) THEN
                !
                IF (PHOLE(LINER) > 0.0) THEN
                    RPHOLE = 0.00363 * (TH**0.38) * (RC(LAYSEG(NSEGE, 1))**(-0.25))
                    HPHOLE = 1 + (TH / (2. * DTHICK(NSEGE) * DLOG(RPHOLE / 0.0005)))
                    QPHOLE = PHOLE(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                            HPHOLE * (RPHOLE**2)
                END IF
                !
                IF (DEFEC(LINER) > 0.0) THEN
                    RDEFEC = 0.0229 * (TH**0.38) * (RC(LAYSEG(NSEGE, 1))**(-0.25))
                    HDEFEC = 1 + (TH / (2. * DTHICK(NSEGE) * DLOG(RDEFEC / 0.00564)))
                    QDEFEC = DEFEC(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                            HDEFEC * (RDEFEC**2)
                END IF
                !
            END IF
            !
            !         BAD CONTACT
            !
            IF (LPQ(NPROF) == 5) THEN
                !
                QPHOLE = PHOLE(LINER) * 0.000177 * TH / THICK(LINER)
                !
                QDEFEC = DEFEC(LINER) * 0.0356 * DSQRT(TH)
                !
            END IF
        END IF
        !
        !       CASE 3 - MED-K/GEOMEM/LOW-K  OR  HIGH-K/GEOMEM/LOW-K  OR
        !                LOW-K (SOIL TYPE 1 OR 2)/GEOMEM/LOW-K (KS)
        !
        IF (LCASE(NPROF) == 3) THEN
            !
            !         PERFECT CONTACT
            !
            IF (LPQ(NPROF) == 1) THEN
                !
                QPHOLE = PHOLE(LINER) * 0.000671 * TH * RC(LAYSEG(NSEGE, 1))
                !
                QDEFEC = DEFEC(LINER) * 0.00758 * TH * RC(LAYSEG(NSEGE, 1))
                !
            END IF
            !
            !         EXCELLENT CONTACT
            !
            IF (LPQ(NPROF) == 2) THEN
                !
                IF (PHOLE(LINER) > 0.0) THEN
                    RPHOLE = 0.052 * (TH**0.5) * (RC(LAYSEG(NSEGE, 1))**(-0.06))
                    HPHOLE = 1 + (TH / (2 * DTHICK(NSEGE) * DLOG(RPHOLE / 0.0005)))
                    QPHOLE = PHOLE(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                            HPHOLE * (RPHOLE**2)
                END IF
                !
                IF (DEFEC(LINER) > 0.0) THEN
                    RDEFEC = 0.0663 * (TH**0.5) * (RC(LAYSEG(NSEGE, 1))**(-0.06))
                    HDEFEC = 1 + (TH / (2. * DTHICK(NSEGE) * DLOG(RDEFEC / 0.00564)))
                    QDEFEC = DEFEC(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                            HDEFEC * (RDEFEC**2)
                END IF
                !
            END IF
            !
            !         GOOD CONTACT
            !
            IF (LPQ(NPROF) == 3) THEN
                !
                IF (PHOLE(LINER) > 0.0) THEN
                    RPHOLE = 0.0449 * (TH**0.45) * (RC(LAYSEG(NSEGE, 1))**(-0.13))
                    HPHOLE = 1 + (TH / (2. * DTHICK(NSEGE) * DLOG(RPHOLE / 0.0005)))
                    QPHOLE = PHOLE(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                            HPHOLE * (RPHOLE**2)
                END IF
                !
                IF (DEFEC(LINER) > 0.0) THEN
                    RDEFEC = 0.0572 * (TH**0.45) * (RC(LAYSEG(NSEGE, 1))**(-0.13))
                    HDEFEC = 1 + (TH / (2. * DTHICK(NSEGE) * DLOG(RDEFEC / 0.00564)))
                    QDEFEC = DEFEC(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                            HDEFEC * (RDEFEC**2)
                END IF
                !
            END IF
            !
            !         POOR CONTACT
            !
            IF (LPQ(NPROF) == 4) THEN
                !
                IF (PHOLE(LINER) > 0.0) THEN
                    RPHOLE = 0.1053 * (TH**0.45) * (RC(LAYSEG(NSEGE, 1))**(-0.13))
                    HPHOLE = 1 + (TH / (2. * DTHICK(NSEGE) * DLOG(RPHOLE / 0.0005)))
                    QPHOLE = PHOLE(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                            HPHOLE * (RPHOLE**2)
                END IF
                !
                IF (DEFEC(LINER) > 0.0) THEN
                    RDEFEC = 0.134 * (TH**0.45) * (RC(LAYSEG(NSEGE, 1))**(-0.13))
                    HDEFEC = 1 + (TH / (2. * DTHICK(NSEGE) * DLOG(RDEFEC / 0.00564)))
                    QDEFEC = DEFEC(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                            HDEFEC * (RDEFEC**2)
                END IF
                !
            END IF
            !
            !         BAD CONTACT
            !
            IF (LPQ(NPROF) == 5) THEN
                !
                QPHOLE = PHOLE(LINER) * 0.000177 * TH / THICK(LINER)
                !
                QDEFEC = DEFEC(LINER) * 0.0356 * DSQRT(TH)
                !
            END IF
        END IF
        !
        !       CASE 4 - LOW-K/GEOMEM/MED-K  OR  LOW-K/GEOMEM/HIGH-K  OR
        !                LOW-K (KS)/GEOMEM/LOW-K (SOIL TYPE 1 OR 2)
        !
        IF (LCASE(NPROF) == 4) THEN
            !
            !         PERFECT CONTACT
            !
            IF (LPQ(NPROF) == 1) THEN
                !
                QPHOLE = PHOLE(LINER) * 0.000671 * (TH + DTHICK(NSEGE)) * &
                        RC(LAYSEG(NSEGE, 1))
                !
                QDEFEC = DEFEC(LINER) * 0.00758 * (TH + DTHICK(NSEGE)) * &
                        RC(LAYSEG(NSEGE, 1))
                !
            END IF
            !
            !         EXCELLENT CONTACT
            !
            IF (LPQ(NPROF) == 2) THEN
                !
                IF (PHOLE(LINER) > 0.0) THEN
                    RPHOLE = 0.0520 * ((TH + DTHICK(NSEGE))**0.5) * &
                            (RC(LAYSEG(NSEGE, 1))**(-0.06))
                    HPHOLE = 1 + ((TH + DTHICK(NSEGE)) / (2 * DTHICK(NSEGE) * &
                            DLOG(RPHOLE / 0.0005)))
                    QPHOLE = PHOLE(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                            HPHOLE * (RPHOLE**2)
                END IF
                !
                IF (DEFEC(LINER) > 0.0) THEN
                    RDEFEC = 0.0663 * ((TH + DTHICK(NSEGE))**0.5) * &
                            (RC(LAYSEG(NSEGE, 1))**(-0.06))
                    HDEFEC = 1 + ((TH + DTHICK(NSEGE)) / (2 * DTHICK(NSEGE) * &
                            DLOG(RDEFEC / 0.00564)))
                    QDEFEC = DEFEC(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                            HDEFEC * (RDEFEC**2)
                END IF
                !
            END IF
            !
            !         GOOD CONTACT
            !
            IF (LPQ(NPROF) == 3) THEN
                !
                IF (PHOLE(LINER) > 0.0) THEN
                    RPHOLE = 0.0449 * ((TH + DTHICK(NSEGE))**0.45) * &
                            (RC(LAYSEG(NSEGE, 1))**(-0.13))
                    HPHOLE = 1 + ((TH + DTHICK(NSEGE)) / (2 * DTHICK(NSEGE) * &
                            DLOG(RPHOLE / 0.0005)))
                    QPHOLE = PHOLE(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                            HPHOLE * (RPHOLE**2)
                END IF
                !
                IF (DEFEC(LINER) > 0.0) THEN
                    RDEFEC = 0.0572 * ((TH + DTHICK(NSEGE))**0.45) * &
                            (RC(LAYSEG(NSEGE, 1))**(-0.13))
                    HDEFEC = 1 + ((TH + DTHICK(NSEGE)) / (2 * DTHICK(NSEGE) * &
                            DLOG(RDEFEC / 0.00564)))
                    QDEFEC = DEFEC(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                            HDEFEC * (RDEFEC**2)
                END IF
                !
            END IF
            !
            !         POOR CONTACT
            !
            IF (LPQ(NPROF) == 4) THEN
                !
                IF (PHOLE(LINER) > 0.0) THEN
                    RPHOLE = 0.1053 * ((TH + DTHICK(NSEGE))**0.45) * &
                            (RC(LAYSEG(NSEGE, 1))**(-0.13))
                    HPHOLE = 1 + ((TH + DTHICK(NSEGE)) / (2 * DTHICK(NSEGE) * &
                            DLOG(RPHOLE / 0.0005)))
                    QPHOLE = PHOLE(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                            HPHOLE * (RPHOLE**2)
                END IF
                !
                IF (DEFEC(LINER) > 0.0) THEN
                    RDEFEC = 0.134 * ((TH + DTHICK(NSEGE))**0.45) * &
                            (RC(LAYSEG(NSEGE, 1))**(-0.13))
                    HDEFEC = 1 + ((TH + DTHICK(NSEGE)) / (2 * DTHICK(NSEGE) * &
                            DLOG(RDEFEC / 0.00564)))
                    QDEFEC = DEFEC(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                            HDEFEC * (RDEFEC**2)
                END IF
                !
            END IF
            !
            !         BAD CONTACT
            !
            IF (LPQ(NPROF) == 5) THEN
                !
                QPHOLE = PHOLE(LINER) * 0.000177 * (TH + DTHICK(NSEGE) / &
                        THICK(LINER))
                !
                QDEFEC = DEFEC(LINER) * 0.0356 * DSQRT((TH + DTHICK(NSEGE)))
                !
            END IF
        END IF
        !
        !       CASE 5 - HIGH-K/GEOMEM/GEOTEX/LOW-K  OR  MED-K/GEOMEM/GEOTEX/LOW-K
        !
        IF (LCASE(NPROF) == 5) THEN
            !
            !         INITIAL GUESS OF RADIUS OF WETTED AREA
            !
            RPHOLE = 0.001
            RDEFEC = 0.0113
            !
            !         SOLVE FOR RADIUS OF WETTED AREA THROUGH PINHOLE ITERATIVELY
            !
            IF (PHOLE(LINER) > 0.0) THEN
                IF (((RC(LAYSEG(NSEGE, 1)) / 100.) * 0.000001) < (4.0 * &
                        TRANS(LINER) * TH / 10000. / 39.37)) THEN
                    DO I = 1, 10
                        RSQR = 4.0 * TRANS(LINER) * TH / 10000. / 39.37
                        DENOM = 2. * DLOG(RPHOLE / 0.0005)
                        DENOM = DENOM + ((0.0005 / RPHOLE)**2) - 1.0
                        RSQR = (RSQR * 100.) / RC(LAYSEG(NSEGE, 1)) / DENOM
                        RPHOLE = SQRT(RSQR)
                    end do
                END IF
                !
                !         CALCULATE LEAKAGE THROUGH PINHOLE
                !
                HPHOLE = 1 + (TH / (2. * DTHICK(NSEGE) * DLOG(RPHOLE / 0.0005)))
                QPHOLE = PHOLE(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                        HPHOLE * RPHOLE**2
            END IF

            !
            !         SOLVE FOR RADIUS OF WETTED AREA THROUGH DEFECT ITERATIVELY
            !
            IF (DEFEC(LINER) > 0.0) THEN
                IF (((RC(LAYSEG(NSEGE, 1)) / 100.) * 0.000127) < (4.0 * &
                        TRANS(LINER) * TH / 10000. / 39.37)) THEN
                    DO I = 1, 10
                        RSQR = 4.0 * TRANS(LINER) * TH / 10000. / 39.37
                        DENOM = 2. * DLOG(RDEFEC / 0.00564)
                        DENOM = DENOM + ((0.00564 / RDEFEC)**2) - 1.0
                        RSQR = (RSQR * 100.) / RC(LAYSEG(NSEGE, 1)) / DENOM
                        RDEFEC = SQRT(RSQR)
                    end do
                END IF
                !
                !           CALCULATE LEAKAGE THROUGH DEFECT
                !
                HDEFEC = 1 + (TH / (2. * DTHICK(NSEGE) * DLOG(RDEFEC / 0.00564)))
                QDEFEC = DEFEC(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                        HDEFEC * RDEFEC**2
            END IF

        END IF
        !
        !       CASE 6 - DRAIN/FML/VERT. PERC. LAYER
        !
        IF (LCASE(NPROF) == 6) THEN
            !
            !         INITIAL QUESS OF RADIUS OF WETTED AREA
            !
            RPHOLE = 0.001
            RDEFEC = 0.0113
            !
            !         SOLVE FOR RADIUS OF WETTED AREA THROUGH PINHOLE ITERATIVELY
            !
            IF (PHOLE(LINER) > 0.0) THEN
                IF (((RC(LAYSEG(NSEGE, 1)) / 100.) * 0.000001) < (4.0 * &
                        TRANS(LINER) * (TH + DTHICK(NSEGE)) / 10000. / 39.37)) THEN
                    DO I = 1, 10
                        RSQR = 4.0 * TRANS(LINER) * (TH + DTHICK(NSEGE)) / 10000.&
                                / 39.37
                        DENOM = 2. * DLOG(RPHOLE / 0.0005)
                        DENOM = DENOM + ((0.0005 / RPHOLE)**2) - 1.0
                        RSQR = (RSQR * 100.) / RC(LAYSEG(NSEGE, 1)) / DENOM
                        RPHOLE = SQRT(RSQR)
                    end do
                END IF
                !
                !         CALCULATE LEAKAGE THROUGH PINHOLE
                !
                HPHOLE = 1 + ((TH + DTHICK(NSEGE)) / (2 * DTHICK(NSEGE) * &
                        DLOG(RPHOLE / 0.0005)))
                QPHOLE = PHOLE(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                        HPHOLE * RPHOLE**2
            END IF

            !
            !         SOLVE FOR RADIUS OF WETTED AREA THROUGH DEFECT ITERATIVELY
            !
            IF (DEFEC(LINER) > 0.0) THEN
                IF (((RC(LAYSEG(NSEGE, 1)) / 100.) * 0.000127) < (4.0 * &
                        TRANS(LINER) * (TH + DTHICK(NSEGE)) / 10000. / 39.37)) THEN
                    DO I = 1, 10
                        RSQR = 4.0 * TRANS(LINER) * (TH + DTHICK(NSEGE)) / 10000.&
                                / 39.37
                        DENOM = 2. * DLOG(RDEFEC / 0.00564)
                        DENOM = DENOM + ((0.00565 / RDEFEC)**2) - 1.0
                        RSQR = (RSQR * 100.) / RC(LAYSEG(NSEGE, 1)) / DENOM
                        RDEFEC = SQRT(RSQR)
                    end do
                END IF
                !
                !           CALCULATE LEAKAGE THROUGH DEFECT
                !
                HDEFEC = 1 + ((TH + DTHICK(NSEGE)) / (2 * DTHICK(NSEGE) * &
                        DLOG(RDEFEC / 0.00564)))
                QDEFEC = DEFEC(LINER) * 26.4 * RC(LAYSEG(NSEGE, 1)) * &
                        HDEFEC * RDEFEC**2
            END IF

        END IF
    END IF
    !
    !     SUM OF VAPOR, PINHOLE, AND DEFECT PERCOLATION
    !
    QPERC = QVAPOR + QPHOLE + QDEFEC
    !
    IF ((LCASE(NPROF) == 3 .AND. LAYER(LP(NPROF)) == 3)&
            .OR. LCASE(NPROF) == 4) THEN
        IF (QPERC > (34016.D0 * RC(LAYSEG(NSEGE, 1)) * (TH + &
                DTHICK(NSEGE)) / DTHICK(NSEGE)))&
                QPERC = 34016.D0 * RC(LAYSEG(NSEGE, 1)) * (TH + &
                        DTHICK(NSEGE)) / DTHICK(NSEGE)
    ELSE
        IF (QPERC > (34016.D0 * RC(LAYSEG(NSEGE, 1))))&
                QPERC = 34016.D0 * RC(LAYSEG(NSEGE, 1))
    END IF
    !
    RETURN
END
!
!
!
!      **************************  RECIRC  **************************
!
!
SUBROUTINE RECIRC (IMO)
    !
    DOUBLE PRECISION DSWUL, DTHICK, DRCUL, DBUBUL, &
            DWPUL, DLAMUL, DRSUL, DUL, DFCUL, DSUBIN, DCHG, DRCRS
    !
    DOUBLE PRECISION PRC1, PRC2, PRC3, PRC4, PRC5, PRC6, DRN1, DRN2, &
            DRN3, DRN4, DRN5, HED1, HED2, HED3, HED4, HED5, &
            RCR1, RCR2, RCR3, RCR4, RCR5, RCRI
    !
    COMMON /BLK3/ PORO(20), FC(20), WP(20), RC(20), SW(20), &
            RS(20), XLAMBD(20), BUB(20), THICK(20), SLOPE(20), &
            XLENG(20), CON(20), SUBIN(20), RECIR(20), PHOLE(20), &
            DEFEC(20), TRANS(20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK4/ LAYER(21), LAYR(20), IPQ(20), ISOIL(20), LAY, &
            LAYSEG(67, 2), LSEG(67), LSEGS(20, 2), LRIN(20), LSIN(20)
    COMMON /BLK7/ STHICK(67), UL(67), FCUL(67), WPUL(67), SWUL(67), &
            RCUL(67), RSUL(67), XLAMBU(67), CONUL(67), BUBUL(67), &
            SUBINS(67), SWULI(67)
    COMMON /BLK12/ PRC1M (12), PRC2M (12), PRC3M (12), &
            PRC4M (12), PRC5M (12), PRC6M (12), DRN1M (12), &
            DRN2M (12), DRN3M (12), DRN4M (12), DRN5M (12), &
            PREM (12), RUNM (12), ETM (12), HED1M (12), &
            HED2M (12), HED3M (12), HED4M (12), HED5M (12), &
            RCRIM (20, 12), SUBINM (20, 12), RCR1M (12), &
            RCR2M (12), RCR3M (12), RCR4M (12), RCR5M (12)
    COMMON /BLK13/ PRC1A, PRC2A, PRC3A, PRC4A, PRC5A, PRC6A, &
            DRN1A, DRN2A, DRN3A, DRN4A, DRN5A, BAL, PREA, RUNA, ETA, &
            STOR, RCR1A, RCR2A, RCR3A, RCR4A, RCR5A, RCRIA(20), HED1A, &
            HED2A, HED3A, HED4A, HED5A, OSWULE, PSWULE, SUBINA(20)
    COMMON /BLK14/ PPRC1, PPRC2, PPRC3, PPRC4, PPRC5, PPRC6, &
            PDRN1, PDRN2, PDRN3, PDRN4, PDRN5, PPRE, PRUN, PSNO, PSW, DSW, &
            PHED1, PHED2, PHED3, PHED4, PHED5, PRCR1, PRCR2, PRCR3, &
            PRCR4, PRCR5, PRCRI(20)
    COMMON /BLK15/ PRC1, PRC2, PRC3, PRC4, PRC5, PRC6, DRN1, DRN2, &
            DRN3, DRN4, DRN5, HED1, HED2, HED3, HED4, HED5, &
            RCR1, RCR2, RCR3, RCR4, RCR5, RCRI(20)
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK31/ DRCUL (67), DBUBUL (67), DRSUL (67), DLAMUL (67), &
            DUL(67), DFCUL(67), DWPUL(67), DSWUL(67), DTHICK(67), &
            DSUBIN(67), DCHG(67), DRCRS(67)
    !
    DO K = 1, LAY
        RCRI(K) = 0.0
    end do
    IF (LD(1) /= 0) THEN
        IF(RECIR(LD(1)) > 0.0) THEN
            RCRI(LAYR(LD(1))) = RCRI(LAYR(LD(1))) + RCR1
            RCRIM(LAYR(LD(1)), IMO) = RCRIM(LAYR(LD(1)), IMO) + RCR1
            RCRIA(LAYR(LD(1))) = RCRIA(LAYR(LD(1))) + RCR1
        END IF
    END IF
    IF (LD(2) /= 0) THEN
        IF(RECIR(LD(2)) > 0.0) THEN
            RCRI(LAYR(LD(2))) = RCRI(LAYR(LD(2))) + RCR2
            RCRIM(LAYR(LD(2)), IMO) = RCRIM(LAYR(LD(2)), IMO) + RCR2
            RCRIA(LAYR(LD(2))) = RCRIA(LAYR(LD(2))) + RCR2
        END IF
    END IF
    IF (LD(3) /= 0) THEN
        IF(RECIR(LD(3)) > 0.0) THEN
            RCRI(LAYR(LD(3))) = RCRI(LAYR(LD(3))) + RCR3
            RCRIM(LAYR(LD(3)), IMO) = RCRIM(LAYR(LD(3)), IMO) + RCR3
            RCRIA(LAYR(LD(3))) = RCRIA(LAYR(LD(3))) + RCR3
        END IF
    END IF
    IF (LD(4) /= 0) THEN
        IF(RECIR(LD(4)) > 0.0) THEN
            RCRI(LAYR(LD(4))) = RCRI(LAYR(LD(4))) + RCR4
            RCRIM(LAYR(LD(4)), IMO) = RCRIM(LAYR(LD(4)), IMO) + RCR4
            RCRIA(LAYR(LD(4))) = RCRIA(LAYR(LD(4))) + RCR4
        END IF
    END IF
    IF (LD(5) /= 0) THEN
        IF(RECIR(LD(5)) > 0.0) THEN
            RCRI(LAYR(LD(5))) = RCRI(LAYR(LD(5))) + RCR5
            RCRIM(LAYR(LD(5)), IMO) = RCRIM(LAYR(LD(5)), IMO) + RCR5
            RCRIA(LAYR(LD(5))) = RCRIA(LAYR(LD(5))) + RCR5
        END IF
    END IF
    DO J = 1, LAY
        DRCRS(J) = 0.0D0
    end do
    DO K = 1, LAY
        IF (RCRI(K) > PRCRI(K)) PRCRI(K) = RCRI(K)
        IF (LRIN(K) == 1) THEN
            DO J = LSEGS(K, 1), LSEGS(K, 2)
                IF (LSEGS(K, 1) > 7) THEN
                    DRCRS(J) = RCRI(K) * DTHICK(J) / THICK(K)
                ELSE IF (K == 1) THEN
                    RCRF = 0.0
                    DO L = LSEGS(K, 1), LSEGS(K, 2)
                        IF (L < LSEGS(K, 2)) THEN
                            DRCRS(L) = RCRI(K) * DTHICK(L) / THICK(K)
                            RCRF = RCRF + (DTHICK(L) / THICK(K))
                        ELSE
                            DRCRS(L) = RCRI(K) * (1.0 - RCRF)
                        END IF
                    end do
                ELSE IF (LSEGS(K, 1) == LSEGS(K, 2)) THEN
                    DRCRS(J) = RCRI(K)
                ELSE
                    TKM1 = 0.0
                    DO M = 1, K - 1
                        TKM1 = TKM1 + THICK(M)
                    end do
                    TJM1 = 0.0
                    IF (LSEGS(K, 1) > 1) THEN
                        DO M = 1, LSEGS(K, 1) - 1
                            TJM1 = TJM1 + DTHICK(M)
                        end do
                    END IF
                    DSEG = TKM1 - TJM1
                    RCRF = (DTHICK(LSEGS(K, 1)) - DSEG) / THICK(K)
                    DRCRS(LSEGS(K, 1)) = RCRI(K) * RCRF
                    DO L = LSEGS(K, 1) + 1, LSEGS(K, 2)
                        IF (L < LSEGS(K, 2)) THEN
                            DRCRS(L) = RCRI(K) * DTHICK(L) / THICK(K)
                            RCRF = RCRF + (DTHICK(L) / THICK(K))
                        ELSE
                            DRCRS(L) = RCRI(K) * (1.0 - RCRF)
                        END IF
                    end do
                END IF
            end do
        END IF
    end do
    RETURN
END
!
!
!
!      ************************* OUTDAT *************************
!
!
!     SUBROUTINE OUTDAT PRINTS THE SOIL
!     CHARACTERISTICS AND DESIGN INFORMATION AND
!     CLIMATOLOGICAL INPUT DATA
!
SUBROUTINE OUTDAT (SWULL)
    !
    CHARACTER*2 DUMMY1, DUMMY2, DUMMY3, DUMMY4
    CHARACTER*40 CITY4, CITY7, CITY13, CITY11
    CHARACTER*60 F4, F7, F13, F11, F10, F8, TITLE
    INTEGER(kind=2) NH, NMIN, NS, NHD, ND2, NM, NY
    DOUBLE PRECISION  DTHICK, DRCUL, DBUBUL, DWPUL, &
            DLAMUL, DRSUL, DUL, DFCUL, DSUBIN, DCHG, DRCRS, DSWUL
    !
    COMMON /BLK0/ F4, F7, F13, F11, F10, F8, TITLE
    COMMON /BLK1/ IT4, IT7, IT13, IU4, IU7, IU13, IU11, IU10, IU8, &
            LMYR, IOD, IOM, IOA, IOS
    COMMON /BLK2/ CITY4, CITY7, CITY13, CITY11
    COMMON /BLK3/ PORO (20), FC (20), WP (20), RC (20), SW (20), &
            RS (20), XLAMBD (20), BUB (20), THICK (20), SLOPE (20), &
            XLENG (20), CON (20), SUBIN (20), RECIR (20), PHOLE (20), &
            DEFEC (20), TRANS (20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN(20), LSIN(20)
    COMMON /BLK5/ AREA, FRUNOF, CN2, OCN2, SSLOPE, SLENG, SMX
    COMMON /BLK6/ ULAI, WIND, RH (366), OSNO, ULAT
    COMMON /BLK7/ STHICK(67), UL(67), FCUL(67), WPUL(67), SWUL(67), &
            RCUL(67), RSUL(67), XLAMBU(67), CONUL(67), BUBUL(67), &
            SUBINS(67), SWULI(67)
    COMMON /BLK11/ ETO, XLAI, STAGE1, CONA, RAIN, RUN, ABST, EAJ, TS2
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK17/ NSEG1, NSEG2, NSEG3, NSEG4, NSEG5, NSEG6, NSEG
    COMMON /BLK18/ RM (12), TM (12), RLAT
    COMMON /BLK19/ IPL, IHV, IPRE, IRUN, ITSOIL, IVEG
    COMMON /BLK28/ WE, XNEGHS, XLIQW, TINDEX, STORGE, EXLAG(4), TWE
    COMMON /BLK31/ DRCUL (67), DBUBUL (67), DRSUL (67), DLAMUL (67), &
            DUL(67), DFCUL(67), DWPUL(67), DSWUL(67), DTHICK(67), &
            DSUBIN(67), DCHG(67), DRCRS(67)
    DIMENSION SWULL (20)
    !
    WRITE (8, 6000) CHAR(12)
    6000 FORMAT(1X, A1/1X, 78('*')/&
            1X, 78('*')/1X, '**', 74X, '**'/1X, '**', 74X, '**'/1X, '**', 14X, &
            'HYDROLOGIC EVALUATION OF LANDFILL PERFORMANCE', 15X, '**'/1X, '**', &
            16X, 'HELP MODEL VERSION 3.07  (1 NOVEMBER 1997)', 16X, '**'/1X, &
            '**', 18X, 'DEVELOPED BY ENVIRONMENTAL LABORATORY ', 18X, '**'/1X, &
            '**', 20X, 'USAE WATERWAYS EXPERIMENT STATION', 21X, '**'/1X, '**', &
            13X, 'FOR USEPA RISK REDUCTION ENGINEERING LABORATORY', 14X, '**'/&
            1X, '**', 74X, '**'/1X, '**', 74X, '**'/1X, 78('*')/&
            1X, 78('*'))
    WRITE (8, 6005) F4, F7, F13, F11, F10, F8
    6005 FORMAT(///1X, 'PRECIPITATION DATA FILE:    ', A50/&
            1X, 'TEMPERATURE DATA FILE:      ', A50/&
            1X, 'SOLAR RADIATION DATA FILE:  ', A50/&
            1X, 'EVAPOTRANSPIRATION DATA:    ', A50/&
            1X, 'SOIL AND DESIGN DATA FILE:  ', A50/&
            1X, 'OUTPUT DATA FILE:           ', A50///)
    !
    CALL GETDAT(NY, NM, ND2)
    CALL GETTIM(NH, NMIN, NS, NHD)
    WRITE(8, 6010) NH, NMIN, NM, ND2, NY
    6010 FORMAT (1X, 'TIME:', I4, ':', I2, '     DATE:', I4, '/', I2, '/', I4///)
    WRITE (8, 6015) TITLE
    6015 FORMAT(1X/1X, 78('*')//6X, 'TITLE:  ', A60//1X, 78('*')///)
    !
    !
    !   INITIALIZES SOIL WATER WHEN USER DOES NOT (IPRE = 0)
    !   IPRE = 0 WHEN PROGRAM INITIALIZES SOIL WATER
    !
    IF (IPRE == 0 .OR. IPRE == 2) THEN
        !
        WRITE (8, 6018)
        6018   FORMAT (6X, &
                'NOTE:  INITIAL MOISTURE CONTENT OF THE LAYERS AND SNOW WATER', &
                ' WERE'/15X, 'COMPUTED AS NEARLY STEADY-STATE VALUES ', &
                'BY THE PROGRAM.'//)
        DO K = 1, LAY
            IF (LAYER(K) /= 4) SW(K) = SWULL(K) / THICK(K)
        end do
    ELSE
        WRITE (8, 6019)
        6019   FORMAT (6X, &
                'NOTE:  INITIAL MOISTURE CONTENT OF THE LAYERS AND SNOW WATER'/&
                15X, 'WERE SPECIFIED BY THE USER.'//)
    END IF
    !
    !    CALCULATE MOISTURE CONTENTS OF EVAPORATIVE ZONE
    !
    ULE = 0.0
    WPULE = 0.0
    SWULE = 0.0
    DO J = 1, 7
        ULE = ULE + DUL(J)
        WPULE = WPULE + DWPUL(J)
        SWULE = SWULE + DSWUL(J)
    end do
    !
    !     PRINTS SOIL CHARACTERISTICS AND DESIGN INFORMATION
    !     FOR EACH LAYER.
    !
    SWT = 0.
    DO I = 1, LAY
        WRITE (8, 6080) I
        6080    FORMAT(1X/36X, 'LAYER', I3/36X, 8('-')/)
        IF (LAYER (I) == 1) WRITE (8, 6081)
        6081    FORMAT(22X, 'TYPE 1 - VERTICAL PERCOLATION LAYER')
        IF (LAYER (I) == 2) WRITE (8, 6082)
        6082    FORMAT(24X, 'TYPE 2 - LATERAL DRAINAGE LAYER')
        IF (LAYER (I) == 3) WRITE (8, 6083)
        6083    FORMAT(26X, 'TYPE 3 - BARRIER SOIL LINER')
        IF (LAYER (I) == 4) WRITE (8, 6084)
        6084    FORMAT(24X, 'TYPE 4 - FLEXIBLE MEMBRANE LINER')
        WRITE(8, 6090) ISOIL(I)
        6090    FORMAT(26X, 'MATERIAL TEXTURE NUMBER', I4)
        IF (IU10 == 1) THEN
            WRITE (8, 6131) THICK (I)
        ELSE
            WRITE (8, 6132) THICK (I) * 2.54
        END IF
        WRITE (8, 6130) PORO (I), FC (I), WP (I), SW (I), RC (I)
        6130    FORMAT(12X, 'POROSITY', 18X, '  =', F12.4, ' VOL/VOL'/&
                12X, 'FIELD CAPACITY              =', F12.4, ' VOL/VOL'/&
                12X, 'WILTING POINT               =', F12.4, ' VOL/VOL'/&
                12X, 'INITIAL SOIL WATER CONTENT  =', F12.4, ' VOL/VOL'/&
                12X, 'EFFECTIVE SAT. HYD. COND.   =', G20.12, ' CM/SEC')
        6131    FORMAT(12X, 'THICKNESS', 17X, '  =', F10.2, '   INCHES')
        6132    FORMAT(12X, 'THICKNESS', 17X, '  =', F10.2, '   CM')
        K = I + 1
        IF (LAYER (I) == 2 .AND. LAYER (K) /= 2) THEN
            WRITE (8, 6140) SLOPE (I)
            6140      FORMAT(12X, 'SLOPE', 21X, '  =', F10.2, '   PERCENT')
            IF(IU10==1) THEN
                WRITE(8, 6141) XLENG (I)
            ELSE
                WRITE(8, 6142) XLENG (I) / 3.281
            END IF
            6141      FORMAT(12X, 'DRAINAGE LENGTH', 11X, '  =', F9.1, '    FEET')
            6142      FORMAT(12X, 'DRAINAGE LENGTH', 11X, '  =', F9.1, '    METERS')
        END IF
        IF (LAYER (I) == 4) THEN
            IF(IU10 == 1) THEN
                WRITE (8, 6151) PHOLE(I), DEFEC(I)
                6151        FORMAT(12X, 'FML PINHOLE DENSITY', 7X, '  =', F10.2, &
                        '   HOLES/ACRE'/12X, 'FML INSTALLATION DEFECTS    =', &
                        F10.2, '   HOLES/ACRE')
            ELSE
                WRITE (8, 6152) PHOLE(I) * 2.471, DEFEC(I) * 2.471
                6152        FORMAT(12X, 'FML PINHOLE DENSITY         =', F10.2, &
                        '   HOLES/HECTARE'/12X, 'FML INSTALLATION DEFECTS    =', &
                        F10.2, '   HOLES/HECTARE')
            END IF
            IF (IPQ(I) == 1) WRITE (8, 6153)
            IF (IPQ(I) == 2) WRITE (8, 6154)
            IF (IPQ(I) == 3) WRITE (8, 6155)
            IF (IPQ(I) == 4) WRITE (8, 6156)
            IF (IPQ(I) == 5) WRITE (8, 6157)
            IF (IPQ(I) == 6) WRITE (8, 6158) TRANS(I)
            6153     FORMAT(12X, 'FML PLACEMENT QUALITY       =  1 - PERFECT  ')
            6154     FORMAT(12X, 'FML PLACEMENT QUALITY       =  2 - EXCELLENT')
            6155     FORMAT(12X, 'FML PLACEMENT QUALITY       =  3 - GOOD     ')
            6156     FORMAT(12X, 'FML PLACEMENT QUALITY       =  4 - POOR     ')
            6157     FORMAT(12X, 'FML PLACEMENT QUALITY       =  5 - BAD      ')
            6158     FORMAT(12X, 'FML PLACEMENT QUALITY       =  6 - W/ GEOTEXTILE'/&
                    12X, 'GEOTEXTILE TRANSMISSIVITY   =', F14.6, ' CM*CM/SEC')
        END IF
        IF (SUBIN(I)>0.0) THEN
            IF(IU10 == 1) THEN
                WRITE (8, 6161) SUBIN(I) * 365.0
                6161       FORMAT(12X, 'SUBSURFACE INFLOW', 11X, '=', F10.2, '   INCHES/YR')
            ELSE
                WRITE (8, 6162) SUBIN(I) * 25.4 * 365.0
                6162       FORMAT(12X, 'SUBSURFACE INFLOW', 11X, '=', F9.1, '    MM/YR')
            END IF
        END IF
        DO L = 1, 5
            !    COMPILER CORRECTIONS BELOW
            IF (LD(L) > 0 .and. LAYR(LD(L)) == I .AND. RECIR(LD(L))&
                    > 0.0) WRITE (8, 6159) RECIR(LD(L)), LD(L)
            !     COMPILER CORRECTIONS ABOVE
            6159      FORMAT(10X, &
                    'NOTE:  ', F6.2, ' PERCENT OF THE DRAINAGE COLLECTED FROM ', &
                    'LAYER #', I2/  19X, 'IS RECIRCULATED INTO THIS LAYER.')
        end do
        IF (LAYER (I) == 2 .AND. LAYER (K) /= 2) THEN
            IF (LAYR(I) > 0 .AND. RECIR(I) > 0.0) WRITE(8, 6143)&
                    RECIR(I), LAYR(I)
            6143      FORMAT(10X, &
                    'NOTE:  ', F6.2, ' PERCENT OF THE DRAINAGE COLLECTED FROM THIS'/&
                    19X, 'LAYER IS RECIRCULATED INTO LAYER #', I3, '.')
        END IF
        IF (I == 1 .AND. ISOIL (1) <= 34 .AND. ISOIL (1) >= 1&
                .AND. ULAI > 0.0) WRITE(8, 6133) CORECT
        6133    FORMAT(10X, 'NOTE:  SATURATED ', &
                'HYDRAULIC CONDUCTIVITY IS MULTIPLIED BY', F6.2/ 19X, &
                'FOR ROOT CHANNELS IN TOP HALF OF EVAPORATIVE ZONE.')
        WRITE (8, 6160)
        6160   FORMAT(//)
        SWT = SWT + SW (I) * THICK (I)
    end do
    !
    !     PRINTS OTHER SITE DESIGN DATA AND SIMULATION DATA.
    !
    !     SNO = (WE + XLIQW + STORGE + TEX) /25.4
    TWAT = OSNO + SWT
    WRITE (8, 6165)
    6165 FORMAT(1X//20X, 'GENERAL DESIGN AND EVAPORATIVE ZONE DATA'/&
            20X, 40('-')/)
    IF (IRUN == 1) THEN
        WRITE (8, 6170) CN2
    ELSE IF (IRUN == 2) THEN
        IF(IU10 == 1) THEN
            WRITE (8, 6171) OCN2, SSLOPE, SLENG, CN2
        ELSE
            WRITE (8, 6172) OCN2, SSLOPE, SLENG / 3.281, CN2
        END IF
    ELSE IF (IRUN == 3) THEN
        IF(IU10 == 1) THEN
            IF(IVEG == 1) WRITE (8, 6173) ITSOIL, SSLOPE, SLENG, CN2
            IF(IVEG == 2) WRITE (8, 6174) ITSOIL, SSLOPE, SLENG, CN2
            IF(IVEG == 3) WRITE (8, 6175) ITSOIL, SSLOPE, SLENG, CN2
            IF(IVEG == 4) WRITE (8, 6176) ITSOIL, SSLOPE, SLENG, CN2
            IF(IVEG == 5) WRITE (8, 6177) ITSOIL, SSLOPE, SLENG, CN2
        ELSE
            IF(IVEG==1) WRITE(8, 6183) ITSOIL, SSLOPE, SLENG / 3.281, CN2
            IF(IVEG==2) WRITE(8, 6184) ITSOIL, SSLOPE, SLENG / 3.281, CN2
            IF(IVEG==3) WRITE(8, 6185) ITSOIL, SSLOPE, SLENG / 3.281, CN2
            IF(IVEG==4) WRITE(8, 6186) ITSOIL, SSLOPE, SLENG / 3.281, CN2
            IF(IVEG==5) WRITE(8, 6187) ITSOIL, SSLOPE, SLENG / 3.281, CN2
        END IF
    END IF
    6170 FORMAT (10X, 'NOTE:  SCS RUNOFF CURVE NUMBER WAS USER-SPECIFIED.'/&
            /9X, 'SCS RUNOFF CURVE NUMBER', 11X, '  =', F10.2)
    6171 FORMAT (10X, &
            'NOTE:  SCS RUNOFF CURVE NUMBER WAS COMPUTED FROM A USER-'/&
            19X, 'SPECIFIED CURVE NUMBER OF', F5.1, ', A SURFACE SLOPE'/&
            19X, 'OF', F4.0, '% AND A SLOPE LENGTH OF', F6.0, ' FEET.'/&
            /9X, 'SCS RUNOFF CURVE NUMBER', 11X, '  =', F10.2)
    6172 FORMAT (10X, &
            'NOTE:  SCS RUNOFF CURVE NUMBER WAS COMPUTED FROM A USER-'/&
            19X, 'SPECIFIED CURVE NUMBER OF', F5.1, ', A SURFACE SLOPE'/&
            19X, 'OF', F4.0, '% AND A SLOPE LENGTH OF', F6.0, ' METERS.'/&
            /9X, 'SCS RUNOFF CURVE NUMBER', 11X, '  =', F10.2)
    6173 FORMAT (10X, &
            'NOTE:  SCS RUNOFF CURVE NUMBER WAS COMPUTED FROM DEFAULT'/&
            19X, 'SOIL DATA BASE USING SOIL TEXTURE #', I2, ' WITH BARE'/&
            19X, 'GROUND CONDITIONS, A SURFACE SLOPE OF', F4.0, '% AND'/&
            19X, 'A SLOPE LENGTH OF', F6.0, ' FEET.'/&
            /9X, 'SCS RUNOFF CURVE NUMBER', 11X, '  =', F10.2)
    6174 FORMAT (10X, &
            'NOTE:  SCS RUNOFF CURVE NUMBER WAS COMPUTED FROM DEFAULT'/&
            19X, 'SOIL DATA BASE USING SOIL TEXTURE #', I2, ' WITH A'/&
            19X, 'POOR STAND OF GRASS, A SURFACE SLOPE OF', F4.0, '%'/&
            19X, 'AND A SLOPE LENGTH OF', F6.0, ' FEET.'/&
            /9X, 'SCS RUNOFF CURVE NUMBER', 11X, '  =', F10.2)
    6175 FORMAT (10X, &
            'NOTE:  SCS RUNOFF CURVE NUMBER WAS COMPUTED FROM DEFAULT'/&
            19X, 'SOIL DATA BASE USING SOIL TEXTURE #', I2, ' WITH A'/&
            19X, 'FAIR STAND OF GRASS, A SURFACE SLOPE OF', F4.0, '%'/&
            19X, 'AND A SLOPE LENGTH OF', F6.0, ' FEET.'/&
            /9X, 'SCS RUNOFF CURVE NUMBER', 11X, '  =', F10.2)
    6176 FORMAT (10X, &
            'NOTE:  SCS RUNOFF CURVE NUMBER WAS COMPUTED FROM DEFAULT'/&
            19X, 'SOIL DATA BASE USING SOIL TEXTURE #', I2, ' WITH A'/&
            19X, 'GOOD STAND OF GRASS, A SURFACE SLOPE OF', F4.0, '%'/&
            19X, 'AND A SLOPE LENGTH OF', F6.0, ' FEET.'/&
            /9X, 'SCS RUNOFF CURVE NUMBER', 11X, '  =', F10.2)
    6177 FORMAT (10X, &
            'NOTE:  SCS RUNOFF CURVE NUMBER WAS COMPUTED FROM DEFAULT'/&
            19X, 'SOIL DATA BASE USING SOIL TEXTURE #', I2, ' WITH AN'/&
            19X, 'EXCELLENT STAND OF GRASS, A SURFACE SLOPE OF', F4.0, '%'/&
            19X, 'AND A SLOPE LENGTH OF', F6.0, ' FEET.'/&
            /9X, 'SCS RUNOFF CURVE NUMBER', 11X, '  =', F10.2)
    6183 FORMAT (10X, &
            'NOTE:  SCS RUNOFF CURVE NUMBER WAS COMPUTED FROM DEFAULT'/&
            19X, 'SOIL DATA BASE USING SOIL TEXTURE #', I2, ' WITH BARE'/&
            19X, 'GROUND CONDITIONS, A SURFACE SLOPE OF', F4.0, '% AND'/&
            19X, 'A SLOPE LENGTH OF', F6.0, ' METERS.'/&
            /9X, 'SCS RUNOFF CURVE NUMBER', 11X, '  =', F10.2)
    6184 FORMAT (10X, &
            'NOTE:  SCS RUNOFF CURVE NUMBER WAS COMPUTED FROM DEFAULT'/&
            19X, 'SOIL DATA BASE USING SOIL TEXTURE #', I2, ' WITH A'/&
            19X, 'POOR STAND OF GRASS, A SURFACE SLOPE OF', F4.0, '%'/&
            19X, 'AND A SLOPE LENGTH OF', F6.0, ' METERS.'/&
            /9X, 'SCS RUNOFF CURVE NUMBER', 11X, '  =', F10.2)
    6185 FORMAT (10X, &
            'NOTE:  SCS RUNOFF CURVE NUMBER WAS COMPUTED FROM DEFAULT'/&
            19X, 'SOIL DATA BASE USING SOIL TEXTURE #', I2, ' WITH A'/&
            19X, 'FAIR STAND OF GRASS, A SURFACE SLOPE OF', F4.0, '%'/&
            19X, 'AND A SLOPE LENGTH OF', F6.0, ' METERS.'/&
            /9X, 'SCS RUNOFF CURVE NUMBER', 11X, '  =', F10.2)
    6186 FORMAT (10X, &
            'NOTE:  SCS RUNOFF CURVE NUMBER WAS COMPUTED FROM DEFAULT'/&
            19X, 'SOIL DATA BASE USING SOIL TEXTURE #', I2, ' WITH A'/&
            19X, 'GOOD STAND OF GRASS, A SURFACE SLOPE OF', F4.0, '%'/&
            19X, 'AND A SLOPE LENGTH OF', F6.0, ' METERS.'/&
            /9X, 'SCS RUNOFF CURVE NUMBER', 11X, '  =', F10.2)
    6187 FORMAT (10X, &
            'NOTE:  SCS RUNOFF CURVE NUMBER WAS COMPUTED FROM DEFAULT'/&
            19X, 'SOIL DATA BASE USING SOIL TEXTURE #', I2, ' WITH AN'/&
            19X, 'EXCELLENT STAND OF GRASS, A SURFACE SLOPE OF', F4.0, '%'/&
            19X, 'AND A SLOPE LENGTH OF', F6.0, ' METERS.'/&
            /9X, 'SCS RUNOFF CURVE NUMBER', 11X, '  =', F10.2)
    IF (IU10 == 1) THEN
        WRITE (8, 6181) FRUNOF * 100.0, AREA, EDEPTH, SWULE, ULE, &
                WPULE, OSNO, SWT, TWAT, SUBINF * 365.0
    ELSE
        WRITE (8, 6182) FRUNOF * 100.0, AREA, EDEPTH * 2.54, &
                SWULE * 2.54, ULE * 2.54, WPULE * 2.54, &
                OSNO * 2.54, SWT * 2.54, TWAT * 2.54, SUBINF * 25.4 * 365.0
    END IF
    6181 FORMAT(9X, 'FRACTION OF AREA ALLOWING RUNOFF    =', F9.1, &
            '    PERCENT'/9X, &
            'AREA PROJECTED ON HORIZONTAL PLANE  =', F11.3, '  ACRES'/9X, &
            'EVAPORATIVE ZONE DEPTH', 14X, '=', F9.1, '    INCHES'/9X, &
            'INITIAL WATER IN EVAPORATIVE ZONE   =', F11.3, '  INCHES'/9X, &
            'UPPER LIMIT OF EVAPORATIVE STORAGE  =', F11.3, '  INCHES'/9X, &
            'LOWER LIMIT OF EVAPORATIVE STORAGE  =', F11.3, '  INCHES'/9X, &
            'INITIAL SNOW WATER                  =', F11.3, '  INCHES'/9X, &
            'INITIAL WATER IN LAYER MATERIALS    =', F11.3, '  INCHES'/9X, &
            'TOTAL INITIAL WATER                 =', F11.3, '  INCHES'/9X, &
            'TOTAL SUBSURFACE INFLOW', 13X, '=', F10.2, '   INCHES/YEAR'//)
    6182 FORMAT(9X, 'FRACTION OF AREA ALLOWING RUNOFF    =', F9.1, &
            '    PERCENT'/9X, &
            'AREA PROJECTED ON HORIZONTAL PLANE  =', F12.4, ' HECTARES'/&
            9X, 'EVAPORATIVE ZONE DEPTH              =', F9.1, '    CM'/&
            9X, 'INITIAL WATER IN EVAPORATIVE ZONE   =', F11.3, '  CM'/&
            9X, 'UPPER LIMIT OF EVAPORATIVE STORAGE  =', F11.3, '  CM'/&
            9X, 'LOWER LIMIT OF EVAPORATIVE STORAGE  =', F11.3, '  CM'/&
            9X, 'INITIAL SNOW WATER                  =', F11.3, '  CM'/&
            9X, 'INITIAL WATER IN LAYER MATERIALS    =', F11.3, '  CM'/&
            9X, 'TOTAL INITIAL WATER                 =', F11.3, '  CM'/9X, &
            'TOTAL SUBSURFACE INFLOW             =', F10.2, '   MM/YR'//)
    !
    IF (IU10 /= IU8) THEN
        IF (IU8 == 1) AREA = AREA * 2.471
        IF (IU8 /= 1) AREA = AREA / 2.471
    END IF
    !
    !    PRINTS WEATHER DATA
    !
    WRITE (8, 6200) CITY11
    6200 FORMAT(//21X, 'EVAPOTRANSPIRATION AND WEATHER DATA '/21X, 35('-')//&
            10X, 'NOTE:  EVAPOTRANSPIRATION DATA WAS OBTAINED FROM'/19X, A40/)
    WRITE (8, 6263) ULAT
    6263 FORMAT(14X, 'STATION LATITUDE', 23X, '= ', F6.2, ' DEGREES')
    WRITE (8, 6210) ULAI, IPL, IHV
    6210 FORMAT(14X, 'MAXIMUM LEAF AREA INDEX                = ', F6.2/&
            14X, 'START OF GROWING SEASON (JULIAN DATE)  = ', I6/&
            14X, 'END OF GROWING SEASON (JULIAN DATE)    = ', I6)
    IF (IU11 == 1) THEN
        WRITE (8, 6221) EDEPTH, WIND
    ELSE
        WRITE (8, 6222) EDEPTH * 2.54, WIND * 1.609
    END IF
    6221 FORMAT(14X, 'EVAPORATIVE ZONE DEPTH   ', 14X, '=', F6.1, '  INCHES'/&
            14X, 'AVERAGE ANNUAL WIND SPEED', 14X, '=', F7.2, ' MPH')
    6222 FORMAT(14X, 'EVAPORATIVE ZONE DEPTH   ', 14X, '=', F6.1, '  CM'/&
            14X, 'AVERAGE ANNUAL WIND SPEED', 14X, '=', F7.2, ' KPH')
    WRITE (8, 6230) RH(90), RH(180), RH(270), RH(360)
    6230 FORMAT(14X, 'AVERAGE 1ST QUARTER RELATIVE HUMIDITY  =', F7.2, ' %'/&
            14X, 'AVERAGE 2ND QUARTER RELATIVE HUMIDITY  =', F7.2, ' %'/&
            14X, 'AVERAGE 3RD QUARTER RELATIVE HUMIDITY  =', F7.2, ' %'/&
            14X, 'AVERAGE 4TH QUARTER RELATIVE HUMIDITY  =', F7.2, ' %')
    IF (IT4 /= 2) THEN
        WRITE (8, 6240) CITY4
    ELSE
        WRITE (8, 6239)
    END IF
    IF (IT4 == 1) THEN
        WRITE (8, 6241)
    ELSE IF (IT4 == 2) THEN
        WRITE (8, 6242) CITY4
        IF (IU4 == 1) THEN
            WRITE (8, 6231) (RM(I), I = 1, 12)
        ELSE
            WRITE (8, 6232) (RM(I), I = 1, 12)
        END IF
    ELSE IF (IT4 == 3) THEN
        WRITE (8, 6243)
    ELSE IF (IT4 == 4) THEN
        WRITE (8, 6244)
    ELSE IF (IT4 == 5) THEN
        WRITE (8, 6245)
    ELSE IF (IT4 == 6) THEN
        WRITE (8, 6246)
    ELSE IF (IT4 == 7) THEN
        WRITE (8, 6247)
    ELSE IF (IT4 == 8) THEN
        WRITE (8, 6248)
    ELSE IF (IT4 == 9) THEN
        WRITE (8, 6249)
    END IF
    6239 FORMAT(///10X, &
            'NOTE:  PRECIPITATION DATA WAS SYNTHETICALLY GENERATED USING')
    6240 FORMAT(///10X, 'NOTE:  PRECIPITATION DATA FOR     ', A40)
    6241 FORMAT(19X, 'WAS ENTERED FROM THE DEFAULT DATA FILE.')
    6242 FORMAT(19X, 'COEFFICIENTS FOR    ', A40)
    6243 FORMAT(19X, 'WAS ENTERED BY THE USER.')
    6244 FORMAT(19X, 'WAS ENTERED AND EDITED BY THE USER.')
    6245 FORMAT(19X, 'WAS ENTERED FROM A NOAA DATA FILE.')
    6246 FORMAT(19X, 'WAS ENTERED FROM A EARTH INFO CLIMATEDATA.')
    6247 FORMAT(19X, 'WAS ENTERED FROM AN ASCII DATA FILE.')
    6248 FORMAT(19X, 'WAS CONVERTED FROM A HELP V.2 DATA FILE.')
    6249 FORMAT(19X, 'WAS ENTERED FROM CANADIAN ENVIRONMENTAL DATA.')
    6231 FORMAT(/19X, 'NORMAL MEAN MONTHLY PRECIPITATION (INCHES)'//&
            6X, 'JAN/JUL', 5X, 'FEB/AUG', 5X, 'MAR/SEP', 5X, &
            'APR/OCT', 5X, 'MAY/NOV', 5X, 'JUN/DEC'/6X, '-------', &
            5(5X, '-------')/&
            1X, F11.2, 5F12.2/1X, F11.2, 5F12.2)
    6232 FORMAT(/21X, 'NORMAL MEAN MONTHLY PRECIPITATION (MM)'//&
            6X, 'JAN/JUL', 5X, 'FEB/AUG', 5X, 'MAR/SEP', 5X, &
            'APR/OCT', 5X, 'MAY/NOV', 5X, 'JUN/DEC'/6X, '-------', &
            5(5X, '-------')/&
            4X, F8.1, 5F12.1/4X, F8.1, 5F12.1)
    IF (IT7 /= 2) THEN
        WRITE (8, 6256) CITY7
    ELSE
        WRITE (8, 6255)
    END IF
    IF (IT7 == 1) THEN
        WRITE (8, 6241)
    ELSE IF (IT7 == 2) THEN
        WRITE (8, 6242) CITY7
        IF (IU7 == 1) THEN
            WRITE (8, 6251) (((TM(I) * 1.8) + 32.), I = 1, 12)
        ELSE
            WRITE (8, 6252) (TM(I), I = 1, 12)
        END IF
    ELSE IF (IT7 == 3) THEN
        WRITE (8, 6243)
    ELSE IF (IT7 == 4) THEN
        WRITE (8, 6244)
    ELSE IF (IT7 == 5) THEN
        WRITE (8, 6245)
    ELSE IF (IT7 == 6) THEN
        WRITE (8, 6246)
    ELSE IF (IT7 == 7) THEN
        WRITE (8, 6247)
    ELSE IF (IT7 == 8) THEN
        WRITE (8, 6248)
    ELSE IF (IT7 == 9) THEN
        WRITE (8, 6249)
    END IF
    6255 FORMAT(///10X, &
            'NOTE:  TEMPERATURE DATA WAS SYNTHETICALLY GENERATED USING')
    6256 FORMAT(///10X, 'NOTE:  TEMPERATURE DATA FOR     ', A40)
    6251 FORMAT(/14X, 'NORMAL MEAN MONTHLY TEMPERATURE (DEGREES ', &
            'FAHRENHEIT)'//&
            6X, 'JAN/JUL', 5X, 'FEB/AUG', 5X, 'MAR/SEP', 5X, &
            'APR/OCT', 5X, 'MAY/NOV', 5X, 'JUN/DEC'/6X, '-------', &
            5(5X, '-------')/&
            4X, F8.2, 5F12.2/4X, F8.2, 5F12.2)
    6252 FORMAT(/15X, 'NORMAL MEAN MONTHLY TEMPERATURE (DEGREES CELSIUS)'//&
            6X, 'JAN/JUL', 5X, 'FEB/AUG', 5X, 'MAR/SEP', 5X, &
            'APR/OCT', 5X, 'MAY/NOV', 5X, 'JUN/DEC'/6X, '-------', &
            5(5X, '-------')/&
            4X, F8.1, 5F12.1/4X, F8.1, 5F12.1)
    IF (IT13 /= 2) THEN
        WRITE (8, 6258) CITY13
    ELSE
        WRITE (8, 6257)
    END IF
    IF (IT13 == 1) THEN
        WRITE (8, 6241)
    ELSE IF (IT13 == 2) THEN
        WRITE (8, 6242) CITY13
        WRITE (8, 6262) RLAT
    ELSE IF (IT13 == 3) THEN
        WRITE (8, 6243)
    ELSE IF (IT13 == 4) THEN
        WRITE (8, 6244)
    ELSE IF (IT13 == 5) THEN
        WRITE (8, 6245)
    ELSE IF (IT13 == 6) THEN
        WRITE (8, 6246)
    ELSE IF (IT13 == 7) THEN
        WRITE (8, 6247)
    ELSE IF (IT13 == 8) THEN
        WRITE (8, 6248)
    ELSE IF (IT13 == 9) THEN
        WRITE (8, 6249)
    END IF
    6257 FORMAT(///10X, &
            'NOTE:  SOLAR RADIATION DATA WAS SYNTHETICALLY GENERATED USING')

    6258 FORMAT(///10X, 'NOTE:  SOLAR RADIATION DATA FOR     ', A40)
    6262 FORMAT(20X, ' AND STATION LATITUDE  = ', F6.2, ' DEGREES'///)
    !
    !
    REWIND 4
    REWIND 7
    REWIND 13
    READ(4, 6300) DUMMY1, DUMMY2, DUMMY3, DUMMY4
    DUMMY2 = DUMMY1
    READ(7, 6300) DUMMY1, DUMMY2, DUMMY3, DUMMY4
    DUMMY4 = DUMMY3
    READ(13, 6300) DUMMY1, DUMMY2, DUMMY3, DUMMY4
    DUMMY1 = DUMMY2
    DUMMY2 = DUMMY4
    6300 FORMAT(A2/A2/A2/A2)

    !
    !
    RETURN
END
!
!
!      ******************************* OUTDAY ******************
!
!     SUBROUTINE OUTDAY PRINTS DAILY RESULTS OF THE SIMULATION.
!
SUBROUTINE OUTDAY (SWE, IODAY, ASTAR, SSTAR, NVAR)
    !
    PARAMETER (MXYR = 100)
    !
    CHARACTER*1 ASTAR, SSTAR
    DOUBLE PRECISION PRC1, PRC2, PRC3, PRC4, PRC5, PRC6, &
            DRN1, DRN2, DRN3, DRN4, DRN5, HED1, HED2, HED3, HED4, HED5, &
            RCR1, RCR2, RCR3, RCR4, RCR5, RCRI, VAR, VARD
    !
    COMMON /BLK1/ IT4, IT7, IT13, IU4, IU7, IU13, IU11, IU10, IU8, &
            LMYR, IOD, IOM, IOA, IOS
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN(20), LSIN(20)
    COMMON /BLK8/ PRE (370), TMPF (366), RAD (366)
    COMMON /BLK10/ ETT, ESS, EP, ES, ET (67)
    COMMON /BLK11/ ETO, XLAI, STAGE1, CONA, RAIN, RUN, ABST, EAJ, TS2
    COMMON /BLK15/ PRC1, PRC2, PRC3, PRC4, PRC5, PRC6, DRN1, DRN2, &
            DRN3, DRN4, DRN5, HED1, HED2, HED3, HED4, HED5, &
            RCR1, RCR2, RCR3, RCR4, RCR5, RCRI(20)
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK33/ JYEAR (MXYR)
    COMMON /BLK34/  VAR (13), VARD (7, 366)
    !
    VAR (1) = PRE (IDA)
    VAR (2) = RUN
    VAR (3) = ETT
    VAR (4) = SWE
    VAR (5) = HED1
    VAR (6) = DRN1 + RCR1
    VAR (7) = PRC1
    VAR (8) = HED2
    VAR (9) = DRN2 + RCR2
    VAR (10) = PRC2
    VAR (11) = HED3
    VAR (12) = DRN3 + RCR3
    VAR (13) = PRC3
    IF (LP(4) > 0) THEN
        VARD (1, IDA) = HED4
        VARD (2, IDA) = DRN4 + RCR4
        VARD (3, IDA) = PRC4
        VARD (4, IDA) = HED5
        VARD (5, IDA) = DRN5 + RCR5
        VARD (6, IDA) = PRC5
        VARD (7, IDA) = PRC6
    END IF
    IF (IU8 /= 1) THEN
        VAR (1) = PRE(IDA) * 25.4
        VAR (2) = RUN * 25.4
        VAR (3) = ETT * 25.4
        VAR (4) = SWE
        VAR (5) = HED1 * 2.54
        VAR (6) = (DRN1 + RCR1) * 25.4
        VAR (7) = PRC1 * 25.4
        VAR (8) = HED2 * 2.54
        VAR (9) = (DRN2 + RCR2) * 25.4
        VAR (10) = PRC2 * 25.4
        VAR (11) = HED3 * 2.54
        VAR (12) = (DRN3 + RCR3) * 25.4
        VAR (13) = PRC3 * 25.4
        IF (LP(4) > 0) THEN
            VARD (1, IDA) = HED4 * 2.54
            VARD (2, IDA) = (DRN4 + RCR4) * 25.4
            VARD (3, IDA) = PRC4 * 25.4
            VARD (4, IDA) = HED5 * 2.54
            VARD (5, IDA) = (DRN5 + RCR5) * 25.4
            VARD (6, IDA) = PRC5 * 25.4
            VARD (7, IDA) = PRC6 * 25.4
        END IF
    END IF
    IF (IDA == 1) THEN
        IF (IODAY == 0) THEN
            IODAY = 1
            IF(LP(1) > 0) NVAR = 7
            IF(LP(2) > 0) NVAR = 10
            IF(LP(3) > 0) NVAR = 13
            IF(LP(4) > 0) NVAR = 16
            IF(LP(5) > 0) NVAR = 19
            IF(LP(6) > 0) NVAR = 20
        END IF
        !
        !     PRINTS HEADING
        !
        WRITE (8, 6110)
        DO K = 1, 5
            IF (LP(K) > 0) THEN
                IF (LAYER(LP(K)) > 2) THEN
                    IF (LAYER(LP(K) - 1) <= 2) THEN
                        WRITE (8, 6121) K, LP(K), K, LP(K) - 1
                    ELSE
                        WRITE (8, 6121) K, LP(K) - 1, K, LP(K) - 2
                    END IF
                END IF
                WRITE (8, 6131) K, LP(K)
            END IF
        end do
        K = 6
        IF (LP(6) > 0) WRITE (8, 6131) K, LP(6)
        IF (NVAR >= 13) THEN
            IF (IU8 == 1) THEN
                WRITE (8, 6141) JYEAR (IYR)
            ELSE
                WRITE (8, 6142) JYEAR (IYR)
            END IF
        ELSE IF (NVAR >= 10) THEN
            IF (IU8 == 1) THEN
                WRITE (8, 6151) JYEAR (IYR)
            ELSE
                WRITE (8, 6152) JYEAR (IYR)
            END IF
        ELSE
            IF (IU8 == 1) THEN
                WRITE (8, 6161) JYEAR (IYR)
            ELSE
                WRITE (8, 6162) JYEAR (IYR)
            END IF
        END IF
    END IF
    !
    6110 FORMAT(///)
    6121 FORMAT(4X, 'HEAD  #', I1, ':  AVERAGE HEAD ON TOP OF LAYER', I3/4X, &
            'DRAIN #', I1, ':  LATERAL DRAINAGE FROM LAYER', &
            I3, ' (RECIRCULATION AND COLLECTION)')
    6131 FORMAT(4X, 'LEAK  #', I1, &
            ':  PERCOLATION OR LEAKAGE THROUGH LAYER', I3)
    6141 FORMAT(1X/1X, 130('*')//52X, 'DAILY OUTPUT FOR YEAR', I5/&
            2X, 128('-')/'          S'/&
            '  DAY  A  O  RAIN  RUNOFF   ET   E. ZONE   HEAD      DRAIN  ', &
            '   LEAK      HEAD      DRAIN     LEAK      HEAD      DRAIN  ', &
            '   LEAK '/&
            '       I  I                       WATER     #1        #1    ', &
            '    #1        #2        #2        #2        #3        #3    ', &
            '    #3  '/&
            '       R  L   IN.    IN.    IN.  IN./IN.    IN.       IN.   ', &
            '    IN.       IN.       IN.       IN.       IN.       IN.   ', &
            '    IN. '/&
            '  ---  -  -  ----- ------ ------ ------- --------- ---------', &
            ' --------- --------- --------- --------- --------- ---------', &
            ' ---------'/)
    6142 FORMAT(1X/1X, 130('*')//52X, 'DAILY OUTPUT FOR YEAR', I5/&
            2X, 128('-')/'          S'/&
            '  DAY  A  O  RAIN  RUNOFF   ET   E. ZONE   HEAD      DRAIN  ', &
            '   LEAK      HEAD      DRAIN     LEAK      HEAD      DRAIN  ', &
            '   LEAK '/&
            '       I  I                       WATER     #1        #1    ', &
            '    #1        #2        #2        #2        #3        #3    ', &
            '    #3  '/&
            '       R  L   MM     MM     MM    CM/CM     CM        MM    ', &
            '    MM        CM        MM        MM        CM        MM    ', &
            '    MM  '/&
            '  ---  -  -  ----- ------ ------ ------- --------- ---------', &
            ' --------- --------- --------- --------- --------- ---------', &
            ' ---------'/)
    6151 FORMAT(1X/1X, 100('*')//37X, 'DAILY OUTPUT FOR YEAR', I5/&
            2X, 98('-')/'          S'/&
            '  DAY  A  O  RAIN  RUNOFF   ET   E. ZONE   HEAD      DRAIN  ', &
            '   LEAK      HEAD      DRAIN     LEAK   '/&
            '       I  I                       WATER     #1        #1    ', &
            '    #1        #2        #2        #2    '/&
            '       R  L   IN.    IN.    IN.  IN./IN.    IN.       IN.   ', &
            '    IN.       IN.       IN.       IN.   '/&
            '  ---  -  -  ----- ------ ------ ------- --------- ---------', &
            ' --------- --------- --------- ---------'/)
    6152 FORMAT(1X/1X, 100('*')//37X, 'DAILY OUTPUT FOR YEAR', I5/&
            2X, 98('-')/'          S'/&
            '  DAY  A  O  RAIN  RUNOFF   ET   E. ZONE   HEAD      DRAIN  ', &
            '   LEAK      HEAD      DRAIN     LEAK   '/&
            '       I  I                       WATER     #1        #1    ', &
            '    #1        #2        #2        #2    '/&
            '       R  L   MM     MM     MM    CM/CM     CM        MM    ', &
            '    MM        CM        MM        MM    '/&
            '  ---  -  -  ----- ------ ------ ------- --------- ---------', &
            ' --------- --------- --------- ---------'/)
    6161 FORMAT(1X/1X, 70('*')//22X, 'DAILY OUTPUT FOR YEAR', I5/&
            2X, 68('-')/'          S'/&
            '  DAY  A  O  RAIN  RUNOFF   ET   E. ZONE   HEAD      DRAIN  ', &
            '   LEAK   '/&
            '       I  I                       WATER     #1        #1    ', &
            '    #1    '/&
            '       R  L   IN.    IN.    IN.  IN./IN.    IN.       IN.   ', &
            '    IN.   '/&
            '  ---  -  -  ----- ------ ------ ------- --------- ---------', &
            ' ---------'/)
    6162 FORMAT(1X/1X, 100('*')//37X, 'DAILY OUTPUT FOR YEAR', I5/&
            2X, 98('-')/'          S'/&
            '  DAY  A  O  RAIN  RUNOFF   ET   E. ZONE   HEAD      DRAIN  ', &
            '   LEAK   '/&
            '       I  I                       WATER     #1        #1    ', &
            '    #1    '/&
            '       R  L   MM     MM     MM    CM/CM     CM        MM    ', &
            '    MM    '/&
            '  ---  -  -  ----- ------ ------ ------- --------- ---------', &
            ' ---------'/)
    !
    !
    !     PRINTS DAILY RESULTS, * INDICATES FREEZING
    !          TEMPERATURES OR FROZEN SOIL
    !
    !   WRITES THE DAILY VARIABLES
    !
    IF (NVAR >= 13) THEN
        IF (IU8 == 1) THEN
            WRITE (8, 7001) IDA, ASTAR, SSTAR, (VAR (J), J = 1, 13)
        ELSE
            WRITE (8, 7002) IDA, ASTAR, SSTAR, (VAR (J), J = 1, 13)
        END IF
    ELSE IF (NVAR >= 10) THEN
        IF (IU8 == 1) THEN
            WRITE (8, 7001) IDA, ASTAR, SSTAR, (VAR (J), J = 1, 10)
        ELSE
            WRITE (8, 7002) IDA, ASTAR, SSTAR, (VAR (J), J = 1, 10)
        END IF
    ELSE
        IF (IU8 == 1) THEN
            WRITE (8, 7001) IDA, ASTAR, SSTAR, (VAR (J), J = 1, 7)
        ELSE
            WRITE (8, 7002) IDA, ASTAR, SSTAR, (VAR (J), J = 1, 7)
        END IF
    END IF
    !
    !
    7001 FORMAT(2X, I3, 2X, A1, 2X, A1, 2X, F5.2, F7.3, F7.3, F8.4, 1X, 3(F9.4, 1X, &
            2(G9.4, 1X)))
    7002 FORMAT(2X, I3, 2X, A1, 2X, A1, 2X, F5.1, F7.2, F7.2, F8.4, 1X, 3(F9.4, 1X, &
            2(G9.4, 1X)))
    7011 FORMAT(2X, I3, 3X, 2(F9.4, 3X, G9.4, 3X, G9.4, 3X), G9.4)
    7012 FORMAT(2X, I3, 3X, 2(F9.4, 3X, G9.4, 3X, G9.4, 3X), G9.4)
    !
    !
    IF (IDA == ND) THEN
        IF (NVAR > 19) THEN
            IF (IU8 == 1) THEN
                WRITE (8, 6241) JYEAR (IYR)
                DO I = 1, ND
                    WRITE (8, 7011) I, (VARD (J, I), J = 1, 7)
                end do
            ELSE
                WRITE (8, 6242) JYEAR (IYR)
                DO I = 1, ND
                    WRITE (8, 7012) I, (VARD (J, I), J = 1, 7)
                end do
            END IF
            WRITE (8, 6176)
        ELSE IF (NVAR > 16) THEN
            IF (IU8 == 1) THEN
                WRITE (8, 6251) JYEAR (IYR)
                DO I = 1, ND
                    WRITE (8, 7011) I, (VARD (J, I), J = 1, 6)
                end do
            ELSE
                WRITE (8, 6252) JYEAR (IYR)
                DO I = 1, ND
                    WRITE (8, 7012) I, (VARD (J, I), J = 1, 6)
                end do
            END IF
            WRITE (8, 6175)
        ELSE IF (NVAR > 13) THEN
            IF (IU8 == 1) THEN
                WRITE (8, 6261) JYEAR (IYR)
                DO I = 1, ND
                    WRITE (8, 7011) I, (VARD (J, I), J = 1, 3)
                end do
            ELSE
                WRITE (8, 6262) JYEAR (IYR)
                DO I = 1, ND
                    WRITE (8, 7012) I, (VARD (J, I), J = 1, 3)
                end do
            END IF
            WRITE (8, 6174)
        ELSE IF (NVAR > 10) THEN
            WRITE (8, 6173)
        ELSE IF (NVAR > 7) THEN
            WRITE (8, 6172)
        ELSE
            WRITE (8, 6171)
        END IF
    END IF
    !
    6241 FORMAT(1X/1X, 130('-')////1X, 89('-')//&
            32X, 'DAILY OUTPUT FOR YEAR', I5//&
            '  DAY     HEAD        DRAIN       LEAK   ', &
            '     HEAD        DRAIN       LEAK        LEAK '/&
            '           #4          #4          #4    ', &
            '      #5          #5          #5          #6  '/&
            '           IN.         IN.         IN.   ', &
            '      IN.         IN.         IN.         IN. '/&
            '  ---   ---------   ---------   ---------', &
            '   ---------   ---------   ---------   ---------'/)
    6242 FORMAT(1X/1X, 130('-')////1X, 89('-')//&
            32X, 'DAILY OUTPUT FOR YEAR', I5//&
            '  DAY     HEAD        DRAIN       LEAK   ', &
            '     HEAD        DRAIN       LEAK        LEAK '/&
            '           #4          #4          #4    ', &
            '      #5          #5          #5          #6  '/&
            '           CM          MM          MM    ', &
            '      CM          MM          MM          MM  '/&
            '  ---   ---------   ---------   ---------', &
            '   ---------   ---------   ---------   ---------'/)
    6251 FORMAT(1X/1X, 130('-')////1X, 77('-')//&
            26X, 'DAILY OUTPUT FOR YEAR', I5//&
            '  DAY     HEAD        DRAIN       LEAK   ', &
            '     HEAD        DRAIN       LEAK   '/&
            '           #4          #4          #4    ', &
            '      #5          #5          #5    '/&
            '           IN.         IN.         IN.   ', &
            '      IN.         IN.         IN.   '/&
            '  ---   ---------   ---------   ---------', &
            '   ---------   ---------   ---------'/)
    6252 FORMAT(1X/1X, 130('-')////1X, 77('-')//&
            26X, 'DAILY OUTPUT FOR YEAR', I5//&
            '  DAY     HEAD        DRAIN       LEAK   ', &
            '     HEAD        DRAIN       LEAK   '/&
            '           #4          #4          #4    ', &
            '      #5          #5          #5    '/&
            '           CM          MM          MM    ', &
            '      CM          MM          MM    '/&
            '  ---   ---------   ---------   ---------', &
            '   ---------   ---------   ---------'/)
    6261 FORMAT(1X/1X, 130('-')////1X, 41('-')//&
            8X, 'DAILY OUTPUT FOR YEAR', I5//&
            '  DAY     HEAD        DRAIN       LEAK   '/&
            '           #4          #4          #4    '/&
            '           IN.         IN.         IN.   '/&
            '  ---   ---------   ---------   ---------'/)
    6262 FORMAT(1X/1X, 130('-')////1X, 41('-')//&
            8X, 'DAILY OUTPUT FOR YEAR', I5//&
            '  DAY     HEAD        DRAIN       LEAK   '/&
            '           #4          #4          #4    '/&
            '           CM          MM          MM    '/&
            '  ---   ---------   ---------   ---------'/)
    !
    6171 FORMAT(1X/1X, 70('*'))
    6172 FORMAT(1X/1X, 100('*'))
    6173 FORMAT(1X/1X, 130('*'))
    6174 FORMAT(1X/1X, 41('*'))
    6175 FORMAT(1X/1X, 77('*'))
    6176 FORMAT(1X/1X, 89('*'))
    RETURN
END
!
!
!      ************************ OUTMO *************************
!
!     SUBROUTINE OUTMO PRINTS MONTHLY TOTALS.
!
SUBROUTINE OUTMO
    !
    PARAMETER (MXYR = 100)
    COMMON /BLK1/ IT4, IT7, IT13, IU4, IU7, IU13, IU11, IU10, IU8, &
            LMYR, IOD, IOM, IOA, IOS
    COMMON /BLK12/ PRC1M (12), PRC2M (12), PRC3M (12), &
            PRC4M (12), PRC5M (12), PRC6M (12), DRN1M (12), &
            DRN2M (12), DRN3M (12), DRN4M (12), DRN5M (12), &
            PREM (12), RUNM (12), ETM (12), HED1M (12), &
            HED2M (12), HED3M (12), HED4M (12), HED5M (12), &
            RCRIM (20, 12), SUBINM (20, 12), RCR1M (12), &
            RCR2M (12), RCR3M (12), RCR4M (12), RCR5M (12)
    COMMON /BLK22/ PRC1M2 (12), PRC2M2 (12), PRC3M2 (12), &
            PRC4M2 (12), PRC5M2 (12), PRC6M2 (12), DRN1M2 (12), &
            DRN2M2 (12), DRN3M2 (12), DRN4M2 (12), DRN5M2 (12), &
            PREM2 (12), RUNM2 (12), ETM2 (12), HED1M2 (12), &
            HED2M2 (12), HED3M2 (12), HED4M2 (12), HED5M2 (12), &
            RCRIM2 (20, 12), RCR1M2 (12), RCR2M2 (12), &
            RCR3M2 (12), RCR4M2 (12), RCR5M2 (12)
    COMMON /BLK23/ PRC1M1 (12), PRC2M1 (12), PRC3M1 (12), &
            PRC4M1 (12), PRC5M1 (12), PRC6M1 (12), DRN1M1 (12), &
            DRN2M1 (12), DRN3M1 (12), DRN4M1 (12), DRN5M1 (12), &
            PREM1 (12), RUNM1 (12), ETM1 (12), HED1M1 (12), &
            HED2M1 (12), HED3M1 (12), HED4M1 (12), HED5M1 (12), &
            RCRIM1 (20, 12), RCR1M1 (12), RCR2M1 (12), &
            RCR3M1 (12), RCR4M1 (12), RCR5M1 (12)
    !
    !     ACCUMULATES MONTHLY TOTALS FOR COMPUTING AVERAGES AND
    !       STANDARD DEVIATIONS.
    !
    IF (IOS == 1) THEN
        DO I = 1, 12
            PREM1(I) = PREM1(I) + PREM(I)
            RUNM1(I) = RUNM1(I) + RUNM(I)
            ETM1(I) = ETM1(I) + ETM(I)
            DRN1M1(I) = DRN1M1(I) + DRN1M(I)
            DRN2M1(I) = DRN2M1(I) + DRN2M(I)
            DRN3M1(I) = DRN3M1(I) + DRN3M(I)
            DRN4M1(I) = DRN4M1(I) + DRN4M(I)
            DRN5M1(I) = DRN5M1(I) + DRN5M(I)
            RCR1M1(I) = RCR1M1(I) + RCR1M(I)
            RCR2M1(I) = RCR2M1(I) + RCR2M(I)
            RCR3M1(I) = RCR3M1(I) + RCR3M(I)
            RCR4M1(I) = RCR4M1(I) + RCR4M(I)
            RCR5M1(I) = RCR5M1(I) + RCR5M(I)
            HED1M1(I) = HED1M1(I) + HED1M(I)
            HED2M1(I) = HED2M1(I) + HED2M(I)
            HED3M1(I) = HED3M1(I) + HED3M(I)
            HED4M1(I) = HED4M1(I) + HED4M(I)
            HED5M1(I) = HED5M1(I) + HED5M(I)
            PRC1M1(I) = PRC1M1(I) + PRC1M(I)
            PRC2M1(I) = PRC2M1(I) + PRC2M(I)
            PRC3M1(I) = PRC3M1(I) + PRC3M(I)
            PRC4M1(I) = PRC4M1(I) + PRC4M(I)
            PRC5M1(I) = PRC5M1(I) + PRC5M(I)
            PRC6M1(I) = PRC6M1(I) + PRC6M(I)
            DO K = 1, 20
                RCRIM1(K, I) = RCRIM1(K, I) + RCRIM(K, I)
            end do
            PREM2(I) = PREM2(I) + PREM(I) * PREM(I)
            RUNM2(I) = RUNM2(I) + RUNM(I) * RUNM(I)
            ETM2(I) = ETM2(I) + ETM(I) * ETM(I)
            DRN1M2(I) = DRN1M2(I) + DRN1M(I) * DRN1M(I)
            DRN2M2(I) = DRN2M2(I) + DRN2M(I) * DRN2M(I)
            DRN3M2(I) = DRN3M2(I) + DRN3M(I) * DRN3M(I)
            DRN4M2(I) = DRN4M2(I) + DRN4M(I) * DRN4M(I)
            DRN5M2(I) = DRN5M2(I) + DRN5M(I) * DRN5M(I)
            RCR1M2(I) = RCR1M2(I) + RCR1M(I) * RCR1M(I)
            RCR2M2(I) = RCR2M2(I) + RCR2M(I) * RCR2M(I)
            RCR3M2(I) = RCR3M2(I) + RCR3M(I) * RCR3M(I)
            RCR4M2(I) = RCR4M2(I) + RCR4M(I) * RCR4M(I)
            RCR5M2(I) = RCR5M2(I) + RCR5M(I) * RCR5M(I)
            HED1M2(I) = HED1M2(I) + HED1M(I) * HED1M(I)
            HED2M2(I) = HED2M2(I) + HED2M(I) * HED2M(I)
            HED3M2(I) = HED3M2(I) + HED3M(I) * HED3M(I)
            HED4M2(I) = HED4M2(I) + HED4M(I) * HED4M(I)
            HED5M2(I) = HED5M2(I) + HED5M(I) * HED5M(I)
            PRC1M2(I) = PRC1M2(I) + PRC1M(I) * PRC1M(I)
            PRC2M2(I) = PRC2M2(I) + PRC2M(I) * PRC2M(I)
            PRC3M2(I) = PRC3M2(I) + PRC3M(I) * PRC3M(I)
            PRC4M2(I) = PRC4M2(I) + PRC4M(I) * PRC4M(I)
            PRC5M2(I) = PRC5M2(I) + PRC5M(I) * PRC5M(I)
            PRC6M2(I) = PRC6M2(I) + PRC6M(I) * PRC6M(I)
            DO K = 1, 20
                RCRIM2(K, I) = RCRIM2(K, I) + RCRIM(K, I) * RCRIM(K, I)
            end do
        end do
    END IF
    IF (IOM == 0) RETURN
    !
    !     PRINTS HEADING FOR MONTHLY RESULTS AND PRINTS
    !     MONTHLY PRECIPITATION, RUNOFF AND EVAPOTRANSPIRATION.
    !
    WRITE (8, 6000)
    IF (IU8 == 1) THEN
        CALL OUTMO1
    ELSE
        CALL OUTMO2
    END IF
    WRITE (8, 6110)
    6000 FORMAT(1X///1X, 79('*'))
    6110 FORMAT(1X/1X, 79('*')//)
    RETURN
END
!
!
!
!      ************************ OUTMO1 *************************
!
!     SUBROUTINE OUTMO PRINTS MONTHLY TOTALS in INCHES.
!
SUBROUTINE OUTMO1
    !
    PARAMETER (MXYR = 100)
    COMMON /BLK3/ PORO (20), FC (20), WP (20), RC (20), SW (20), &
            RS (20), XLAMBD (20), BUB (20), THICK (20), SLOPE (20), &
            XLENG (20), CON (20), SUBIN (20), RECIR (20), PHOLE (20), &
            DEFEC (20), TRANS (20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN(20), LSIN(20)
    COMMON /BLK12/ PRC1M (12), PRC2M (12), PRC3M (12), &
            PRC4M (12), PRC5M (12), PRC6M (12), DRN1M (12), &
            DRN2M (12), DRN3M (12), DRN4M (12), DRN5M (12), &
            PREM (12), RUNM (12), ETM (12), HED1M (12), &
            HED2M (12), HED3M (12), HED4M (12), HED5M (12), &
            RCRIM (20, 12), SUBINM (20, 12), RCR1M (12), &
            RCR2M (12), RCR3M (12), RCR4M (12), RCR5M (12)
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK32/ SHED1 (12), SHED2 (12), SHED3 (12), SHED4 (12), &
            SHED5 (12)
    COMMON /BLK33/ JYEAR (MXYR)
    DIMENSION MDAY(12)
    !
    !     ACCUMULATES MONTHLY TOTALS FOR COMPUTING AVERAGES AND
    !       STANDARD DEVIATIONS.
    !
    !
    !     PRINTS HEADING FOR MONTHLY RESULTS AND PRINTS
    !     MONTHLY PRECIPITATION, RUNOFF AND EVAPOTRANSPIRATION.
    !
    MDAY(1) = 31
    MDAY(2) = 28
    MDAY(3) = 31
    MDAY(4) = 30
    MDAY(5) = 31
    MDAY(6) = 30
    MDAY(7) = 31
    MDAY(8) = 31
    MDAY(9) = 30
    MDAY(10) = 31
    MDAY(11) = 30
    MDAY(12) = 31
    WRITE (8, 6011) JYEAR (IYR)
    WRITE (8, 6020)
    !
    !    DETERMINES IF THE YEAR IS A LEAP YEAR.
    !
    MND = LEAP (JYEAR(IYR), NT)
    !
    MONTHE = 12
    MONTHB = MONTHE - 11
    MONTH6 = MONTHE - 6
    MONTH7 = MONTHE - 5
    WRITE (8, 6031) (PREM (J), J = MONTHB, MONTHE)
    WRITE (8, 6041) (RUNM (J), J = MONTHB, MONTHE)
    WRITE (8, 6051) (ETM (J), J = MONTHB, MONTHE)
    DO K = 1, LP(1)
        IF (LSIN(K) > 0) THEN
            DO J = 1, 12
                SUBINM (K, J) = MDAY(J) * SUBIN(K)
            end do
            IF (MND == 366) SUBINM(K, 2) = SUBINM(K, 2) + SUBIN(K)
            WRITE(8, 6076) (SUBINM(K, J), &
                    J = MONTHB, MONTH6), K, (SUBINM (K, J), J = MONTH7, MONTHE)
        END IF
        IF (LRIN(K) > 0) WRITE(8, 6077) (RCRIM(K, J), &
                J = MONTHB, MONTH6), K, (RCRIM (K, J), J = MONTH7, MONTHE)
    end do
    IF (LD(1) > 0) THEN
        WRITE (8, 6061) (DRN1M (J), J = MONTHB, MONTH6), &
                LD(1), (DRN1M (J), J = MONTH7, MONTHE)
        IF (LAYR(LD(1)) > 0) WRITE (8, 6066) (RCR1M (J), &
                J = MONTHB, MONTH6), LD(1), (RCR1M (J), J = MONTH7, MONTHE)
    END IF
    WRITE (8, 6071) (PRC1M (J), J = MONTHB, MONTH6), &
            LP(1), (PRC1M (J), J = MONTH7, MONTHE)
    IF (LP(2) > 0) THEN
        DO K = LP(1) + 1, LP(2)
            IF (LSIN(K) > 0) THEN
                DO J = 1, 12
                    SUBINM (K, J) = MDAY(J) * SUBIN(K)
                end do
                IF (MND == 366) SUBINM(K, 2) = SUBINM(K, 2) + SUBIN(K)
                WRITE(8, 6076) (SUBINM(K, J), &
                        J = MONTHB, MONTH6), K, (SUBINM (K, J), J = MONTH7, MONTHE)
            END IF
            IF (LRIN(K) > 0) WRITE(8, 6077) (RCRIM(K, J), &
                    J = MONTHB, MONTH6), K, (RCRIM (K, J), J = MONTH7, MONTHE)
        end do
        IF (LD(2) > 0) THEN
            WRITE (8, 6061) (DRN2M (J), J = MONTHB, MONTH6), &
                    LD(2), (DRN2M (J), J = MONTH7, MONTHE)
            IF (LAYR(LD(2)) > 0) WRITE (8, 6066) (RCR2M (J), &
                    J = MONTHB, MONTH6), LD(2), (RCR2M (J), J = MONTH7, MONTHE)
        END IF
        WRITE (8, 6071) (PRC2M (J), J = MONTHB, MONTH6), &
                LP(2), (PRC2M (J), J = MONTH7, MONTHE)
    END IF
    IF (LP(3) > 0) THEN
        DO K = LP(2) + 1, LP(3)
            IF (LSIN(K) > 0) THEN
                DO J = 1, 12
                    SUBINM (K, J) = MDAY(J) * SUBIN(K)
                end do
                IF (MND == 366) SUBINM(K, 2) = SUBINM(K, 2) + SUBIN(K)
                WRITE(8, 6076) (SUBINM(K, J), &
                        J = MONTHB, MONTH6), K, (SUBINM (K, J), J = MONTH7, MONTHE)
            END IF
            IF (LRIN(K) > 0) WRITE(8, 6077) (RCRIM(K, J), &
                    J = MONTHB, MONTH6), K, (RCRIM (K, J), J = MONTH7, MONTHE)
        end do
        IF (LD(3) > 0) THEN
            WRITE (8, 6061) (DRN3M (J), J = MONTHB, MONTH6), &
                    LD(3), (DRN3M (J), J = MONTH7, MONTHE)
            IF (LAYR(LD(3)) > 0) WRITE (8, 6066) (RCR3M (J), &
                    J = MONTHB, MONTH6), LD(3), (RCR3M (J), J = MONTH7, MONTHE)
        END IF
        WRITE (8, 6071) (PRC3M (J), J = MONTHB, MONTH6), &
                LP(3), (PRC3M (J), J = MONTH7, MONTHE)
    END IF
    IF (LP(4) > 0) THEN
        DO K = LP(3) + 1, LP(4)
            IF (LSIN(K) > 0) THEN
                DO J = 1, 12
                    SUBINM (K, J) = MDAY(J) * SUBIN(K)
                end do
                IF (MND == 366) SUBINM(K, 2) = SUBINM(K, 2) + SUBIN(K)
                WRITE(8, 6076) (SUBINM(K, J), &
                        J = MONTHB, MONTH6), K, (SUBINM (K, J), J = MONTH7, MONTHE)
            END IF
            IF (LRIN(K) > 0) WRITE(8, 6077) (RCRIM(K, J), &
                    J = MONTHB, MONTH6), K, (RCRIM (K, J), J = MONTH7, MONTHE)
        end do
        IF (LD(4) > 0) THEN
            WRITE (8, 6061) (DRN4M (J), J = MONTHB, MONTH6), &
                    LD(4), (DRN4M (J), J = MONTH7, MONTHE)
            IF (LAYR(LD(4)) > 0) WRITE (8, 6066) (RCR4M (J), &
                    J = MONTHB, MONTH6), LD(4), (RCR4M (J), J = MONTH7, MONTHE)
        END IF
        WRITE (8, 6071) (PRC4M (J), J = MONTHB, MONTH6), &
                LP(4), (PRC4M (J), J = MONTH7, MONTHE)
    END IF
    IF (LP(5) > 0) THEN
        DO K = LP(4) + 1, LP(5)
            IF (LSIN(K) > 0) THEN
                DO J = 1, 12
                    SUBINM (K, J) = MDAY(J) * SUBIN(K)
                end do
                IF (MND == 366) SUBINM(K, 2) = SUBINM(K, 2) + SUBIN(K)
                WRITE(8, 6076) (SUBINM(K, J), &
                        J = MONTHB, MONTH6), K, (SUBINM (K, J), J = MONTH7, MONTHE)
            END IF
            IF (LRIN(K) > 0) WRITE(8, 6077) (RCRIM(K, J), &
                    J = MONTHB, MONTH6), K, (RCRIM (K, J), J = MONTH7, MONTHE)
        end do
        IF (LD(5) > 0) THEN
            WRITE (8, 6061) (DRN5M (J), J = MONTHB, MONTH6), &
                    LD(5), (DRN5M (J), J = MONTH7, MONTHE)
            IF (LAYR(LD(5)) > 0) WRITE (8, 6066) (RCR5M (J), &
                    J = MONTHB, MONTH6), LD(5), (RCR5M (J), J = MONTH7, MONTHE)
        END IF
        WRITE (8, 6071) (PRC5M (J), J = MONTHB, MONTH6), &
                LP(5), (PRC5M (J), J = MONTH7, MONTHE)
    END IF
    IF (LP(6) > 0) THEN
        DO K = LP(5) + 1, LP(6)
            IF (LSIN(K) > 0) THEN
                DO J = 1, 12
                    SUBINM (K, J) = MDAY(J) * SUBIN(K)
                end do
                IF (MND == 366) SUBINM(K, 2) = SUBINM(K, 2) + SUBIN(K)
                WRITE(8, 6076) (SUBINM(K, J), &
                        J = MONTHB, MONTH6), K, (SUBINM (K, J), J = MONTH7, MONTHE)
            END IF
            IF (LRIN(K) > 0) WRITE(8, 6077) (RCRIM(K, J), &
                    J = MONTHB, MONTH6), K, (RCRIM (K, J), J = MONTH7, MONTHE)
        end do
        WRITE (8, 6071) (PRC6M (J), J = MONTHB, MONTH6), &
                LP(6), (PRC6M (J), J = MONTH7, MONTHE)
    END IF
    IF (LP(1) > 0) THEN
        IF (LAYER (LP(1)) > 2) THEN
            WRITE (8, 6081)
            IF (LAYER(LP(1) - 1) <= 2) THEN
                WRITE (8, 6090) (HED1M (J), J = MONTHB, MONTH6), &
                        LP(1), (HED1M (J), J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED1 (J), J = 1, 6), &
                        LP(1), (SHED1 (J), J = 7, 12)
            ELSE
                WRITE (8, 6090) (HED1M (J), J = MONTHB, MONTH6), &
                        LP(1) - 1, (HED1M (J), J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED1 (J), J = 1, 6), &
                        LP(1) - 1, (SHED1 (J), J = 7, 12)
            END IF
        END IF
    END IF
    IF (LP(2) > 0) THEN
        IF (LAYER (LP(2)) > 2) THEN
            IF (LAYER(LP(2) - 1) <= 2) THEN
                WRITE (8, 6090) (HED2M (J), J = MONTHB, MONTH6), &
                        LP(2), (HED2M (J), J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED2 (J), J = 1, 6), &
                        LP(2), (SHED2 (J), J = 7, 12)
            ELSE
                WRITE (8, 6090) (HED2M (J), J = MONTHB, MONTH6), &
                        LP(2) - 1, (HED2M (J), J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED2 (J), J = 1, 6), &
                        LP(2) - 1, (SHED2 (J), J = 7, 12)
            END IF
        END IF
    END IF
    IF (LP(3) > 0) THEN
        IF (LAYER (LP(3)) > 2) THEN
            IF (LAYER(LP(3) - 1) <= 2) THEN
                WRITE (8, 6090) (HED3M (J), J = MONTHB, MONTH6), &
                        LP(3), (HED3M (J), J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED3 (J), J = 1, 6), &
                        LP(3), (SHED3 (J), J = 7, 12)
            ELSE
                WRITE (8, 6090) (HED3M (J), J = MONTHB, MONTH6), &
                        LP(3) - 1, (HED3M (J), J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED3 (J), J = 1, 6), &
                        LP(3) - 1, (SHED3 (J), J = 7, 12)
            END IF
        END IF
    END IF
    IF (LP(4) > 0) THEN
        IF (LAYER (LP(4)) > 2) THEN
            IF (LAYER(LP(4) - 1) <= 2) THEN
                WRITE (8, 6090) (HED4M (J), J = MONTHB, MONTH6), &
                        LP(4), (HED4M (J), J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED4 (J), J = 1, 6), &
                        LP(4), (SHED4 (J), J = 7, 12)
            ELSE
                WRITE (8, 6090) (HED4M (J), J = MONTHB, MONTH6), &
                        LP(4) - 1, (HED4M (J), J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED4 (J), J = 1, 6), &
                        LP(4) - 1, (SHED4 (J), J = 7, 12)
            END IF
        END IF
    END IF
    IF (LP(5) > 0) THEN
        IF (LAYER (LP(5)) > 2) THEN
            IF (LAYER(LP(5) - 1) <= 2) THEN
                WRITE (8, 6090) (HED5M (J), J = MONTHB, MONTH6), &
                        LP(5), (HED5M (J), J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED5 (J), J = 1, 6), &
                        LP(5), (SHED5 (J), J = 7, 12)
            ELSE
                WRITE (8, 6090) (HED5M (J), J = MONTHB, MONTH6), &
                        LP(5) - 1, (HED5M (J), J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED5 (J), J = 1, 6), &
                        LP(5) - 1, (SHED5 (J), J = 7, 12)
            END IF
        END IF
    END IF
    6011 FORMAT(1X/19X, 'MONTHLY TOTALS (IN INCHES) FOR YEAR', I5/&
            1X, 79('-'))
    6020 FORMAT(1X, /33X, 'JAN/JUL FEB/AUG MAR/SEP ', &
            'APR/OCT MAY/NOV JUN/DEC'/32X, 6(' -------'))
    6031   FORMAT(1X, /' PRECIPITATION         ', &
            10X, F5.2, 5(3X, F5.2)/30X, 6(3X, F5.2))
    6041   FORMAT(1X/' RUNOFF         ', 15X, &
            6(2X, F6.3)/31X, 6(2X, F6.3))
    6051   FORMAT(1X/' EVAPOTRANSPIRATION', 12X, 6(2X, F6.3)/1X, &
            5X, '        ', 17X, 6(2X, F6.3))
    6076 FORMAT(1X/' SUBSURFACE INFLOW INTO  ', 7X, 6F8.4/3X, &
            'LAYER', I3, '         ', 12X, 6F8.4)
    6066 FORMAT(1X, /' LATERAL DRAINAGE RECIRCULATED', 2X, 6F8.4/3X, &
            'FROM LAYER', I3, '         ', 7X, 6F8.4)
    6061 FORMAT(1X, /' LATERAL DRAINAGE COLLECTED   ', 2X, 6F8.4/3X, &
            'FROM LAYER', I3, '         ', 7X, 6F8.4)
    6071 FORMAT(1X/' PERCOLATION/LEAKAGE THROUGH   ', 1X, 6F8.4/3X, &
            'LAYER', I3, '         ', 12X, 6F8.4)
    6077 FORMAT(1X, /' LATERAL DRAINAGE RECIRCULATED', 2X, 6F8.4/3X, &
            'INTO LAYER', I3, '         ', 7X, 6F8.4)
    6081       FORMAT(1X//1X, 79('-')/20X, &
            'MONTHLY SUMMARIES FOR DAILY HEADS (INCHES)'/1X, 79('-')/)
    6090 FORMAT(1X/' AVERAGE DAILY HEAD ON ', 8X, 6F8.3/3X, &
            'TOP OF LAYER', I3, '  ', 11X, 6F8.3)
    6100 FORMAT(1X/' STD. DEVIATION OF DAILY ', 6X, 6(F8.3)/3X, &
            'HEAD ON TOP OF LAYER', I3, 5X, 6F8.3)
    RETURN
END
!
!
!
!
!      ************************ OUTMO2 *************************
!
!     SUBROUTINE OUTMO PRINTS MONTHLY TOTALS in MM and CM.
!
SUBROUTINE OUTMO2
    !
    PARAMETER (MXYR = 100)
    COMMON /BLK3/ PORO (20), FC (20), WP (20), RC (20), SW (20), &
            RS (20), XLAMBD (20), BUB (20), THICK (20), SLOPE (20), &
            XLENG (20), CON (20), SUBIN (20), RECIR (20), PHOLE (20), &
            DEFEC (20), TRANS (20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN(20), LSIN(20)
    COMMON /BLK12/ PRC1M (12), PRC2M (12), PRC3M (12), &
            PRC4M (12), PRC5M (12), PRC6M (12), DRN1M (12), &
            DRN2M (12), DRN3M (12), DRN4M (12), DRN5M (12), &
            PREM (12), RUNM (12), ETM (12), HED1M (12), &
            HED2M (12), HED3M (12), HED4M (12), HED5M (12), &
            RCRIM (20, 12), SUBINM (20, 12), RCR1M (12), &
            RCR2M (12), RCR3M (12), RCR4M (12), RCR5M (12)
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK32/ SHED1 (12), SHED2 (12), SHED3 (12), SHED4 (12), &
            SHED5 (12)
    COMMON /BLK33/ JYEAR (MXYR)
    DIMENSION MDAY(12)
    !
    !     ACCUMULATES MONTHLY TOTALS FOR COMPUTING AVERAGES AND
    !       STANDARD DEVIATIONS.
    !
    !
    !     PRINTS HEADING FOR MONTHLY RESULTS AND PRINTS
    !     MONTHLY PRECIPITATION, RUNOFF AND EVAPOTRANSPIRATION.
    !
    MDAY(1) = 31
    MDAY(2) = 28
    MDAY(3) = 31
    MDAY(4) = 30
    MDAY(5) = 31
    MDAY(6) = 30
    MDAY(7) = 31
    MDAY(8) = 31
    MDAY(9) = 30
    MDAY(10) = 31
    MDAY(11) = 30
    MDAY(12) = 31
    !
    !    DETERMINES IF THE YEAR IS A LEAP YEAR.
    !
    MND = LEAP (JYEAR(IYR), NT)
    !
    WRITE (8, 6012) JYEAR (IYR)
    WRITE (8, 6020)
    MONTHE = 12
    MONTHB = MONTHE - 11
    MONTH6 = MONTHE - 6
    MONTH7 = MONTHE - 5
    WRITE (8, 6032) (PREM (J) * 25.4, J = MONTHB, MONTHE)
    WRITE (8, 6042) (RUNM (J) * 25.4, J = MONTHB, MONTHE)
    WRITE (8, 6052) (ETM (J) * 25.4, J = MONTHB, MONTHE)
    DO K = 1, LP(1)
        IF (LSIN(K) > 0) THEN
            DO J = 1, 12
                SUBINM (K, J) = MDAY(J) * SUBIN(K)
            end do
            IF (MND == 366) SUBINM(K, 2) = SUBINM(K, 2) + SUBIN(K)
            WRITE(8, 6077) (SUBINM(K, J) * 25.4, &
                    J=MONTHB, MONTH6), K, (SUBINM (K, J) * 25.4, J = MONTH7, MONTHE)
        END IF
        IF (LRIN(K) > 0) WRITE(8, 6065) (RCRIM(K, J) * 25.4, &
                J=MONTHB, MONTH6), K, (RCRIM (K, J) * 25.4, J = MONTH7, MONTHE)
    end do
    IF (LD(1) > 0) THEN
        WRITE (8, 6062) (DRN1M (J) * 25.4, J=MONTHB, MONTH6), &
                LD(1), (DRN1M (J) * 25.4, J = MONTH7, MONTHE)
        IF (LAYR(LD(1)) > 0) WRITE (8, 6067) (RCR1M (J) * 25.4, &
                J=MONTHB, MONTH6), LD(1), (RCR1M (J) * 25.4, J = MONTH7, MONTHE)
    END IF
    WRITE (8, 6072) (PRC1M (J) * 25.4, J = MONTHB, MONTH6), &
            LP(1), (PRC1M (J) * 25.4, J = MONTH7, MONTHE)
    IF (LP(2) > 0) THEN
        DO K = LP(1) + 1, LP(2)
            IF (LSIN(K) > 0) THEN
                DO J = 1, 12
                    SUBINM (K, J) = MDAY(J) * SUBIN(K)
                end do
                IF (MND == 366) SUBINM(K, 2) = SUBINM(K, 2) + SUBIN(K)
                WRITE(8, 6077) (SUBINM(K, J) * 25.4, &
                        J=MONTHB, MONTH6), K, (SUBINM (K, J) * 25.4, J = MONTH7, MONTHE)
            END IF
            IF (LRIN(K) > 0) WRITE(8, 6065) (RCRIM(K, J) * 25.4, &
                    J=MONTHB, MONTH6), K, (RCRIM (K, J) * 25.4, J = MONTH7, MONTHE)
        end do
        IF (LD(2) > 0) THEN
            WRITE (8, 6062) (DRN2M (J) * 25.4, J=MONTHB, MONTH6), &
                    LD(2), (DRN2M (J) * 25.4, J = MONTH7, MONTHE)
            IF (LAYR(LD(2)) > 0) WRITE (8, 6067) (RCR2M (J) * 25.4, &
                    J=MONTHB, MONTH6), LD(2), (RCR2M(J) * 25.4, J = MONTH7, MONTHE)
        END IF
        WRITE (8, 6072) (PRC2M (J) * 25.4, J = MONTHB, MONTH6), &
                LP(2), (PRC2M (J) * 25.4, J = MONTH7, MONTHE)
    END IF
    IF (LP(3) > 0) THEN
        DO K = LP(2) + 1, LP(3)
            IF (LSIN(K) > 0) THEN
                DO J = 1, 12
                    SUBINM (K, J) = MDAY(J) * SUBIN(K)
                end do
                IF (MND == 366) SUBINM(K, 2) = SUBINM(K, 2) + SUBIN(K)
                WRITE(8, 6077) (SUBINM(K, J) * 25.4, &
                        J=MONTHB, MONTH6), K, (SUBINM (K, J) * 25.4, J = MONTH7, MONTHE)
            END IF
            IF (LRIN(K) > 0) WRITE(8, 6065) (RCRIM(K, J) * 25.4, &
                    J=MONTHB, MONTH6), K, (RCRIM (K, J) * 25.4, J = MONTH7, MONTHE)
        end do
        IF (LD(3) > 0) THEN
            WRITE (8, 6062) (DRN3M (J) * 25.4, J=MONTHB, MONTH6), &
                    LD(3), (DRN3M (J) * 25.4, J = MONTH7, MONTHE)
            IF (LAYR(LD(3)) > 0) WRITE (8, 6067) (RCR3M (J) * 25.4, &
                    J=MONTHB, MONTH6), LD(3), (RCR3M(J) * 25.4, J = MONTH7, MONTHE)
        END IF
        WRITE (8, 6072) (PRC3M (J) * 25.4, J = MONTHB, MONTH6), &
                LP(3), (PRC3M (J) * 25.4, J = MONTH7, MONTHE)
    END IF
    IF (LP(4) > 0) THEN
        DO K = LP(3) + 1, LP(4)
            IF (LSIN(K) > 0) THEN
                DO J = 1, 12
                    SUBINM (K, J) = MDAY(J) * SUBIN(K)
                end do
                IF (MND == 366) SUBINM(K, 2) = SUBINM(K, 2) + SUBIN(K)
                WRITE(8, 6077) (SUBINM(K, J) * 25.4, &
                        J=MONTHB, MONTH6), K, (SUBINM (K, J) * 25.4, J = MONTH7, MONTHE)
            END IF
            IF (LRIN(K) > 0) WRITE(8, 6065) (RCRIM(K, J) * 25.4, &
                    J=MONTHB, MONTH6), K, (RCRIM (K, J) * 25.4, J = MONTH7, MONTHE)
        end do
        IF (LD(4) > 0) THEN
            WRITE (8, 6062) (DRN4M (J) * 25.4, J=MONTHB, MONTH6), &
                    LD(4), (DRN4M (J) * 25.4, J = MONTH7, MONTHE)
            IF (LAYR(LD(4)) > 0) WRITE (8, 6067) (RCR4M (J) * 25.4, &
                    J=MONTHB, MONTH6), LD(4), (RCR4M(J) * 25.4, J = MONTH7, MONTHE)
        END IF
        WRITE (8, 6072) (PRC4M (J) * 25.4, J = MONTHB, MONTH6), &
                LP(4), (PRC4M (J) * 25.4, J = MONTH7, MONTHE)
    END IF
    IF (LP(5) > 0) THEN
        DO K = LP(4) + 1, LP(5)
            IF (LSIN(K) > 0) THEN
                DO J = 1, 12
                    SUBINM (K, J) = MDAY(J) * SUBIN(K)
                end do
                IF (MND == 366) SUBINM(K, 2) = SUBINM(K, 2) + SUBIN(K)
                WRITE(8, 6077) (SUBINM(K, J) * 25.4, &
                        J=MONTHB, MONTH6), K, (SUBINM (K, J) * 25.4, J = MONTH7, MONTHE)
            END IF
            IF (LRIN(K) > 0) WRITE(8, 6065) (RCRIM(K, J) * 25.4, &
                    J=MONTHB, MONTH6), K, (RCRIM (K, J) * 25.4, J = MONTH7, MONTHE)
        end do
        IF (LD(5) > 0) THEN
            WRITE (8, 6062) (DRN5M (J) * 25.4, J=MONTHB, MONTH6), &
                    LD(5), (DRN5M (J) * 25.4, J = MONTH7, MONTHE)
            IF (LAYR(LD(5)) > 0) WRITE (8, 6067) (RCR5M (J) * 25.4, &
                    J=MONTHB, MONTH6), LD(5), (RCR5M(J) * 25.4, J = MONTH7, MONTHE)
        END IF
        WRITE (8, 6072) (PRC5M (J) * 25.4, J = MONTHB, MONTH6), &
                LP(5), (PRC5M (J) * 25.4, J = MONTH7, MONTHE)
    END IF
    IF (LP(6) > 0) THEN
        DO K = LP(5) + 1, LP(6)
            IF (LSIN(K) > 0) THEN
                DO J = 1, 12
                    SUBINM (K, J) = MDAY(J) * SUBIN(K)
                end do
                IF (MND == 366) SUBINM(K, 2) = SUBINM(K, 2) + SUBIN(K)
                WRITE(8, 6077) (SUBINM(K, J) * 25.4, &
                        J=MONTHB, MONTH6), K, (SUBINM (K, J) * 25.4, J = MONTH7, MONTHE)
            END IF
            IF (LRIN(K) > 0) WRITE(8, 6065) (RCRIM(K, J) * 25.4, &
                    J=MONTHB, MONTH6), K, (RCRIM (K, J) * 25.4, J = MONTH7, MONTHE)
        end do
        WRITE (8, 6072) (PRC6M (J) * 25.4, J = MONTHB, MONTH6), &
                LP(6), (PRC6M (J), J = MONTH7, MONTHE)
    END IF
    IF (LP(1) > 0) THEN
        IF (LAYER (LP(1)) > 2) THEN
            WRITE (8, 6082)
            IF (LAYER(LP(1) - 1) <= 2) THEN
                WRITE (8, 6090) (HED1M (J) * 2.54, J = MONTHB, MONTH6), &
                        LP(1), (HED1M (J) * 2.54, J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED1 (J) * 2.54, J = 1, 6), &
                        LP(1), (SHED1 (J) * 2.54, J = 7, 12)
            ELSE
                WRITE (8, 6090) (HED1M (J) * 2.54, J = MONTHB, MONTH6), &
                        LP(1)-1, (HED1M (J) * 2.54, J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED1 (J) * 2.54, J = 1, 6), &
                        LP(1)-1, (SHED1 (J) * 2.54, J = 7, 12)
            END IF
        END IF
    END IF
    IF (LP(2) > 0) THEN
        IF (LAYER (LP(2)) > 2) THEN
            IF (LAYER(LP(2) - 1) <= 2) THEN
                WRITE (8, 6090) (HED2M (J) * 2.54, J = MONTHB, MONTH6), &
                        LP(2), (HED2M (J) * 2.54, J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED2 (J) * 2.54, J = 1, 6), &
                        LP(2), (SHED2 (J) * 2.54, J = 7, 12)
            ELSE
                WRITE (8, 6090) (HED2M (J) * 2.54, J = MONTHB, MONTH6), &
                        LP(2)-1, (HED2M (J) * 2.54, J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED2 (J) * 2.54, J = 1, 6), &
                        LP(2)-1, (SHED2 (J) * 2.54, J = 7, 12)
            END IF
        END IF
    END IF
    IF (LP(3) > 0) THEN
        IF (LAYER (LP(3)) > 2) THEN
            IF (LAYER(LP(3) - 1) <= 2) THEN
                WRITE (8, 6090) (HED3M (J) * 2.54, J = MONTHB, MONTH6), &
                        LP(3), (HED3M (J) * 2.54, J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED3 (J) * 2.54, J = 1, 6), &
                        LP(3), (SHED3 (J) * 2.54, J = 7, 12)
            ELSE
                WRITE (8, 6090) (HED3M (J) * 2.54, J = MONTHB, MONTH6), &
                        LP(3)-1, (HED3M (J) * 2.54, J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED3 (J) * 2.54, J = 1, 6), &
                        LP(3)-1, (SHED3 (J) * 2.54, J = 7, 12)
            END IF
        END IF
    END IF
    IF (LP(4) > 0) THEN
        IF (LAYER (LP(4)) > 2) THEN
            IF (LAYER(LP(4) - 1) <= 2) THEN
                WRITE (8, 6090) (HED4M (J) * 2.54, J = MONTHB, MONTH6), &
                        LP(4), (HED4M (J) * 2.54, J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED4 (J) * 2.54, J = 1, 6), &
                        LP(4), (SHED4 (J) * 2.54, J = 7, 12)
            ELSE
                WRITE (8, 6090) (HED4M (J) * 2.54, J = MONTHB, MONTH6), &
                        LP(4)-1, (HED4M (J) * 2.54, J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED4 (J) * 2.54, J = 1, 6), &
                        LP(4)-1, (SHED4 (J) * 2.54, J = 7, 12)
            END IF
        END IF
    END IF
    IF (LP(5) > 0) THEN
        IF (LAYER (LP(5)) > 2) THEN
            IF (LAYER(LP(5) - 1) <= 2) THEN
                WRITE (8, 6090) (HED5M (J) * 2.54, J = MONTHB, MONTH6), &
                        LP(5), (HED5M (J) * 2.54, J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED5 (J) * 2.54, J = 1, 6), &
                        LP(5), (SHED5 (J) * 2.54, J = 7, 12)
            ELSE
                WRITE (8, 6090) (HED5M (J) * 2.54, J = MONTHB, MONTH6), &
                        LP(5)-1, (HED5M (J) * 2.54, J = MONTH7, MONTHE)
                WRITE (8, 6100) (SHED5 (J) * 2.54, J = 1, 6), &
                        LP(5)-1, (SHED5 (J) * 2.54, J = 7, 12)
            END IF
        END IF
    END IF
    6012  FORMAT(1X/22X, 'MONTHLY TOTALS (MM) FOR YEAR', I5/&
            1X, 79('-'))
    6020 FORMAT(1X, /33X, 'JAN/JUL FEB/AUG MAR/SEP ', &
            'APR/OCT MAY/NOV JUN/DEC'/32X, 6(' -------'))
    6032   FORMAT(1X, /' PRECIPITATION         ', &
            10X, F5.1, 5(3X, F5.1)/30X, 6(3X, F5.1))
    6042   FORMAT(1X/' RUNOFF         ', 15X, &
            6(2X, F6.2)/31X, 6(2X, F6.2))
    6052   FORMAT(1X/' EVAPOTRANSPIRATION', 12X, 6(2X, F6.2)/1X, &
            5X, '        ', 17X, 6(2X, F6.2))
    6077 FORMAT(1X/' SUBSURFACE INFLOW INTO  ', 7X, 6F8.3/3X, &
            'LAYER', I3, '         ', 12X, 6F8.3)
    6065 FORMAT(1X, /' LATERAL DRAINAGE RECIRCULATED', 2X, 6F8.3/3X, &
            'INTO LAYER', I3, '         ', 7X, 6F8.3)
    6067 FORMAT(1X, /' LATERAL DRAINAGE RECIRCULATED', 2X, 6F8.3/3X, &
            'FROM LAYER', I3, '         ', 7X, 6F8.3)
    6062 FORMAT(1X, /' LATERAL DRAINAGE COLLECTED   ', 2X, 6F8.3/3X, &
            'FROM LAYER', I3, '         ', 7X, 6F8.3)
    6072 FORMAT(1X/' PERCOLATION/LEAKAGE THROUGH   ', 1X, 6F8.3/3X, &
            'LAYER', I3, '         ', 12X, 6F8.3)
    6082       FORMAT(1X//1X, 79('-')/22X, &
            'MONTHLY SUMMARIES FOR DAILY HEADS (CM)'/1X, 79('-')/)
    6090 FORMAT(1X/' AVERAGE DAILY HEAD ON ', 8X, 6F8.3/3X, &
            'TOP OF LAYER', I3, '  ', 11X, 6F8.3)
    6100 FORMAT(1X/' STD. DEVIATION OF DAILY ', 6X, 6(F8.3)/3X, &
            'HEAD ON TOP OF LAYER', I3, 5X, 6F8.3)
    RETURN
END
!
!
!      ************************* OUTYR ************************
!
!     SUBROUTINE OUTYR PRINTS THE ANNUAL TOTALS
!     FOR A YEAR OF SIMULATION RESULTS.
!
SUBROUTINE OUTYR
    !
    PARAMETER (MXYR = 100)
    !
    COMMON /BLK1/ IT4, IT7, IT13, IU4, IU7, IU13, IU11, IU10, IU8, &
            LMYR, IOD, IOM, IOA, IOS
    COMMON /BLK3/ PORO (20), FC (20), WP (20), RC (20), SW (20), &
            RS (20), XLAMBD (20), BUB (20), THICK (20), SLOPE (20), &
            XLENG (20), CON (20), SUBIN (20), RECIR (20), PHOLE (20), &
            DEFEC (20), TRANS (20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN(20), LSIN(20)
    COMMON /BLK5/ AREA, FRUNOF, CN2, OCN2, SSLOPE, SLENG, SMX
    COMMON /BLK9/ PINF, ES1T, SMELT, GMRO, ADDRUN, OLDSNO, SNO, WF(7), &
            SSNO
    COMMON /BLK12/ PRC1M (12), PRC2M (12), PRC3M (12), &
            PRC4M (12), PRC5M (12), PRC6M (12), DRN1M (12), &
            DRN2M (12), DRN3M (12), DRN4M (12), DRN5M (12), &
            PREM (12), RUNM (12), ETM (12), HED1M (12), &
            HED2M (12), HED3M (12), HED4M (12), HED5M (12), &
            RCRIM (20, 12), SUBINM (20, 12), RCR1M (12), &
            RCR2M (12), RCR3M (12), RCR4M (12), RCR5M (12)
    COMMON /BLK13/ PRC1A, PRC2A, PRC3A, PRC4A, PRC5A, PRC6A, &
            DRN1A, DRN2A, DRN3A, DRN4A, DRN5A, BAL, PREA, RUNA, ETA, &
            STOR, RCR1A, RCR2A, RCR3A, RCR4A, RCR5A, RCRIA(20), HED1A, &
            HED2A, HED3A, HED4A, HED5A, OSWULE, PSWULE, SUBINA(20)
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK24/ PRC1A2, PRC2A2, PRC3A2, PRC4A2, PRC5A2, PRC6A2, &
            DRN1A2, DRN2A2, DRN3A2, DRN4A2, DRN5A2, PREA2, RUNA2, ETA2, &
            STOR2, RCR1A2, RCR2A2, RCR3A2, RCR4A2, RCR5A2, RCRIA2(20), &
            HED1A2, HED2A2, HED3A2, HED4A2, HED5A2
    COMMON /BLK25/ PRC1A1, PRC2A1, PRC3A1, PRC4A1, PRC5A1, PRC6A1, &
            DRN1A1, DRN2A1, DRN3A1, DRN4A1, DRN5A1, PREA1, RUNA1, ETA1, &
            STOR1, RCR1A1, RCR2A1, RCR3A1, RCR4A1, RCR5A1, RCRIA1(20), &
            HED1A1, HED2A1, HED3A1, HED4A1, HED5A1
    COMMON /BLK28/ WE, XNEGHS, XLIQW, TINDEX, STORGE, EXLAG(4), TWE
    COMMON /BLK33/ JYEAR (MXYR)
    DIMENSION TRCRIA(20), FRCRIA(20), TSUBIA(20), FSUBIA(20)
    !
    HED1A = 0.0
    HED2A = 0.0
    HED3A = 0.0
    HED4A = 0.0
    HED5A = 0.0
    DO I = 1, 12
        HED1A = HED1A + HED1M(I)
        HED2A = HED2A + HED2M(I)
        HED3A = HED3A + HED3M(I)
        HED4A = HED4A + HED4M(I)
        HED5A = HED5A + HED5M(I)
    end do
    HED1A = HED1A / 12.0
    HED2A = HED2A / 12.0
    HED3A = HED3A / 12.0
    HED4A = HED4A / 12.0
    HED5A = HED5A / 12.0
    IF (IOS == 1) THEN
        PREA1 = PREA1 + PREA
        RUNA1 = RUNA1 + RUNA
        ETA1 = ETA1 + ETA
        DRN1A1 = DRN1A1 + DRN1A
        DRN2A1 = DRN2A1 + DRN2A
        DRN3A1 = DRN3A1 + DRN3A
        DRN4A1 = DRN4A1 + DRN4A
        DRN5A1 = DRN5A1 + DRN5A
        RCR1A1 = RCR1A1 + RCR1A
        RCR2A1 = RCR2A1 + RCR2A
        RCR3A1 = RCR3A1 + RCR3A
        RCR4A1 = RCR4A1 + RCR4A
        RCR5A1 = RCR5A1 + RCR5A
        HED1A1 = HED1A1 + HED1A
        HED2A1 = HED2A1 + HED2A
        HED3A1 = HED3A1 + HED3A
        HED4A1 = HED4A1 + HED4A
        HED5A1 = HED5A1 + HED5A
        PRC1A1 = PRC1A1 + PRC1A
        PRC2A1 = PRC2A1 + PRC2A
        PRC3A1 = PRC3A1 + PRC3A
        PRC4A1 = PRC4A1 + PRC4A
        PRC5A1 = PRC5A1 + PRC5A
        PRC6A1 = PRC6A1 + PRC6A
        STOR1 = STOR1 + STOR
        DO K = 1, 20
            RCRIA1(K) = RCRIA1(K) + RCRIA(K)
        end do
        PREA2 = PREA2 + PREA * PREA
        RUNA2 = RUNA2 + RUNA * RUNA
        ETA2 = ETA2 + ETA * ETA
        DRN1A2 = DRN1A2 + DRN1A * DRN1A
        DRN2A2 = DRN2A2 + DRN2A * DRN2A
        DRN3A2 = DRN3A2 + DRN3A * DRN3A
        DRN4A2 = DRN4A2 + DRN4A * DRN4A
        DRN5A2 = DRN5A2 + DRN5A * DRN5A
        RCR1A2 = RCR1A2 + RCR1A * RCR1A
        RCR2A2 = RCR2A2 + RCR2A * RCR2A
        RCR3A2 = RCR3A2 + RCR3A * RCR3A
        RCR4A2 = RCR4A2 + RCR4A * RCR4A
        RCR5A2 = RCR5A2 + RCR5A * RCR5A
        HED1A2 = HED1A2 + HED1A * HED1A
        HED2A2 = HED2A2 + HED2A * HED2A
        HED3A2 = HED3A2 + HED3A * HED3A
        HED4A2 = HED4A2 + HED4A * HED4A
        HED5A2 = HED5A2 + HED5A * HED5A
        PRC1A2 = PRC1A2 + PRC1A * PRC1A
        PRC2A2 = PRC2A2 + PRC2A * PRC2A
        PRC3A2 = PRC3A2 + PRC3A * PRC3A
        PRC4A2 = PRC4A2 + PRC4A * PRC4A
        PRC5A2 = PRC5A2 + PRC5A * PRC5A
        PRC6A2 = PRC6A2 + PRC6A * PRC6A
        STOR2 = STOR2 + STOR * STOR
        DO K = 1, 20
            RCRIA2(K) = RCRIA2(K) + RCRIA(K) * RCRIA(K)
        end do
    END IF
    IF (IOA == 0) RETURN
    !
    !     CONVERTS RESULTS FROM INCHES TO CU. FT. AND TO
    !     PERCENT OF THE ANNUAL PRECIPITATION AND PRINTS
    !     RUNOFF AND EVAPOTRANSPIRATION.
    !
    !     SNO = TWE / 25.4
    IF (IU8 == 1) THEN
        TPREA = AREA * 43560. * PREA / 12.0
        FPREA = 100.0
        TRUNA = AREA * 43560. * RUNA / 12.0
        FRUNA = RUNA * 100.0 / PREA
        TETA = AREA * 43560. * ETA / 12.0
        FETA = ETA * 100.0 / PREA
        TPRC6A = PRC6A * AREA * 43560. / 12.0
        FPRC6A = PRC6A * 100.0 / PREA
        TPRC5A = PRC5A * AREA * 43560. / 12.0
        FPRC5A = PRC5A * 100.0 / PREA
        TPRC4A = PRC4A * AREA * 43560. / 12.0
        FPRC4A = PRC4A * 100.0 / PREA
        TPRC3A = PRC3A * AREA * 43560. / 12.0
        FPRC3A = PRC3A * 100.0 / PREA
        TPRC2A = PRC2A * AREA * 43560. / 12.0
        FPRC2A = PRC2A * 100.0 / PREA
        TPRC1A = PRC1A * AREA * 43560. / 12.0
        FPRC1A = PRC1A * 100.0 / PREA
        TDRN5A = DRN5A * AREA * 43560. / 12.0
        FDRN5A = DRN5A * 100.0 / PREA
        TDRN4A = DRN4A * AREA * 43560. / 12.0
        FDRN4A = DRN4A * 100.0 / PREA
        TDRN3A = DRN3A * AREA * 43560. / 12.0
        FDRN3A = DRN3A * 100.0 / PREA
        TDRN2A = DRN2A * AREA * 43560. / 12.0
        FDRN2A = DRN2A * 100.0 / PREA
        TDRN1A = DRN1A * AREA * 43560. / 12.0
        FDRN1A = DRN1A * 100.0 / PREA
        TRCR5A = RCR5A * AREA * 43560. / 12.0
        FRCR5A = RCR5A * 100.0 / PREA
        TRCR4A = RCR4A * AREA * 43560. / 12.0
        FRCR4A = RCR4A * 100.0 / PREA
        TRCR3A = RCR3A * AREA * 43560. / 12.0
        FRCR3A = RCR3A * 100.0 / PREA
        TRCR2A = RCR2A * AREA * 43560. / 12.0
        FRCR2A = RCR2A * 100.0 / PREA
        TRCR1A = RCR1A * AREA * 43560. / 12.0
        FRCR1A = RCR1A * 100.0 / PREA
        TBAL = AREA * 43560. * BAL / 12.0
        FBAL = BAL * 100.0 / PREA
        TOSNO = AREA * 43560. * OLDSNO / 12.0
        FOSNO = OLDSNO * 100.0 / PREA
        TSNO = AREA * 43560. * SSNO / 12.0
        FSNO = SSNO * 100.0 / PREA
        TPSW = PSWULE * AREA * 43560. / 12.0
        TOSW = OSWULE * AREA * 43560. / 12.0
        TSTOR = AREA * 43560. * STOR / 12.0
        FSTOR = STOR * 100.0 / PREA
        !
        !    DETERMINES IF THE YEAR IS A LEAP YEAR.
        !
        MND = LEAP (JYEAR(IYR), NT)
        !
        DO K = 1, 20
            TRCRIA(K) = RCRIA(K) * AREA * 43560. / 12.0
            FRCRIA(K) = RCRIA(K) * 100.0 / PREA
            SUBINA(K) = MND * SUBIN(K)
            TSUBIA(K) = SUBINA(K) * AREA * 43560. / 12.0
            FSUBIA(K) = SUBINA(K) * 100.0 / PREA
        end do
    ELSE
        PREA = PREA * 25.4
        TPREA = AREA * 10.0 * PREA
        FPREA = 100.0
        RUNA = RUNA * 25.4
        TRUNA = AREA * 10.0 * RUNA
        FRUNA = RUNA * 100.0 / PREA
        ETA = ETA * 25.4
        TETA = AREA * 10.0 * ETA
        FETA = ETA * 100.0 / PREA
        PRC6A = PRC6A * 25.4
        TPRC6A = PRC6A * AREA * 10.0
        FPRC6A = PRC6A * 100.0 / PREA
        PRC5A = PRC5A * 25.4
        TPRC5A = PRC5A * AREA * 10.0
        FPRC5A = PRC5A * 100.0 / PREA
        PRC4A = PRC4A * 25.4
        TPRC4A = PRC4A * AREA * 10.0
        FPRC4A = PRC4A * 100.0 / PREA
        PRC3A = PRC3A * 25.4
        TPRC3A = PRC3A * AREA * 10.0
        FPRC3A = PRC3A * 100.0 / PREA
        PRC2A = PRC2A * 25.4
        TPRC2A = PRC2A * AREA * 10.0
        FPRC2A = PRC2A * 100.0 / PREA
        PRC1A = PRC1A * 25.4
        TPRC1A = PRC1A * AREA * 10.0
        FPRC1A = PRC1A * 100.0 / PREA
        DRN5A = DRN5A * 25.4
        TDRN5A = DRN5A * AREA * 10.0
        FDRN5A = DRN5A * 100.0 / PREA
        DRN4A = DRN4A * 25.4
        TDRN4A = DRN4A * AREA * 10.0
        FDRN4A = DRN4A * 100.0 / PREA
        DRN3A = DRN3A * 25.4
        TDRN3A = DRN3A * AREA * 10.0
        FDRN3A = DRN3A * 100.0 / PREA
        DRN2A = DRN2A * 25.4
        TDRN2A = DRN2A * AREA * 10.0
        FDRN2A = DRN2A * 100.0 / PREA
        DRN1A = DRN1A * 25.4
        TDRN1A = DRN1A * AREA * 10.0
        FDRN1A = DRN1A * 100.0 / PREA
        RCR5A = RCR5A * 25.4
        TRCR5A = RCR5A * AREA * 10.0
        FRCR5A = RCR5A * 100.0 / PREA
        RCR4A = RCR4A * 25.4
        TRCR4A = RCR4A * AREA * 10.0
        FRCR4A = RCR4A * 100.0 / PREA
        RCR3A = RCR3A * 25.4
        TRCR3A = RCR3A * AREA * 10.0
        FRCR3A = RCR3A * 100.0 / PREA
        RCR2A = RCR2A * 25.4
        TRCR2A = RCR2A * AREA * 10.0
        FRCR2A = RCR2A * 100.0 / PREA
        RCR1A = RCR1A * 25.4
        TRCR1A = RCR1A * AREA * 10.0
        FRCR1A = RCR1A * 100.0 / PREA
        BAL = BAL * 25.4
        TBAL = AREA * 10.0 * BAL
        FBAL = BAL * 100.0 / PREA
        OLDSNO = OLDSNO * 25.4
        TOSNO = AREA * 10.0 * OLDSNO
        FOSNO = OLDSNO * 100.0 / PREA
        SSNO = SSNO * 25.4
        TSNO = AREA * 10.0 * SSNO
        FSNO = SSNO * 100.0 / PREA
        PSWULE = PSWULE * 25.4
        TPSW = PSWULE * AREA * 10.0
        OSWULE = OSWULE * 25.4
        TOSW = OSWULE * AREA * 10.0
        STOR = STOR * 25.4
        TSTOR = AREA * 10.0 * STOR
        FSTOR = STOR * 100.0 / PREA
        HED1A = HED1A * 25.4
        HED2A = HED2A * 25.4
        HED3A = HED3A * 25.4
        HED4A = HED4A * 25.4
        HED5A = HED5A * 25.4
        DO K = 1, 20
            RCRIA(K) = RCRIA(K) * 25.4
            TRCRIA(K) = RCRIA(K) * AREA * 10.0
            FRCRIA(K) = RCRIA(K) * 100.0 / PREA
            !
            !    DETERMINES IF THE YEAR IS A LEAP YEAR.
            !
            !    COMPILER CORRECTIONS BELOW
            !     nyear not defined (see some 100 lines above)
            MND = LEAP (JYEAR(IYR), NT)
            !     COMPILER CORRECTIONS ABOVE
            !
            SUBINA(K) = MND * SUBIN(K)
            SUBINA(K) = SUBINA(K) * 25.4
            TSUBIA(K) = SUBINA(K) * AREA * 10.0
            FSUBIA(K) = SUBINA(K) * 100.0 / PREA
        end do
    END IF
    WRITE (8, 6000)
    !
    !     PRINTS HEADING AND PRECIPITATION IN INCHES,
    !     CU. FT. AND PERCENT OF THE ANNUAL PRECIPITATION.
    !
    IF (IU8 == 1) THEN
        WRITE (8, 6011) JYEAR (IYR), PREA, TPREA, FPREA
    ELSE
        WRITE (8, 6012) JYEAR (IYR), PREA, TPREA, FPREA
    END IF
    WRITE (8, 6021) RUNA, TRUNA, FRUNA
    WRITE (8, 6031) ETA, TETA, FETA
    IF (LP(1) > 0) THEN
        DO K = 1, LP(1)
            IF (LSIN(K) > 0) WRITE(8, 6111) K, SUBINA(K), TSUBIA(K), &
                    FSUBIA(K)
            IF (LRIN(K) > 0) WRITE(8, 6121) K, RCRIA(K), TRCRIA(K), &
                    FRCRIA(K)
        end do
        IF (LD(1) > 0) THEN
            WRITE(8, 6041) LD(1), DRN1A, TDRN1A, FDRN1A
            IF (LAYR(LD(1)) > 0) WRITE(8, 6131) LD(1), RCR1A, TRCR1A, &
                    FRCR1A
        END IF
        WRITE(8, 6051) LP(1), PRC1A, TPRC1A, FPRC1A
        IF (LAYER (LP(1)) > 2 .AND. LAYER (LP(1) - 1) <= 2)&
                WRITE(8, 6052)  LP(1), HED1A
        IF (LAYER (LP(1)) > 2 .AND. LAYER (LP(1) - 1) > 2)&
                WRITE(8, 6052)  (LP(1) - 1), HED1A
    END IF
    IF (LP(2) > 0) THEN
        DO K = LP(1) + 1, LP(2)
            IF (LSIN(K) > 0) WRITE(8, 6111) K, SUBINA(K), TSUBIA(K), &
                    FSUBIA(K)
            IF (LRIN(K) > 0) WRITE(8, 6121) K, RCRIA(K), TRCRIA(K), &
                    FRCRIA(K)
        end do
        IF (LD(2) > 0) THEN
            WRITE(8, 6041) LD(2), DRN2A, TDRN2A, FDRN2A
            IF (LAYR(LD(2)) > 0) WRITE(8, 6131) LD(2), RCR2A, TRCR2A, &
                    FRCR2A
        END IF
        WRITE(8, 6051) LP(2), PRC2A, TPRC2A, FPRC2A
        IF (LAYER (LP(2)) > 2 .AND. LAYER (LP(2) - 1) <= 2)&
                WRITE(8, 6052)  LP(2), HED2A
        IF (LAYER (LP(2)) > 2 .AND. LAYER (LP(2) - 1) > 2)&
                WRITE(8, 6052)  (LP(2) - 1), HED2A
    END IF
    IF (LP(3) > 0) THEN
        DO K = LP(2) + 1, LP(3)
            IF (LSIN(K) > 0) WRITE(8, 6111) K, SUBINA(K), TSUBIA(K), &
                    FSUBIA(K)
            IF (LRIN(K) > 0) WRITE(8, 6121) K, RCRIA(K), TRCRIA(K), &
                    FRCRIA(K)
        end do
        IF (LD(3) > 0) THEN
            WRITE(8, 6041) LD(3), DRN3A, TDRN3A, FDRN3A
            IF (LAYR(LD(3)) > 0) WRITE(8, 6131) LD(3), RCR3A, TRCR3A, &
                    FRCR3A
        END IF
        WRITE(8, 6051) LP(3), PRC3A, TPRC3A, FPRC3A
        IF (LAYER (LP(3)) > 2 .AND. LAYER (LP(3) - 1) <= 2)&
                WRITE(8, 6052)  LP(3), HED3A
        IF (LAYER (LP(3)) > 2 .AND. LAYER (LP(3) - 1) > 2)&
                WRITE(8, 6052)  (LP(3) - 1), HED3A
    END IF
    IF (LP(4) > 0) THEN
        DO K = LP(3) + 1, LP(4)
            IF (LSIN(K) > 0) WRITE(8, 6111) K, SUBINA(K), TSUBIA(K), &
                    FSUBIA(K)
            IF (LRIN(K) > 0) WRITE(8, 6121) K, RCRIA(K), TRCRIA(K), &
                    FRCRIA(K)
        end do
        IF (LD(4) > 0) THEN
            WRITE(8, 6041) LD(4), DRN4A, TDRN4A, FDRN4A
            IF (LAYR(LD(4)) > 0) WRITE(8, 6131) LD(4), RCR4A, TRCR4A, &
                    FRCR4A
        END IF
        WRITE(8, 6051) LP(4), PRC4A, TPRC4A, FPRC4A
        IF (LAYER (LP(4)) > 2 .AND. LAYER (LP(4) - 1) <= 2)&
                WRITE(8, 6052)  LP(4), HED4A
        IF (LAYER (LP(4)) > 2 .AND. LAYER (LP(4) - 1) > 2)&
                WRITE(8, 6052)  (LP(4) - 1), HED4A
    END IF
    IF (LP(5) > 0) THEN
        DO K = LP(4) + 1, LP(5)
            IF (LSIN(K) > 0) WRITE(8, 6111) K, SUBINA(K), TSUBIA(K), &
                    FSUBIA(K)
            IF (LRIN(K) > 0) WRITE(8, 6121) K, RCRIA(K), TRCRIA(K), &
                    FRCRIA(K)
        end do
        IF (LD(5) > 0) THEN
            WRITE(8, 6041) LD(5), DRN5A, TDRN5A, FDRN5A
            IF (LAYR(LD(5)) > 0) WRITE(8, 6131) LD(5), RCR5A, TRCR5A, &
                    FRCR5A
        END IF
        WRITE(8, 6051) LP(5), PRC5A, TPRC5A, FPRC5A
        IF (LAYER (LP(5)) > 2 .AND. LAYER (LP(5) - 1) <= 2)&
                WRITE(8, 6052)  LP(5), HED5A
        IF (LAYER (LP(5)) > 2 .AND. LAYER (LP(5) - 1) > 2)&
                WRITE(8, 6052)  (LP(5) - 1), HED5A
    END IF
    IF (LP(6) > 0) THEN
        DO K = LP(5) + 1, LP(6)
            IF (LSIN(K) > 0) WRITE(8, 6111) K, SUBINA(K), TSUBIA(K), &
                    FSUBIA(K)
            IF (LRIN(K) > 0) WRITE(8, 6121) K, RCRIA(K), TRCRIA(K), &
                    FRCRIA(K)
        end do
        WRITE(8, 6051) LP(6), PRC6A, TPRC6A, FPRC6A
    END IF
    WRITE (8, 6056) STOR, TSTOR, FSTOR
    WRITE (8, 6061) OSWULE, TOSW
    WRITE (8, 6071) PSWULE, TPSW
    WRITE (8, 6081) OLDSNO, TOSNO, FOSNO
    WRITE (8, 6091) SSNO, TSNO, FSNO
    IF (IU8 /= 1) THEN
        OSWULE = OSWULE / 25.4
        PSWULE = PSWULE / 25.4
        OLDSNO = OLDSNO / 25.4
        SSNO = SSNO / 25.4
    END IF
    WRITE (8, 6101) BAL, TBAL, FBAL
    WRITE (8, 6110)
    6000 FORMAT(1X/1X, 79('*'))
    6110 FORMAT(1X/1X, 79('*'), 3(/))
    6011 FORMAT(1X/27X, 'ANNUAL TOTALS FOR YEAR', I5/ 1X, 79('-')/&
            41X, ' INCHES ', 8X, ' CU. FEET ', 4X, 'PERCENT'/&
            41X, 8('-'), 8X, 10('-'), 4X, 7('-')/&
            3X, 'PRECIPITATION', 20X, F12.2, 6X, F13.3, 1X, F9.2)
    6012 FORMAT(1X/27X, 'ANNUAL TOTALS FOR YEAR', I5/ 1X, 79('-')/&
            41X, '   MM   ', 8X, 'CU. METERS', 4X, 'PERCENT'/&
            40X, 10('-'), 7X, 10('-'), 4X, 7('-')/&
            3X, 'PRECIPITATION', 20X, F12.2, 6X, F13.3, 1X, F9.2)
    6021 FORMAT(1X/3X, 'RUNOFF', 27X, F13.3, 5X, F13.3, 1X, F9.2)
    6031 FORMAT(1X/3X, 'EVAPOTRANSPIRATION', 15X, &
            F13.3, 5X, F13.3, 1X, F9.2)
    6041 FORMAT(1X/3X, 'DRAINAGE COLLECTED FROM LAYER', &
            I3, 1X, F14.4, 4X, F13.3, 1X, F9.2)
    6051 FORMAT(1X/3X, 'PERC./LEAKAGE THROUGH LAYER', I3, &
            3X, F16.6, 2X, F13.3, 1X, F9.2)
    6052 FORMAT(1X/3X, 'AVG. HEAD ON TOP OF LAYER', I3, 5X, &
            F14.4)
    6056 FORMAT(1X/3X, 'CHANGE IN WATER STORAGE', 10X, &
            F13.3, 5X, F13.3, 1X, F9.2)
    6061 FORMAT(1X/3X, 'SOIL WATER ', &
            'AT START OF YEAR', 4X, F15.3, 5X, F13.3)
    6071 FORMAT(1X/3X, 'SOIL WATER AT END ', &
            'OF YEAR', 6X, F15.3, 5X, F13.3)
    6081 FORMAT(1X/3X, 'SNOW WATER AT START ', &
            'OF YEAR', 4X, F15.3, 5X, F13.3, 1X, F9.2)
    6091 FORMAT(1X/3X, 'SNOW WATER AT END OF YEAR', &
            6X, F15.3, 5X, F13.3, 1X, F9.2)
    6101 FORMAT(1X/3X, 'ANNUAL WATER BUDGET BALANCE', 7X, F13.4, &
            4X, F13.3, 1X, F9.2)
    6111 FORMAT(1X/3X, 'SUBSURFACE INFLOW INTO LAYER', &
            I3, 2X, F16.6, 2X, F13.3, 1X, F9.2)
    6121 FORMAT(1X/3X, 'RECIRCULATION INTO LAYER', &
            I3, 6X, F16.6, 2X, F13.3, 1X, F9.2)
    6131 FORMAT(1X/3X, 'RECIRCULATION FROM LAYER', &
            I3, 6X, F16.6, 2X, F13.3, 1X, F9.2)
    RETURN
END
!
!
!      ********************* OUTAV *********************
!
SUBROUTINE OUTAV
    !
    !    SUBROUTINE OUTAV COMPUTES AND PRINTS THE AVERAGED
    !    RESULTS FOR THE SIMULATION.
    !
    PARAMETER (MXYR = 100)
    !
    COMMON /BLK1/ IT4, IT7, IT13, IU4, IU7, IU13, IU11, IU10, IU8, &
            LMYR, IOD, IOM, IOA, IOS
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN(20), LSIN(20)
    COMMON /BLK5/ AREA, FRUNOF, CN2, OCN2, SSLOPE, SLENG, SMX
    COMMON /BLK12/ PRC1M (12), PRC2M (12), PRC3M (12), &
            PRC4M (12), PRC5M (12), PRC6M (12), DRN1M (12), &
            DRN2M (12), DRN3M (12), DRN4M (12), DRN5M (12), &
            PREM (12), RUNM (12), ETM (12), HED1M (12), &
            HED2M (12), HED3M (12), HED4M (12), HED5M (12), &
            RCRIM (20, 12), SUBINM (20, 12), RCR1M (12), &
            RCR2M (12), RCR3M (12), RCR4M (12), RCR5M (12)
    COMMON /BLK13/ PRC1A, PRC2A, PRC3A, PRC4A, PRC5A, PRC6A, &
            DRN1A, DRN2A, DRN3A, DRN4A, DRN5A, BAL, PREA, RUNA, ETA, &
            STOR, RCR1A, RCR2A, RCR3A, RCR4A, RCR5A, RCRIA(20), HED1A, &
            HED2A, HED3A, HED4A, HED5A, OSWULE, PSWULE, SUBINA(20)
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK22/ PRC1M2 (12), PRC2M2 (12), PRC3M2 (12), &
            PRC4M2 (12), PRC5M2 (12), PRC6M2 (12), DRN1M2 (12), &
            DRN2M2 (12), DRN3M2 (12), DRN4M2 (12), DRN5M2 (12), &
            PREM2 (12), RUNM2 (12), ETM2 (12), HED1M2 (12), &
            HED2M2 (12), HED3M2 (12), HED4M2 (12), HED5M2 (12), &
            RCRIM2 (20, 12), RCR1M2 (12), RCR2M2 (12), &
            RCR3M2 (12), RCR4M2 (12), RCR5M2 (12)
    COMMON /BLK23/ PRC1M1 (12), PRC2M1 (12), PRC3M1 (12), &
            PRC4M1 (12), PRC5M1 (12), PRC6M1 (12), DRN1M1 (12), &
            DRN2M1 (12), DRN3M1 (12), DRN4M1 (12), DRN5M1 (12), &
            PREM1 (12), RUNM1 (12), ETM1 (12), HED1M1 (12), &
            HED2M1 (12), HED3M1 (12), HED4M1 (12), HED5M1 (12), &
            RCRIM1 (20, 12), RCR1M1 (12), RCR2M1 (12), &
            RCR3M1 (12), RCR4M1 (12), RCR5M1 (12)
    COMMON /BLK24/ PRC1A2, PRC2A2, PRC3A2, PRC4A2, PRC5A2, PRC6A2, &
            DRN1A2, DRN2A2, DRN3A2, DRN4A2, DRN5A2, PREA2, RUNA2, ETA2, &
            STOR2, RCR1A2, RCR2A2, RCR3A2, RCR4A2, RCR5A2, RCRIA2(20), &
            HED1A2, HED2A2, HED3A2, HED4A2, HED5A2
    COMMON /BLK25/ PRC1A1, PRC2A1, PRC3A1, PRC4A1, PRC5A1, PRC6A1, &
            DRN1A1, DRN2A1, DRN3A1, DRN4A1, DRN5A1, PREA1, RUNA1, ETA1, &
            STOR1, RCR1A1, RCR2A1, RCR3A1, RCR4A1, RCR5A1, RCRIA1(20), &
            HED1A1, HED2A1, HED3A1, HED4A1, HED5A1
    COMMON /BLK33/ JYEAR (MXYR)
    DIMENSION APRC1M (12), APRC2M (12), APRC3M (12), APRC4M (12), &
            APRC5M (12), APRC6M (12), ADRN1M (12), ADRN2M (12), &
            ADRN3M (12), ADRN4M (12), ADRN5M (12), APREM (12), &
            ARUNM (12), AETM (12), ARCR1M (12), ARCR2M (12), &
            ARCR3M (12), ARCR4M (12), ARCR5M (12), ARCRIM (20, 12), &
            AHED1M (12), AHED2M (12), AHED3M (12), AHED4M (12), AHED5M (12)
    DIMENSION SPRC1M (12), SPRC2M (12), SPRC3M (12), SPRC4M (12), &
            SPRC5M (12), SPRC6M (12), SDRN1M (12), SDRN2M (12), &
            SDRN3M (12), SDRN4M (12), SDRN5M (12), SPREM (12), &
            SRUNM (12), SETM (12), SRCR1M (12), SRCR2M (12), &
            SRCR3M (12), SRCR4M (12), SRCR5M (12), SRCRIM (20, 12), &
            SHED1M (12), SHED2M (12), SHED3M (12), SHED4M (12), SHED5M (12)
    DIMENSION ASUBIA (20), FASUBA (20), TASUBA (20), &
            ARCRIA (20), SRCRIA (20), FARCIA (20), TARCIA (20)
    !
    !    INITIALIZES STANDARD DEVIATIONS OF AVERAGED MONTHLY VARIABLES
    !
    DO M = 1, 12
        SPREM (M) = 0.0
        SRUNM (M) = 0.0
        SETM (M) = 0.0
        SPRC1M (M) = 0.0
        SPRC2M (M) = 0.0
        SPRC3M (M) = 0.0
        SPRC4M (M) = 0.0
        SPRC5M (M) = 0.0
        SPRC6M (M) = 0.0
        SDRN1M (M) = 0.0
        SDRN2M (M) = 0.0
        SDRN3M (M) = 0.0
        SDRN4M (M) = 0.0
        SDRN5M (M) = 0.0
        SHED1M (M) = 0.0
        SHED2M (M) = 0.0
        SHED3M (M) = 0.0
        SHED4M (M) = 0.0
        SHED5M (M) = 0.0
        SRCR1M (M) = 0.0
        SRCR2M (M) = 0.0
        SRCR3M (M) = 0.0
        SRCR4M (M) = 0.0
        SRCR5M (M) = 0.0
        DO K = 1, 20
            SRCRIM (K, M) = 0.0
        end do
    end do
    !
    !    COMPUTES AVERAGED MONTHLY VARIABLES IN INCHES.
    !
    DO M = 1, 12
        APREM (M) = PREM1 (M) / LMYR
        ARUNM (M) = RUNM1 (M) / LMYR
        AETM (M) = ETM1 (M) / LMYR
        APRC1M (M) = PRC1M1 (M) / LMYR
        APRC2M (M) = PRC2M1 (M) / LMYR
        APRC3M (M) = PRC3M1 (M) / LMYR
        APRC4M (M) = PRC4M1 (M) / LMYR
        APRC5M (M) = PRC5M1 (M) / LMYR
        APRC6M (M) = PRC6M1 (M) / LMYR
        ADRN1M (M) = DRN1M1 (M) / LMYR
        ADRN2M (M) = DRN2M1 (M) / LMYR
        ADRN3M (M) = DRN3M1 (M) / LMYR
        ADRN4M (M) = DRN4M1 (M) / LMYR
        ADRN5M (M) = DRN5M1 (M) / LMYR
        AHED1M (M) = HED1M1 (M) / LMYR
        AHED2M (M) = HED2M1 (M) / LMYR
        AHED3M (M) = HED3M1 (M) / LMYR
        AHED4M (M) = HED4M1 (M) / LMYR
        AHED5M (M) = HED5M1 (M) / LMYR
        ARCR1M (M) = RCR1M1 (M) / LMYR
        ARCR2M (M) = RCR2M1 (M) / LMYR
        ARCR3M (M) = RCR3M1 (M) / LMYR
        ARCR4M (M) = RCR4M1 (M) / LMYR
        ARCR5M (M) = RCR5M1 (M) / LMYR
        DO K = 1, 20
            ARCRIM (K, M) = RCRIM1 (K, M) / LMYR
        end do
    end do
    IF (LMYR > 1) THEN
        DO M = 1, 12
            SPREM (M) = PREM2 (M) - ((PREM1 (M) * PREM1 (M)) / LMYR)
            SRUNM (M) = RUNM2 (M) - ((RUNM1 (M) * RUNM1 (M)) / LMYR)
            SETM (M) = ETM2 (M) - ((ETM1 (M) * ETM1 (M)) / LMYR)
            SPRC1M (M) = PRC1M2 (M) - ((PRC1M1 (M) * PRC1M1 (M)) / LMYR)
            SPRC2M (M) = PRC2M2 (M) - ((PRC2M1 (M) * PRC2M1 (M)) / LMYR)
            SPRC3M (M) = PRC3M2 (M) - ((PRC3M1 (M) * PRC3M1 (M)) / LMYR)
            SPRC4M (M) = PRC4M2 (M) - ((PRC4M1 (M) * PRC4M1 (M)) / LMYR)
            SPRC5M (M) = PRC5M2 (M) - ((PRC5M1 (M) * PRC5M1 (M)) / LMYR)
            SPRC6M (M) = PRC6M2 (M) - ((PRC6M1 (M) * PRC6M1 (M)) / LMYR)
            SDRN1M (M) = DRN1M2 (M) - ((DRN1M1 (M) * DRN1M1 (M)) / LMYR)
            SDRN2M (M) = DRN2M2 (M) - ((DRN2M1 (M) * DRN2M1 (M)) / LMYR)
            SDRN3M (M) = DRN3M2 (M) - ((DRN3M1 (M) * DRN3M1 (M)) / LMYR)
            SDRN4M (M) = DRN4M2 (M) - ((DRN4M1 (M) * DRN4M1 (M)) / LMYR)
            SDRN5M (M) = DRN5M2 (M) - ((DRN5M1 (M) * DRN5M1 (M)) / LMYR)
            SHED1M (M) = HED1M2 (M) - ((HED1M1 (M) * HED1M1 (M)) / LMYR)
            SHED2M (M) = HED2M2 (M) - ((HED2M1 (M) * HED2M1 (M)) / LMYR)
            SHED3M (M) = HED3M2 (M) - ((HED3M1 (M) * HED3M1 (M)) / LMYR)
            SHED4M (M) = HED4M2 (M) - ((HED4M1 (M) * HED4M1 (M)) / LMYR)
            SHED5M (M) = HED5M2 (M) - ((HED5M1 (M) * HED5M1 (M)) / LMYR)
            SRCR1M (M) = RCR1M2 (M) - ((RCR1M1 (M) * RCR1M1 (M)) / LMYR)
            SRCR2M (M) = RCR2M2 (M) - ((RCR2M1 (M) * RCR2M1 (M)) / LMYR)
            SRCR3M (M) = RCR3M2 (M) - ((RCR3M1 (M) * RCR3M1 (M)) / LMYR)
            SRCR4M (M) = RCR4M2 (M) - ((RCR4M1 (M) * RCR4M1 (M)) / LMYR)
            SRCR5M (M) = RCR5M2 (M) - ((RCR5M1 (M) * RCR5M1 (M)) / LMYR)
            IF (SPREM (M)  < 0.0) SPREM (M) = 0.0
            IF (SRUNM (M)  < 0.0) SRUNM (M) = 0.0
            IF (SETM (M)   < 0.0) SETM (M) = 0.0
            IF (SPRC1M (M) < 0.0) SPRC1M (M) = 0.0
            IF (SPRC2M (M) < 0.0) SPRC2M (M) = 0.0
            IF (SPRC3M (M) < 0.0) SPRC3M (M) = 0.0
            IF (SPRC4M (M) < 0.0) SPRC4M (M) = 0.0
            IF (SPRC5M (M) < 0.0) SPRC5M (M) = 0.0
            IF (SPRC6M (M) < 0.0) SPRC6M (M) = 0.0
            IF (SDRN1M (M) < 0.0) SDRN1M (M) = 0.0
            IF (SDRN2M (M) < 0.0) SDRN2M (M) = 0.0
            IF (SDRN3M (M) < 0.0) SDRN3M (M) = 0.0
            IF (SDRN4M (M) < 0.0) SDRN4M (M) = 0.0
            IF (SDRN5M (M) < 0.0) SDRN5M (M) = 0.0
            IF (SHED1M (M) < 0.0) SHED1M (M) = 0.0
            IF (SHED2M (M) < 0.0) SHED2M (M) = 0.0
            IF (SHED3M (M) < 0.0) SHED3M (M) = 0.0
            IF (SHED4M (M) < 0.0) SHED4M (M) = 0.0
            IF (SHED5M (M) < 0.0) SHED5M (M) = 0.0
            IF (SRCR1M (M) < 0.0) SRCR1M (M) = 0.0
            IF (SRCR2M (M) < 0.0) SRCR2M (M) = 0.0
            IF (SRCR3M (M) < 0.0) SRCR3M (M) = 0.0
            IF (SRCR4M (M) < 0.0) SRCR4M (M) = 0.0
            IF (SRCR5M (M) < 0.0) SRCR5M (M) = 0.0
            DO K = 1, 20
                SRCRIM(K, M) = RCRIM2(K, M) - ((RCRIM1(K, M) * RCRIM1(K, M)) / LMYR)
                IF (SRCRIM (K, M) < 0.0) SRCRIM (K, M) = 0.0
                SRCRIM(K, M) = SQRT(SRCRIM(K, M) / (LMYR - 1))
            end do
            SPREM (M) = SQRT(SPREM (M) / (LMYR - 1))
            SRUNM (M) = SQRT(SRUNM (M) / (LMYR - 1))
            SETM (M) = SQRT(SETM (M) / (LMYR - 1))
            SPRC1M (M) = SQRT(SPRC1M (M) / (LMYR - 1))
            SPRC2M (M) = SQRT(SPRC2M (M) / (LMYR - 1))
            SPRC3M (M) = SQRT(SPRC3M (M) / (LMYR - 1))
            SPRC4M (M) = SQRT(SPRC4M (M) / (LMYR - 1))
            SPRC5M (M) = SQRT(SPRC5M (M) / (LMYR - 1))
            SPRC6M (M) = SQRT(SPRC6M (M) / (LMYR - 1))
            SDRN1M (M) = SQRT(SDRN1M (M) / (LMYR - 1))
            SDRN2M (M) = SQRT(SDRN2M (M) / (LMYR - 1))
            SDRN3M (M) = SQRT(SDRN3M (M) / (LMYR - 1))
            SDRN4M (M) = SQRT(SDRN4M (M) / (LMYR - 1))
            SDRN5M (M) = SQRT(SDRN5M (M) / (LMYR - 1))
            SHED1M (M) = SQRT(SHED1M (M) / (LMYR - 1))
            SHED2M (M) = SQRT(SHED2M (M) / (LMYR - 1))
            SHED3M (M) = SQRT(SHED3M (M) / (LMYR - 1))
            SHED4M (M) = SQRT(SHED4M (M) / (LMYR - 1))
            SHED5M (M) = SQRT(SHED5M (M) / (LMYR - 1))
            SRCR1M (M) = SQRT(SRCR1M (M) / (LMYR - 1))
            SRCR2M (M) = SQRT(SRCR2M (M) / (LMYR - 1))
            SRCR3M (M) = SQRT(SRCR3M (M) / (LMYR - 1))
            SRCR4M (M) = SQRT(SRCR4M (M) / (LMYR - 1))
            SRCR5M (M) = SQRT(SRCR5M (M) / (LMYR - 1))
        end do
    END IF
    !
    !    INITIALIZES STANDARD DEVIATIONS OF AVERAGED ANNUAL VARIABLES.
    !
    SPREA = 0.0
    SRUNA = 0.0
    SETA = 0.0
    SSTOR = 0.0
    SPRC1A = 0.0
    SPRC2A = 0.0
    SPRC3A = 0.0
    SPRC4A = 0.0
    SPRC5A = 0.0
    SPRC6A = 0.0
    SDRN1A = 0.0
    SDRN2A = 0.0
    SDRN3A = 0.0
    SDRN4A = 0.0
    SDRN5A = 0.0
    SHED1A = 0.0
    SHED2A = 0.0
    SHED3A = 0.0
    SHED4A = 0.0
    SHED5A = 0.0
    SRCR1A = 0.0
    SRCR2A = 0.0
    SRCR3A = 0.0
    SRCR4A = 0.0
    SRCR5A = 0.0
    DO K = 1, 20
        ASUBIA (K) = SUBINA (K)
        SRCRIA (K) = 0.0
    end do
    !
    !    COMPUTES AVERAGED ANNUAL VARIABLES IN INCHES.
    !
    APREA = PREA1 / LMYR
    ARUNA = RUNA1 / LMYR
    AETA = ETA1 / LMYR
    ASTOR = STOR1 / LMYR
    APRC1A = PRC1A1 / LMYR
    APRC2A = PRC2A1 / LMYR
    APRC3A = PRC3A1 / LMYR
    APRC4A = PRC4A1 / LMYR
    APRC5A = PRC5A1 / LMYR
    APRC6A = PRC6A1 / LMYR
    ADRN1A = DRN1A1 / LMYR
    ADRN2A = DRN2A1 / LMYR
    ADRN3A = DRN3A1 / LMYR
    ADRN4A = DRN4A1 / LMYR
    ADRN5A = DRN5A1 / LMYR
    ARCR1A = RCR1A1 / LMYR
    ARCR2A = RCR2A1 / LMYR
    ARCR3A = RCR3A1 / LMYR
    ARCR4A = RCR4A1 / LMYR
    ARCR5A = RCR5A1 / LMYR
    AHED1A = HED1A1 / LMYR
    AHED2A = HED2A1 / LMYR
    AHED3A = HED3A1 / LMYR
    AHED4A = HED4A1 / LMYR
    AHED5A = HED5A1 / LMYR
    DO K = 1, 20
        ARCRIA(K) = RCRIA1(K) / LMYR
    end do
    IF (LMYR > 1) THEN
        SPREA = PREA2 - ((PREA1 * PREA1) / LMYR)
        SRUNA = RUNA2 - ((RUNA1 * RUNA1) / LMYR)
        SETA = ETA2 - ((ETA1 * ETA1) / LMYR)
        SSTOR = STOR2 - ((STOR1 * STOR1) / LMYR)
        SPRC1A = PRC1A2 - ((PRC1A1 * PRC1A1) / LMYR)
        SPRC2A = PRC2A2 - ((PRC2A1 * PRC2A1) / LMYR)
        SPRC3A = PRC3A2 - ((PRC3A1 * PRC3A1) / LMYR)
        SPRC4A = PRC4A2 - ((PRC4A1 * PRC4A1) / LMYR)
        SPRC5A = PRC5A2 - ((PRC5A1 * PRC5A1) / LMYR)
        SPRC6A = PRC6A2 - ((PRC6A1 * PRC6A1) / LMYR)
        SDRN1A = DRN1A2 - ((DRN1A1 * DRN1A1) / LMYR)
        SDRN2A = DRN2A2 - ((DRN2A1 * DRN2A1) / LMYR)
        SDRN3A = DRN3A2 - ((DRN3A1 * DRN3A1) / LMYR)
        SDRN4A = DRN4A2 - ((DRN4A1 * DRN4A1) / LMYR)
        SDRN5A = DRN5A2 - ((DRN5A1 * DRN5A1) / LMYR)
        SRCR1A = RCR1A2 - ((RCR1A1 * RCR1A1) / LMYR)
        SRCR2A = RCR2A2 - ((RCR2A1 * RCR2A1) / LMYR)
        SRCR3A = RCR3A2 - ((RCR3A1 * RCR3A1) / LMYR)
        SRCR4A = RCR4A2 - ((RCR4A1 * RCR4A1) / LMYR)
        SRCR5A = RCR5A2 - ((RCR5A1 * RCR5A1) / LMYR)
        SHED1A = HED1A2 - ((HED1A1 * HED1A1) / LMYR)
        SHED2A = HED2A2 - ((HED2A1 * HED2A1) / LMYR)
        SHED3A = HED3A2 - ((HED3A1 * HED3A1) / LMYR)
        SHED4A = HED4A2 - ((HED4A1 * HED4A1) / LMYR)
        SHED5A = HED5A2 - ((HED5A1 * HED5A1) / LMYR)
        IF (SPREA  < 0.0) SPREA = 0.0
        IF (SRUNA  < 0.0) SRUNA = 0.0
        IF (SETA   < 0.0) SETA = 0.0
        IF (SSTOR  < 0.0) SSTOR = 0.0
        IF (SPRC1A < 0.0) SPRC1A = 0.0
        IF (SPRC2A < 0.0) SPRC2A = 0.0
        IF (SPRC3A < 0.0) SPRC3A = 0.0
        IF (SPRC4A < 0.0) SPRC4A = 0.0
        IF (SPRC5A < 0.0) SPRC5A = 0.0
        IF (SPRC6A < 0.0) SPRC6A = 0.0
        IF (SDRN1A < 0.0) SDRN1A = 0.0
        IF (SDRN2A < 0.0) SDRN2A = 0.0
        IF (SDRN3A < 0.0) SDRN3A = 0.0
        IF (SDRN4A < 0.0) SDRN4A = 0.0
        IF (SDRN5A < 0.0) SDRN5A = 0.0
        IF (SRCR1A < 0.0) SRCR1A = 0.0
        IF (SRCR2A < 0.0) SRCR2A = 0.0
        IF (SRCR3A < 0.0) SRCR3A = 0.0
        IF (SRCR4A < 0.0) SRCR4A = 0.0
        IF (SRCR5A < 0.0) SRCR5A = 0.0
        IF (SHED1A < 0.0) SHED1A = 0.0
        IF (SHED2A < 0.0) SHED2A = 0.0
        IF (SHED3A < 0.0) SHED3A = 0.0
        IF (SHED4A < 0.0) SHED4A = 0.0
        IF (SHED5A < 0.0) SHED5A = 0.0
        DO K = 1, 20
            SRCRIA (K) = RCRIA2 (K) - ((RCRIA1 (K) * RCRIA1 (K)) / LMYR)
            IF (SRCRIA (K) < 0.0) SRCRIA (K) = 0.0
            SRCRIA (K) = SQRT(SRCRIA (K) / (LMYR - 1))
        end do
        SPREA = SQRT(SPREA / (LMYR - 1))
        SRUNA = SQRT(SRUNA / (LMYR - 1))
        SETA = SQRT(SETA / (LMYR - 1))
        SSTOR = SQRT(SSTOR / (LMYR - 1))
        SPRC1A = SQRT(SPRC1A / (LMYR - 1))
        SPRC2A = SQRT(SPRC2A / (LMYR - 1))
        SPRC3A = SQRT(SPRC3A / (LMYR - 1))
        SPRC4A = SQRT(SPRC4A / (LMYR - 1))
        SPRC5A = SQRT(SPRC5A / (LMYR - 1))
        SPRC6A = SQRT(SPRC6A / (LMYR - 1))
        SDRN1A = SQRT(SDRN1A / (LMYR - 1))
        SDRN2A = SQRT(SDRN2A / (LMYR - 1))
        SDRN3A = SQRT(SDRN3A / (LMYR - 1))
        SDRN4A = SQRT(SDRN4A / (LMYR - 1))
        SDRN5A = SQRT(SDRN5A / (LMYR - 1))
        SRCR1A = SQRT(SRCR1A / (LMYR - 1))
        SRCR2A = SQRT(SRCR2A / (LMYR - 1))
        SRCR3A = SQRT(SRCR3A / (LMYR - 1))
        SRCR4A = SQRT(SRCR4A / (LMYR - 1))
        SRCR5A = SQRT(SRCR5A / (LMYR - 1))
        SHED1A = SQRT(SHED1A / (LMYR - 1))
        SHED2A = SQRT(SHED2A / (LMYR - 1))
        SHED3A = SQRT(SHED3A / (LMYR - 1))
        SHED4A = SQRT(SHED4A / (LMYR - 1))
        SHED5A = SQRT(SHED5A / (LMYR - 1))
    END IF
    !
    !    COMPUTES AVERAGED ANNUAL VARIABLES IN CU. FT. AND IN
    !    PERCENT OF AVERAGE ANNUAL PRECIPITATION.
    !
    IF(IU8 == 1) THEN
        TAPREA = APREA * AREA * 43560. / 12.0
        FAPREA = 100.0
        TARUNA = ARUNA * AREA * 43560. / 12.0
        FARUNA = ARUNA * 100.0 / APREA
        TAETA = AETA * AREA * 43560. / 12.0
        FAETA = AETA * 100.0 / APREA
        TASTOR = ASTOR * AREA * 43560. / 12.0
        FASTOR = ASTOR * 100.0 / APREA
        TAPC6A = APRC6A * AREA * 43560. / 12.0
        FAPC6A = APRC6A * 100.0 / APREA
        TAPC5A = APRC5A * AREA * 43560. / 12.0
        FAPC5A = APRC5A * 100.0 / APREA
        TAPC4A = APRC4A * AREA * 43560. / 12.0
        FAPC4A = APRC4A * 100.0 / APREA
        TAPC3A = APRC3A * AREA * 43560. / 12.0
        FAPC3A = APRC3A * 100.0 / APREA
        TAPC2A = APRC2A * AREA * 43560. / 12.0
        FAPC2A = APRC2A * 100.0 / APREA
        TAPC1A = APRC1A * AREA * 43560. / 12.0
        FAPC1A = APRC1A * 100.0 / APREA
        TADN5A = ADRN5A * AREA * 43560. / 12.0
        FADN5A = ADRN5A * 100.0 / APREA
        TADN4A = ADRN4A * AREA * 43560. / 12.0
        FADN4A = ADRN4A * 100.0 / APREA
        TADN3A = ADRN3A * AREA * 43560. / 12.0
        FADN3A = ADRN3A * 100.0 / APREA
        TADN2A = ADRN2A * AREA * 43560. / 12.0
        FADN2A = ADRN2A * 100.0 / APREA
        TADN1A = ADRN1A * AREA * 43560. / 12.0
        FADN1A = ADRN1A * 100.0 / APREA
        TARC5A = ARCR5A * AREA * 43560. / 12.0
        FARC5A = ARCR5A * 100.0 / APREA
        TARC4A = ARCR4A * AREA * 43560. / 12.0
        FARC4A = ARCR4A * 100.0 / APREA
        TARC3A = ARCR3A * AREA * 43560. / 12.0
        FARC3A = ARCR3A * 100.0 / APREA
        TARC2A = ARCR2A * AREA * 43560. / 12.0
        FARC2A = ARCR2A * 100.0 / APREA
        TARC1A = ARCR1A * AREA * 43560. / 12.0
        FARC1A = ARCR1A * 100.0 / APREA
        DO K = 1, 20
            TASUBA (K) = ASUBIA (K) * AREA * 43560. / 12.0
            TARCIA (K) = ARCRIA (K) * AREA * 43560. / 12.0
            FASUBA (K) = ASUBIA (K) * 100.0 / APREA
            FARCIA (K) = ARCRIA (K) * 100.0 / APREA
        end do
    ELSE
        DO M = 1, 12
            APREM (M) = APREM (M) * 25.4
            ARUNM (M) = ARUNM (M) * 25.4
            AETM (M) = AETM (M) * 25.4
            APRC1M (M) = APRC1M (M) * 25.4
            APRC2M (M) = APRC2M (M) * 25.4
            APRC3M (M) = APRC3M (M) * 25.4
            APRC4M (M) = APRC4M (M) * 25.4
            APRC5M (M) = APRC5M (M) * 25.4
            APRC6M (M) = APRC6M (M) * 25.4
            ADRN1M (M) = ADRN1M (M) * 25.4
            ADRN2M (M) = ADRN2M (M) * 25.4
            ADRN3M (M) = ADRN3M (M) * 25.4
            ADRN4M (M) = ADRN4M (M) * 25.4
            ADRN5M (M) = ADRN5M (M) * 25.4
            AHED1M (M) = AHED1M (M) * 2.54
            AHED2M (M) = AHED2M (M) * 2.54
            AHED3M (M) = AHED3M (M) * 2.54
            AHED4M (M) = AHED4M (M) * 2.54
            AHED5M (M) = AHED5M (M) * 2.54
            ARCR1M (M) = ARCR1M (M) * 25.4
            ARCR2M (M) = ARCR2M (M) * 25.4
            ARCR3M (M) = ARCR3M (M) * 25.4
            ARCR4M (M) = ARCR4M (M) * 25.4
            ARCR5M (M) = ARCR5M (M) * 25.4
            DO K = 1, 20
                SUBINM(K, M) = SUBINM(K, M) * 25.4
                ARCRIM(K, M) = ARCRIM(K, M) * 25.4
                SRCRIM(K, M) = SRCRIM(K, M) * 25.4
            end do
            SPREM (M) = SPREM (M) * 25.4
            SRUNM (M) = SRUNM (M) * 25.4
            SETM (M) = SETM (M) * 25.4
            SPRC1M (M) = SPRC1M (M) * 25.4
            SPRC2M (M) = SPRC2M (M) * 25.4
            SPRC3M (M) = SPRC3M (M) * 25.4
            SPRC4M (M) = SPRC4M (M) * 25.4
            SPRC5M (M) = SPRC5M (M) * 25.4
            SPRC6M (M) = SPRC6M (M) * 25.4
            SDRN1M (M) = SDRN1M (M) * 25.4
            SDRN2M (M) = SDRN2M (M) * 25.4
            SDRN3M (M) = SDRN3M (M) * 25.4
            SDRN4M (M) = SDRN4M (M) * 25.4
            SDRN5M (M) = SDRN5M (M) * 25.4
            SHED1M (M) = SHED1M (M) * 2.54
            SHED2M (M) = SHED2M (M) * 2.54
            SHED3M (M) = SHED3M (M) * 2.54
            SHED4M (M) = SHED4M (M) * 2.54
            SHED5M (M) = SHED5M (M) * 2.54
            SRCR1M (M) = SRCR1M (M) * 25.4
            SRCR2M (M) = SRCR2M (M) * 25.4
            SRCR3M (M) = SRCR3M (M) * 25.4
            SRCR4M (M) = SRCR4M (M) * 25.4
            SRCR5M (M) = SRCR5M (M) * 25.4
        end do
        APREA = APREA * 25.4
        ARUNA = ARUNA * 25.4
        AETA = AETA * 25.4
        ASTOR = ASTOR * 25.4
        APRC1A = APRC1A * 25.4
        APRC2A = APRC2A * 25.4
        APRC3A = APRC3A * 25.4
        APRC4A = APRC4A * 25.4
        APRC5A = APRC5A * 25.4
        APRC6A = APRC6A * 25.4
        ADRN1A = ADRN1A * 25.4
        ADRN2A = ADRN2A * 25.4
        ADRN3A = ADRN3A * 25.4
        ADRN4A = ADRN4A * 25.4
        ADRN5A = ADRN5A * 25.4
        ARCR1A = ARCR1A * 25.4
        ARCR2A = ARCR2A * 25.4
        ARCR3A = ARCR3A * 25.4
        ARCR4A = ARCR4A * 25.4
        ARCR5A = ARCR5A * 25.4
        AHED1A = AHED1A * 25.4
        AHED2A = AHED2A * 25.4
        AHED3A = AHED3A * 25.4
        AHED4A = AHED4A * 25.4
        AHED5A = AHED5A
        DO K = 1, 20
            ARCRIA (K) = ARCRIA (K) * 25.4
            SRCRIA (K) = SRCRIA (K) * 25.4
        end do
        SPREA = SPREA * 25.4
        SRUNA = SRUNA * 25.4
        SETA = SETA * 25.4
        SPRC1A = SPRC1A * 25.4
        SPRC2A = SPRC2A * 25.4
        SPRC3A = SPRC3A * 25.4
        SPRC4A = SPRC4A * 25.4
        SPRC5A = SPRC5A * 25.4
        SPRC6A = SPRC6A * 25.4
        SDRN1A = SDRN1A * 25.4
        SDRN2A = SDRN2A * 25.4
        SDRN3A = SDRN3A * 25.4
        SDRN4A = SDRN4A * 25.4
        SDRN5A = SDRN5A * 25.4
        SRCR1A = SRCR1A * 25.4
        SRCR2A = SRCR2A * 25.4
        SRCR3A = SRCR3A * 25.4
        SRCR4A = SRCR4A * 25.4
        SRCR5A = SRCR5A * 25.4
        SHED1A = SHED1A * 25.4
        SHED2A = SHED2A * 25.4
        SHED3A = SHED3A * 25.4
        SHED4A = SHED4A * 25.4
        SHED5A = SHED5A * 25.4
        TAPREA = APREA * AREA * 10.0
        FAPREA = 100.0
        TARUNA = ARUNA * AREA * 10.0
        FARUNA = ARUNA * 100.0 / APREA
        TAETA = AETA * AREA * 10.0
        FAETA = AETA * 100.0 / APREA
        TASTOR = ASTOR * AREA * 10.0
        FASTOR = ASTOR * 100.0 / APREA
        TAPC6A = APRC6A * AREA * 10.0
        FAPC6A = APRC6A * 100.0 / APREA
        TAPC5A = APRC5A * AREA * 10.0
        FAPC5A = APRC5A * 100.0 / APREA
        TAPC4A = APRC4A * AREA * 10.0
        FAPC4A = APRC4A * 100.0 / APREA
        TAPC3A = APRC3A * AREA * 10.0
        FAPC3A = APRC3A * 100.0 / APREA
        TAPC2A = APRC2A * AREA * 10.0
        FAPC2A = APRC2A * 100.0 / APREA
        TAPC1A = APRC1A * AREA * 10.0
        FAPC1A = APRC1A * 100.0 / APREA
        TADN5A = ADRN5A * AREA * 10.0
        FADN5A = ADRN5A * 100.0 / APREA
        TADN4A = ADRN4A * AREA * 10.0
        FADN4A = ADRN4A * 100.0 / APREA
        TADN3A = ADRN3A * AREA * 10.0
        FADN3A = ADRN3A * 100.0 / APREA
        TADN2A = ADRN2A * AREA * 10.0
        FADN2A = ADRN2A * 100.0 / APREA
        TADN1A = ADRN1A * AREA * 10.0
        FADN1A = ADRN1A * 100.0 / APREA
        TARC5A = ARCR5A * AREA * 10.0
        FARC5A = ARCR5A * 100.0 / APREA
        TARC4A = ARCR4A * AREA * 10.0
        FARC4A = ARCR4A * 100.0 / APREA
        TARC3A = ARCR3A * AREA * 10.0
        FARC3A = ARCR3A * 100.0 / APREA
        TARC2A = ARCR2A * AREA * 10.0
        FARC2A = ARCR2A * 100.0 / APREA
        TARC1A = ARCR1A * AREA * 10.0
        FARC1A = ARCR1A * 100.0 / APREA
        DO K = 1, 20
            TASUBA (K) = ASUBIA (K) * AREA * 10.0
            TARCIA (K) = ARCRIA (K) * AREA * 10.0
            FASUBA (K) = ASUBIA (K) * 100.0 / APREA
            FARCIA (K) = ARCRIA (K) * 100.0 / APREA
        end do
    END IF
    !
    !    PRINTS HEADING FOR AVERAGE MONTHLY RESULTS AND PRINTS
    !    AVERAGED MONTHLY PRECIPITATION, RUNOFF AND
    !    EVAPOTRANSPIRATION.
    !
    WRITE (8, 6000)
    IF (IU8 == 1) THEN
        WRITE (8, 6011) JYEAR (1), JYEAR (LMYR)
    ELSE
        WRITE (8, 6012) JYEAR (1), JYEAR (LMYR)
    END IF
    WRITE (8, 6020)
    WRITE (8, 6030) (APREM (J), J = 1, 12)
    WRITE (8, 6040) (SPREM (J), J = 1, 12)
    WRITE (8, 6050) (ARUNM (J), J = 1, 12)
    WRITE (8, 6060) (SRUNM (J), J = 1, 12)
    WRITE (8, 6070) (AETM (J), J = 1, 12)
    WRITE (8, 6080) (SETM (J), J = 1, 12)
    DO K = 1, LP(1)
        IF (LSIN(K) > 0) WRITE(8, 6170) K, (SUBINM(K, J), J = 1, 12)
        IF (LRIN(K) > 0) WRITE(8, 6150) K, (ARCRIM(K, J), J = 1, 12)
        IF (LRIN(K) > 0) WRITE(8, 6160) (SRCRIM(K, J), J = 1, 12)
    end do
    IF (LD(1) > 0) THEN
        WRITE (8, 6090) LD(1), (ADRN1M (J), J = 1, 12)
        WRITE (8, 6100) (SDRN1M (J), J = 1, 12)
        IF (LAYR(LD(1)) > 0) WRITE (8, 6130) LD(1), (ARCR1M (J), &
                J = 1, 12)
        IF (LAYR(LD(1)) > 0) WRITE (8, 6140) (SRCR1M(J), J = 1, 12)
    END IF
    WRITE (8, 6110) LP(1), (APRC1M (J), J = 1, 12)
    WRITE (8, 6120) (SPRC1M (J), J = 1, 12)
    IF (LP(2) > 0) THEN
        DO K = LP(1) + 1, LP(2)
            IF (LSIN(K) > 0) WRITE(8, 6170) K, (SUBINM(K, J), J = 1, 12)
            IF (LRIN(K) > 0) WRITE(8, 6150) K, (ARCRIM(K, J), J = 1, 12)
            IF (LRIN(K) > 0) WRITE(8, 6160) (SRCRIM(K, J), J = 1, 12)
        end do
        IF (LD(2) > 0) THEN
            WRITE (8, 6090) LD(2), (ADRN2M (J), J = 1, 12)
            WRITE (8, 6100) (SDRN2M (J), J = 1, 12)
            IF (LAYR(LD(2)) > 0) WRITE (8, 6130) LD(2), (ARCR2M (J), &
                    J = 1, 12)
            IF (LAYR(LD(2)) > 0) WRITE (8, 6140) (SRCR2M(J), J = 1, 12)
        END IF
        WRITE (8, 6110) LP(2), (APRC2M (J), J = 1, 12)
        WRITE (8, 6120) (SPRC2M (J), J = 1, 12)
    END IF
    IF (LP(3) > 0) THEN
        DO K = LP(2) + 1, LP(3)
            IF (LSIN(K) > 0) WRITE(8, 6170) K, (SUBINM(K, J), J = 1, 12)
            IF (LRIN(K) > 0) WRITE(8, 6150) K, (ARCRIM(K, J), J = 1, 12)
            IF (LRIN(K) > 0) WRITE(8, 6160) (SRCRIM(K, J), J = 1, 12)
        end do
        IF (LD(3) > 0) THEN
            WRITE (8, 6090) LD(3), (ADRN3M (J), J = 1, 12)
            WRITE (8, 6100) (SDRN3M (J), J = 1, 12)
            IF (LAYR(LD(3)) > 0) WRITE (8, 6130) LD(3), (ARCR3M (J), &
                    J = 1, 12)
            IF (LAYR(LD(3)) > 0) WRITE (8, 6140) (SRCR3M(J), J = 1, 12)
        END IF
        WRITE (8, 6110) LP(3), (APRC3M (J), J = 1, 12)
        WRITE (8, 6120) (SPRC3M (J), J = 1, 12)
    END IF
    IF (LP(4) > 0) THEN
        DO K = LP(3) + 1, LP(4)
            IF (LSIN(K) > 0) WRITE(8, 6170) K, (SUBINM(K, J), J = 1, 12)
            IF (LRIN(K) > 0) WRITE(8, 6150) K, (ARCRIM(K, J), J = 1, 12)
            IF (LRIN(K) > 0) WRITE(8, 6160) (SRCRIM(K, J), J = 1, 12)
        end do
        IF (LD(4) > 0) THEN
            WRITE (8, 6090) LD(4), (ADRN4M (J), J = 1, 12)
            WRITE (8, 6100) (SDRN4M (J), J = 1, 12)
            IF (LAYR(LD(4)) > 0) WRITE (8, 6130) LD(4), (ARCR4M (J), &
                    J = 1, 12)
            IF (LAYR(LD(4)) > 0) WRITE (8, 6140) (SRCR4M(J), J = 1, 12)
        END IF
        WRITE (8, 6110) LP(4), (APRC4M (J), J = 1, 12)
        WRITE (8, 6120) (SPRC4M (J), J = 1, 12)
    END IF
    IF (LP(5) > 0) THEN
        DO K = LP(4) + 1, LP(5)
            IF (LSIN(K) > 0) WRITE(8, 6170) K, (SUBINM(K, J), J = 1, 12)
            IF (LRIN(K) > 0) WRITE(8, 6150) K, (ARCRIM(K, J), J = 1, 12)
            IF (LRIN(K) > 0) WRITE(8, 6160) (SRCRIM(K, J), J = 1, 12)
        end do
        IF (LD(5) > 0) THEN
            WRITE (8, 6090) LD(5), (ADRN5M (J), J = 1, 12)
            WRITE (8, 6100) (SDRN5M (J), J = 1, 12)
            IF (LAYR(LD(5)) > 0) WRITE (8, 6130) LD(5), (ARCR5M (J), &
                    J = 1, 12)
            IF (LAYR(LD(5)) > 0) WRITE (8, 6140) (SRCR5M(J), J = 1, 12)
        END IF
        WRITE (8, 6110) LP(5), (APRC5M (J), J = 1, 12)
        WRITE (8, 6120) (SPRC5M (J), J = 1, 12)
    END IF
    IF (LP(6) > 0) THEN
        DO K = LP(5) + 1, LP(6)
            IF (LSIN(K) > 0) WRITE(8, 6170) K, (SUBINM(K, J), J = 1, 12)
            IF (LRIN(K) > 0) WRITE(8, 6150) K, (ARCRIM(K, J), J = 1, 12)
            IF (LRIN(K) > 0) WRITE(8, 6160) (SRCRIM(K, J), J = 1, 12)
        end do
        WRITE (8, 6110) LP(6), (APRC6M (J), J = 1, 12)
        WRITE (8, 6120) (SPRC6M (J), J = 1, 12)
    END IF
    IF (LP(1) > 0) THEN
        IF (LAYER (LP(1)) > 2) THEN
            IF (IU8 == 1) THEN
                WRITE (8, 6191)
            ELSE
                WRITE (8, 6192)
            END IF
            IF (LAYER (LP(1) - 1) <= 2) THEN
                WRITE (8, 6200) LP(1), (AHED1M (J), J = 1, 12)
            ELSE
                WRITE (8, 6200) (LP(1) - 1), (AHED1M (J), J = 1, 12)
            END IF
            WRITE (8, 6210) (SHED1M (J), J = 1, 12)
        END IF
    END IF
    IF (LP(2) > 0) THEN
        IF (LAYER (LP(2)) > 2) THEN
            IF (LAYER (LP(2) - 1) <= 2) THEN
                WRITE (8, 6200) LP(2), (AHED2M (J), J = 1, 12)
            ELSE
                WRITE (8, 6200) (LP(2) - 1), (AHED2M (J), J = 1, 12)
            END IF
            WRITE (8, 6210) (SHED2M (J), J = 1, 12)
        END IF
    END IF
    IF (LP(3) > 0) THEN
        IF (LAYER (LP(3)) > 2) THEN
            IF (LAYER (LP(3) - 1) <= 2) THEN
                WRITE (8, 6200) LP(3), (AHED3M (J), J = 1, 12)
            ELSE
                WRITE (8, 6200) (LP(3) - 1), (AHED3M (J), J = 1, 12)
            END IF
            WRITE (8, 6210) (SHED3M (J), J = 1, 12)
        END IF
    END IF
    IF (LP(4) > 0) THEN
        IF (LAYER (LP(4)) > 2) THEN
            IF (LAYER (LP(4) - 1) <= 2) THEN
                WRITE (8, 6200) LP(4), (AHED4M (J), J = 1, 12)
            ELSE
                WRITE (8, 6200) (LP(4) - 1), (AHED4M (J), J = 1, 12)
            END IF
            WRITE (8, 6210) (SHED4M (J), J = 1, 12)
        END IF
    END IF
    IF (LP(5) > 0) THEN
        IF (LAYER (LP(5)) > 2) THEN
            IF (LAYER (LP(5) - 1) <= 2) THEN
                WRITE (8, 6200) LP(5), (AHED5M (J), J = 1, 12)
            ELSE
                WRITE (8, 6200) (LP(5) - 1), (AHED5M (J), J = 1, 12)
            END IF
            WRITE (8, 6210) (SHED5M (J), J = 1, 12)
        END IF
    END IF
    !
    !
    6000 FORMAT(1X, 3(/), 1X, 79('*'))
    6011 FORMAT(1X/10X, 'AVERAGE MONTHLY VALUES IN INCHES FOR YEARS', &
            I5, ' THROUGH', I5/1X, 79('-'))
    6012 FORMAT(1X/13X, 'AVERAGE MONTHLY VALUES (MM) FOR YEARS', &
            I5, ' THROUGH', I5/1X, 79('-'))
    6020 FORMAT(1X/26X, 'JAN/JUL  FEB/AUG  MAR/SEP  ', &
            'APR/OCT  MAY/NOV  JUN/DEC'/24X, 6('  -------'))
    6030 FORMAT('   PRECIPITATION'/3X, 13('-')/4X, ' TOTALS', 13X, &
            1X, F7.2, 5(2X, F7.2)/23X, 6(2X, F7.2))
    6040 FORMAT(1X/4X, ' STD. DEVIATIONS', &
            5X, F7.2, 5(2X, F7.2)/23X, 6(2X, F7.2))
    6050 FORMAT(1X/'   RUNOFF'/3X, 6('-')/4X, ' TOTALS', 13X, &
            6(1X, F8.3)/24X, 6(1X, F8.3))
    6060 FORMAT(1X/4X, ' STD. DEVIATIONS', 4X, &
            6(1X, F8.3)/24X, 6(1X, F8.3))
    6070 FORMAT(1X/'   EVAPOTRANSPIRATION'/3X, 18('-')/&
            4X, ' TOTALS', 13X, 6(1X, F8.3)/24X, 6(1X, F8.3))
    6080 FORMAT(1X/4X, ' STD. DEVIATIONS', 4X, 6(1X, F8.3)/&
            24X, 6(1X, F8.3))
    6090 FORMAT(1X/'   LATERAL DRAINAGE COLLECTED FROM', &
            ' LAYER', I3, /3X, 40('-'), /4X, ' TOTALS', 14X, 6F9.4/25X, 6F9.4)
    6100 FORMAT(1X/4X, ' STD. DEVIATIONS', 5X, 6F9.4/25X, 6F9.4)
    6110 FORMAT(1X/'   PERCOLATION/LEAKAGE THROUGH', &
            ' LAYER', I3, /3X, 36('-'), /4X, ' TOTALS', 14X, 6F9.4/25X, 6F9.4)
    6120 FORMAT(1X/4X, ' STD. DEVIATIONS', 5X, 6F9.4/25X, 6F9.4)
    6130 FORMAT(1X/'   LATERAL DRAINAGE RECIRCULATED FROM', &
            ' LAYER', I3, /3X, 43('-'), /4X, ' TOTALS', 14X, 6F9.4/25X, 6F9.4)
    6140 FORMAT(1X/4X, ' STD. DEVIATIONS', 5X, 6F9.4/25X, 6F9.4)
    6150 FORMAT(1X/'   LATERAL DRAINAGE RECIRCULATED INTO', &
            ' LAYER', I3, /3X, 43('-'), /4X, ' TOTALS', 14X, 6F9.4/25X, 6F9.4)
    6160 FORMAT(1X/4X, ' STD. DEVIATIONS', 5X, 6F9.4/25X, 6F9.4)
    6170 FORMAT(1X/'   SUBSURFACE INFLOW INTO', &
            ' LAYER', I3, /3X, 31('-'), /4X, ' TOTALS', 14X, 6F9.4/25X, 6F9.4)
    6180 FORMAT(1X/4X, ' STD. DEVIATIONS', 5X, 6F9.4/25X, 6F9.4)
    6191 FORMAT(1X//1X, 79('-')/16X, 'AVERAGES OF MONTHLY AVERAGED DAILY ', &
            'HEADS (INCHES)'/1X, 79('-')/)
    6192 FORMAT(1X//1X, 79('-')/18X, 'AVERAGES OF MONTHLY AVERAGED DAILY ', &
            'HEADS (CM)'/1X, 79('-')/)
    6200 FORMAT(1X/'   DAILY AVERAGE HEAD ON TOP OF', &
            ' LAYER', I3, /3X, 37('-'), /4X, ' AVERAGES', 12X, 6F9.4/25X, 6F9.4)
    6210 FORMAT(1X/4X, ' STD. DEVIATIONS', 5X, 6F9.4/25X, 6F9.4)
    !
    !
    !
    !    WRITES HEADING FOR AVERAGE ANNUAL RESULTS AND PRINTS AVERAGE
    !    ANNUAL PRECIPITATION, RUNOFF AND EVAPOTRANSPIRATION.
    !
    WRITE (8, 6330)
    IF (IU8 == 1) THEN
        WRITE (8, 6341) JYEAR (1), JYEAR (LMYR), APREA, &
                SPREA, TAPREA, FAPREA
    ELSE
        WRITE (8, 6342) JYEAR (1), JYEAR (LMYR), APREA, &
                SPREA, TAPREA, FAPREA
    END IF
    WRITE (8, 6350) ARUNA, SRUNA, TARUNA, FARUNA
    WRITE (8, 6360) AETA, SETA, TAETA, FAETA
    !
    IF (LP(1) > 0) THEN
        DO K = 1, LP(1)
            IF (LSIN(K) > 0) WRITE(8, 6290) ASUBIA(K), &
                    TASUBA(K), FASUBA(K), K
            IF (LRIN(K) > 0) WRITE(8, 6280) ARCRIA(K), SRCRIA(K), &
                    TARCIA(K), FARCIA(K), K
        end do
        IF (LD(1) > 0) THEN
            WRITE(8, 6370) ADRN1A, SDRN1A, TADN1A, FADN1A, LD(1)
            IF (LAYR(LD(1)) > 0) WRITE(8, 6270) ARCR1A, SRCR1A, &
                    TARC1A, FARC1A, LD(1)
        END IF
        WRITE(8, 6380) APRC1A, SPRC1A, TAPC1A, FAPC1A, LP(1)
        IF (LAYER (LP(1)) > 2) THEN
            IF (LAYER (LP(1) - 1) > 2) THEN
                WRITE(8, 6300) AHED1A, SHED1A, (LP(1) - 1)
            ELSE
                WRITE(8, 6300) AHED1A, SHED1A, LP(1)
            END IF
        END IF
    END IF
    IF (LP(2) > 0) THEN
        DO K = LP(1) + 1, LP(2)
            IF (LSIN(K) > 0) WRITE(8, 6290) ASUBIA(K), &
                    TASUBA(K), FASUBA(K), K
            IF (LRIN(K) > 0) WRITE(8, 6280) ARCRIA(K), SRCRIA(K), &
                    TARCIA(K), FARCIA(K), K
        end do
        IF (LD(2) > 0) THEN
            WRITE(8, 6370) ADRN2A, SDRN2A, TADN2A, FADN2A, LD(2)
            IF (LAYR(LD(2)) > 0) WRITE(8, 6270) ARCR2A, SRCR2A, &
                    TARC2A, FARC2A, LD(2)
        END IF
        WRITE(8, 6380) APRC2A, SPRC2A, TAPC2A, FAPC2A, LP(2)
        IF (LAYER (LP(2)) > 2) THEN
            IF (LAYER (LP(2) - 1) > 2) THEN
                WRITE(8, 6300) AHED2A, SHED2A, (LP(2) - 1)
            ELSE
                WRITE(8, 6300) AHED2A, SHED2A, LP(2)
            END IF
        END IF
    END IF
    IF (LP(3) > 0) THEN
        DO K = LP(2) + 1, LP(3)
            IF (LSIN(K) > 0) WRITE(8, 6290) ASUBIA(K), &
                    TASUBA(K), FASUBA(K), K
            IF (LRIN(K) > 0) WRITE(8, 6280) ARCRIA(K), SRCRIA(K), &
                    TARCIA(K), FARCIA(K), K
        end do
        IF (LD(3) > 0) THEN
            WRITE(8, 6370) ADRN3A, SDRN3A, TADN3A, FADN3A, LD(3)
            IF (LAYR(LD(3)) > 0) WRITE(8, 6270) ARCR3A, SRCR3A, &
                    TARC3A, FARC3A, LD(3)
        END IF
        WRITE(8, 6380) APRC3A, SPRC3A, TAPC3A, FAPC3A, LP(3)
        IF (LAYER (LP(3)) > 2) THEN
            IF (LAYER (LP(3) - 1) > 2) THEN
                WRITE(8, 6300) AHED3A, SHED3A, (LP(3) - 1)
            ELSE
                WRITE(8, 6300) AHED3A, SHED3A, LP(3)
            END IF
        END IF
    END IF
    IF (LP(4) > 0) THEN
        DO K = LP(3) + 1, LP(4)
            IF (LSIN(K) > 0) WRITE(8, 6290) ASUBIA(K), &
                    TASUBA(K), FASUBA(K), K
            IF (LRIN(K) > 0) WRITE(8, 6280) ARCRIA(K), SRCRIA(K), &
                    TARCIA(K), FARCIA(K), K
        end do
        IF (LD(4) > 0) THEN
            WRITE(8, 6370) ADRN4A, SDRN4A, TADN4A, FADN4A, LD(4)
            IF (LAYR(LD(4)) > 0) WRITE(8, 6270) ARCR4A, SRCR4A, &
                    TARC4A, FARC4A, LD(4)
        END IF
        WRITE(8, 6380) APRC4A, SPRC4A, TAPC4A, FAPC4A, LP(4)
        IF (LAYER (LP(4)) > 2) THEN
            IF (LAYER (LP(4) - 1) > 2) THEN
                WRITE(8, 6300) AHED4A, SHED4A, (LP(4) - 1)
            ELSE
                WRITE(8, 6300) AHED4A, SHED4A, LP(4)
            END IF
        END IF
    END IF
    IF (LP(5) > 0) THEN
        DO K = LP(4) + 1, LP(5)
            IF (LSIN(K) > 0) WRITE(8, 6290) ASUBIA(K), &
                    TASUBA(K), FASUBA(K), K
            IF (LRIN(K) > 0) WRITE(8, 6280) ARCRIA(K), SRCRIA(K), &
                    TARCIA(K), FARCIA(K), K
        end do
        IF (LD(5) > 0) THEN
            WRITE(8, 6370) ADRN5A, SDRN5A, TADN5A, FADN5A, LD(5)
            IF (LAYR(LD(5)) > 0) WRITE(8, 6270) ARCR5A, SRCR5A, &
                    TARC5A, FARC5A, LD(5)
        END IF
        WRITE(8, 6380) APRC5A, SPRC5A, TAPC5A, FAPC5A, LP(5)
        IF (LAYER (LP(5)) > 2) THEN
            IF (LAYER (LP(5) - 1) > 2) THEN
                WRITE(8, 6300) AHED5A, SHED5A, (LP(5) - 1)
            ELSE
                WRITE(8, 6300) AHED5A, SHED5A, LP(5)
            END IF
        END IF
    END IF
    IF (LP(6) > 0) THEN
        DO K = LP(5) + 1, LP(6)
            IF (LSIN(K) > 0) WRITE(8, 6290) ASUBIA(K), &
                    TASUBA(K), FASUBA(K), K
            IF (LRIN(K) > 0) WRITE(8, 6280) ARCRIA(K), SRCRIA(K), &
                    TARCIA(K), FARCIA(K), K
        end do
        WRITE(8, 6380) APRC6A, SPRC6A, TAPC6A, FAPC6A, LP(6)
    END IF
    WRITE (8, 6385) ASTOR, SSTOR, TASTOR, FASTOR
    !
    WRITE (8, 6390)
    !
    6270 FORMAT(1X, /2X, 'DRAINAGE RECIRCULATED', 7X, &
            F11.5, 1X, '(', F9.5, ')', 2X, F12.3, 1X, F10.5/4X, 'FROM LAYER', I3)
    6280 FORMAT(1X, /2X, 'DRAINAGE RECIRCULATED', 7X, &
            F11.5, 1X, '(', F9.5, ')', 2X, F12.3, 1X, F10.5/4X, 'INTO LAYER', I3)
    6290 FORMAT(1X, /2X, 'SUBSURFACE INFLOW INTO', 6X, &
            F11.5, 14X, F12.3, 1X, F10.5/4X, 'LAYER', I3)
    6300 FORMAT(1X, /2X, 'AVERAGE HEAD ON TOP    ', 3X, &
            F11.3, 1X, '(', F9.3, ')'/4X, 'OF LAYER', I3)
    6330 FORMAT(1X/1X, 79('*'), 4(/), 1X, 79('*'))
    6341 FORMAT(1X/6X, 'AVERAGE ANNUAL TOTALS & (STD. DEVIATIONS) FOR', &
            ' YEARS', I5, ' THROUGH', I5/1X, 79('-')/37X, ' INCHES ', 10X, &
            ' CU. FEET ', 6X, 'PERCENT'/32X, 19('-'), 3X, 13('-'), 3X, &
            9('-')/2X, 'PRECIPITATION', 15X, F8.2, 4X, '(', F8.3, ')', &
            2X, F11.1, 3X, F8.2)
    6342 FORMAT(1X/6X, 'AVERAGE ANNUAL TOTALS & (STD. DEVIATIONS) FOR', &
            ' YEARS', I5, ' THROUGH', I5/1X, 79('-')/37X, '   MM   ', 11X, &
            'CU. METERS', 5X, 'PERCENT'/31X, 20('-'), 5X, 11('-'), 3X, &
            9('-')/2X, 'PRECIPITATION', 15X, F8.2, 4X, '(', F8.3, ')', &
            2X, F11.1, 3X, F8.2)
    6350 FORMAT(1X/2X, 'RUNOFF', 23X, F8.3, 3X, '(', F8.4, ')', 3X, F11.2, 3X, F8.3)
    6360 FORMAT(1X/2X, 'EVAPOTRANSPIRATION', 11X, F8.3, 3X, '(', F8.4, ')', &
            3X, F11.2, 3X, F8.3)
    6370 FORMAT(1X, /2X, 'LATERAL DRAINAGE COLLECTED', 2X, &
            F11.5, 1X, '(', F9.5, ')', 2X, F12.3, 1X, F10.5/4X, 'FROM LAYER', I3)
    6380 FORMAT(1X/2X, 'PERCOLATION/LEAKAGE THROUGH', 1X, &
            F11.5, 1X, '(', F9.5, ')', 2X, F12.3, 2X, F10.5/4X, 'LAYER', I3)
    6385 FORMAT(1X/2X, 'CHANGE IN WATER STORAGE', 5X, F9.3, 3X, '(', F8.4, ')', &
            3X, F11.2, 3X, F8.3)
    6390 FORMAT(1X/1X, 79('*'), 3(/))
    RETURN
END
!
!
!
!      ************************ OUTPEK ************************
!
!     SUBROUTINE OUTPEK PRINTS PEAK DAILY RESULTS
!     FOR THE SIMULATION.
SUBROUTINE OUTPEK
    !
    PARAMETER (MXYR = 100)
    !
    DOUBLE PRECISION ELKS, DPHED1, DPHED2, DPHED3, DPHED4, DPHED5
    !
    COMMON /BLK1/ IT4, IT7, IT13, IU4, IU7, IU13, IU11, IU10, IU8, &
            LMYR, IOD, IOM, IOA, IOS
    COMMON /BLK3/ PORO(20), FC(20), WP(20), RC(20), SW(20), &
            RS(20), XLAMBD(20), BUB(20), THICK(20), SLOPE(20), &
            XLENG(20), CON(20), SUBIN(20), RECIR(20), PHOLE(20), &
            DEFEC(20), TRANS(20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS(20, 2), LRIN(20), LSIN(20)
    COMMON /BLK5/ AREA, FRUNOF, CN2, OCN2, SSLOPE, SLENG, SMX
    COMMON /BLK14/ PPRC1, PPRC2, PPRC3, PPRC4, PPRC5, PPRC6, &
            PDRN1, PDRN2, PDRN3, PDRN4, PDRN5, PPRE, PRUN, PSNO, PSW, DSW, &
            PHED1, PHED2, PHED3, PHED4, PHED5, PRCR1, PRCR2, PRCR3, &
            PRCR4, PRCR5, PRCRI(20)
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK33/ JYEAR (MXYR)
    COMMON /BLK35/ NSEG1B, NSEG2B, NSEG3B, NSEG4B, NSEG5B, &
            NSEG1D, NSEG2D, NSEG3D, NSEG4D, NSEG5D
    DIMENSION TPRCRI (20)
    !
    !     PRINTS HEADING,CONVERTS RESULTS FROM INCHES TO
    !     CU. FT., AND PRINTS PRECIPITATION AND RUNOFF.
    !
    MND = 0
    WRITE (8, 6000) CHAR(12)
    WRITE (8, 6010) JYEAR (1), JYEAR (LMYR)
    IF (IU8 == 1) THEN
        TPPRE = 43560. * AREA * PPRE / 12.0
        TPRUN = 43560. * AREA * PRUN / 12.0
        TPPRC6 = 43560. * AREA * PPRC6 / 12.0
        TPPRC5 = 43560. * AREA * PPRC5 / 12.0
        TPPRC4 = 43560. * AREA * PPRC4 / 12.0
        TPPRC3 = 43560. * AREA * PPRC3 / 12.0
        TPPRC2 = 43560. * AREA * PPRC2 / 12.0
        TPPRC1 = 43560. * AREA * PPRC1 / 12.0
        TPDRN5 = 43560. * AREA * PDRN5 / 12.0
        TPDRN4 = 43560. * AREA * PDRN4 / 12.0
        TPDRN3 = 43560. * AREA * PDRN3 / 12.0
        TPDRN2 = 43560. * AREA * PDRN2 / 12.0
        TPDRN1 = 43560. * AREA * PDRN1 / 12.0
        TPRCR5 = 43560. * AREA * PRCR5 / 12.0
        TPRCR4 = 43560. * AREA * PRCR4 / 12.0
        TPRCR3 = 43560. * AREA * PRCR3 / 12.0
        TPRCR2 = 43560. * AREA * PRCR2 / 12.0
        TPRCR1 = 43560. * AREA * PRCR1 / 12.0
        TPSNO = 43560. * AREA * PSNO / 12.0
        DO K = 1, 20
            TPRCRI(K) = 43560. * AREA * PRCRI(K) / 12.0
        end do
        WRITE (8, 6011)
    ELSE
        PPRE = PPRE * 25.4
        PRUN = PRUN * 25.4
        PPRC6 = PPRC6 * 25.4
        PPRC5 = PPRC5 * 25.4
        PPRC4 = PPRC4 * 25.4
        PPRC3 = PPRC3 * 25.4
        PPRC2 = PPRC2 * 25.4
        PPRC1 = PPRC1 * 25.4
        PDRN5 = PDRN5 * 25.4
        PDRN4 = PDRN4 * 25.4
        PDRN3 = PDRN3 * 25.4
        PDRN2 = PDRN2 * 25.4
        PDRN1 = PDRN1 * 25.4
        PHED5 = PHED5 * 25.4
        PHED4 = PHED4 * 25.4
        PHED3 = PHED3 * 25.4
        PHED2 = PHED2 * 25.4
        PHED1 = PHED1 * 25.4
        PRCR5 = PRCR5 * 25.4
        PRCR4 = PRCR4 * 25.4
        PRCR3 = PRCR3 * 25.4
        PRCR2 = PRCR2 * 25.4
        PRCR1 = PRCR1 * 25.4
        PSNO = PSNO * 25.4
        DO K = 1, 20
            PRCRI(K) = PRCRI(K) * 25.4
        end do
        TPPRE = AREA * PPRE * 10.0
        TPRUN = AREA * PRUN * 10.0
        TPPRC6 = AREA * PPRC6 * 10.0
        TPPRC5 = AREA * PPRC5 * 10.0
        TPPRC4 = AREA * PPRC4 * 10.0
        TPPRC3 = AREA * PPRC3 * 10.0
        TPPRC2 = AREA * PPRC2 * 10.0
        TPPRC1 = AREA * PPRC1 * 10.0
        TPDRN5 = AREA * PDRN5 * 10.0
        TPDRN4 = AREA * PDRN4 * 10.0
        TPDRN3 = AREA * PDRN3 * 10.0
        TPDRN2 = AREA * PDRN2 * 10.0
        TPDRN1 = AREA * PDRN1 * 10.0
        TPRCR5 = AREA * PRCR5 * 10.0
        TPRCR4 = AREA * PRCR4 * 10.0
        TPRCR3 = AREA * PRCR3 * 10.0
        TPRCR2 = AREA * PRCR2 * 10.0
        TPRCR1 = AREA * PRCR1 * 10.0
        TPSNO = AREA * PSNO * 10.0
        DO K = 1, 20
            TPRCRI(K) = AREA * PRCRI(K) * 10.0
        end do
        WRITE (8, 6012)
    END IF
    WRITE (8, 6020) PPRE, TPPRE
    WRITE (8, 6030) PRUN, TPRUN
    IF (LP(1) > 0) THEN
        DO L = 1, LP(1)
            IF (LRIN(L) > 0) WRITE (8, 6035) L, PRCRI(L), TPRCRI(L)
        end do
        IF (LD(1) > 0) THEN
            WRITE (8, 6040) LD(1), PDRN1, TPDRN1
            IF(RECIR(LD(1)) > 0.0)WRITE(8, 6045) LD(1), PRCR1, TPRCR1
        END IF
        WRITE (8, 6050) LP(1), PPRC1, TPPRC1
        IF (LAYER (LP(1)) > 2) THEN
            IF (LAYER (LP(1) - 1) > 2) THEN
                WRITE (8, 6060) (LP(1) - 1), PHED1
            ELSE
                WRITE (8, 6060) LP(1), PHED1
            END IF
            IF (LD(1) > 0) THEN
                LAYD = LAYSEG(NSEG1D, 2)
                DPHED1 = PHED1
                IF(IU8 == 1) CALL LATKS (DPHED1, NSEG1B, NSEG1D, ELKS)
                IF(IU8/=1) CALL LATKS (DPHED1 / 25.4, NSEG1B, NSEG1D, ELKS)
                PDRN1 = PRCR1 + PDRN1
                CALL MOUND (PDRN1, ELKS, LAYD, PMHED1, PLEN1)
                MND = 1
                IF (IU8 == 1) THEN
                    IF (LAYER (LP(1) - 1) > 2) THEN
                        WRITE (8, 6065) (LP(1) - 1), PMHED1, LAYD, PLEN1
                    ELSE
                        WRITE (8, 6065) LP(1), PMHED1, LAYD, PLEN1
                    END IF
                ELSE
                    IF (LAYER (LP(1) - 1) > 2) THEN
                        WRITE (8, 6066) (LP(1) - 1), PMHED1, LAYD, PLEN1
                    ELSE
                        WRITE (8, 6066) LP(1), PMHED1, LAYD, PLEN1
                    END IF
                END IF
            END IF
        END IF
    END IF
    IF (LP(2) > 0) THEN
        DO L = LP(1) + 1, LP(2)
            IF (LRIN(L) > 0) WRITE (8, 6035) L, PRCRI(L), TPRCRI(L)
        end do
        IF (LD(2) > 0) THEN
            WRITE (8, 6040) LD(2), PDRN2, TPDRN2
            IF(RECIR(LD(2)) > 0.0)WRITE(8, 6045) LD(2), PRCR2, TPRCR2
        END IF
        WRITE (8, 6050) LP(2), PPRC2, TPPRC2
        IF (LAYER (LP(2)) > 2) THEN
            IF (LAYER (LP(2) - 1) > 2) THEN
                WRITE (8, 6060) (LP(2) - 1), PHED2
            ELSE
                WRITE (8, 6060) LP(2), PHED2
            END IF
            IF (LD(2) > 0) THEN
                LAYD = LAYSEG(NSEG2D, 2)
                DPHED2 = PHED2
                IF(IU8 == 1) CALL LATKS (DPHED2, NSEG2B, NSEG2D, ELKS)
                IF(IU8/=1) CALL LATKS (DPHED2 / 25.4, NSEG2B, NSEG2D, ELKS)
                PDRN2 = PRCR2 + PDRN2
                CALL MOUND (PDRN2, ELKS, LAYD, PMHED2, PLEN2)
                MND = 1
                IF (IU8 == 1) THEN
                    IF (LAYER (LP(2) - 1) > 2) THEN
                        WRITE (8, 6065) (LP(2) - 1), PMHED2, LAYD, PLEN2
                    ELSE
                        WRITE (8, 6065) LP(2), PMHED2, LAYD, PLEN2
                    END IF
                ELSE
                    IF (LAYER (LP(2) - 1) > 2) THEN
                        WRITE (8, 6066) (LP(2) - 1), PMHED2, LAYD, PLEN2
                    ELSE
                        WRITE (8, 6066) LP(2), PMHED2, LAYD, PLEN2
                    END IF
                END IF
            END IF
        END IF
    END IF
    IF (LP(3) > 0) THEN
        DO L = LP(2) + 1, LP(3)
            IF (LRIN(L) > 0) WRITE (8, 6035) L, PRCRI(L), TPRCRI(L)
        end do
        IF (LD(3) > 0) THEN
            WRITE (8, 6040) LD(3), PDRN3, TPDRN3
            IF(RECIR(LD(3)) > 0.0)WRITE(8, 6045) LD(3), PRCR3, TPRCR3
        END IF
        WRITE (8, 6050) LP(3), PPRC3, TPPRC3
        IF (LAYER (LP(3)) > 2) THEN
            IF (LAYER (LP(3) - 1) > 2) THEN
                WRITE (8, 6060) (LP(3) - 1), PHED3
            ELSE
                WRITE (8, 6060) LP(3), PHED3
            END IF
            IF (LD(3) > 0) THEN
                LAYD = LAYSEG(NSEG3D, 2)
                DPHED3 = PHED3
                IF(IU8 == 1) CALL LATKS (DPHED3, NSEG3B, NSEG3D, ELKS)
                IF(IU8/=1) CALL LATKS (DPHED3 / 25.4, NSEG3B, NSEG3D, ELKS)
                PDRN3 = PRCR3 + PDRN3
                CALL MOUND (PDRN3, ELKS, LAYD, PMHED3, PLEN3)
                MND = 1
                IF (IU8 == 1) THEN
                    IF (LAYER (LP(3) - 1) > 2) THEN
                        WRITE (8, 6065) (LP(3) - 1), PMHED3, LAYD, PLEN3
                    ELSE
                        WRITE (8, 6065) LP(3), PMHED3, LAYD, PLEN3
                    END IF
                ELSE
                    IF (LAYER (LP(3) - 1) > 2) THEN
                        WRITE (8, 6066) (LP(3) - 1), PMHED3, LAYD, PLEN3
                    ELSE
                        WRITE (8, 6066) LP(3), PMHED3, LAYD, PLEN3
                    END IF
                END IF
            END IF
        END IF
    END IF
    IF (LP(4) > 0) THEN
        DO L = LP(3) + 1, LP(4)
            IF (LRIN(L) > 0) WRITE (8, 6035) L, PRCRI(L), TPRCRI(L)
        end do
        IF (LD(4) > 0) THEN
            WRITE (8, 6040) LD(4), PDRN4, TPDRN4
            IF(RECIR(LD(4)) > 0.0)WRITE(8, 6045) LD(4), PRCR4, TPRCR4
        END IF
        WRITE (8, 6050) LP(4), PPRC4, TPPRC4
        IF (LAYER (LP(4)) > 2) THEN
            IF (LAYER (LP(4) - 1) > 2) THEN
                WRITE (8, 6060) (LP(4) - 1), PHED4
            ELSE
                WRITE (8, 6060) LP(4), PHED4
            END IF
            IF (LD(4) > 0) THEN
                LAYD = LAYSEG(NSEG4D, 2)
                DPHED4 = PHED4
                IF(IU8 == 1) CALL LATKS (DPHED4, NSEG4B, NSEG4D, ELKS)
                IF(IU8/=1) CALL LATKS (DPHED4 / 25.4, NSEG4B, NSEG4D, ELKS)
                PDRN4 = PRCR4 + PDRN4
                CALL MOUND (PDRN4, ELKS, LAYD, PMHED4, PLEN4)
                MND = 1
                IF (IU8 == 1) THEN
                    IF (LAYER (LP(4) - 1) > 2) THEN
                        WRITE (8, 6065) (LP(4) - 1), PMHED4, LAYD, PLEN4
                    ELSE
                        WRITE (8, 6065) LP(4), PMHED4, LAYD, PLEN4
                    END IF
                ELSE
                    IF (LAYER (LP(4) - 1) > 2) THEN
                        WRITE (8, 6066) (LP(4) - 1), PMHED4, LAYD, PLEN4
                    ELSE
                        WRITE (8, 6066) LP(4), PMHED4, LAYD, PLEN4
                    END IF
                END IF
            END IF
        END IF
    END IF
    IF (LP(5) > 0) THEN
        DO L = LP(4) + 1, LP(5)
            IF (LRIN(L) > 0) WRITE (8, 6035) L, PRCRI(L), TPRCRI(L)
        end do
        IF (LD(5) > 0) THEN
            WRITE (8, 6040) LD(5), PDRN5, TPDRN5
            IF(RECIR(LD(5)) > 0.0)WRITE(8, 6045) LD(5), PRCR5, TPRCR5
        END IF
        WRITE (8, 6050) LP(5), PPRC5, TPPRC5
        IF (LAYER (LP(5)) > 2) THEN
            IF (LAYER (LP(5) - 1) > 2) THEN
                WRITE (8, 6060) (LP(5) - 1), PHED5
            ELSE
                WRITE (8, 6060) LP(5), PHED5
            END IF
            IF (LD(5) > 0) THEN
                LAYD = LAYSEG(NSEG5D, 2)
                DPHED5 = PHED5
                IF(IU8 == 1) CALL LATKS (DPHED5, NSEG5B, NSEG5D, ELKS)
                IF(IU8/=1) CALL LATKS (DPHED5 / 25.4, NSEG5B, NSEG5D, ELKS)
                PDRN5 = PRCR5 + PDRN5
                CALL MOUND (PDRN5, ELKS, LAYD, PMHED5, PLEN5)
                MND = 1
                IF (IU8 == 1) THEN
                    IF (LAYER (LP(5) - 1) > 2) THEN
                        WRITE (8, 6065) (LP(5) - 1), PMHED5, LAYD, PLEN5
                    ELSE
                        WRITE (8, 6065) LP(5), PMHED5, LAYD, PLEN5
                    END IF
                ELSE
                    IF (LAYER (LP(5) - 1) > 2) THEN
                        WRITE (8, 6066) (LP(5) - 1), PMHED5, LAYD, PLEN5
                    ELSE
                        WRITE (8, 6066) LP(5), PMHED5, LAYD, PLEN5
                    END IF
                END IF
            END IF
        END IF
    END IF
    IF (LP(6) > 0) THEN
        DO L = LP(5) + 1, LP(6)
            IF (LRIN(L) > 0) WRITE (8, 6035) L, PRCRI(L), TPRCRI(L)
        end do
        WRITE (8, 6050) LP(6), PPRC6, TPPRC6
    END IF
    !
    !     PRINTS SNOW AND VEGETATIVE SOIL WATER CONTENT.
    !
    WRITE (8, 6070) PSNO, TPSNO
    WRITE (8, 6080) PSW
    WRITE (8, 6090) DSW
    IF (MND == 1) WRITE (8, 6100)
    WRITE (8, 6002)
    6000 FORMAT(1X, A1/1X, 78('*'))
    6002 FORMAT(1X/1X, 78('*'), 4(/))
    6010 FORMAT(1X/17X, 'PEAK DAILY VALUES FOR YEARS', &
            I5, ' THROUGH', I5/4X, 72('-'))
    6011   FORMAT(49X, '(INCHES)', 6X, '(CU. FT.)'/48X, 10('-'), 3X, 13('-'))
    6012   FORMAT(51X, '(MM)', 7X, '(CU. METERS)'/48X, 10('-'), 4X, 12('-'))
    6020 FORMAT(7X, 'PRECIPITATION', 24X, F10.2, 4X, F14.3)
    6030 FORMAT(1X/7X, 'RUNOFF', 31X, F11.3, 4X, F14.4)
    6035   FORMAT(1X/7X, 'DRAINAGE RECIRCULATED INTO LAYER', &
            I3, 2X, F13.5, F17.5)
    6040   FORMAT(1X/7X, 'DRAINAGE COLLECTED FROM LAYER', &
            I3, 5X, F13.5, F17.5)
    6045   FORMAT(1X/7X, 'DRAINAGE RECIRCULATED FROM LAYER', &
            I3, 2X, F13.5, F17.5)
    6050   FORMAT(1X/7X, 'PERCOLATION/LEAKAGE THROUGH LAYER', I3, &
            3X, F12.6, F16.5)
    6060   FORMAT(1X/7X, 'AVERAGE HEAD ON TOP OF LAYER', I3, 8X, F9.3)
    6065   FORMAT(1X/7X, 'MAXIMUM HEAD ON TOP OF LAYER', I3, 8X, F9.3//&
            7X, 'LOCATION OF MAXIMUM HEAD IN LAYER', I3/&
            13X, '(DISTANCE FROM DRAIN)    ', 8X, F7.1, ' FEET')
    6066   FORMAT(1X/7X, 'MAXIMUM HEAD ON TOP OF LAYER', I3, 8X, F9.3//&
            7X, 'LOCATION OF MAXIMUM HEAD IN LAYER', I3/&
            13X, '(DISTANCE FROM DRAIN)    ', 8X, F7.1, ' METERS')
    6070 FORMAT(1X/7X, 'SNOW WATER', 26X, F11.2, 4X, F15.4)
    6080 FORMAT(1X//7X, 'MAXIMUM VEG. ', &
            'SOIL WATER (VOL/VOL)', 17X, F7.4)
    6090 FORMAT(1X/7X, 'MINIMUM VEG. SOIL WATER ', &
            '(VOL/VOL)', 17X, F7.4)
    6100 FORMAT(1X//8X, '***  Maximum heads are computed using McEnroe', &
    2H's,' equations.  ***'//13X,'Reference:Maximum Saturated ',&
            'Depth over Landfill Liner'/25X,'by Bruce M. McEnroe, ',&
            'University of Kansas'/25X,'ASCE Journal of ',&
            'Environmental Engineering'/25X,'Vol. 119, No. 2, March 1993, ',&
            ' pp. 262-270.'/)
    RETURN
END

!
!
!      ************************ OUTSW ************************
!
!     SUBROUTINE OUTSW PRINTS MOISTURE CONTENTS OF THE LAYERS
!
SUBROUTINE OUTSW
    PARAMETER (MXYR = 100)
    !
    !
    DOUBLE PRECISION DSWUL, DTHICK, DRCUL, DBUBUL, &
            DWPUL, DLAMUL, DRSUL, DUL, DFCUL, DSUBIN, DCHG, DRCRS
    !
    COMMON /BLK1/ IT4, IT7, IT13, IU4, IU7, IU13, IU11, IU10, IU8, &
            LMYR, IOD, IOM, IOA, IOS
    COMMON /BLK3/ PORO (20), FC (20), WP (20), RC (20), SW (20), &
            RS (20), XLAMBD (20), BUB (20), THICK (20), SLOPE (20), &
            XLENG (20), CON (20), SUBIN (20), RECIR (20), PHOLE (20), &
            DEFEC (20), TRANS (20), EDEPTH, SUBINF, SEDEP, CORECT
    COMMON /BLK4/ LAYER (21), LAYR (20), IPQ (20), ISOIL (20), LAY, &
            LAYSEG (67, 2), LSEG (67), LSEGS (20, 2), LRIN(20), LSIN(20)
    COMMON /BLK7/ STHICK(67), UL(67), FCUL(67), WPUL(67), SWUL(67), &
            RCUL(67), RSUL(67), XLAMBU(67), CONUL(67), BUBUL(67), &
            SUBINS(67), SWULI(67)
    COMMON /BLK9/ PINF, ES1T, SMELT, GMRO, ADDRUN, OLDSNO, SNO, WF(7), &
            SSNO
    COMMON /BLK16/ LD(5), LP(6), LPQ(5), LCASE(5), IYR, IDA, MO, &
            NSTEP(6), ND
    COMMON /BLK17/ NSEG1, NSEG2, NSEG3, NSEG4, NSEG5, NSEG6, NSEG
    COMMON /BLK31/ DRCUL (67), DBUBUL (67), DRSUL (67), DLAMUL (67), &
            DUL(67), DFCUL(67), DWPUL(67), DSWUL(67), DTHICK(67), &
            DSUBIN(67), DCHG(67), DRCRS(67)
    COMMON /BLK33/ JYEAR (MXYR)
    !
    !
    DIMENSION SWULL (20), FSWULL (20)
    !
    !
    DO K = 1, 20
        SWULL(K) = 0.0
    end do
    J = 1
    OTHCK1 = 0.0
    THICK1 = STHICK(J)
    SWULJ = DSWUL(J) + (DCHG(J) / 2.0D0)
    THICK2 = 0.0
    DO K = 1, LP(1)
        IF (LAYER(K) > 2) GO TO 1260
        THICK2 = THICK2 + THICK(K)
        1270   CONTINUE
        IF (LAYSEG(J, 2) < K) GO TO 1280
        IF (THICK1 > THICK2) THEN
            DSEG = THICK2 - OTHCK1
            OTHCK1 = THICK2
            SWULL(K) = SWULL(K) + ((DSEG * SWULJ) / STHICK(J))
            GO TO 1250
        ELSE
            SWULL(K) = SWULL(K) + (SWULJ * (THICK1 - OTHCK1) / STHICK(J))
        END IF
        1280   CONTINUE
        J = J + 1
        IF (J > NSEG1) GO TO 1260
        IF (LSEG(J) > 2) GO TO 1260
        SWULJ = DSWUL(J) + (DCHG(J) / 2.0D0)
        OTHCK1 = THICK1
        THICK1 = THICK1 + STHICK(J)
        IF (LAYSEG(J, 1) > K) GO TO 1250
        GO TO 1270
    1250 CONTINUE
    end do
    1260 CONTINUE
    IF (NSEG > NSEG1) THEN
        DO J = NSEG1 + 1, NSEG
            IF (LSEG(J) <= 2) THEN
                SWULL(LAYSEG(J, 2)) = SWULL(LAYSEG(J, 2)) + DSWUL(J) + &
                        DCHG(J) / 2.0D0
            END IF
        end do
    END IF
    DO K = 1, LAY
        IF (LAYER(K) > 2) SWULL(K) = SW(K) * THICK(K)
    end do
    6000 FORMAT(1X, A1/1X, 78('*'))
    WRITE (8, 6000) CHAR(12)
    DO J = 1, LAY
        FSWULL(J) = SWULL(J) / THICK(J)
    end do
    WRITE (8, 6012) JYEAR (LMYR)
    6012 FORMAT(1X/20X, 'FINAL WATER STORAGE AT END OF YEAR', I5/5X, 70('-'))
    IF (IU8 == 1) THEN
        WRITE (8, 6021)
        6021   FORMAT(21X, 'LAYER', 8X, &
                '(INCHES)', 7X, '(VOL/VOL)'/21X, 5('-'), 8X, 8('-'), 7X, 9('-'))
    ELSE
        WRITE (8, 6022)
        6022   FORMAT(21X, 'LAYER', 10X, &
                '(CM)', 9X, '(VOL/VOL)'/21X, 5('-'), 9X, 6('-'), 8X, 9('-'))
        DO J = 1, LAY
            SWULL (J) = SWULL (J) * 2.54
        end do
        SSNO = SSNO * 2.54
    END IF
    DO J = 1, LAY
        WRITE (8, 6023) J, SWULL(J), FSWULL(J)
        6023   FORMAT (22X, I2, 7X, F11.4, 6X, F9.4/)
    end do
    WRITE (8, 6072) SSNO
    6072 FORMAT(19X, 'SNOW WATER', 2X, F10.3)
    WRITE (8, 6100)
    6100 FORMAT(1X/1X, 78('*')/1X, 78('*'), 4(/))
    RETURN
END
!
