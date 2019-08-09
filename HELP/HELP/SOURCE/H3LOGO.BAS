
     '
     'HELP Acknowledgement and reference screen program
     '
VIEW PRINT 1 TO 25
COLOR 12, 0: KEY OFF: CLS : LOCATE 1, 1, O
1000 PRINT "                  Hydrologic Evaluation of Landfill Performance ": COLOR 14, 0
1010 PRINT
1030 PRINT "               ÜÜÜ     ÜÜÜ    ÜÜÜÜÜÜÜÜÜÜ    ÜÜÜ          ÜÜÜÜÜÜÜÜÜ "
1040 PRINT "                Û       Û      Û       ß     Û            Û      Û "
1050 PRINT "                Û       Û      Û             Û            Û      Û "
1051 PRINT "                Û       Û      Û    Ü        Û            Û      Û "
1060 PRINT "                ÛßßßßßßßÛ      ÛßßßßÛ        Û            Ûßßßßßßß "
1061 PRINT "                Û       Û      Û             Û            Û   "
1062 PRINT "                Û       Û      Û             Û            Û   "
1070 PRINT "               ÜÛÜ     ÜÛÜ    ÜÛÜÜÜÜÜÜÜÛ    ÜÛÜÜÜÜÜÜÛ    ÜÛÜ  "
1080 PRINT : COLOR 6, 0
1090 PRINT "                          Version .01   9 August 2019"
1100 PRINT : COLOR 10, 0
1110 PRINT "                 ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»"
1120 PRINT "                 º"; : COLOR 11: PRINT "                        Developed by                             "; : COLOR 10: PRINT "º"
1150 PRINT "                 º"; : COLOR 11: PRINT "         U.S. E.P.A, Office of Research & Development            "; : COLOR 10: PRINT "º"
1160 PRINT "                 º"; : COLOR 11: PRINT " Center for Environmental Solutions & Emergency Response (CESER) "; : COLOR 10: PRINT "º"
1180 PRINT "                 º"; : COLOR 11: PRINT "        Land Remediation & Technology Division (LRTD)            "; : COLOR 10: PRINT "º"
1121 PRINT "                 º"; : COLOR 11: PRINT "       Environmental Decision Analytics Branch (EDAB)            "; : COLOR 10: PRINT "º"
1181 PRINT "                 º"; : COLOR 11: PRINT "           Cincinnati, Ohio 45246                                "; : COLOR 10: PRINT "º"
1190 PRINT "                 ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼"
1200 PRINT : COLOR 15, 0
1220 PRINT "                           Press any key to continue...";
1250 A$ = INKEY$: IF A$ = "" GOTO 1250
COLOR 10, 0
3000 PRINT "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
3010 PRINT "³"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "³"
3030 PRINT "³"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "³"
3040 PRINT "³"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "³"
3151 PRINT "³"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "³"
3050 PRINT "³"; : COLOR 14: PRINT "                             DISCLAIMER STATEMENT                             "; : COLOR 10: PRINT "³"
3051 PRINT "³"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "³"
3060 PRINT "³"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "³"
3061 PRINT "³"; : COLOR 11: PRINT "          This program is furnished by the U.S. Government and is             "; : COLOR 10: PRINT "³"
3062 PRINT "³"; : COLOR 11: PRINT "     accepted and used by the recipient with the express understanding        "; : COLOR 10: PRINT "³"
3070 PRINT "³"; : COLOR 11: PRINT "     that the U.S. Government gives no warranties, expressed or implied,      "; : COLOR 10: PRINT "³"
3080 PRINT "³"; : COLOR 11: PRINT "     concerning the accuracy, reliability, usability, or suitability for      "; : COLOR 10: PRINT "³"
3090 PRINT "³"; : COLOR 11: PRINT "     any particular purpose of the information and data contained in this     "; : COLOR 10: PRINT "³"
3100 PRINT "³"; : COLOR 11: PRINT "     program or furnished in connection therewith.  The United States of      "; : COLOR 10: PRINT "³"
3110 PRINT "³"; : COLOR 11: PRINT "     America shall be under no liability whatsoever to any person by          "; : COLOR 10: PRINT "³"
3120 PRINT "³"; : COLOR 11: PRINT "     reason of any use made thereof.  This program belongs to the U.S.        "; : COLOR 10: PRINT "³"
3150 PRINT "³"; : COLOR 11: PRINT "     Government; therefore, the recipient further agrees not to assert        "; : COLOR 10: PRINT "³"
3160 PRINT "³"; : COLOR 11: PRINT "     any proprietary rights therein, or to represent this program to          "; : COLOR 10: PRINT "³"
3180 PRINT "³"; : COLOR 11: PRINT "     anyone as other than a U.S. Government program.                          "; : COLOR 10: PRINT "³"
3121 PRINT "³"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "³"
3161 PRINT "³"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "³"
3181 PRINT "³"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "³"
3190 PRINT "³"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "³"
3220 PRINT "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
COLOR 15, 0
3230 PRINT "                           Press any key to continue...";
2250 A$ = INKEY$: IF A$ = "" GOTO 2250
COLOR 10, 0
2000 PRINT "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
2161 PRINT "³"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "³"
2040 PRINT "³"; : COLOR 14: PRINT "                                   PREFACE                                    "; : COLOR 10: PRINT "³"
2180 PRINT "³"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "³"
2051 PRINT "³"; : COLOR 11: PRINT "       This program was developed by US Army Engineer Waterways Experiment    "; : COLOR 10: PRINT "³"
2060 PRINT "³"; : COLOR 11: PRINT "   Station under funding from the USEPA.  The program was written to provide  "; : COLOR 10: PRINT "³"
2061 PRINT "³"; : COLOR 11: PRINT "   technical support and resources for the RCRA and CERCLA programs.  This    "; : COLOR 10: PRINT "³"
2062 PRINT "³"; : COLOR 11: PRINT "   is the third major release of the HELP model.  Version 3 was developed     "; : COLOR 10: PRINT "³"
2070 PRINT "³"; : COLOR 11: PRINT "   under the direction of Dr. Paul R. Schroeder.  The user interface was      "; : COLOR 10: PRINT "³"
2080 PRINT "³"; : COLOR 11: PRINT "   written by Dr. Nadim Aziz of Clemson University.  The model is based in    "; : COLOR 10: PRINT "³"
2090 PRINT "³"; : COLOR 11: PRINT "   part on USDA-ARS models:  CREAMS, EPIC, SWRRB, SPUR, HYDRO-17 and WGEN.    "; : COLOR 10: PRINT "³"
2110 PRINT "³"; : COLOR 11: PRINT "       HELP was designed to perform water balance analyses for design and     "; : COLOR 10: PRINT "³"
2120 PRINT "³"; : COLOR 11: PRINT "   evaluation of land disposal facilities.  Use of the HELP model requires    "; : COLOR 10: PRINT "³"
2150 PRINT "³"; : COLOR 11: PRINT "   good understanding of landfill design practices.  To insure valid results  "; : COLOR 10: PRINT "³"
2160 PRINT "³"; : COLOR 11: PRINT "   the user should review the overall design prior to using the model.        "; : COLOR 10: PRINT "³"
2151 PRINT "³"; : COLOR 11: PRINT "       Design guidance for landfills is available in the following:           "; : COLOR 10: PRINT "³"
2200 PRINT "³"; : COLOR 11: PRINT "          USEPA.  1989.  Technical Guidance Document:  Final Covers on        "; : COLOR 10: PRINT "³"
2001 PRINT "³"; : COLOR 11: PRINT "   Hazardous Waste Landfills and Surface Impoundments.  EPA/530-SW-89-047.    "; : COLOR 10: PRINT "³"
2002 PRINT "³"; : COLOR 11: PRINT "   Office of Solid Waste and Emergency Response, Washington, DC.              "; : COLOR 10: PRINT "³"
2181 PRINT "³"; : COLOR 11: PRINT "          USEPA.  1987.  Minimum Technology Guidance on Double Liner Systems  "; : COLOR 10: PRINT "³"
2190 PRINT "³"; : COLOR 11: PRINT "   for Landfills and Surface Impoundments - Design, Construction and          "; : COLOR 10: PRINT "³"
2121 PRINT "³"; : COLOR 11: PRINT "   Operation.  EPA/530-SW-85-014.  NTIS PB 87-151072.  OSWER, Washington, DC. "; : COLOR 10: PRINT "³"
2183 PRINT "³"; : COLOR 11: PRINT "                                                                              "; : COLOR 10: PRINT "³"
2220 PRINT "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
COLOR 15, 0
2230 PRINT "                           Press any key to continue...";
3250 A$ = INKEY$: IF A$ = "" GOTO 3250
     END








