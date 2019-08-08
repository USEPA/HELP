:::::::::::::::::::::::::::::::::::::::::::
:: SYNGEN builds fine.
::gfortran SYNGEN.FOR -o SYNGEN.exe
:::::::::::::::::::::::::::::::::::::::::::
:: Need to build all others, but various errors. Probably due to F77 instead of F90:
::gfortran -std=legacy ASCIMISC.FOR -o ASCIMISC.exe
::gfortran -std=legacy ASCIPREC.FOR -o ASCIPREC.exe
::gfortran -std=legacy ASCISRAD.FOR -o ASCISRAD.exe
::gfortran -std=legacy ASCITEMP.FOR -o ASCITEMP.exe

::gfortran -std=legacy UTLTY.OBJ HELP3O.FOR -o HELP30.exe
:::::::::::::::::::::::::::::::::::::::::::

gfortran -std=legacy SYNGEN.FOR ASCIMISC.FOR ASCIPREC.FOR ASCISRAD.FOR ASCITEMP.FOR HELP3O.FOR -o HELP30.exe
