@echo off
rem Select this folder:
cd DPTEST

REM Show some info before starting
cls
echo.
echo - The freeware DOS program [1mDataPerfect 2.3 Testdrive[0m is about to start...
echo.
echo - This is merely to confirm vDos is correctly working on your system (running     a [1mDOS program[0m).
echo.
echo - As in WordPerfect for DOS, use the [1mF7[0m key to step back or exit this program.
echo.
pause

rem Start DP26YI with parameter /s:
DP26YI /s

REM Show some more after DP26YI closes
cls
echo.
echo - Back at the command prompt (we didn't EXIT vDos).
echo.
echo - You can enter [1mDOS commands[0m here, for instance [1mEXIT[0m to close this window.
echo.
echo - Read the [1mReadme.pdf[0m document.
echo.
echo - Edit the [1mautoexec.txt[0m file in Windows (Notepad) to start your DOS program.
echo.
echo - Then have a look at the configuration settings in [1mconfig.txt[0m.

rem Return to the vDos C:\ folder
cd ..

rem The end of this batch file, so we return to the autoexec file that called it.