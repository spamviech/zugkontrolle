:: this batch script will run floskell on all source code files
:: it is required, since the hie-version produces weird results for doc strings (e.g. with gadts)
@echo off

FOR /R %%f IN (app\*) DO CALL :RUN_FLOSKELL %%f || GOTO HANDLE_FAIL
FOR /R %%f IN (src\Zug\*) DO CALL :RUN_FLOSKELL %%f || GOTO HANDLE_FAIL
FOR /D %%d IN (src\Zug\*) DO FOR /R %%f in (%%d\*) DO CALL :RUN_FLOSKELL %%f || GOTO HANDLE_FAIL
FOR /D %%c IN (src\Zug\*) DO FOR /D %%d IN (%%c\*) DO FOR /R %%f in (%%d\*) DO CALL :RUN_FLOSKELL %%f || GOTO HANDLE_FAIL
FOR /D %%b IN (src\Zug\*) DO FOR /D %%c IN (%%b\*) DO FOR /D %%d IN (%%c\*) DO FOR /R %%f in (%%d\*) DO CALL :RUN_FLOSKELL %%f || GOTO HANDLE_FAIL
FOR /D %%a IN (src\Zug\*) DO FOR /D %%b IN (%%a\*) DO FOR /D %%c IN (%%b\*) DO FOR /D %%d IN (%%c\*) DO FOR /R %%f in (%%d\*) DO CALL :RUN_FLOSKELL %%f || GOTO HANDLE_FAIL

GOTO END

:RUN_FLOSKELL
ECHO floskell %~1
floskell %~1 || GOTO HANDLE_FAIL
EXIT /B 0

:HANDLE_FAIL
EXIT /B 1

:END
EXIT /B 0
