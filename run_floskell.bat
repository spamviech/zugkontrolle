:: this batch script will run floskell on all source code files
:: it is required, since the hie-version produces weird results for doc strings (e.g. with gadts)
@echo off

CALL :APP
CALL :SRC Zugkontrolle-Base
CALL :SRC Zugkontrolle-Cmd
CALL :SRC Zugkontrolle-Gtk

GOTO END

:APP
ECHO -------------------App-------------------
FOR /R %%f IN (app\*) DO CALL :RUN_FLOSKELL %%f || GOTO HANDLE_FAIL
FOR /R %%f IN (app\Zug\*) DO CALL :RUN_FLOSKELL %%f || GOTO HANDLE_FAIL
EXIT /B 0

:SRC
Echo -------------------%~1 -------------------
CALL :SEARCH_DIR %~1\src\Zug
EXIT /B 0

:SEARCH_DIR
FOR /R %%f in (%~1\*) DO CALL :RUN_FLOSKELL %%f || GOTO HANDLE_FAIL
FOR /D %%d in (%~1\*) DO CALL :SEARCH_DIR %%d
EXIT /B 0

:RUN_FLOSKELL
ECHO floskell %~1
floskell %~1 || GOTO HANDLE_FAIL
EXIT /B 0

:HANDLE_FAIL
EXIT /B 1

:END
EXIT /B 0
