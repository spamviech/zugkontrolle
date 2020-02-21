FOR /R %%f IN (app\*) DO floskell %%f || GOTO HANDLE_FAIL
FOR /R %%f IN (src\Zug\*) DO floskell %%f || GOTO HANDLE_FAIL
FOR /D %%d IN (src\Zug\*) DO FOR /R %%f in (%%d\*) DO floskell %%f || GOTO HANDLE_FAIL
FOR /D %%c IN (src\Zug\*) DO FOR /D %%d IN (%%c\*) DO FOR /R %%f in (%%d\*) DO floskell %%f || GOTO HANDLE_FAIL
FOR /D %%b IN (src\Zug\*) DO FOR /D %%c IN (%%b\*) DO FOR /D %%d IN (%%c\*) DO FOR /R %%f in (%%d\*) DO floskell %%f || GOTO HANDLE_FAIL

GOTO END

:HANDLE_FAIL
EXIT /B 1

:END
EXIT /B 0
