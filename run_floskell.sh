#!/bin/sh

# this shell script will run floskell on all source code files
# it is required, since the hie-version produces weird results for doc strings (e.g. with gadts)

set -e
# Any subsequent(*) commands which fail will cause the shell script to exit immediately

APP0=./app/*.hs
SRC00=./Zugkontrolle-Base/src/Zug/*.hs
SRC01=./Zugkontrolle-Base/src/Zug/*/*.hs
SRC02=./Zugkontrolle-Base/src/Zug/*/*/*.hs
SRC03=./Zugkontrolle-Base/src/Zug/*/*/*/*.hs
SRC04=./Zugkontrolle-Base/src/Zug/*/*/*/*/*.hs
SRC10=./Zugkontrolle-Cmd/src/Zug/*.hs
SRC11=./Zugkontrolle-Cmd/src/Zug/*/*.hs
SRC12=./Zugkontrolle-Cmd/src/Zug/*/*/*.hs
SRC13=./Zugkontrolle-Cmd/src/Zug/*/*/*/*.hs
SRC14=./Zugkontrolle-Cmd/src/Zug/*/*/*/*/*.hs
SRC20=./Zugkontrolle-Gtk/src/Zug/*.hs
SRC21=./Zugkontrolle-Gtk/src/Zug/*/*.hs
SRC22=./Zugkontrolle-Gtk/src/Zug/*/*/*.hs
SRC23=./Zugkontrolle-Gtk/src/Zug/*/*/*/*.hs
SRC24=./Zugkontrolle-Gtk/src/Zug/*/*/*/*/*.hs
for f in $APP0 $SRC00 $SRC01 $SRC02 $SRC03 $SRC04 $SRC10 $SRC11 $SRC12 $SRC13 $SRC14 $SRC20 $SRC21 $SRC22 $SRC23 $SRC24
do
    echo "floskell $f"
    floskell $f
done