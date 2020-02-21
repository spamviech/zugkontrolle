#!/bin/sh

set -e
# Any subsequent(*) commands which fail will cause the shell script to exit immediately

APP=./app/*.hs
SRC0=./src/Zug/*.hs
SRC1=./src/Zug/*/*.hs
SRC2=./src/Zug/*/*/*.hs
SRC3=./src/Zug/*/*/*/*.hs
for f in $APP $SRC0 $SRC1 $SRC2 $SRC3
do
    echo "floskell $f"
    floskell $f
done