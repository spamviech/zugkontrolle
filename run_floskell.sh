#!/bin/sh

# this shell script will run floskell on all source code files
# it is required, since the hie-version produces weird results for doc strings (e.g. with gadts)

set -e
# Any subsequent(*) commands which fail will cause the shell script to exit immediately

APP0=./app/*.hs
SRC0=./src/Zug/*.hs
SRC1=./src/Zug/*/*.hs
SRC2=./src/Zug/*/*/*.hs
SRC3=./src/Zug/*/*/*/*.hs
for f in $APP0 $SRC0 $SRC1 $SRC2 $SRC3
do
    echo "floskell $f"
    floskell $f
done