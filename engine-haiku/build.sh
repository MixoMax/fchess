#!/bin/sh
# Builds libchess.so (shared library) and ./perft (verification driver).
# Override the compiler with:  FC=ifx ./build.sh
set -e
FC=${FC:-gfortran}
FFLAGS="-O3 -std=f2008 -fwrapv -Wall -Wno-unused-dummy-argument"

$FC $FFLAGS -fPIC -shared -o libchess.so chess_engine.f90
$FC $FFLAGS -o perft chess_engine.f90 perft_main.f90
echo "built: libchess.so, perft"