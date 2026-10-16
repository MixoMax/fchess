#!/bin/sh
# Builds libchess.so (shared library), ./perft, and ./search.
# Override the compiler with:  FC=ifx ./build.sh
set -e
FC=${FC:-gfortran}
FFLAGS="-O3 -std=f2008 -fwrapv -Wall -Wno-unused-dummy-argument -mbmi2 -flto -fopenmp -fno-omit-frame-pointer -fno-optimize-sibling-calls -fno-stack-check -fno-stack-protector"

mkdir -p bin

$FC $FFLAGS -fPIC -shared -o bin/libfchess.so chess_engine.f90 chess_search.f90
$FC $FFLAGS -o bin/perft chess_engine.f90 perft_main.f90
$FC $FFLAGS -o bin/search chess_engine.f90 chess_search.f90 search_main.f90
echo "built: bin/ libfchess.so, perft, search"