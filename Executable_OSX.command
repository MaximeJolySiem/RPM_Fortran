#!/bin/bash
cd "$(dirname "$BASH_SOURCE")"
gfortran -O3 -ffast-math -march=native -funroll-loops -fno-protect-parens -flto -fopenmp Modules/ModuleMath.f90 Modules/ModuleFilter.f90 Modules/ModuleRead.f90 Modules/ModuleDerivatives.f90 Modules/ModuleFunction.f90 Modules/ModuleWrite.f90 Main.f90 -J Modules/ -I Modules/ -o RPM_OSX_executable
./RPM_OSX_executable