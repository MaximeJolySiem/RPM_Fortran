@echo off
setlocal
gfortran -fopenmp Modules/ModuleMath.f90 Modules/ModuleRead.f90 Modules/ModuleDerivatives.f90 Modules/ModuleFunction.f90 Modules/ModuleWrite.f90 Main.f90 -J Modules/ -I Modules/ -o RPM_Window_executable
RPM_Window_executable.exe