#!/bin/bash
cd "$(dirname "$BASH_SOURCE")" || {
    echo "Error getting script directory" >&2
    exit 1
}
gfortran -fopenmp Modules/ModuleMath.f90 Modules/ModuleFilter.f90 Modules/ModuleRead.f90 Modules/ModuleDerivatives.f90 Modules/ModuleFunction.f90 Modules/ModuleWrite.f90 Main.f90 -J Modules/ -I Modules/ -o RPM_OSX_executable
./RPM_OSX_executable