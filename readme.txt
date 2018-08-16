RPM : an aeroacoustic tool for synthesizing turbulence using steady computation results.

The code is written using Fortran 90 format. A GUI interface written in Python is enable for making SNGM_config file to configure the RPM simulation.

To use the GUI Python script, please install Python. The cript is using tkinter and is working for version over 2.7 included.

To use the RPM simulation, please install gfortran.

For Windows : you can install MinGW Installation Manager, then instal gfortran with this last one. Please make sure that libgomp is also installed, as the script is using openmp for parallel computing.
For Linux : type "sudo apt-get install gfortran" in a terminal, it will be installed directly.
For Mac : please refer to : https://gcc.gnu.org/wiki/GFortranBinaries

The code needs a .vtk file containing all the RANS result. This file format can be obtained using Paraview and exporting the datas in .vtk format. You have to make sure that : 
1) There is a line DIMENSIONS XXX XXX X
2) There is a line SPACING XXX XXX X
3) There is a line ORIGIN XXX XXX X
4) There is a line POINT_DATA XXX
5) The data are written in the following order : X_VELOCITY, Y_VELOCITY, TKE, SDR, Z_VORTICITY, vtkValidPointMask

