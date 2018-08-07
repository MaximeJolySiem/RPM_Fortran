PROGRAM main

use ModuleDerivatives ! Appel du module derivatives
use ModuleFunction ! Appel du module function
use ModuleRead ! Appel du module read
! use omp_lib ! Appel du module pour parallelisation

implicit none

!Declaration des variables
integer, parameter :: seed = 86456

real :: x_min, x_max, y_min, y_max, delta, x
real :: total_time,test, Lambda_min
real :: testprob
real :: start, finish
real :: dt, T, Volume

integer :: nx,ny,line_no,i,j
integer :: Nparticle, Nparticle_available, Particle_available_counter
integer :: TimeStep,Nt
integer :: hour, minute, seconde, k

INTEGER :: nb_ticks_t0,nb_ticks_initial, nb_ticks_final, nb_ticks_max, nb_ticks_sec, nb_ticks
REAL :: elapsed_time  ! real time in seconds

real, allocatable :: RANS_data(:,:), X_VELOCITY(:,:), Y_VELOCITY(:,:), TKE(:,:), SDR(:,:), Z_VORTICITY(:,:), vtkMask(:,:)
real, allocatable :: StreamFunction(:,:),Lambda(:,:),Ux(:,:), Uy(:,:), dUy_x(:,:), dUx_y(:,:), Lsum_X(:,:), Lsum_Y(:,:)
real, allocatable :: Particle(:,:), ParticleSeeder(:,:), NumberPart(:), Vorticity(:,:)
real, allocatable :: PartSeeder(:,:)
real, allocatable :: X_VELOCITYTemp(:), Y_VELOCITYTemp(:), TKETemp(:), SDRTemp(:), Z_VORTICITYTemp(:), vtkMaskTemp(:)
real, dimension(5) :: MeshCaracteristics

integer :: Parallel_computing

character*10 string, format

data format /'(F10.2)'/

! call init_random_seed()
call srand(seed)

call omp_set_num_threads(8) ! set the number of threads to 8

! Mesh caracteristics
x_min = GetRealVtkValue("x_min")
x_max = GetRealVtkValue("x_max")
y_min = GetRealVtkValue("y_min")
y_max = GetRealVtkValue("y_max")
delta = GetRealVtkValue("Delta")

MeshCaracteristics(1) = x_min
MeshCaracteristics(2) = x_max
MeshCaracteristics(3) = y_min
MeshCaracteristics(4) = y_max
MeshCaracteristics(5) = delta

nx = (x_max-x_min)/delta+1
ny = (y_max-y_min)/delta+1

! Enable parallel computing
Parallel_computing = GetIntVtkValue("Parallel_computing")

! Particle number
Nparticle = GetIntVtkValue("Nparticles")
Volume = sqrt((x_max-x_min)*(y_max-y_min)/Nparticle)
Lambda_min = 0

! Time caracteristics
dt = GetRealVtkValue("viz_interval")
T = GetRealVtkValue("end_time")

if (dt>T) then
	stop " : Enter final time greater than time step"
end if

write(*,*) 'BEGINING COMPUTING'

Nt = T/dt

! Take RANS data in array
allocate (RANS_data(nx*ny,6))

open (18, file='RANS.csv')
read(18,*)

do line_no=1,nx*ny
	read(18,*) RANS_data(line_no,:)
end do

allocate (X_VELOCITY(ny,nx),Y_VELOCITY(ny,nx),TKE(ny,nx),SDR(ny,nx),Z_VORTICITY(ny,nx),vtkMask(ny,nx),Lambda(ny,nx))

allocate(X_VELOCITYTemp(nx*ny),Y_VELOCITYTemp(nx*ny),TKETemp(nx*ny),SDRTemp(nx*ny),Z_VORTICITYTemp(nx*ny),vtkMaskTemp(nx*ny))

X_VELOCITYTemp = GetVtkField('X_VELOCITY')
Y_VELOCITYTemp = GetVtkField('Y_VELOCITY')
Z_VORTICITYTemp = GetVtkField('Z_VORTICITY')
TKETemp = GetVtkField('TKE')
SDRTemp = GetVtkField('SDR')
vtkMaskTemp = GetVtkField('vtkValidPointMask')


do i = 1,ny
	do j = 1,nx
		X_VELOCITY(i,j) = X_VELOCITYTemp(j+(i-1)*nx)
		Y_VELOCITY(i,j) = Y_VELOCITYTemp(j+(i-1)*nx)
		TKE(i,j) = TKETemp(j+(i-1)*nx)
		SDR(i,j) = SDRTemp(j+(i-1)*nx)
		Z_VORTICITY(i,j) = Z_VORTICITYTemp(j+(i-1)*nx)
		vtkMask(i,j) = vtkMaskTemp(j+(i-1)*nx)
		if (6*sqrt(TKE(i,j))/SDR(i,j) >= Lambda_min) then
			Lambda(i,j) = 6*sqrt(TKE(i,j))/SDR(i,j)
		else
			Lambda(i,j) = Lambda_min
		end if
	end do
end do

deallocate (X_VELOCITYTemp,Y_VELOCITYTemp,Z_VORTICITYTemp,TKETemp,SDRTemp,vtkMaskTemp,SDR)

! Seeding
PartSeeder = GetSeeder(X_VELOCITY, Y_VELOCITY, delta)

allocate (StreamFunction(ny,nx))
allocate (Ux(ny,nx))
allocate (Uy(ny,nx))

! Initialisation particle
allocate (ParticleSeeder(Nparticle,4))
do i = 1,Nparticle
	ParticleSeeder(i,1) = i
	ParticleSeeder(i,2) = x_min + rand()*(x_max-x_min)
	ParticleSeeder(i,3) = y_min + rand()*(y_max-y_min)
	ParticleSeeder(i,4) = r4_normal_01 ()
end do

! Removing particle in airfoil
allocate (NumberPart(Nparticle))

Particle_available_counter = 0

do i = 1,Nparticle
	if (Get_value(ParticleSeeder(i,2),ParticleSeeder(i,3),MeshCaracteristics,vtkMask) == 1) then
		NumberPart(i) = 1
		Particle_available_counter = Particle_available_counter+1
	end if
end do

allocate (Particle(Particle_available_counter,4))

k = 1
do i = 1,Nparticle
	if (NumberPart(i) == 1) then
		Particle(k,1) = ParticleSeeder(i,1)
		Particle(k,2) = ParticleSeeder(i,2)
		Particle(k,3) = ParticleSeeder(i,3)
		Particle(k,4) = ParticleSeeder(i,4)
		k = k+1
	end if
end do

deallocate (NumberPart,ParticleSeeder)

! First step
TimeStep = 1
call WriteParticle(TimeStep,Particle)


call Calc_Fluctuation_opt(MeshCaracteristics,Particle,TKE,Lambda,StreamFunction,vtkMask,Parallel_computing)


allocate (dUy_x(ny,nx),dUx_y(ny,nx),Lsum_X(ny,nx),Lsum_Y(ny,nx),Vorticity(ny,nx))

! Moving particle, calculating the stream function for each step
do i = 1,Nt
	CALL SYSTEM_CLOCK(COUNT_RATE=nb_ticks_sec, COUNT_MAX=nb_ticks_max)
	CALL SYSTEM_CLOCK(COUNT=nb_ticks_initial)
	call MoveParticle(dt,MeshCaracteristics,Particle,X_VELOCITY,Y_VELOCITY,PartSeeder)
	!call WriteParticle(TimeStep,Particle)
	call Calc_Fluctuation_opt(MeshCaracteristics,Particle,TKE,Lambda,StreamFunction,vtkMask,Parallel_computing)
	StreamFunction = StreamFunction*Volume
	call der2_x(Uy, StreamFunction, vtkMask, MeshCaracteristics) 
	Uy = -Uy !Uy = -dpsi/dx
	call der2_y(Ux, StreamFunction, vtkMask, MeshCaracteristics) !Ux = dpsi/dy
	call der2_x(dUy_x, Uy, vtkMask, MeshCaracteristics)
	call der2_y(dUx_y, Ux, vtkMask, MeshCaracteristics)
	Vorticity = dUy_x-dUx_y
	Lsum_X = Y_VELOCITY*Vorticity + Uy*Z_VORTICITY
	Lsum_Y = -X_VELOCITY*Vorticity - Ux*Z_VORTICITY



	call WriteData(TimeStep,Lsum_X,'Lsum_X')
	call WriteData(TimeStep,Lsum_Y,'Lsum_Y')
	call WriteData(TimeStep,Vorticity,'Vor_Z')
	call WriteData(TimeStep,Ux,'Vel_X')
	call WriteData(TimeStep,Uy,'Vel_Y')
	call WriteParticle(TimeStep,Particle)

	CALL SYSTEM_CLOCK(COUNT=nb_ticks_final)

	if (i == 1) then
		nb_ticks_t0 = nb_ticks_initial
		nb_ticks = nb_ticks_final - nb_ticks_initial
		elapsed_time = REAL(nb_ticks) / nb_ticks_sec
		total_time = Nt*(elapsed_time)
		print *, 'ESTIMATION TIME NEEDED: '//hourstr(Total_time-(elapsed_time))
	end if
	
	print *, 'Done step number '//trim(str(TimeStep))//' over '//trim(str(Nt))
	
	nb_ticks = nb_ticks_final - nb_ticks_t0
    elapsed_time = REAL(nb_ticks) / nb_ticks_sec
	
	if (total_time-elapsed_time>0) then
		print *, 'Time until end: '//hourstr(Total_time-elapsed_time)
		
	else
		print *, 'Time until end: '//hourstr(0.)
		
	end if
	
	TimeStep = TimeStep+1
end do




END PROGRAM
