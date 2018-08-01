PROGRAM main

use ModuleDerivatives ! Appel du module derivatives
use ModuleFunction ! Appel du module function
! use omp_lib ! Appel du module pour parallelisation

implicit none

!Declaration des variables
integer, parameter :: seed = 86456

real :: x_min, x_max, y_min, y_max, delta, x
real :: total_time,test,Particle_available_counter, Lambda_min
real :: testprob
real :: start, finish
real :: dt, T, Volume

integer :: nx,ny,line_no,i,j
integer :: Nparticle, Nparticle_available
integer :: TimeStep,Nt
integer :: hour, minute, seconde, k

real, allocatable :: RANS_data(:,:), X_VELOCITY(:,:), Y_VELOCITY(:,:), TKE(:,:), SDR(:,:), Z_VORTICITY(:,:), vtkMask(:,:)
real, allocatable :: StreamFunction(:,:),Lambda(:,:),Ux(:,:), Uy(:,:), dUy_x(:,:), dUx_y(:,:), Lsum_X(:,:), Lsum_Y(:,:)
real, allocatable :: Particle(:,:), ParticleSeeder(:,:), NumberPart(:), Vorticity(:,:)
real, allocatable :: PartSeeder(:,:)
real, dimension(5) :: MeshCaracteristics

character*10 string, format

data format /'(F10.2)'/

! call init_random_seed()
call srand(seed)

! Mesh caracteristics
delta = 2.e-4;
x_min = 0.32;
x_max = 0.48;
y_min = -0.02;
y_max = 0.02;

MeshCaracteristics(1) = x_min
MeshCaracteristics(2) = x_max
MeshCaracteristics(3) = y_min
MeshCaracteristics(4) = y_max
MeshCaracteristics(5) = delta

nx = (x_max-x_min)/delta+1
ny = (y_max-y_min)/delta+1

! Particle number
write(*,*) 'Enter the particle number'
read(*,*) Nparticle
Volume = sqrt((x_max-x_min)*(y_max-y_min)/Nparticle)
Lambda_min = 0

! Time caracteristics
write(*,*) 'Enter time step (s)'
read (*,*) dt
write(*,*) 'Enter final time (s)'
read(*,*) T

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

do i = 1,ny
	do j = 1,nx
		X_VELOCITY(i,j) = RANS_data(j+(i-1)*nx,1)
		Y_VELOCITY(i,j) = RANS_data(j+(i-1)*nx,2)
		TKE(i,j) = RANS_data(j+(i-1)*nx,3)
		SDR(i,j) = RANS_data(j+(i-1)*nx,4)
		Z_VORTICITY(i,j) = RANS_data(j+(i-1)*nx,5)
		vtkMask(i,j) = RANS_data(j+(i-1)*nx,6)
		if (6*sqrt(TKE(i,j))/SDR(i,j) >= Lambda_min) then
			Lambda(i,j) = 6*sqrt(TKE(i,j))/SDR(i,j)
		else
			Lambda(i,j) = Lambda_min
		end if
	end do
end do
deallocate (SDR)

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
do i = 1,Nparticle
	if (Get_value(ParticleSeeder(i,2),ParticleSeeder(i,3),MeshCaracteristics,vtkMask) == 1) then
		NumberPart(i) = 1
		Particle_available_counter = Particle_available_counter+1
	end if
end do

Nparticle_available = floor(Particle_available_counter)

allocate (Particle(Nparticle_available,4))

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


call Calc_Fluctuation(MeshCaracteristics,Particle,TKE,Lambda,StreamFunction,vtkMask)


allocate (dUy_x(ny,nx),dUx_y(ny,nx),Lsum_X(ny,nx),Lsum_Y(ny,nx),Vorticity(ny,nx))

! Moving particle, calculating the stream function for each step
do i = 1,Nt
	call cpu_time(start)
	call MoveParticle(dt,MeshCaracteristics,Particle,X_VELOCITY,Y_VELOCITY,PartSeeder)
	!call WriteParticle(TimeStep,Particle)
	call Calc_Fluctuation(MeshCaracteristics,Particle,TKE,Lambda,StreamFunction,vtkMask)
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


	call cpu_time(finish)



	if (i == 1) then
		total_time = Nt*(finish-start)
		print *, 'ESTIMATION TIME NEEDED: '//hourstr(Total_time-(finish-start))
	end if
	print *, 'Done step number '//trim(str(TimeStep))//' over '//trim(str(Nt))
	if (Total_time-finish>0) then
		print *, 'Time until end: '//hourstr(Total_time-finish)
	else
		print *, 'Time until end: '//hourstr(0.)
	end if
	TimeStep = TimeStep+1
end do




END PROGRAM
