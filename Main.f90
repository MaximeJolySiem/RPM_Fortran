PROGRAM main

use ModuleMath
use ModuleDerivatives ! Appel du module derivatives
use ModuleFunction ! Appel du module function
use ModuleRead ! Appel du module read
use ModuleWrite
! use omp_lib ! Appel du module pour parallelisation

implicit none

!Declaration des variables
integer, parameter :: seed = 86456

real :: x_min, x_max, y_min, y_max, delta, x
real :: total_time, Lambda_min
real :: dt, T, Volume
real :: pi = 3.14159265358

complex :: i1 = (0,-1)

integer :: nx,ny,i,j,Radius,k,ii
integer :: Nparticle
integer :: TimeStep,Nt,Init_T
integer :: write_Particle, write_Vel, write_Vor, write_Lsum, write_binary_format, write_fourier
integer :: IsSave
integer :: Get_Number_Thread
integer :: Number_freq

INTEGER :: nb_ticks_t0,nb_ticks_initial, nb_ticks_final, nb_ticks_max, nb_ticks_sec, nb_ticks
REAL :: elapsed_time  ! real time in seconds

real, allocatable :: X_VELOCITY(:,:), Y_VELOCITY(:,:), TKE(:,:), SDR(:,:), Z_VORTICITY(:,:), vtkMask(:,:)
real, allocatable :: StreamFunction(:,:),Lambda(:,:),Ux(:,:), Uy(:,:), dUy_x(:,:), dUx_y(:,:), Lsum_X(:,:), Lsum_Y(:,:)
real, allocatable :: Lsum(:,:), Vorticity_write(:,:), Velocity(:,:)
real, allocatable :: Particle(:,:), NumberPart(:), Vorticity(:,:)
real, allocatable :: PartSeeder(:,:)
real, allocatable :: X_VELOCITYTemp(:), Y_VELOCITYTemp(:), TKETemp(:), SDRTemp(:), Z_VORTICITYTemp(:), vtkMaskTemp(:)
real, dimension(5) :: MeshCaracteristics

complex, allocatable :: L_fourier(:,:,:)

character(len=:), allocatable :: File_path, FilterType, ScalingType, PathSave

integer :: Parallel_computing

logical :: dir_e







!Verify and create Data folder
inquire(file='./Output/.', exist=dir_e)

if (.not. dir_e ) then
  call system('mkdir Output')
end if


! call init_random_seed()
call srand(seed)

!VTK Path file
allocate(character(len(GetStringVtkValue("cfddatafile"))) :: File_path)
File_path = GetStringVtkValue("cfddatafile")

allocate(character(len(GetStringVtkValue("filter"))) :: FilterType)
FilterType = GetStringVtkValue("filter")

allocate(character(len(GetStringVtkValue("amplitudeScalingParticleOrGrid"))) :: ScalingType)
ScalingType = GetStringVtkValue("amplitudeScalingParticleOrGrid")

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
Get_Number_Thread = GetIntVtkValue("Parallel_Number_Thread")
call omp_set_num_threads(Get_Number_Thread)
if (Parallel_computing == 1) then
	write(*,*) 'PARALLEL COMPUTING ENABLE'
	write(*,*) 'NUMBER OF THREAD USED : ', str(Get_Number_Thread)
	write(*,*) ''
end if

!Writing parameters
write_Particle = GetIntVtkValue("write_Particle")
write_Vel = GetIntVtkValue("write_Vel")
write_Vor = GetIntVtkValue("write_Vor")
write_Lsum = GetIntVtkValue("write_Lsum")
write_binary_format = GetIntVtkValue("write_binary_format")

! Particle number
Nparticle = GetIntVtkValue("Nparticles")
Volume = sqrt((x_max-x_min)*(y_max-y_min)/Nparticle)
Lambda_min = GetRealVtkValue("turb_Lmin")
Radius = GetIntVtkValue("Rconst")

! Time caracteristics
dt = GetRealVtkValue("viz_interval")
T = GetRealVtkValue("end_time")
write_fourier = GetRealVtkValue("write_fourier")
Number_freq = GetIntVtkValue("Number_freq")

if (dt>T) then
	stop " : Enter final time greater than time step"
end if

Nt = T/dt

allocate (X_VELOCITY(ny,nx),Y_VELOCITY(ny,nx),TKE(ny,nx),SDR(ny,nx),Z_VORTICITY(ny,nx),vtkMask(ny,nx),Lambda(ny,nx))
allocate(X_VELOCITYTemp(nx*ny),Y_VELOCITYTemp(nx*ny),TKETemp(nx*ny),SDRTemp(nx*ny),Z_VORTICITYTemp(nx*ny),vtkMaskTemp(nx*ny))



write(*,*) 'READING RANS DATA'
write(*,*) 'Number of points : ', str(nx*ny)

X_VELOCITYTemp = GetVtkField('X_VELOCITY',File_path)
Y_VELOCITYTemp = GetVtkField('Y_VELOCITY',File_path)
Z_VORTICITYTemp = GetVtkField('Z_VORTICITY',File_path)
TKETemp = GetVtkField('TKE',File_path)
SDRTemp = GetVtkField('SDR',File_path)
vtkMaskTemp = GetVtkField('vtkValidPointMask',File_path)


do j = 1,nx
	do i = 1,ny
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

! Initialisation particle
write(*,*) 'PARTICLE SEEDING INITIALIZATION'
IsSave = GetIntVtkValue("Enable_save")

if (IsSave == 1) then
	allocate(character(len(GetStringVtkValue("Save_path"))) :: PathSave)
	PathSave= GetStringVtkValue("Save_path")
	Particle = ReadParticleSave(PathSave)
	call GetTimeStepSave(TimeStep,PathSave)
	write(*,*) 'SAVE USED, CURRENT TIME STEP : ', str(TimeStep-1)
else
	! First step
	Particle = InitParticle(MeshCaracteristics,Nparticle,vtkMask)
	TimeStep = 1
end if



Init_T = TimeStep

! Initializing output datas
allocate (StreamFunction(ny,nx))
allocate (Ux(ny,nx))
allocate (Uy(ny,nx))
allocate (dUy_x(ny,nx),dUx_y(ny,nx),Lsum_X(ny,nx),Lsum_Y(ny,nx),Vorticity(ny,nx))
allocate (Velocity(ny*nx,2),Vorticity_write(ny*nx,1),Lsum(nx*ny,2))
allocate (L_fourier(nx*ny,2,Number_freq))












write(*,*) ''
write(*,*) 'BEGINING COMPUTING'
write(*,*) ''


! Moving particle, calculating the stream function for each step
do i = Init_T,Nt



	CALL SYSTEM_CLOCK(COUNT_RATE=nb_ticks_sec, COUNT_MAX=nb_ticks_max)
	CALL SYSTEM_CLOCK(COUNT=nb_ticks_initial)
	





	if (Parallel_computing == 1) then
		call MoveParticle_opt(dt,MeshCaracteristics,Particle,X_VELOCITY,Y_VELOCITY,PartSeeder)
		call Calc_Fluctuation_opt(MeshCaracteristics,Particle,TKE,Lambda,StreamFunction,vtkMask, &
			& Parallel_computing,Radius,FilterType,ScalingType)
	else
		call MoveParticle(dt,MeshCaracteristics,Particle,X_VELOCITY,Y_VELOCITY,PartSeeder)
		call Calc_Fluctuation(MeshCaracteristics,Particle,TKE,Lambda,StreamFunction,vtkMask, &
			& Parallel_computing,Radius,FilterType,ScalingType)
	end if




	StreamFunction = StreamFunction*Volume
	if (Parallel_computing == 1) then
		!$OMP SECTIONS
		!$OMP SECTION
		call der2_x(Uy, -StreamFunction, vtkMask, MeshCaracteristics) !Uy = -dpsi/dx
		!$OMP SECTION
		call der2_y(Ux, StreamFunction, vtkMask, MeshCaracteristics) !Ux = dpsi/dy
		!$OMP END SECTIONS

		!$OMP SECTIONS
		!$OMP SECTION
		call der2_x(dUy_x, Uy, vtkMask, MeshCaracteristics)
		!$OMP SECTION
		call der2_y(dUx_y, Ux, vtkMask, MeshCaracteristics)
		!$OMP END SECTIONS
	else
		call der2_x(Uy, -StreamFunction, vtkMask, MeshCaracteristics) !Uy = -dpsi/dx
		call der2_y(Ux, StreamFunction, vtkMask, MeshCaracteristics) !Ux = dpsi/dy
		call der2_x(dUy_x, Uy, vtkMask, MeshCaracteristics)
		call der2_y(dUx_y, Ux, vtkMask, MeshCaracteristics)
	end if
	Vorticity = dUy_x-dUx_y
	Lsum_X = Y_VELOCITY*Vorticity + Uy*Z_VORTICITY
	Lsum_Y = -X_VELOCITY*Vorticity - Ux*Z_VORTICITY

	Velocity(:,1) = reshape(transpose(Ux),(/nx*ny/))
	Velocity(:,2) = reshape(transpose(Uy),(/nx*ny/))
	Vorticity_write(:,1) = reshape(transpose(Vorticity),(/nx*ny/))
	Lsum(:,1) = reshape(transpose(Lsum_X),(/nx*ny/))
	Lsum(:,2) = reshape(transpose(Lsum_Y),(/nx*ny/))


	
	
	
	
	
	
	if (write_fourier == 1) then
		!$OMP PARALLEL DO PRIVATE(ii)
		do ii = 1,Number_freq
			L_fourier(:,1,ii) = L_fourier(:,1,ii) + Lsum(:,1)*exp(2*i1*pi*(ii-1)/Nt)*dt*0.5*(1-cos(2*pi*(ii-1)/(Nt-1)))*1.63
			L_fourier(:,2,ii) = L_fourier(:,2,ii) + Lsum(:,2)*exp(2*i1*pi*(ii-1)/Nt)*dt*0.5*(1-cos(2*pi*(ii-1)/(Nt-1)))*1.63
		end do
		!$OMP END PARALLEL DO
	end if










	if (Parallel_computing == 1) then
		if (write_binary_format == 0) then
			!$OMP SECTIONS
			!$OMP SECTION
			call WriteParticle(TimeStep,Particle,write_Particle)
			!$OMP SECTION
			call WriteData(TimeStep,Velocity,'Vel',write_Vel)
			!$OMP SECTION
			call WriteData(TimeStep,Vorticity_write,'Vor',write_Vor)
			!$OMP SECTION
			call WriteData(TimeStep,Lsum,'Lsum',write_Lsum)
			!$OMP END SECTIONS

		else
			!$OMP SECTIONS
			!$OMP SECTION
			call WriteBinParticle(TimeStep,Particle,write_Particle)
			!$OMP SECTION
			call WriteBinData(TimeStep,Velocity,'Vel',write_Vel)	
			!$OMP SECTION
			call WriteBinData(TimeStep,Vorticity_write,'Vor',write_Vor)
			!$OMP SECTION
			call WriteBinData(TimeStep,Lsum,'Lsum',write_Lsum)
			!$OMP END SECTIONS
		end if

	else
		if (write_binary_format == 0) then
			call WriteParticle(TimeStep,Particle,write_Particle)
			call WriteData(TimeStep,Velocity,'Vel',write_Vel)
			call WriteData(TimeStep,Vorticity_write,'Vor',write_Vor)
			call WriteData(TimeStep,Lsum,'Lsum',write_Lsum)

		else
			call WriteBinParticle(TimeStep,Particle,write_Particle)
			call WriteBinData(TimeStep,Velocity,'Vel',write_Vel)	
			call WriteBinData(TimeStep,Vorticity_write,'Vor',write_Vor)
			call WriteBinData(TimeStep,Lsum,'Lsum',write_Lsum)
		
		end if
	end if		








	
	
	CALL SYSTEM_CLOCK(COUNT=nb_ticks_final)

	if (i == Init_T) then
		nb_ticks_t0 = nb_ticks_initial
		nb_ticks = nb_ticks_final - nb_ticks_initial
		elapsed_time = REAL(nb_ticks) / nb_ticks_sec
		total_time = (Nt+1-Init_T)*(elapsed_time)
		print *, 'ESTIMATION TIME NEEDED: '//hourstr(Total_time-(elapsed_time))
		print *, ''
	end if
	
	print *, 'Done step number '//trim(str(TimeStep))//' over '//trim(str(Nt))
	
	nb_ticks = nb_ticks_final - nb_ticks_t0
    elapsed_time = REAL(nb_ticks) / nb_ticks_sec
	
	if (elapsed_time*(Nt-Init_T+1)/(i-Init_T+1)-elapsed_time>0) then
		print *, 'Time until end: '//hourstr(elapsed_time*(Nt-Init_T+1)/(i-Init_T+1)-elapsed_time)// &
		& '. Total time estimated : '//hourstr(elapsed_time*(Nt-Init_T+1)/(i-Init_T+1))
		
	else
		print *, 'Time until end: '//hourstr(0.)
		
	end if
	
	TimeStep = TimeStep+1
end do

if (write_fourier == 1) then
	write(*,*) "WRITING FREQUENCY DATA"
	if (write_binary_format == 0) then
		call WriteFourier(L_fourier,'fw_lamb_',write_fourier)
	else
		call WriteBinFourier(L_fourier,'fw_lamb_',write_fourier)
	end if
end if


print *, 'Simulation finished.'




END PROGRAM
