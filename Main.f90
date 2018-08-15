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

real :: x_min, x_max, y_min, y_max, delta
real :: total_time, Lambda_min
real :: dt, T, Volume
real :: pi = 3.14159265358

complex :: i1 = (0,-1)

integer :: nx,ny,i,j,Radius,ii
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
real, allocatable :: Particle(:,:), Vorticity(:,:)
real, allocatable :: PartSeeder(:,:)
real, allocatable :: X_VELOCITYTemp(:), Y_VELOCITYTemp(:), TKETemp(:), SDRTemp(:), Z_VORTICITYTemp(:), vtkMaskTemp(:)
real, dimension(5) :: MeshCaracteristics

complex, allocatable :: L_fourier(:,:,:)

character(len=:), allocatable :: File_path, FilterType, ScalingType, PathSave
character(len=20) :: string_temp_print

logical :: dir_e










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

nx = int((x_max-x_min)/delta+1)
ny = int((y_max-y_min)/delta+1)

! Enable parallel computing
Get_Number_Thread = GetIntVtkValue("Parallel_Number_Thread")
call omp_set_num_threads(Get_Number_Thread)

write(*,*) ''
write(*,*) 'PARALLEL COMPUTING ENABLE'
write(*,*) 'NUMBER OF THREAD USED : ', str(Get_Number_Thread)
write(*,*) ''


!Writing parameters
write_Particle = GetIntVtkValue("write_Particle")
write_Vel = GetIntVtkValue("write_Vel")
write_Vor = GetIntVtkValue("write_Vor")
write_Lsum = GetIntVtkValue("write_Lsum")
write_binary_format = GetIntVtkValue("write_binary_format")
write_fourier = GetIntVtkValue("write_fourier")

! Particle number
Nparticle = GetIntVtkValue("Nparticles")
Volume = sqrt((x_max-x_min)*(y_max-y_min)/Nparticle)
Lambda_min = GetRealVtkValue("turb_Lmin")
Radius = GetIntVtkValue("Rconst")

! Time caracteristics
dt = GetRealVtkValue("viz_interval")
T = GetRealVtkValue("end_time")
Nt = int(T/dt)

! Frequency caracteristics
Number_freq = GetIntVtkValue("Number_freq")





!Verify and create Data folders
if (write_Particle+write_Vel+write_Vor+write_Lsum > 0) then
	inquire(file='./Output_time/.', exist=dir_e)

	if (.not. dir_e ) then
	  call system('mkdir Output_time')
	end if
end if


if (write_fourier == 1) then
	inquire(file='./Output_frequency/.', exist=dir_e)

	if (.not. dir_e ) then
	  call system('mkdir Output_frequency')
	end if
	write(*,*) 'FOURIER TRANSFORM ENABLE'
	write(*,*) 'Number of frequencies : ', str(Number_freq)
	write(*,*) 'Frequency = ', trim(str(0)), 'Hz to ', trim(str(int(Number_freq/(T)))), 'Hz. Resolution = ' , trim(str(int(1/T))), 'Hz'
	write(*,*) ''
end if





allocate (X_VELOCITY(ny,nx),Y_VELOCITY(ny,nx),TKE(ny,nx),SDR(ny,nx),Z_VORTICITY(ny,nx),vtkMask(ny,nx),Lambda(ny,nx))
allocate(X_VELOCITYTemp(nx*ny),Y_VELOCITYTemp(nx*ny),TKETemp(nx*ny),SDRTemp(nx*ny),Z_VORTICITYTemp(nx*ny),vtkMaskTemp(nx*ny))



write(*,*) 'READING RANS DATA'
write(*,*) 'Number of points : ', str(nx*ny)
write(*,*) ''

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
write(*,*) 'RPM PARAMETERS'
write(*,*) 'Filter type : ', FilterType
write(*,*) 'Radius of consideration : ', str(Radius)
write(string_temp_print,*) dt
write(*,*) 'Time step : ', trim(string_temp_print)
write(string_temp_print,*) T
write(*,*) 'End time : ', trim(string_temp_print)
write(*,*) 'Number of samples : ', trim(str(Nt))
write(*,*) ''

call WriteInformation(nx, ny, write_Particle, write_Vel, write_Vor, write_Lsum, &
	& write_fourier, write_binary_format, Particle, Nt, Number_freq)


write(*,*) ''
write(*,*) 'BEGINING COMPUTING'
write(*,*) ''




! Moving particle, calculating the stream function for each step
do i = Init_T,Nt



	CALL SYSTEM_CLOCK(COUNT_RATE=nb_ticks_sec, COUNT_MAX=nb_ticks_max)
	CALL SYSTEM_CLOCK(COUNT=nb_ticks_initial)
	


	call MoveParticle(dt,MeshCaracteristics,Particle,X_VELOCITY,Y_VELOCITY,PartSeeder)
	call Calc_Fluctuation(MeshCaracteristics,Particle,TKE,Lambda,StreamFunction,vtkMask, &
		& Radius,FilterType,ScalingType)


	StreamFunction = StreamFunction*Volume

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
			L_fourier(:,1,ii) = L_fourier(:,1,ii) + Lsum(:,1)*exp(2*i1*pi*(ii-1)*i/Nt)*dt*0.5*(1-cos(2*pi*(i-1)/(Nt-1)))*1.63
			L_fourier(:,2,ii) = L_fourier(:,2,ii) + Lsum(:,2)*exp(2*i1*pi*(ii-1)*i/Nt)*dt*0.5*(1-cos(2*pi*(i-1)/(Nt-1)))*1.63
		end do
		!$OMP END PARALLEL DO
	end if







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
		call WriteFourier(L_fourier,'fw_lamb2_',write_fourier)
	else
		call WriteBinFourier(L_fourier,'fw_lamb5e5_',write_fourier)
	end if
end if


print *, 'Simulation finished.'




END PROGRAM
