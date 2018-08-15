module ModuleFunction

	use ModuleMath
	use ModuleFilter
	use ModuleRead

	implicit none

	contains

	function Get_value(x,y,MeshCaracteristics,Data_to_get)

		implicit none

		real :: x,y,dx,dy,dfx,dfy,dfxy,Get_value
		real :: delta
		integer :: i_min,j_min
		real,dimension(:,:) :: Data_to_get
		real,dimension(5) :: MeshCaracteristics

		delta = MeshCaracteristics(5)


		j_min = floor((x-MeshCaracteristics(1))/delta)+1
		i_min = floor((y-MeshCaracteristics(3))/delta)+1

		if (j_min>800) then
			print *, x, j_min
		end if

		dfy = Data_to_get(i_min,j_min+1)-Data_to_get(i_min,j_min)
		dfx = Data_to_get(i_min+1,j_min)-Data_to_get(i_min,j_min)
		dfxy = Data_to_get(i_min,j_min)+Data_to_get(i_min+1,j_min+1)-Data_to_get(i_min,j_min+1)-Data_to_get(i_min+1,j_min)

		dx = x-(MeshCaracteristics(1)+(j_min-1)*delta)
		dy = y-(MeshCaracteristics(3)+(i_min-1)*delta)

		Get_value = (dfx*dx + dfy*dy + dfxy*dx*dy)/delta + Data_to_get(i_min,j_min)

	end function Get_value




	function GetSeeder(X_VELOCITY, Y_VELOCITY, delta)
		
		implicit none

		real :: delta
		integer :: nx, ny, i
		real, allocatable :: GetSeeder(:,:), Vx_left(:), Vy_bot(:), Vy_top(:)
		real, dimension(:,:) :: X_VELOCITY, Y_VELOCITY
		
		nx = size(X_VELOCITY(1,:))
		ny = size(X_VELOCITY(:,1))

		allocate(Vx_left(ny), Vy_bot(nx), Vy_top(nx), GetSeeder(3,max(nx,ny)))

		Vx_left(:) = X_VELOCITY(:,1)
		Vy_top(:) = abs(Y_VELOCITY(1,:))
		Vy_bot(:) = abs(Y_VELOCITY(ny,:))

		do i = 1,nx
			GetSeeder(1,i) = trapz(Vy_top(1:i),delta)
			GetSeeder(3,i) = trapz(Vy_bot(1:i),delta)
		end do

		do i = 1,ny
			GetSeeder(2,i) = trapz(Vx_left(1:i),delta)
		end do

		GetSeeder(1,:) = GetSeeder(1,:)/MAXVAL(GetSeeder(1,:))
		GetSeeder(2,:) = GetSeeder(2,:)/MAXVAL(GetSeeder(2,:))
		GetSeeder(3,:) = GetSeeder(3,:)/MAXVAL(GetSeeder(3,:))

		deallocate(Vx_left, Vy_bot, Vy_top)

	end function GetSeeder

	
	
	
	function GetBox(x_part,y_part,Lambda_part,MeshCaracteristics,Radius)
	
		real :: x_part, y_part, Lambda_part, x_min, y_min, x_max, y_max
		real :: delta
		integer :: Radius
		
		integer, dimension(4) :: GetBox 
		real,dimension(5) :: MeshCaracteristics
		
		delta = MeshCaracteristics(5)
		x_min = x_part - Radius*Lambda_part
		x_max = x_part + Radius*Lambda_part
		y_min = y_part - Radius*Lambda_part
		y_max = y_part + Radius*Lambda_part
		
		
		if (x_min < MeshCaracteristics(1)+delta/2) then
			x_min = MeshCaracteristics(1)+delta/2
		end if
		if (x_max > MeshCaracteristics(2)-delta/2) then
			x_max = MeshCaracteristics(2)-delta/2
		end if
		if (y_min < MeshCaracteristics(3)+delta/2) then
			y_min = MeshCaracteristics(3)+delta/2
		end if
		if (y_max > MeshCaracteristics(4)-delta/2) then
			y_max = MeshCaracteristics(4)-delta/2
		end if
		
		GetBox(1) = floor((y_min-MeshCaracteristics(3))/delta)+1
		GetBox(2) = floor((y_max-MeshCaracteristics(3))/delta)+2
		GetBox(3) = floor((x_min-MeshCaracteristics(1))/delta)+1
		GetBox(4) = floor((x_max-MeshCaracteristics(1))/delta)+2
		
	end function GetBox
	
	
	
	
	
	
	

	function InitParticle(MeshCaracteristics,Nparticle,vtkMask)
	
	real,dimension(:,:) :: vtkMask
	real,dimension(5) :: MeshCaracteristics
	real, allocatable :: InitParticle(:,:), ParticleSeeder(:,:), NumberPart(:)
	integer :: Nparticle, Particle_available_counter, k, i
	real :: x_min, x_max, y_min, y_max
	
	x_min = MeshCaracteristics(1)
	x_max = MeshCaracteristics(2)
	y_min = MeshCaracteristics(3)
	y_max = MeshCaracteristics(4)
	
	allocate (ParticleSeeder(Nparticle,3))
	do i = 1,Nparticle
		ParticleSeeder(i,1) = x_min + rand()*(x_max-x_min)
		ParticleSeeder(i,2) = y_min + rand()*(y_max-y_min)
		ParticleSeeder(i,3) = r4_normal_01 ()
	end do

	! Removing particle in airfoil
	allocate (NumberPart(Nparticle))

	Particle_available_counter = 0

	do i = 1,Nparticle
		if (Get_value(ParticleSeeder(i,1),ParticleSeeder(i,2),MeshCaracteristics,vtkMask) == 1) then
			NumberPart(i) = 1
			Particle_available_counter = Particle_available_counter+1
		end if
	end do

	allocate (InitParticle(Particle_available_counter,3))

	k = 1
	do i = 1,Nparticle
		if (NumberPart(i) == 1) then
			InitParticle(k,1) = ParticleSeeder(i,1)
			InitParticle(k,2) = ParticleSeeder(i,2)
			InitParticle(k,3) = ParticleSeeder(i,3)
			k = k+1
		end if
	end do

	deallocate (NumberPart,ParticleSeeder)
	
	end function InitParticle

	
	
	
	
	





	subroutine MoveParticle(dt,MeshCaracteristics,Particle,X_VELOCITY,Y_VELOCITY,PartSeeder)

		implicit none

		real :: dt,x_part,y_part,ux_part,uy_part, vel_temp
		real :: Prob_top, Prob_bot
		real :: RandomInjector,RandomPosition,NewPosition
		integer :: i,N,ny,ii
		real,dimension(:,:) :: Particle, X_VELOCITY, Y_VELOCITY, PartSeeder
		real,dimension(5) :: MeshCaracteristics
		
		ny = size(X_VELOCITY(:,1))


		Prob_top = sum(abs(Y_VELOCITY(1,:)))/(sum(abs(Y_VELOCITY(1,:)))+sum(abs(Y_VELOCITY(ny,:)))+sum(X_VELOCITY(:,1)))
		Prob_bot = Prob_top+sum(abs(Y_VELOCITY(ny,:)))/(sum(abs(Y_VELOCITY(1,:)))+sum(abs(Y_VELOCITY(ny,:)))+sum(X_VELOCITY(:,1)))


		N = size(Particle(:,1))
		
		!$OMP PARALLEL DO PRIVATE(i, x_part, y_part, ux_part, uy_part, RandomInjector, RandomPosition, ii, NewPosition, vel_temp)
		do i = 1,N
			x_part = Particle(i,1)
			y_part = Particle(i,2)
			ux_part = Get_value(x_part,y_part,MeshCaracteristics,X_VELOCITY)
			uy_part = Get_value(x_part,y_part,MeshCaracteristics,Y_VELOCITY)
			Particle(i,1) = x_part + ux_part*dt
			Particle(i,2) = y_part + uy_part*dt

			if (Particle(i,1) > MeshCaracteristics(2)-MeshCaracteristics(5)/2) then
				RandomInjector = (rand()*0.999999999999+0.000000000001)

				if (RandomInjector<=Prob_top) then
					RandomPosition = (rand()*0.999999999999+0.000000000001)
					ii = 1
					do while(PartSeeder(1,ii)<RandomPosition)
						ii = ii + 1
					end do
					ii = ii-1
							

					NewPosition = RandomPosition/(PartSeeder(1,ii+1)-PartSeeder(1,ii)) &
					& +ii-1/(PartSeeder(1,ii+1)-PartSeeder(1,ii))*PartSeeder(1,ii)
					NewPosition = MeshCaracteristics(1)+(NewPosition-1)*MeshCaracteristics(5)
					
					vel_temp = abs(RandomPosition*(Y_VELOCITY(1,ii+1)-Y_VELOCITY(1,ii)) &
					& +Y_VELOCITY(1,ii))
					
					Particle(i,1) = NewPosition
					Particle(i,2) = MeshCaracteristics(4) - (rand()*0.999999999999+0.000000000001)*vel_temp*dt - MeshCaracteristics(5)/10
					Particle(i,3) = r4_normal_01 ()


				elseif (Prob_top<RandomInjector) then
					if (RandomInjector<=Prob_bot) then
						RandomPosition = (rand()*0.999999999999+0.000000000001)
						ii = 1
						do while(PartSeeder(3,ii)<RandomPosition)
							ii = ii + 1
						end do
						ii = ii-1

						NewPosition = RandomPosition/(PartSeeder(3,ii+1)-PartSeeder(3,ii)) &
						& +ii-1/(PartSeeder(3,ii+1)-PartSeeder(3,ii))*PartSeeder(3,ii)
						NewPosition = MeshCaracteristics(1)+(NewPosition-1)*MeshCaracteristics(5)
						
						vel_temp = abs(RandomPosition*(Y_VELOCITY(ny,ii+1)-Y_VELOCITY(ny,ii)) &
						& +Y_VELOCITY(1,ii))
									
						Particle(i,1) = NewPosition
						Particle(i,2) = MeshCaracteristics(3) + (rand()*0.999999999999+0.000000000001)*vel_temp*dt + MeshCaracteristics(5)/10
						Particle(i,3) = r4_normal_01 ()
						
					else
						RandomPosition = (rand()*0.999999999999+0.000000000001)
						ii = 1
						do while(PartSeeder(2,ii)<RandomPosition)
							ii = ii + 1
						end do
						ii = ii-1
											
						NewPosition = RandomPosition/(PartSeeder(2,ii+1)-PartSeeder(2,ii)) &
						& +ii-1/(PartSeeder(2,ii+1)-PartSeeder(2,ii))*PartSeeder(2,ii)
						NewPosition = MeshCaracteristics(3)+(NewPosition-1)*MeshCaracteristics(5)
											
						vel_temp = abs(RandomPosition*(X_VELOCITY(ii+1,1)-X_VELOCITY(ii,1)) &
						& +X_VELOCITY(ii,1))
					
						Particle(i,1) = MeshCaracteristics(1) + (rand()*0.999999999999+0.000000000001)*vel_temp*dt + MeshCaracteristics(5)/10
						Particle(i,2) = NewPosition
						Particle(i,3) = r4_normal_01 ()
					end if

				end if

			end if

		end do
		!$OMP END PARALLEL DO
	end subroutine MoveParticle






	
	
	
	
	

	
	subroutine Calc_Fluctuation(MeshCaracteristics,Particle,TKE,Lambda,StreamFunction, &
								   & vtkMask,Radius,FilterType,ScalingType)

		implicit none

		real :: delta,x_grid,y_grid,TKE_grid,Lambda_part,R
		integer :: nx,ny,N_particle,i,j,k,Radius
		real, allocatable :: StreamFunction(:,:), temp(:,:)
		real, dimension(:,:) :: Particle, TKE, Lambda, vtkMask
		real, dimension(5) :: MeshCaracteristics
		integer, dimension(4) :: Box
		real :: pi = 3.14159265358

		character(len=*) :: FilterType, ScalingType

		delta = MeshCaracteristics(5)
		nx = size(TKE(1,:))
		ny = size(TKE(:,1))
		allocate (temp(ny,nx))
		temp = 0
		N_particle = size(Particle(:,1))
		
		
		if (FilterType == "Gaussian") then

			if (ScalingType == "Particle") then
				!$OMP PARALLEL DO reduction(+:temp) PRIVATE(Lambda_part, Box, i, j, x_grid, y_grid, R, TKE_grid, k)
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,1),Particle(k,2),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,1),Particle(k,2),Lambda_part,MeshCaracteristics,Radius)
					do j = Box(3), Box(4)
						do i = Box(1), Box(2)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,1))**2 + (y_grid-Particle(k,2))**2)
							TKE_grid = Get_value(Particle(k,1),Particle(k,2),MeshCaracteristics,TKE)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + Gaussian_filter(R,TKE_grid,Lambda_part)*Particle(k,3)
						end do
					end do
				end do	
				!$OMP END PARALLEL DO

			elseif (ScalingType == "kGrid_lambdaParticle") then
				!$OMP PARALLEL DO reduction(+:temp) PRIVATE(Lambda_part, Box, i, j, x_grid, y_grid, R, k)
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,1),Particle(k,2),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,1),Particle(k,2),Lambda_part,MeshCaracteristics,Radius)
					do j = Box(3), Box(4)
						do i = Box(1), Box(2)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,1))**2 + (y_grid-Particle(k,2))**2)
							temp(i,j) = temp(i,j) + Gaussian_filter_grid(R,Lambda_part)*Particle(k,3)
						end do
					end do
				end do	
				!$OMP END PARALLEL DO

			else
				stop 0		
			end if
			
			
			
		else if (FilterType == "VonKarman") then

			if (ScalingType == "Particle") then
				!$OMP PARALLEL DO reduction(+:temp) PRIVATE(Lambda_part, Box, i, j, x_grid, y_grid, R, TKE_grid, k)
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,1),Particle(k,2),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,1),Particle(k,2),Lambda_part,MeshCaracteristics,Radius)
					do j = Box(3), Box(4)
						do i = Box(1), Box(2)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,1))**2 + (y_grid-Particle(k,2))**2)
							TKE_grid = Get_value(Particle(k,1),Particle(k,2),MeshCaracteristics,TKE)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + VonKarman_filter(R,TKE_grid,Lambda_part)*Particle(k,3)
						end do
					end do
				end do	
				!$OMP END PARALLEL DO

			elseif (ScalingType == "kGrid_lambdaParticle") then
				!$OMP PARALLEL DO reduction(+:temp) PRIVATE(Lambda_part, Box, i, j, x_grid, y_grid, R, k)
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,1),Particle(k,2),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,1),Particle(k,2),Lambda_part,MeshCaracteristics,Radius)
					do j = Box(3), Box(4)
						do i = Box(1), Box(2)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,1))**2 + (y_grid-Particle(k,2))**2)
							temp(i,j) = temp(i,j) + VonKarman_filter_grid(R,Lambda_part)*Particle(k,3)
						end do
					end do
				end do	
				!$OMP END PARALLEL DO

			else
				stop 0		
			end if

			
			
			
		else if (FilterType == "Liepmann") then

			if (ScalingType == "Particle") then
				!$OMP PARALLEL DO reduction(+:temp) PRIVATE(Lambda_part, Box, i, j, x_grid, y_grid, R, TKE_grid, k)
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,1),Particle(k,2),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,1),Particle(k,2),Lambda_part,MeshCaracteristics,Radius)
					do j = Box(3), Box(4)
						do i = Box(1), Box(2)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,1))**2 + (y_grid-Particle(k,2))**2)
							TKE_grid = Get_value(Particle(k,1),Particle(k,2),MeshCaracteristics,TKE)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + Liepmann_filter(R,TKE_grid,Lambda_part)*Particle(k,3)
						end do
					end do
				end do	
				!$OMP END PARALLEL DO

			elseif (ScalingType == "kGrid_lambdaParticle") then
				!$OMP PARALLEL DO reduction(+:temp) PRIVATE(Lambda_part, Box, i, j, x_grid, y_grid, R, k)
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,1),Particle(k,2),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,1),Particle(k,2),Lambda_part,MeshCaracteristics,Radius)
					do j = Box(3), Box(4)
						do i = Box(1), Box(2)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,1))**2 + (y_grid-Particle(k,2))**2)
							temp(i,j) = temp(i,j) + Liepmann_filter_grid(R,Lambda_part)*Particle(k,3)
						end do
					end do
				end do	
				!$OMP END PARALLEL DO

			else
				stop 0		
			end if

		end if

		if (ScalingType == "kGrid_lambdaParticle") then
			if (FilterType == "Gaussian") then
				StreamFunction = temp*sqrt(TKE*2/pi)
			elseif (FilterType == "VonKarman") then
				StreamFunction = temp*sqrt(TKE)
			else
				StreamFunction = temp*sqrt(TKE*2/3)
			end if
		else
			StreamFunction = temp
		end if
		deallocate (temp)

	end subroutine Calc_Fluctuation
	
	
	
	
	

	
end Module ModuleFunction