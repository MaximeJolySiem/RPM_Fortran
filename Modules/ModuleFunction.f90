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
		integer :: i_min,i_max,j_min,j_max
		real,dimension(:,:) :: Data_to_get
		real,dimension(5) :: MeshCaracteristics
		
		delta = MeshCaracteristics(5)


		j_min = floor((x-MeshCaracteristics(1))/delta)+1
		j_max = floor((x-MeshCaracteristics(1))/delta)+2
		i_min = floor((y-MeshCaracteristics(3))/delta)+1
		i_max = floor((y-MeshCaracteristics(3))/delta)+2

		dfy = Data_to_get(i_min,j_max)-Data_to_get(i_min,j_min)
		dfx = Data_to_get(i_max,j_min)-Data_to_get(i_min,j_min)
		dfxy = Data_to_get(i_min,j_min)+Data_to_get(i_max,j_max)-Data_to_get(i_min,j_max)-Data_to_get(i_max,j_min)

		dx = x-(MeshCaracteristics(1)+(j_min-1)*delta)
		dy = y-(MeshCaracteristics(3)+(i_min-1)*delta)

		Get_value = dfx*dx/delta + dfy*dy/delta + dfxy*dx*dy/delta + Data_to_get(i_min,j_min)

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

	end function GetSeeder

	
	
	
	function GetBox(x_part,y_part,Lambda_part,MeshCaracteristics,Radius)
	
		real :: x_part, y_part, Lambda_part, x_min, y_min, x_max, y_max
		real :: delta
		integer :: Radius
		
		integer, dimension(4) :: GetBox !GetBox will get [i_min,i_max,j_min,j_max] then indexes of [y_min, y_max, x_min, x_max]
		real,dimension(5) :: MeshCaracteristics
		
		delta = MeshCaracteristics(5)
		x_min = x_part - Radius*Lambda_part
		x_max = x_part + Radius*Lambda_part
		y_min = y_part - Radius*Lambda_part
		y_max = y_part + Radius*Lambda_part
		
		
		if (x_min < MeshCaracteristics(1)) then
			x_min = MeshCaracteristics(1)+1e-12
		end if
		if (x_max > MeshCaracteristics(2)) then
			x_max = MeshCaracteristics(2)-1e-12
		end if
		if (y_min < MeshCaracteristics(3)) then
			y_min = MeshCaracteristics(3)+1e-12
		end if
		if (y_max > MeshCaracteristics(4)) then
			y_max = MeshCaracteristics(4)-1e-12
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

	allocate (InitParticle(Particle_available_counter,4))

	k = 1
	do i = 1,Nparticle
		if (NumberPart(i) == 1) then
			InitParticle(k,1) = ParticleSeeder(i,1)
			InitParticle(k,2) = ParticleSeeder(i,2)
			InitParticle(k,3) = ParticleSeeder(i,3)
			InitParticle(k,4) = ParticleSeeder(i,4)
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
		
		do i = 1,N
			x_part = Particle(i,2)
			y_part = Particle(i,3)
			ux_part = Get_value(x_part,y_part,MeshCaracteristics,X_VELOCITY)
			uy_part = Get_value(x_part,y_part,MeshCaracteristics,Y_VELOCITY)
			Particle(i,2) = x_part + ux_part*dt
			Particle(i,3) = y_part + uy_part*dt
			

			if (Particle(i,2) > MeshCaracteristics(2)) then
				RandomInjector = rand()

				if (RandomInjector<=Prob_top) then
					RandomPosition = rand()
					ii = 1
					do while(PartSeeder(1,ii)<RandomPosition)
						ii = ii + 1
					end do
					ii = ii-1
					
					NewPosition = RandomPosition/(PartSeeder(1,ii+1)-PartSeeder(1,ii)) &
					& +ii-1/(PartSeeder(1,ii+1)-PartSeeder(1,ii))*PartSeeder(1,ii)
					NewPosition = MeshCaracteristics(1)+NewPosition*MeshCaracteristics(5)
					
					vel_temp = abs(RandomPosition*(Y_VELOCITY(1,ii+1)-Y_VELOCITY(1,ii)) &
					& +Y_VELOCITY(1,ii))
					
					Particle(i,2) = NewPosition
					Particle(i,3) = MeshCaracteristics(4) - rand()*vel_temp*dt
					Particle(i,4) = r4_normal_01 ()


				elseif (Prob_top<RandomInjector) then
					if (RandomInjector<=Prob_bot) then
						RandomPosition = rand()
						ii = 1
						do while(PartSeeder(3,ii)<RandomPosition)
							ii = ii + 1
						end do
						ii = ii-1
						
						NewPosition = RandomPosition/(PartSeeder(3,ii+1)-PartSeeder(3,ii)) &
						& +ii-1/(PartSeeder(3,ii+1)-PartSeeder(3,ii))*PartSeeder(3,ii)
						NewPosition = MeshCaracteristics(1)+NewPosition*MeshCaracteristics(5)
						
						vel_temp = abs(RandomPosition*(Y_VELOCITY(ny,ii+1)-Y_VELOCITY(ny,ii)) &
						& +Y_VELOCITY(1,ii))
									
						Particle(i,2) = NewPosition
						Particle(i,3) = MeshCaracteristics(3) + rand()*vel_temp*dt
						Particle(i,4) = r4_normal_01 ()
						
					else
						RandomPosition = rand()
						ii = 1
						do while(PartSeeder(2,ii)<RandomPosition)
							ii = ii + 1
						end do
						ii = ii-1
						
						NewPosition = RandomPosition/(PartSeeder(2,ii+1)-PartSeeder(2,ii)) &
						& +ii-1/(PartSeeder(2,ii+1)-PartSeeder(2,ii))*PartSeeder(2,ii)
						NewPosition = MeshCaracteristics(3)+NewPosition*MeshCaracteristics(5)
											
						vel_temp = abs(RandomPosition*(X_VELOCITY(ii+1,1)-X_VELOCITY(ii,1)) &
						& +X_VELOCITY(ii,1))
					
						Particle(i,2) = MeshCaracteristics(1) + rand()*vel_temp*dt
						Particle(i,3) = NewPosition
						Particle(i,4) = r4_normal_01 ()
					end if

				end if

			end if

		end do
	end subroutine MoveParticle




	
	
	
	
	
	
	
	
	


	subroutine Calc_Fluctuation(MeshCaracteristics,Particle,TKE,Lambda,StreamFunction, &
	                           & vtkMask,Parallel_computing,Radius,FilterType,ScalingType)


		implicit none

		real :: delta,x_grid,y_grid,TKE_grid,Lambda_grid,x_part,y_part,TKE_part,Lambda_part,temp2,R
		integer :: nx,ny,N_particle,i,j,k,Radius
		real, allocatable :: StreamFunction(:,:), temp(:,:)
		real, dimension(:,:) :: Particle, TKE, Lambda, vtkMask
		real, dimension(5) :: MeshCaracteristics
		integer, dimension(4) :: Box

		character(len=*) :: FilterType, ScalingType

		integer :: Parallel_computing

		delta = MeshCaracteristics(5)
		nx = size(TKE(1,:))
		ny = size(TKE(:,1))
		allocate (temp(ny,nx))
		temp = 0
		
		N_particle = size(Particle(:,1))
		
		
		
		
		if (FilterType == "Gaussian") then

			if (ScalingType == "Particle") then
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,2),Particle(k,3),Lambda_part,MeshCaracteristics,Radius)
					do i = Box(1), Box(2)
						do j = Box(3), Box(4)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,2))**2 + (y_grid-Particle(k,3))**2)
							TKE_grid = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,TKe)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + Gaussian_filter(R,TKE_grid,Lambda_part)*Particle(k,4)
						end do
					end do
				end do	

			elseif (ScalingType == "kGrid_lambdaParticle") then
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,2),Particle(k,3),Lambda_part,MeshCaracteristics,Radius)
					do i = Box(1), Box(2)
						do j = Box(3), Box(4)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,2))**2 + (y_grid-Particle(k,3))**2)
							TKE_grid = TKE(i,j)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + Gaussian_filter(R,TKE_grid,Lambda_part)*Particle(k,4)
						end do
					end do
				end do	

			else
				stop 0		
			end if	
			
			
			
			
		else if (FilterType == "VonKarman") then

			if (ScalingType == "Particle") then
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,2),Particle(k,3),Lambda_part,MeshCaracteristics,Radius)
					do i = Box(1), Box(2)
						do j = Box(3), Box(4)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,2))**2 + (y_grid-Particle(k,3))**2)
							TKE_grid = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,TKe)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + VonKarman_filter(R,TKE_grid,Lambda_part)*Particle(k,4)
						end do
					end do
				end do	

			elseif (ScalingType == "kGrid_lambdaParticle") then
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,2),Particle(k,3),Lambda_part,MeshCaracteristics,Radius)
					do i = Box(1), Box(2)
						do j = Box(3), Box(4)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,2))**2 + (y_grid-Particle(k,3))**2)
							TKE_grid = TKE(i,j)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + VonKarman_filter(R,TKE_grid,Lambda_part)*Particle(k,4)
						end do
					end do
				end do	

			else
				stop 0		
			end if	
		
		
		
		
		else if (FilterType == "Liepmann") then

			if (ScalingType == "Particle") then
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,2),Particle(k,3),Lambda_part,MeshCaracteristics,Radius)
					do i = Box(1), Box(2)
						do j = Box(3), Box(4)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,2))**2 + (y_grid-Particle(k,3))**2)
							TKE_grid = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,TKe)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + Liepmann_filter(R,TKE_grid,Lambda_part)*Particle(k,4)
						end do
					end do
				end do	

			elseif (ScalingType == "kGrid_lambdaParticle") then
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,2),Particle(k,3),Lambda_part,MeshCaracteristics,Radius)
					do i = Box(1), Box(2)
						do j = Box(3), Box(4)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,2))**2 + (y_grid-Particle(k,3))**2)
							TKE_grid = TKE(i,j)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + Liepmann_filter(R,TKE_grid,Lambda_part)*Particle(k,4)
						end do
					end do
				end do	

			else
				stop 0		
			end if
		end if

		StreamFunction = temp
		deallocate (temp)

	end subroutine Calc_Fluctuation

	
	
	
	
	
	
	
	
	
	
	

	
	subroutine Calc_Fluctuation_opt(MeshCaracteristics,Particle,TKE,Lambda,StreamFunction, &
								   & vtkMask,Parallel_computing,Radius,FilterType,ScalingType)

		implicit none

		real :: delta,x_grid,y_grid,TKE_grid,Lambda_grid,x_part,y_part,TKE_part,Lambda_part,temp2,R
		integer :: nx,ny,N_particle,i,j,k,Radius
		real, allocatable :: StreamFunction(:,:), temp(:,:)
		real, dimension(:,:) :: Particle, TKE, Lambda, vtkMask
		real, dimension(5) :: MeshCaracteristics
		integer, dimension(4) :: Box

		character(len=*) :: FilterType, ScalingType

		integer :: Parallel_computing

		delta = MeshCaracteristics(5)
		nx = size(TKE(1,:))
		ny = size(TKE(:,1))
		allocate (temp(ny,nx))
		temp = 0
		
		N_particle = size(Particle(:,1))
		
		
		
		
		if (FilterType == "Gaussian") then

			if (ScalingType == "Particle") then
				!$OMP PARALLEL DO reduction(+:temp) PRIVATE(Lambda_part, Box, i, j, x_grid, y_grid, R, TKE_grid)
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,2),Particle(k,3),Lambda_part,MeshCaracteristics,Radius)
					do i = Box(1), Box(2)
						do j = Box(3), Box(4)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,2))**2 + (y_grid-Particle(k,3))**2)
							TKE_grid = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,TKE)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + Gaussian_filter(R,TKE_grid,Lambda_part)*Particle(k,4)
						end do
					end do
				end do	
				!$OMP END PARALLEL DO

			elseif (ScalingType == "kGrid_lambdaParticle") then
				!$OMP PARALLEL DO reduction(+:temp) PRIVATE(Lambda_part, Box, i, j, x_grid, y_grid, R, TKE_grid)
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,2),Particle(k,3),Lambda_part,MeshCaracteristics,Radius)
					do i = Box(1), Box(2)
						do j = Box(3), Box(4)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,2))**2 + (y_grid-Particle(k,3))**2)
							TKE_grid = TKE(i,j)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + Gaussian_filter(R,TKE_grid,Lambda_part)*Particle(k,4)
						end do
					end do
				end do	
				!$OMP END PARALLEL DO

			else
				stop 0		
			end if
			
			
			
		else if (FilterType == "VonKarman") then

			if (ScalingType == "Particle") then
				!$OMP PARALLEL DO reduction(+:temp) PRIVATE(Lambda_part, Box, i, j, x_grid, y_grid, R, TKE_grid)
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,2),Particle(k,3),Lambda_part,MeshCaracteristics,Radius)
					do i = Box(1), Box(2)
						do j = Box(3), Box(4)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,2))**2 + (y_grid-Particle(k,3))**2)
							TKE_grid = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,TKE)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + VonKarman_filter(R,TKE_grid,Lambda_part)*Particle(k,4)
						end do
					end do
				end do	
				!$OMP END PARALLEL DO

			elseif (ScalingType == "kGrid_lambdaParticle") then
				!$OMP PARALLEL DO reduction(+:temp) PRIVATE(Lambda_part, Box, i, j, x_grid, y_grid, R, TKE_grid)
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,2),Particle(k,3),Lambda_part,MeshCaracteristics,Radius)
					do i = Box(1), Box(2)
						do j = Box(3), Box(4)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,2))**2 + (y_grid-Particle(k,3))**2)
							TKE_grid = TKE(i,j)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + VonKarman_filter(R,TKE_grid,Lambda_part)*Particle(k,4)
						end do
					end do
				end do	
				!$OMP END PARALLEL DO

			else
				stop 0		
			end if

			
			
			
		else if (FilterType == "Liepmann") then

			if (ScalingType == "Particle") then
				!$OMP PARALLEL DO reduction(+:temp) PRIVATE(Lambda_part, Box, i, j, x_grid, y_grid, R, TKE_grid)
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,2),Particle(k,3),Lambda_part,MeshCaracteristics,Radius)
					do i = Box(1), Box(2)
						do j = Box(3), Box(4)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,2))**2 + (y_grid-Particle(k,3))**2)
							TKE_grid = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,TKE)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + Liepmann_filter(R,TKE_grid,Lambda_part)*Particle(k,4)
						end do
					end do
				end do	
				!$OMP END PARALLEL DO

			elseif (ScalingType == "kGrid_lambdaParticle") then
				!$OMP PARALLEL DO reduction(+:temp) PRIVATE(Lambda_part, Box, i, j, x_grid, y_grid, R, TKE_grid)
				do k = 1, N_particle
					Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
					Box = GetBox(Particle(k,2),Particle(k,3),Lambda_part,MeshCaracteristics,Radius)
					do i = Box(1), Box(2)
						do j = Box(3), Box(4)
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
							R = sqrt((x_grid-Particle(k,2))**2 + (y_grid-Particle(k,3))**2)
							TKE_grid = TKE(i,j)*vtkMask(i,j)
							temp(i,j) = temp(i,j) + Liepmann_filter(R,TKE_grid,Lambda_part)*Particle(k,4)
						end do
					end do
				end do	
				!$OMP END PARALLEL DO

			else
				stop 0		
			end if

		end if

		StreamFunction = temp
		deallocate (temp)

	end subroutine Calc_Fluctuation_opt
	
	
	
	
	

	
end Module ModuleFunction