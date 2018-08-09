module ModuleFunction

	use ModuleMath

	implicit none

	contains







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






	subroutine Calc_Fluctuation(MeshCaracteristics,Particle,TKE,Lambda,StreamFunction,vtkMask)

		implicit none

		real :: delta,x_grid,y_grid,TKE_grid,Lambda_grid,x_part,y_part,TKE_part,Lambda_part,temp
		integer :: nx,ny,N_particle,i,j,k
		real, allocatable :: StreamFunction(:,:)
		real, dimension(:,:) :: Particle, TKE, Lambda, vtkMask
		real, dimension(5) :: MeshCaracteristics
		real, dimension(2) :: coord

		delta = MeshCaracteristics(5)
		nx = size(TKE(1,:))
		ny = size(TKE(:,1))
		N_particle = size(Particle(:,1))

		do i = 1,ny
			do j = 1,nx
				if (vtkMask(i,j) == 1) then
						x_grid = MeshCaracteristics(1) + (j-1)*delta
						y_grid = MeshCaracteristics(3) + (i-1)*delta
						Lambda_grid = Lambda(i,j)
						TKE_grid = TKE(i,j)
						temp = 0
						do k = 1,N_particle
							coord(1) = x_grid-Particle(k,2)
							coord(2) = y_grid-Particle(k,3)
							if (coord(1)**2+coord(2)**2 < 4*Lambda_grid**2) then
								TKE_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,TKE)
								Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
								if (Lambda_part>1e-4) then
									if (TKE_part>0) then
										temp = temp + Calc_stream(coord,TKE_grid,Lambda_part)*Particle(k,4)
									end if
								end if
							end if

						end do
						StreamFunction(i,j) = temp
				end if
			end do
		end do

	end subroutine Calc_Fluctuation

	
	
	

	
	subroutine Calc_Fluctuation_opt(MeshCaracteristics,Particle,TKE,Lambda,StreamFunction,vtkMask,Parallel_computing,Radius)

		implicit none

		real :: delta,x_grid,y_grid,TKE_grid,Lambda_grid,x_part,y_part,TKE_part,Lambda_part,temp2
		integer :: nx,ny,N_particle,i,j,k,Radius
		real, allocatable :: StreamFunction(:,:), temp(:,:)
		real, dimension(:,:) :: Particle, TKE, Lambda, vtkMask
		real, dimension(5) :: MeshCaracteristics
		real, dimension(2) :: coord
		integer, dimension(4) :: Box
		
		integer :: Parallel_computing
		
		delta = MeshCaracteristics(5)
		nx = size(TKE(1,:))
		ny = size(TKE(:,1))
		allocate (temp(ny,nx))
		temp = 0
		
		N_particle = size(Particle(:,1))
		
		if (Parallel_computing == 1) then
			!$OMP PARALLEL DO reduction(+:temp) PRIVATE(Lambda_part, Box, i, j, x_grid, y_grid, coord, TKE_grid)
			do k = 1, N_particle
				Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
				Box = GetBox(Particle(k,2),Particle(k,3),Lambda_part,MeshCaracteristics,Radius)
				do i = Box(1), Box(2)
					do j = Box(3), Box(4)
						if (vtkMask(i,j) == 1) then
						
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
						
							coord(1) = x_grid-Particle(k,2)
							coord(2) = y_grid-Particle(k,3)
						
							TKE_grid = TKE(i,j)
							
							temp(i,j) = temp(i,j) + Calc_stream(coord,TKE_grid,Lambda_part)*Particle(k,4)
						
						end if
					end do
				end do
			end do
			!$OMP END PARALLEL DO
			
			
		else
			do k = 1, N_particle
			
			Lambda_part = Get_value(Particle(k,2),Particle(k,3),MeshCaracteristics,Lambda)
			Box = GetBox(Particle(k,2),Particle(k,3),Lambda_part,MeshCaracteristics,Radius)
			do i = Box(1), Box(2)
					do j = Box(3), Box(4)
						if (vtkMask(i,j) == 1) then
						
							x_grid = MeshCaracteristics(1) + (j-1)*delta
							y_grid = MeshCaracteristics(3) + (i-1)*delta
						
							coord(1) = x_grid-Particle(k,2)
							coord(2) = y_grid-Particle(k,3)
						
							TKE_grid = TKE(i,j)
							
							temp(i,j) = temp(i,j) + Calc_stream(coord,TKE_grid,Lambda_part)*Particle(k,4)
						
						end if
					end do
				end do
			end do		
		end if
		StreamFunction = temp
		deallocate (temp)

	end subroutine Calc_Fluctuation_opt
	
	
	
	
	
	
	
	function Calc_stream(coord,TKE_part,Lambda_part)

		implicit none

		real :: pi
		real :: TKE_part,Lambda_part,R,Calc_stream
		real,dimension(2) :: coord

		pi = 3.14159265358
		R = sqrt(coord(1)**2+coord(2)**2)
		Calc_stream = sqrt(TKE_part*2/pi)*exp(-pi*R**2/(2*Lambda_part**2))

	end function Calc_stream


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
		integer :: j_min, j_max, i_min, i_max, Radius
		
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
	
end Module ModuleFunction