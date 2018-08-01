module ModuleDerivatives
	implicit none
	! Finite Difference coefficient #Maxime
	real,dimension(0:8) :: FDo9p  = (/ 0.007650904064,-0.059463584768,0.244678631765,-0.841570125482, &
		& 0.0000000000000000000,0.841570125482,-0.244678631765,0.059463584768,-0.007650904064 /)
	real,dimension(0:6) :: FDs7p  = (/ -0.0166666666666667,0.150000000000000000,-0.7500000000000000000, &
		& 0.000000000000000000,0.7500000000000000000,-0.150000000000000000,0.0166666666666667 /)
	real,dimension(0:6) :: FDs24 = (/ 0.048264094108, -0.488255830845, -0.366015590723, 1.04800545857, &
		 &-0.289325926394, 0.050392437692, -0.003064639693 /)
	real,dimension(0:6) :: FDs15 = (/ -0.212932721951, -1.060320390770, 2.078926116439, -1.287179452384, & 
		& 0.685176395471, -0.245320613994, 0.041650667189 /)
	real,dimension(0:6) :: FDs06 = (/ -2.225833963270, 4.827779580575, -5.001388453836, 3.911103941646, & 
		& -2.115267458633, 0.718882784412, -0.115276430895 /)
	real,dimension(0:6) :: FDs42 = (/ 0.003064639693, -0.050392437692, 0.289325926394, -1.04800545857, &
		& 0.366015590723, 0.488255830845, -0.048264094108 /)
	real,dimension(0:6) :: FDs51 = (/ -0.041650667189, 0.245320613994, -0.685176395471, 1.287179452384, &
		& -2.078926116439, 1.060320390770, 0.212932721951 /)
	real,dimension(0:6) :: FDs60 = (/ 0.115276430895, -0.718882784412, 2.115267458633, -3.911103941646, &
		& 5.001388453836, -4.827779580575, 2.225833963270 /)
	! Selective Filter coefficient #Maxime
	real,dimension(0:8) :: SFo9p = (/ 0.008228661760, -0.045211119360, 0.120007591680, -0.204788880640, &
		& 0.243527493120, -0.204788880640, 0.120007591680, -0.045211119360, 0.008228661760 /)
	real,dimension(0:6) :: SFs7p = (/ -0.015625, 0.09375, -0.234375, 0.3125, -0.234375, 0.09375, -0.015625 /)
	real,dimension(0:6) :: SFs24 = (/ 0.032649010764, -0.143339502575, 0.273321177980, -0.294622121167, &
		& 0.186711738069, -0.062038376258, 0.007318073189 /)
	real,dimension(0:6) :: SFs15 = (/ -0.085777408970, 0.277628171524, -0.356848072173, 0.223119093072, &
		& -0.057347064865, -0.000747264596, -0.000027453993 /)
	real,dimension(0:3) :: SFs03 = (/ 0.320882352941, -0.465, 0.179117647059, -0.035 /)
	real,dimension(0:6) :: SFs42 = (/ 0.007318073189, -0.062038376258, 0.186711738069, -0.294622121167, &
		& 0.273321177980, -0.143339502575, 0.032649010764 /)
	real,dimension(0:6) :: SFs51 = (/ -0.000027453993, -0.000747264596, -0.057347064865, 0.223119093072, &
		& -0.356848072173, 0.277628171524, -0.085777408970 /)
	real,dimension(0:3) :: SFs30 = (/ -0.035, 0.179117647059, -0.465, 0.320882352941 /)
	
	contains



subroutine der2_x(temp, var2, vtkMask, MeshCaracteristics)

	implicit none

	real :: delta
	integer :: i,j,nx,ny
	real, allocatable :: temp2(:,:)
	real,dimension(:,:) :: temp, var2, vtkMask
	real,dimension(5) :: MeshCaracteristics
	
	delta = MeshCaracteristics(5)
	nx = size(temp(1,:))
	ny = size(temp(:,1))
	allocate(temp2(ny,nx))

	do i = 1,ny
					j=1
					temp2(i,j)=(1.0/delta)*(FDs06(0)*var2(i,j)+FDs06(1)*var2(i,j+1)+FDs06(2)*var2(i,j+2)+ &
						& FDs06(3)*var2(i,j+3)+FDs06(4)*var2(i,j+4)+FDs06(5)*var2(i,j+5)+FDs06(6)*var2(i,j+6))
					j=2
					temp2(i,j)=(1.0/delta)*(FDs15(0)*var2(i,j-1)+FDs15(1)*var2(i,j)+FDs15(2)*var2(i,j+1)+ &
						& FDs15(3)*var2(i,j+2)+FDs15(4)*var2(i,j+3)+FDs15(5)*var2(i,j+4)+FDs15(6)*var2(i,j+5))
					j=3
					temp2(i,j)=(1.0/delta)*(FDs24(0)*var2(i,j-2)+FDs24(1)*var2(i,j-1)+FDs24(2)*var2(i,j)+ &
						& FDs24(3)*var2(i,j+1)+FDs24(4)*var2(i,j+2)+FDs24(5)*var2(i,j+3)+FDs24(6)*var2(i,j+4))
					j=4
					temp2(i,j)=(1.0/delta)*(FDs7p(0)*var2(i,j-3)+FDs7p(1)*var2(i,j-2)+FDs7p(2)*var2(i,j-1)+ &
						& FDs7p(3)*var2(i,j)+FDs7p(4)*var2(i,j+1)+FDs7p(5)*var2(i,j+2)+FDs7p(6)*var2(i,j+3))
					j=nx
					temp2(i,j)=(1.0/delta)*(FDs60(0)*var2(i,j-6)+FDs60(1)*var2(i,j-5)+FDs60(2)*var2(i,j-4)+ &
						& FDs60(3)*var2(i,j-3)+FDs60(4)*var2(i,j-2)+FDs60(5)*var2(i,j-1)+FDs60(6)*var2(i,j))
					j=nx-1
					temp2(i,j)=(1.0/delta)*(FDs51(0)*var2(i,j-5)+FDs51(1)*var2(i,j-4)+FDs51(2)*var2(i,j-3)+ &
						& FDs51(3)*var2(i,j-2)+FDs51(4)*var2(i,j-1)+FDs51(5)*var2(i,j)+FDs51(6)*var2(i,j+1))
					j=nx-2
					temp2(i,j)=(1.0/delta)*(FDs42(0)*var2(i,j-4)+FDs42(1)*var2(i,j-3)+FDs42(2)*var2(i,j-2)+ &
						& FDs42(3)*var2(i,j-1)+FDs42(4)*var2(i,j)+FDs42(5)*var2(i,j+1)+FDs42(6)*var2(i,j+2))
					j=nx-3
					temp2(i,j)=(1.0/delta)*(FDs7p(0)*var2(i,j-3)+FDs7p(1)*var2(i,j-2)+FDs7p(2)*var2(i,j-1)+ &
						& FDs7p(3)*var2(i,j)+FDs7p(4)*var2(i,j+1)+FDs7p(5)*var2(i,j+2)+FDs7p(6)*var2(i,j+3))

					do j=5,nx-4
						if (vtkMask(i,j) == 0) then
							temp(i,j)=0

						elseif (vtkMask(i,j-1) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs06(0)*var2(i,j)+FDs06(1)*var2(i,j+1)+FDs06(2)*var2(i,j+2)+ &
						& FDs06(3)*var2(i,j+3)+FDs06(4)*var2(i,j+4)+FDs06(5)*var2(i,j+5)+FDs06(6)*var2(i,j+6))

						elseif (vtkMask(i,j-2) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs15(0)*var2(i,j-1)+FDs15(1)*var2(i,j)+FDs15(2)*var2(i,j+1)+ &
						& FDs15(3)*var2(i,j+2)+FDs15(4)*var2(i,j+3)+FDs15(5)*var2(i,j+4)+FDs15(6)*var2(i,j+5))

						elseif (vtkMask(i,j-3) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs24(0)*var2(i,j-2)+FDs24(1)*var2(i,j-1)+FDs24(2)*var2(i,j)+ &
						& FDs24(3)*var2(i,j+1)+FDs24(4)*var2(i,j+2)+FDs24(5)*var2(i,j+3)+FDs24(6)*var2(i,j+4))

						elseif (vtkMask(i,j-4) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs7p(0)*var2(i,j-3)+FDs7p(1)*var2(i,j-2)+FDs7p(2)*var2(i,j-1)+ &
						& FDs7p(3)*var2(i,j)+FDs7p(4)*var2(i,j+1)+FDs7p(5)*var2(i,j+2)+FDs7p(6)*var2(i,j+3))

						elseif (vtkMask(i,j+1) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs60(0)*var2(i,j-6)+FDs60(1)*var2(i,j-5)+FDs60(2)*var2(i,j-4)+ &
						& FDs60(3)*var2(i,j-3)+FDs60(4)*var2(i,j-2)+FDs60(5)*var2(i,j-1)+FDs60(6)*var2(i,j))

						elseif (vtkMask(i,j+2) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs51(0)*var2(i,j-5)+FDs51(1)*var2(i,j-4)+FDs51(2)*var2(i,j-3)+ &
						& FDs51(3)*var2(i,j-2)+FDs51(4)*var2(i,j-1)+FDs51(5)*var2(i,j)+FDs51(6)*var2(i,j+1))

						elseif (vtkMask(i,j+3) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs42(0)*var2(i,j-4)+FDs42(1)*var2(i,j-3)+FDs42(2)*var2(i,j-2)+ &
						& FDs42(3)*var2(i,j-1)+FDs42(4)*var2(i,j)+FDs42(5)*var2(i,j+1)+FDs42(6)*var2(i,j+2))

						elseif (vtkMask(i,j+4) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs7p(0)*var2(i,j-3)+FDs7p(1)*var2(i,j-2)+FDs7p(2)*var2(i,j-1)+ &
						& FDs7p(3)*var2(i,j)+FDs7p(4)*var2(i,j+1)+FDs7p(5)*var2(i,j+2)+FDs7p(6)*var2(i,j+3))

						else
							temp2(i,j)=(1.0/delta)*(FDo9p(0)*var2(i,j-4)+FDo9p(1)*var2(i,j-3)+FDo9p(2)*var2(i,j-2)+ &
								& FDo9p(3)*var2(i,j-1)+FDo9p(4)*var2(i,j)+FDo9p(5)*var2(i,j+1)+FDo9p(6)*var2(i,j+2)+ &
								& FDo9p(7)*var2(i,j+3)+FDo9p(8)*var2(i,j+4))
						end if
					end do
				end do
				call Filter_x(temp,temp2, vtkMask, MeshCaracteristics)
				deallocate(temp2)
end subroutine der2_x






subroutine der2_y(temp, var2, vtkMask, MeshCaracteristics)


	implicit none

	real :: delta
	integer :: i,j,nx,ny
	real, allocatable :: temp2(:,:)
	real,dimension(:,:) :: temp, var2, vtkMask
	real,dimension(5) :: MeshCaracteristics

	delta = MeshCaracteristics(5)
	nx = size(temp(1,:))
	ny = size(temp(:,1))
	allocate(temp2(ny,nx))
	
	do j = 1,nx
					i=1
					temp2(i,j)=(1.0/delta)*(FDs06(0)*var2(i,j)+FDs06(1)*var2(i+1,j)+FDs06(2)*var2(i+2,j)+ &
						& FDs06(3)*var2(i+3,j)+FDs06(4)*var2(i+4,j)+FDs06(5)*var2(i+5,j)+FDs06(6)*var2(i+6,j))
					i=2
					temp2(i,j)=(1.0/delta)*(FDs15(0)*var2(i-1,j)+FDs15(1)*var2(i,j)+FDs15(2)*var2(i+1,j)+ &
						& FDs15(3)*var2(i+2,j)+FDs15(4)*var2(i+3,j)+FDs15(5)*var2(i+4,j)+FDs15(6)*var2(i+5,j))
					i=3
					temp2(i,j)=(1.0/delta)*(FDs24(0)*var2(i-2,j)+FDs24(1)*var2(i-1,j)+FDs24(2)*var2(i,j)+ &
						& FDs24(3)*var2(i+1,j)+FDs24(4)*var2(i+2,j)+FDs24(5)*var2(i+3,j)+FDs24(6)*var2(i+4,j))
					i=4
					temp2(i,j)=(1.0/delta)*(FDs7p(0)*var2(i-3,j)+FDs7p(1)*var2(i-2,j)+FDs7p(2)*var2(i-1,j)+ &
						& FDs7p(3)*var2(i,j)+FDs7p(4)*var2(i+1,j)+FDs7p(5)*var2(i+2,j)+FDs7p(6)*var2(i+3,j))
					i=ny
					temp2(i,j)=(1.0/delta)*(FDs60(0)*var2(i-6,j)+FDs60(1)*var2(i-5,j)+FDs60(2)*var2(i-4,j)+ &
						& FDs60(3)*var2(i-3,j)+FDs60(4)*var2(i-2,j)+FDs60(5)*var2(i-1,j)+FDs60(6)*var2(i,j))
					i=ny-1
					temp2(i,j)=(1.0/delta)*(FDs51(0)*var2(i-5,j)+FDs51(1)*var2(i-4,j)+FDs51(2)*var2(i-3,j)+ &
						& FDs51(3)*var2(i-2,j)+FDs51(4)*var2(i-1,j)+FDs51(5)*var2(i,j)+FDs51(6)*var2(i+1,j))
					i=ny-2
					temp2(i,j)=(1.0/delta)*(FDs42(0)*var2(i-4,j)+FDs42(1)*var2(i-3,j)+FDs42(2)*var2(i-2,j)+ &
						& FDs42(3)*var2(i-1,j)+FDs42(4)*var2(i,j)+FDs42(5)*var2(i+1,j)+FDs42(6)*var2(i+2,j))
					i=ny-3
					temp2(i,j)=(1.0/delta)*(FDs7p(0)*var2(i-3,j)+FDs7p(1)*var2(i-2,j)+FDs7p(2)*var2(i-1,j)+ &
						& FDs7p(3)*var2(i,j)+FDs7p(4)*var2(i+1,j)+FDs7p(5)*var2(i+2,j)+FDs7p(6)*var2(i+3,j))

					do i=5,ny-4
						if (vtkMask(i,j) == 0) then
							temp2(i,j)=0

						elseif (vtkMask(i-1,j) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs06(0)*var2(i,j)+FDs06(1)*var2(i+1,j)+FDs06(2)*var2(i+2,j)+ &
								& FDs06(3)*var2(i+3,j)+FDs06(4)*var2(i+4,j)+FDs06(5)*var2(i+5,j)+FDs06(6)*var2(i+6,j))

						elseif (vtkMask(i-2,j) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs15(0)*var2(i-1,j)+FDs15(1)*var2(i,j)+FDs15(2)*var2(i+1,j)+ &
								& FDs15(3)*var2(i+2,j)+FDs15(4)*var2(i+3,j)+FDs15(5)*var2(i+4,j)+FDs15(6)*var2(i+5,j))

						elseif (vtkMask(i-3,j) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs24(0)*var2(i-2,j)+FDs24(1)*var2(i-1,j)+FDs24(2)*var2(i,j)+ &
								& FDs24(3)*var2(i+1,j)+FDs24(4)*var2(i+2,j)+FDs24(5)*var2(i+3,j)+FDs24(6)*var2(i+4,j))

						elseif (vtkMask(i-4,j) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs7p(0)*var2(i-3,j)+FDs7p(1)*var2(i-2,j)+FDs7p(2)*var2(i-1,j)+ &
								& FDs7p(3)*var2(i,j)+FDs7p(4)*var2(i+1,j)+FDs7p(5)*var2(i+2,j)+FDs7p(6)*var2(i+3,j))

						elseif (vtkMask(i+1,j) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs60(0)*var2(i-6,j)+FDs60(1)*var2(i-5,j)+FDs60(2)*var2(i-4,j)+ &
								& FDs60(3)*var2(i-3,j)+FDs60(4)*var2(i-2,j)+FDs60(5)*var2(i-1,j)+FDs60(6)*var2(i,j))

						elseif (vtkMask(i+2,j) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs51(0)*var2(i-5,j)+FDs51(1)*var2(i-4,j)+FDs51(2)*var2(i-3,j)+ &
								& FDs51(3)*var2(i-2,j)+FDs51(4)*var2(i-1,j)+FDs51(5)*var2(i,j)+FDs51(6)*var2(i+1,j))

						elseif (vtkMask(i+3,j) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs42(0)*var2(i-4,j)+FDs42(1)*var2(i-3,j)+FDs42(2)*var2(i-2,j)+ &
								& FDs42(3)*var2(i-1,j)+FDs42(4)*var2(i,j)+FDs42(5)*var2(i+1,j)+FDs42(6)*var2(i+2,j))

						elseif (vtkMask(i+4,j) == 0) then
							temp2(i,j)=(1.0/delta)*(FDs7p(0)*var2(i-3,j)+FDs7p(1)*var2(i-2,j)+FDs7p(2)*var2(i-1,j)+ &
								& FDs7p(3)*var2(i,j)+FDs7p(4)*var2(i+1,j)+FDs7p(5)*var2(i+2,j)+FDs7p(6)*var2(i+3,j))

						else
							temp2(i,j)=(1.0/delta)*(FDo9p(0)*var2(i-4,j)+FDo9p(1)*var2(i-3,j)+FDo9p(2)*var2(i-2,j)+ &
								& FDo9p(3)*var2(i-1,j)+FDo9p(4)*var2(i,j)+FDo9p(5)*var2(i+1,j)+FDo9p(6)*var2(i+2,j)+ &
								& FDo9p(7)*var2(i+3,j)+FDo9p(8)*var2(i+4,j))
						end if
					end do
				end do
				call Filter_y(temp,temp2, vtkMask, MeshCaracteristics)
				deallocate(temp2)
end subroutine der2_y







subroutine Filter_x(temp, temp2, vtkMask, MeshCaracteristics)

	implicit none

	real :: delta
	integer :: i,j,nx,ny
	real, dimension(:,:) :: temp, temp2, vtkMask
	real, dimension(5) :: MeshCaracteristics
	
	delta = MeshCaracteristics(5)
	
	nx = size(temp(1,:))
	ny = size(temp(:,1))

	do i = 1,ny
			j=1
			temp(i,j)=temp2(i,j)-(SFs03(0)*temp2(i,j)+SFs03(1)*temp2(i,j+1)+SFs03(2)*temp2(i,j+2)+SFs03(3)*temp2(i,j+3))
			j=2
			temp(i,j)=temp2(i,j)-(SFs15(0)*temp2(i,j-1)+SFs15(1)*temp2(i,j)+SFs15(2)*temp2(i,j+1)+SFs15(3)*temp2(i,j+2)+ &
				& SFs15(4)*temp2(i,j+3)+SFs15(5)*temp2(i,j+4)+SFs15(6)*temp2(i,j+5))
			j=3
			temp(i,j)=temp2(i,j)-(SFs24(0)*temp2(i,j-2)+SFs24(1)*temp2(i,j-1)+SFs24(2)*temp2(i,j)+SFs24(3)*temp2(i,j+1)+ &
				& SFs24(4)*temp2(i,j+2)+SFs24(5)*temp2(i,j+3)+SFs24(6)*temp2(i,j+4))
			j=4
			temp(i,j)=temp2(i,j)-(SFs7p(0)*temp2(i,j-3)+SFs7p(1)*temp2(i,j-2)+SFs7p(2)*temp2(i,j-1)+SFs7p(3)*temp2(i,j)+ &
				& SFs7p(4)*temp2(i,j+1)+SFs7p(5)*temp2(i,j+2)+SFs7p(6)*temp2(i,j+3))
			j=nx
			temp(i,j)=temp2(i,j)-(SFs30(0)*temp2(i,j-3)+SFs30(1)*temp2(i,j-2)+SFs30(2)*temp2(i,j-1)+SFs30(3)*temp2(i,j))
			j=nx-1
			temp(i,j)=temp2(i,j)-(SFs51(0)*temp2(i,j-5)+SFs51(1)*temp2(i,j-4)+SFs51(2)*temp2(i,j-3)+SFs51(3)*temp2(i,j-2)+ &
				& SFs51(4)*temp2(i,j-1)+SFs51(5)*temp2(i,j)+SFs51(6)*temp2(i,j+1))
			j=nx-2
			temp(i,j)=temp2(i,j)-(SFs42(0)*temp2(i,j-4)+SFs42(1)*temp2(i,j-3)+SFs42(2)*temp2(i,j-2)+SFs42(3)*temp2(i,j-1)+ &
				& SFs42(4)*temp2(i,j)+SFs42(5)*temp2(i,j+1)+SFs42(6)*temp2(i,j+2))
			j=nx-3
			temp(i,j)=temp2(i,j)-(SFs7p(0)*temp2(i,j-3)+SFs7p(1)*temp2(i,j-2)+SFs7p(2)*temp2(i,j-1)+SFs7p(3)*temp2(i,j)+ &
				& SFs7p(4)*temp2(i,j+1)+SFs7p(5)*temp2(i,j+2)+SFs7p(6)*temp2(i,j+3))
			do j=5,nx-4
				if (vtkMask(i,j) == 0) then
				temp(i,j)=0

				elseif (vtkMask(i,j-1) == 0) then
					temp(i,j)=temp2(i,j)-(SFs03(0)*temp2(i,j)+SFs03(1)*temp2(i,j+1)+SFs03(2)*temp2(i,j+2)+SFs03(3)*temp2(i,j+3))

				elseif (vtkMask(i,j-2) == 0) then
					temp(i,j)=temp2(i,j)-(SFs15(0)*temp2(i,j-1)+SFs15(1)*temp2(i,j)+SFs15(2)*temp2(i,j+1)+SFs15(3)*temp2(i,j+2)+ &
						& SFs15(4)*temp2(i,j+3)+SFs15(5)*temp2(i,j+4)+SFs15(6)*temp2(i,j+5))

				elseif (vtkMask(i,j-3) == 0) then
					temp(i,j)=temp2(i,j)-(SFs24(0)*temp2(i,j-2)+SFs24(1)*temp2(i,j-1)+SFs24(2)*temp2(i,j)+SFs24(3)*temp2(i,j+1)+ &
						& SFs24(4)*temp2(i,j+2)+SFs24(5)*temp2(i,j+3)+SFs24(6)*temp2(i,j+4))

				elseif (vtkMask(i,j-4) == 0) then
					temp(i,j)=temp2(i,j)-(SFs7p(0)*temp2(i,j-3)+SFs7p(1)*temp2(i,j-2)+SFs7p(2)*temp2(i,j-1)+SFs7p(3)*temp2(i,j)+ &
						& SFs7p(4)*temp2(i,j+1)+SFs7p(5)*temp2(i,j+2)+SFs7p(6)*temp2(i,j+3))

				elseif (vtkMask(i,j+1) == 0) then
					temp(i,j)=temp2(i,j)-(SFs30(0)*temp2(i,j-3)+SFs30(1)*temp2(i,j-2)+SFs30(2)*temp2(i,j-1)+SFs30(3)*temp2(i,j))

				elseif (vtkMask(i,j+2) == 0) then
					temp(i,j)=temp2(i,j)-(SFs51(0)*temp2(i,j-5)+SFs51(1)*temp2(i,j-4)+SFs51(2)*temp2(i,j-3)+SFs51(3)*temp2(i,j-2)+ &
						& SFs51(4)*temp2(i,j-1)+SFs51(5)*temp2(i,j)+SFs51(6)*temp2(i,j+1))

				elseif (vtkMask(i,j+3) == 0) then
					temp(i,j)=temp2(i,j)-(SFs42(0)*temp2(i,j-4)+SFs42(1)*temp2(i,j-3)+SFs42(2)*temp2(i,j-2)+SFs42(3)*temp2(i,j-1)+ &
						& SFs42(4)*temp2(i,j)+SFs42(5)*temp2(i,j+1)+SFs42(6)*temp2(i,j+2))

				elseif (vtkMask(i,j+4) == 0) then
					temp(i,j)=temp2(i,j)-(SFs7p(0)*temp2(i,j-3)+SFs7p(1)*temp2(i,j-2)+SFs7p(2)*temp2(i,j-1)+SFs7p(3)*temp2(i,j)+ &
						& SFs7p(4)*temp2(i,j+1)+SFs7p(5)*temp2(i,j+2)+SFs7p(6)*temp2(i,j+3))

				else
				temp(i,j)=temp2(i,j)-(SFo9p(0)*temp2(i,j-4)+SFo9p(1)*temp2(i,j-3)+SFo9p(2)*temp2(i,j-2)+SFo9p(3)*temp2(i,j-1)+ &
					& SFo9p(4)*temp2(i,j)+SFo9p(5)*temp2(i,j+1)+SFo9p(6)*temp2(i,j+2)+SFo9p(7)*temp2(i,j+3)+SFo9p(8)*temp2(i,j+4))
				end if
			end do
		end do

end subroutine Filter_x










subroutine Filter_y(temp, temp2, vtkMask, MeshCaracteristics)

	implicit none

	real :: delta
	integer :: i,j,nx,ny
	real, dimension(:,:) :: temp, temp2, vtkMask
	real, dimension(5) :: MeshCaracteristics

	delta = MeshCaracteristics(5)
	nx = size(temp(1,:))
	ny = size(temp(:,1))

	do j = 1,nx
			i=1
			temp(i,j)=temp2(i,j)-(SFs03(0)*temp2(i,j)+SFs03(1)*temp2(i+1,j)+SFs03(2)*temp2(i+2,j)+SFs03(3)*temp2(i+3,j))
			i=2
			temp(i,j)=temp2(i,j)-(SFs15(0)*temp2(i-1,j)+SFs15(1)*temp2(i,j)+SFs15(2)*temp2(i+1,j)+SFs15(3)*temp2(i+2,j)+ &
				& SFs15(4)*temp2(i+3,j)+SFs15(5)*temp2(i+4,j)+SFs15(6)*temp2(i+5,j))
			i=3
			temp(i,j)=temp2(i,j)-(SFs24(0)*temp2(i-2,j)+SFs24(1)*temp2(i-1,j)+SFs24(2)*temp2(i,j)+SFs24(3)*temp2(i+1,j)+ &
				& SFs24(4)*temp2(i+2,j)+SFs24(5)*temp2(i+3,j)+SFs24(6)*temp2(i+4,j))
			i=4
			temp(i,j)=temp2(i,j)-(SFs7p(0)*temp2(i-3,j)+SFs7p(1)*temp2(i-2,j)+SFs7p(2)*temp2(i-1,j)+SFs7p(3)*temp2(i,j)+ &
				& SFs7p(4)*temp2(i+1,j)+SFs7p(5)*temp2(i+2,j)+SFs7p(6)*temp2(i+3,j))
			i=ny
			temp(i,j)=temp2(i,j)-(SFs30(0)*temp2(i-3,j)+SFs30(1)*temp2(i-2,j)+SFs30(2)*temp2(i-1,j)+SFs30(3)*temp2(i,j))
			i=ny-1
			temp(i,j)=temp2(i,j)-(SFs51(0)*temp2(i-5,j)+SFs51(1)*temp2(i-4,j)+SFs51(2)*temp2(i-3,j)+SFs51(3)*temp2(i-2,j)+ &
				& SFs51(4)*temp2(i-1,j)+SFs51(5)*temp2(i,j)+SFs51(6)*temp2(i+1,j))
			i=ny-2
			temp(i,j)=temp2(i,j)-(SFs42(0)*temp2(i-4,j)+SFs42(1)*temp2(i-3,j)+SFs42(2)*temp2(i-2,j)+SFs42(3)*temp2(i-1,j)+ &
				& SFs42(4)*temp2(i,j)+SFs42(5)*temp2(i+1,j)+SFs42(6)*temp2(i+2,j))
			i=ny-3
			temp(i,j)=temp2(i,j)-(SFs7p(0)*temp2(i-3,j)+SFs7p(1)*temp2(i-2,j)+SFs7p(2)*temp2(i-1,j)+SFs7p(3)*temp2(i,j)+ &
				& SFs7p(4)*temp2(i+1,j)+SFs7p(5)*temp2(i+2,j)+SFs7p(6)*temp2(i+3,j))
			do i=5,ny-4
				if (vtkMask(i,j) == 0) then
					temp(i,j)=0

				elseif (vtkMask(i-1,j) == 0) then
					temp(i,j)=temp2(i,j)-(SFs03(0)*temp2(i,j)+SFs03(1)*temp2(i+1,j)+SFs03(2)*temp2(i+2,j)+SFs03(3)*temp2(i+3,j))

				elseif (vtkMask(i-2,j) == 0) then
					temp(i,j)=temp2(i,j)-(SFs15(0)*temp2(i-1,j)+SFs15(1)*temp2(i,j)+SFs15(2)*temp2(i+1,j)+SFs15(3)*temp2(i+2,j)+ &
						& SFs15(4)*temp2(i+3,j)+SFs15(5)*temp2(i+4,j)+SFs15(6)*temp2(i+5,j))

				elseif (vtkMask(i-3,j) == 0) then
					temp(i,j)=temp2(i,j)-(SFs24(0)*temp2(i-2,j)+SFs24(1)*temp2(i-1,j)+SFs24(2)*temp2(i,j)+SFs24(3)*temp2(i+1,j)+ &
						& SFs24(4)*temp2(i+2,j)+SFs24(5)*temp2(i+3,j)+SFs24(6)*temp2(i+4,j))

				elseif (vtkMask(i-4,j) == 0) then
					temp(i,j)=temp2(i,j)-(SFs7p(0)*temp2(i-3,j)+SFs7p(1)*temp2(i-2,j)+SFs7p(2)*temp2(i-1,j)+SFs7p(3)*temp2(i,j)+ &
						& SFs7p(4)*temp2(i+1,j)+SFs7p(5)*temp2(i+2,j)+SFs7p(6)*temp2(i+3,j))

				elseif (vtkMask(i+1,j) == 0) then
					temp(i,j)=temp2(i,j)-(SFs30(0)*temp2(i-3,j)+SFs30(1)*temp2(i-2,j)+SFs30(2)*temp2(i-1,j)+SFs30(3)*temp2(i,j))

				elseif (vtkMask(i+2,j) == 0) then
					temp(i,j)=temp2(i,j)-(SFs51(0)*temp2(i-5,j)+SFs51(1)*temp2(i-4,j)+SFs51(2)*temp2(i-3,j)+SFs51(3)*temp2(i-2,j)+ &
						& SFs51(4)*temp2(i-1,j)+SFs51(5)*temp2(i,j)+SFs51(6)*temp2(i+1,j))

				elseif (vtkMask(i+3,j) == 0) then
					temp(i,j)=temp2(i,j)-(SFs42(0)*temp2(i-4,j)+SFs42(1)*temp2(i-3,j)+SFs42(2)*temp2(i-2,j)+SFs42(3)*temp2(i-1,j)+ &
						& SFs42(4)*temp2(i,j)+SFs42(5)*temp2(i+1,j)+SFs42(6)*temp2(i+2,j))

				elseif (vtkMask(i+4,j) == 0) then
					temp(i,j)=temp2(i,j)-(SFs7p(0)*temp2(i-3,j)+SFs7p(1)*temp2(i-2,j)+SFs7p(2)*temp2(i-1,j)+SFs7p(3)*temp2(i,j)+ &
						& SFs7p(4)*temp2(i+1,j)+SFs7p(5)*temp2(i+2,j)+SFs7p(6)*temp2(i+3,j))

				else
					temp(i,j)=temp2(i,j)-(SFo9p(0)*temp2(i-4,j)+SFo9p(1)*temp2(i-3,j)+SFo9p(2)*temp2(i-2,j)+SFo9p(3)*temp2(i-1,j)+ &
						& SFo9p(4)*temp2(i,j)+SFo9p(5)*temp2(i+1,j)+SFo9p(6)*temp2(i+2,j)+SFo9p(7)*temp2(i+3,j)+SFo9p(8)*temp2(i+4,j))
				end if
			end do
		end do
end subroutine Filter_y


end Module ModuleDerivatives