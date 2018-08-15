module ModuleWrite

	use ModuleMath

	implicit none

	contains

	subroutine WriteParticle(TimeStep,Particle_to_write,Condition)

		implicit none

		real,dimension(:,:) :: Particle_to_write
		integer :: i,N,TimeStep,Condition

		if (Condition == 0) then
			return
		end if

		N = size(Particle_to_write(:,1))

		OPEN(11, FILE='Output_time/'//'Particle'//trim(str(TimeStep))//'.csv', ACTION="write", STATUS="replace")

		DO i = 1,N
			write(11,*) Particle_to_write(i,:)
    	END DO

    	close(11)

	end subroutine WriteParticle

	subroutine WriteBinParticle(TimeStep,Particle_to_write,Condition)

		implicit none

		real,dimension(:,:) :: Particle_to_write
		integer :: i,N,TimeStep,Condition

		if (Condition == 0) then
			return
		end if

		N = size(Particle_to_write(:,1))

		OPEN(11, FILE='Output_time/'//'Particle'//trim(str(TimeStep))//'.bin', FORM='UNFORMATTED', ACTION="write", STATUS="replace")

		DO i = 1,N
			write(11) Particle_to_write(i,:)
    	END DO

    	close(11)

	end subroutine WriteBinParticle




	subroutine WriteData(TimeStep,DataToWrite,FileName,Condition)

		implicit none

		real,dimension(:,:) :: DataToWrite
		integer :: i,N,TimeStep,Condition
		character(len=*) :: FileName

		if (Condition == 0) then
			return
		end if

		N = size(DataToWrite(:,1))

		OPEN(9, FILE='Output_time/'//FileName//trim(str(TimeStep))//'.csv', ACTION="write", STATUS="replace")
		DO i = 1,N
			write(9,*) DataToWrite(i,:)
    	END DO

    	close(9)

	end subroutine WriteData




	subroutine WriteBinData(TimeStep,DataToWrite,FileName,Condition)

		implicit none

		real,dimension(:,:) :: DataToWrite
		integer :: i,N,TimeStep,Condition
		character(len=*) :: FileName

		if (Condition == 0) then
			return
		end if

		N = size(DataToWrite(:,1))

		OPEN(9, FILE='Output_time/'//FileName//trim(str(TimeStep))//'.bin', FORM='UNFORMATTED', ACTION="write", STATUS="replace")
		DO i = 1,N
			write(9) DataToWrite(i,:)
    	END DO

    	close(9)

	end subroutine WriteBinData



	subroutine WriteFourier(DataToWrite,FileName,Condition)

		implicit none

		complex,dimension(:,:,:) :: DataToWrite
		integer :: i,Condition,Number_freq
		character(len=*) :: FileName

		if (Condition == 0) then
			return
		end if

		Number_freq = size(DataToWrite(1,1,:))
		!$OMP PARALLEL DO PRIVATE(i)
		DO i = 1,Number_freq
			OPEN(i, FILE='Output_frequency/'//FileName//trim(str(i))//'.csv', ACTION="write", STATUS="replace")
			write(i,*) DataToWrite(:,1,i)
			write(i,*) DataToWrite(:,2,i)
			close(i)
    	END DO
		!$OMP END PARALLEL DO
	end subroutine WriteFourier




	subroutine WriteBinFourier(DataToWrite,FileName,Condition)

		implicit none

		complex,dimension(:,:,:) :: DataToWrite
		integer :: i,Condition,Number_freq
		character(len=*) :: FileName

		if (Condition == 0) then
			return
		end if

		Number_freq = size(DataToWrite(1,1,:))
		!$OMP PARALLEL DO PRIVATE(i)
		DO i = 1,Number_freq
			OPEN(i, FILE='Output_frequency/'//FileName//trim(str(i))//'.bin', FORM='UNFORMATTED', ACTION="write", STATUS="replace")
			write(i) DataToWrite(:,1,i)
			write(i) DataToWrite(:,2,i)
			close(i)
    	END DO
		!$OMP END PARALLEL DO
	end subroutine WriteBinFourier





	subroutine WriteInformation(nx, ny, write_Particle, write_Vel, write_Vor, write_Lsum, &
	&  write_fourier, write_binary_format, Particle, Nt, Number_freq)



		implicit none

		integer :: write_Particle, write_Vel, write_Vor, write_Lsum, write_fourier, nx, ny
		integer :: Total_Memory, write_binary_format, Nt, Number_freq
		real, dimension(:,:) :: Particle

		write(*,*) 'WRITING PARAMETERS'

		if (write_Particle + write_Vel + write_Vor + write_Lsum + write_fourier == 0) then
			write(*,*) 'WARNING : WRITING NO DATA FILES'
		else

			Total_Memory = 0

			if (write_Particle == 1) then
				write(*,*) 'Writing particle : ', trim(str(int(1e-6*(36*write_binary_format+54*(1-write_binary_format)) & 
					& *size(Particle(:,1))*Nt))), ' Mo'
				Total_Memory = Total_Memory + int(1e-6*(36*write_binary_format+54*(1-write_binary_format)) & 
					& *size(Particle(:,1))*Nt)
			end if
			if (write_Vel == 1) then
				write(*,*) 'Writing velociy : ', trim(str(int(1e-6*nx*ny*(16*write_binary_format+35*(1-write_binary_format)) &
					& *Nt))), ' Mo'
				Total_Memory = Total_Memory + int(1e-6*nx*ny*(16*write_binary_format+35*(1-write_binary_format)) &
					& *Nt)
			end if
			if (write_Vor == 1) then
				write(*,*) 'Writing vorticity : ', trim(str(int(1e-6*nx*ny*(12*write_binary_format+18*(1-write_binary_format)) &
					& *Nt))), ' Mo'
				Total_Memory = Total_Memory + int(1e-6*nx*ny*(12*write_binary_format+18*(1-write_binary_format)) &
					& *Nt)
			end if
			if (write_Lsum == 1) then
				write(*,*) 'Writing lamb vector : ', trim(str(int(1e-6*nx*ny*(16*write_binary_format+35*(1-write_binary_format)) &
					& *Nt))), ' Mo'
				Total_Memory = Total_Memory + int(1e-6*nx*ny*(16*write_binary_format+35*(1-write_binary_format)) &
					& *Nt)
			end if
			if (write_fourier == 1) then
				write(*,*) 'Writing fourier transform : ', trim(str(int(1e-6*nx*ny*(16*write_binary_format+72*(1-write_binary_format)) &
					& *Number_freq))), ' Mo'
				Total_Memory = Total_Memory + int(1e-6*nx*ny*(16*write_binary_format+72*(1-write_binary_format)) &
					& *Number_freq)
			end if
			write(*,*) ''
			write(*,*) 'TOTAL MEMORY NEEDED : ', trim(str(Total_Memory)), ' Mo'

		end if
	end subroutine WriteInformation

	
end Module ModuleWrite