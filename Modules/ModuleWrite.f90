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

		OPEN(11, FILE='Output/'//'Particle'//trim(str(TimeStep))//'.csv', ACTION="write", STATUS="replace")

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

		OPEN(11, FILE='Output/'//'Particle'//trim(str(TimeStep))//'.bin', FORM='UNFORMATTED', ACTION="write", STATUS="replace")

		DO i = 1,N
			write(11) Particle_to_write(i,:)
    	END DO

    	close(11)

	end subroutine WriteBinParticle




	subroutine WriteData(TimeStep,DataToWrite,FileName,Condition)

		implicit none

		real,dimension(:,:) :: DataToWrite
		integer :: i,j,N,TimeStep,Condition
		character(len=*) :: FileName

		if (Condition == 0) then
			return
		end if

		N = size(DataToWrite(:,1))

		OPEN(9, FILE='Output/'//FileName//trim(str(TimeStep))//'.csv', ACTION="write", STATUS="replace")
		DO i = 1,N
			write(9,*) DataToWrite(i,:)
    	END DO

    	close(9)

	end subroutine WriteData




	subroutine WriteBinData(TimeStep,DataToWrite,FileName,Condition)

		implicit none

		real,dimension(:,:) :: DataToWrite
		integer :: i,j,N,TimeStep,Condition
		character(len=*) :: FileName

		if (Condition == 0) then
			return
		end if

		N = size(DataToWrite(:,1))

		OPEN(9, FILE='Output/'//FileName//trim(str(TimeStep))//'.bin', FORM='UNFORMATTED', ACTION="write", STATUS="replace")
		DO i = 1,N
			write(9) DataToWrite(i,:)
    	END DO

    	close(9)

	end subroutine WriteBinData

	
end Module ModuleWrite