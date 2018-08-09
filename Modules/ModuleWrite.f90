module ModuleWrite

	use ModuleMath

	implicit none

	contains

	subroutine WriteParticle(TimeStep,Particle_to_write)

		implicit none

		real,dimension(:,:) :: Particle_to_write
		integer :: i,N,TimeStep

		N = size(Particle_to_write(:,1))

		OPEN(11, FILE='Data/'//'Particle'//trim(str(TimeStep))//'.csv', ACTION="write", STATUS="replace")

		DO i = 1,N
			write(11,*) Particle_to_write(i,:)
    	END DO

    	close(11)

	end subroutine WriteParticle

	subroutine WriteBinParticle(TimeStep,Particle_to_write)

		implicit none

		real,dimension(:,:) :: Particle_to_write
		integer :: i,N,TimeStep

		N = size(Particle_to_write(:,1))

		OPEN(11, FILE='Data/'//'Particle'//trim(str(TimeStep))//'.bin', FORM='UNFORMATTED', ACTION="write", STATUS="replace")

		DO i = 1,N
			write(11) Particle_to_write(i,:)
    	END DO

    	close(11)

	end subroutine WriteBinParticle


	subroutine WriteData(TimeStep,DataToWrite,FileName)

		implicit none

		real,dimension(:,:) :: DataToWrite
		integer :: i,j,nx,ny,TimeStep
		character(len=*) :: FileName
		nx = size(DataToWrite(1,:))
		ny = size(DataToWrite(:,1))
		OPEN(9, FILE='Data/'//FileName//trim(str(TimeStep))//'.csv', ACTION="write", STATUS="replace")
		DO i = 1,ny
			DO j = 1,nx
				write(9,*) DataToWrite(i,j)
	    	END DO
    	END DO

    	close(9)

	end subroutine WriteData


	subroutine WriteBinData(TimeStep,DataToWrite,FileName)

		implicit none

		real,dimension(:,:) :: DataToWrite
		integer :: i,j,nx,ny,TimeStep
		character(len=*) :: FileName
		nx = size(DataToWrite(1,:))
		ny = size(DataToWrite(:,1))
		OPEN(9, FILE='Data/'//FileName//trim(str(TimeStep))//'.bin', FORM='UNFORMATTED', ACTION="write", STATUS="replace")
		DO i = 1,ny
			DO j = 1,nx
				write(9) DataToWrite(i,j)
	    	END DO
    	END DO

    	close(9)

	end subroutine WriteBinData
	
	
end Module ModuleWrite