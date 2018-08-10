module ModuleRead
	implicit none

	contains

	function GetRealVtkValue(Word_to_search)

		character*100 :: string
		integer :: line_no, io, nlines, len_word
		real :: GetRealVtkValue

		character(len=*) :: Word_to_search

		len_word = len(Word_to_search)

		open (18, file='SNGM_config')

		do while(.true.)
			read(18,'(a)',end=10) string
			if(string.eq."") then 
				string = " "
			end if

			if(string(1:len_word) == word_to_search) then
				READ(string(len_word+2:100),*) GetRealVtkValue
				exit
			end if
		end do
		10  continue

		close(18)

	end function GetRealVtkValue




	function GetIntVtkValue(Word_to_search)

		character*100 :: string
		integer :: line_no, io, nlines, len_word
		integer :: GetIntVtkValue

		character(len=*) :: Word_to_search

		len_word = len(Word_to_search)

		open (18, file='SNGM_config')

		do while(.true.)
			read(18,'(a)',end=10) string
			if(string.eq."") then 
				string = " "
			end if

			if(string(1:len_word) == word_to_search) then
				READ(string(len_word+2:100),*) GetIntVtkValue
				exit
			end if
		end do
		10  continue

		close(18)

	end function GetIntVtkValue

	
	
	
	function GetStringVtkValue(StringToSearch)

		character*200 :: string
		integer :: line_no, io, nlines, len_word
		character(len=*) :: StringToSearch
		character(len=:), allocatable :: GetStringVtkValue

		len_word = len(StringToSearch)


		open (18, file='SNGM_config')

		do while(.true.)
			read (18,'(a)',end=10) string
			if(string.eq."") then 
				string = " "
			end if

			if(string(1:len_word) == StringToSearch) then
				allocate(character(len(trim(string(len_word+2:200)))) :: GetStringVtkValue)
				GetStringVtkValue = trim(string(len_word+2:200))
				exit
			end if
		end do
		10  continue
		
		close(18)



	end function GetStringVtkValue	
	
	
	
	
	
	
	
	function GetVtkField(Word_to_search,File_path)

		integer :: len_word, k_init, k_end, Number_line, Point_number, ii, Number_cursor
		real :: temp
		real, allocatable :: GetVtkField(:)
		character*200 :: string
		character(len=*) :: Word_to_search, File_path
		character(len=:), allocatable :: Temp_word

		len_word = len(Word_to_search)

		open (20, file= File_path)
			do while(.true.)
				read(20,'(a)',end=11) string
				if(string.eq."") then 
					string = " "
				end if
				if(string(1:10) == 'POINT_DATA') then
					READ(string(12:200),*) Point_number
					exit
				end if
				
			end do
			11  continue
		close(20)


		if (modulo(Point_number,9) == 0) then
			Number_line = Point_number/9
		else
			Number_line = (Point_number-modulo(Point_number,9))/9+1
		end if

		allocate(GetVtkField(Point_number))

		open (20, file=File_path)

		do while(.true.)

			read(20,'(a)',end=10) string
			
			if(string.eq."") then 
				string = " "
			end if
			
			
			
			if(string(1:len_word) == word_to_search) then
				do ii = 1,Number_line
					Number_cursor = 1
					read(20,'(a)',end=10) string
					allocate(character(len(trim(string))) :: Temp_word)
					Temp_word = trim(string)
					k_init = 1
					k_end = 1
					
					do k_end = 1,len(Temp_word)-1
						if(Temp_word(k_end:k_end) == " ") then
							READ(Temp_word(k_init:k_end),*) temp
							GetVtkField(Number_cursor+9*(ii-1)) = temp
							k_init = k_end+1
							Number_cursor = Number_cursor+1
						end if
					end do
					
					READ(Temp_word(k_init:len(Temp_word)),*) temp
					GetVtkField(Number_cursor+9*(ii-1)) = temp
					
					deallocate(Temp_word)
					
					end do
				exit
			end if
			
			
			
			
		end do
		10  continue
		
		close(20)

	end function GetVtkField	
	
	


	function ReadParticleSave(Particle_path)

		character(len=*) :: Particle_path
		character*3 :: extension
		real, allocatable :: ReadParticleSave(:,:)
		integer :: count, Nword, i

		Nword = len(Particle_path)
		extension = Particle_path(Nword-2:Nword)

		if (extension == "bin") then
			OPEN(3,FILE=Particle_path,FORM='UNFORMATTED',ACTION='READ',STATUS='OLD')
		else if (extension == "csv") then
			OPEN(3,FILE=Particle_path,FORM='formatted',ACTION='READ',STATUS='OLD')
		end if

		count = 0
		DO 
			if (extension == "bin") then
		    	READ (3, END=10) 
		    else if (extension == "csv") then
		    	READ (3, *, END=10) 
		    end if
		    count = count + 1 
		END DO 

		10 close(3)

		allocate(ReadParticleSave(count,4))


		if (extension == "bin") then
			OPEN(3,FILE=Particle_path,FORM='UNFORMATTED',ACTION='READ',STATUS='OLD')
		else if (extension == "csv") then
			OPEN(3,FILE=Particle_path,FORM='formatted',ACTION='READ',STATUS='OLD')
		end if

		do i = 1,count
			if (extension == "bin") then
		    	READ(3) ReadParticleSave(i,:)
		    else if (extension == "csv") then
		    	READ(3,*) ReadParticleSave(i,:)
		    end if
			
		end do
		close(3)


	end function ReadParticleSave	
	


	subroutine GetTimeStepSave(TimeStep,Particle_path)

		integer :: Nword, k, TimeStep
		character(len=*) :: Particle_path

		Nword = len(Particle_path)

		do k = 1,100
			if (Particle_path(Nword-3-k:Nword-3-k) == "e") then
				exit
			end if
		end do

		READ(Particle_path(Nword-2-k:Nword-4),*) TimeStep

		TimeStep = TimeStep+1

	end subroutine GetTimeStepSave	
	

END module ModuleRead
