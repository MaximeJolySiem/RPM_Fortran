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


END module ModuleRead
