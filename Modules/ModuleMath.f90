module ModuleMath

	implicit none

	contains

	character(len=20) function str(k)
!   	"Convert an integer to string."
    	integer, intent(in) :: k
    	write (str, *) k
    	str = adjustl(str)
	end function str


	character(len=20) function hourstr(Total_time)
 	
		implicit none

    	real :: Total_time
    	integer :: hour,minute,seconde
    	character(len=3) :: strh
    	character(len=2) :: strm,strs
    	hour = floor(Total_time/3600)
		minute = floor((Total_time-hour*3600)/60)
		seconde = floor((Total_time-hour*3600-minute*60))
    	write (strh, '(I3.3,A5)') hour
    	write (strm, '(I2.2,A5)') minute
    	write (strs, '(I2.2,A5)') seconde
    	hourstr = strh//':'//strm//':'//strs
	end function hourstr


	function r4_normal_01 ()

	  implicit none
	  real ( kind = 4 ) r1
	  real ( kind = 4 ) r2
	  real ( kind = 4 ) r4_normal_01
	  real ( kind = 4 ), parameter :: r4_pi = 3.141592653589793E+00
	  real ( kind = 4 ) r4_uniform_01
	  real ( kind = 4 ) x

	  r1 = rand()
	  r2 = rand()
	  x = sqrt ( - 2.0E+00 * log ( r1 ) ) * cos ( 2.0E+00 * r4_pi * r2 )

	  r4_normal_01 = x

	end function r4_normal_01
	
	
	
	
	function trapz(y,delta)
		
		implicit none

		real :: trapz, delta
		integer :: N,i
		real, dimension(:) :: y

		N = size(y)
		trapz = 0
		do i = 1,N-1
			trapz = trapz + delta*(y(i+1)+y(i))/2
		end do
	end function trapz
	
	
end Module ModuleMath