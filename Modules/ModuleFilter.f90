module ModuleFilter

	use ModuleMath

	implicit none

	real,dimension(7) :: HyperGeom1  = (/ 1.,1.5,0.4602272727272727,0.058656417112299, &
		& 0.004064507164032,0.000176595828506,0.000000113802154 /)

	real,dimension(8) :: HyperGeom2  = (/ 1.,1.040816326530612,0.26790247243087,0.030426591459741, &
		& 0.001935131216840,0.000078535900579,0.000002208558563,0.000000045560159 /)

	real :: VonKarman_constante_factor=1.665704432226795
	real :: VonKarman_alpha_zeta=1.338985279065280
	real :: VonKarman_B=0.94856635532996846076
	real :: VonKarman_C=1.21673332546745177014

	contains

	function Gaussian_filter(coord,TKE,Lambda)

		implicit none

		real :: pi
		real :: TKE,Lambda,R,Gaussian_filter
		real,dimension(2) :: coord

		pi = 3.14159265358
		R = sqrt(coord(1)**2+coord(2)**2)
		Gaussian_filter = sqrt(TKE*2/pi)*exp(-pi*R**2/(2*Lambda**2))

	end function Gaussian_filter
	

	function VonKarman_filter(coord,TKE,Lambda)

		implicit none

		real :: pi, temp
		real :: TKE,Lambda,R,VonKarman_filter
		real,dimension(2) :: coord

		R = sqrt(coord(1)**2+coord(2)**2)

		temp = R/(2*VonKarman_alpha_zeta*Lambda)

		VonKarman_filter = VonKarman_constante_factor*sqrt(TKE)*(VonKarman_B*EvalPoly(HyperGeom1,temp**2) &
						   & - VonKarman_C*temp**(1./3.)*EvalPoly(HyperGeom2,temp**2))

	end function VonKarman_filter


	function Liepmann_filter(coord,TKE,Lambda)

		implicit none

		real :: pi, temp
		real :: TKE,Lambda,R,Liepmann_filter
		real,dimension(2) :: coord

		R = sqrt(coord(1)**2+coord(2)**2)

		temp = R/(2*VonKarman_alpha_zeta*Lambda)

		Liepmann_filter = VonKarman_constante_factor*sqrt(TKE)*(VonKarman_B*EvalPoly(HyperGeom1,temp**2) &
						   & - VonKarman_C*temp**(1./3)*EvalPoly(HyperGeom2,temp**2))

	end function Liepmann_filter



end Module ModuleFilter