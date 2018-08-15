module ModuleFilter

	use ModuleMath

	implicit none

	real :: pi = 3.14159265358
	private :: pi
	
	real,dimension(8) :: HyperGeom1_Liep  = (/ 1.000000000000000,   1.666666666666667,   0.535714285714286,   0.070346320346320, &
		& 0.004982864357864,   0.000220295055821, 0.000006651420768,   0.000000145798339 /)
	real,dimension(8) :: HyperGeom2_Liep  = (/ 1.000000000000000,   0.960000000000000,   0.237037037037037,   0.026181605668785, &
		& 0.001630688242346,   0.000065079621463, 0.000001804874835,   0.000000036790382 /)

	real :: Liepmann_A=0.73510519389572273268
	real :: Liepmann_B=1.85407467730137191843
	real :: Liepmann_C=1.35195648013456945799
	
	
	real,dimension(7) :: HyperGeom1_VK  = (/ 1.,1.5,0.4602272727272727,0.058656417112299, &
		& 0.004064507164032,0.000176595828506,0.000000113802154 /)
	real,dimension(8) :: HyperGeom2_VK  = (/ 1.,1.040816326530612,0.26790247243087,0.030426591459741, &
		& 0.001935131216840,0.000078535900579,0.000002208558563,0.000000045560159 /)

	real :: VonKarman_constante_factor=1.665704432226795
	real :: VonKarman_alpha_zeta=1.338985279065280
	real :: VonKarman_B=0.94856635532996846076
	real :: VonKarman_C=1.21673332546745177014

	contains

	function Gaussian_filter(R,TKE,Lambda)

		implicit none

		real :: TKE,Lambda,R,Gaussian_filter
		
		Gaussian_filter = sqrt(TKE*2/pi)*exp(-pi*R**2/(2*Lambda**2))

	end function Gaussian_filter
	


	function Gaussian_filter_grid(R,Lambda)

		implicit none

		real :: Lambda,R,Gaussian_filter_grid
		
		Gaussian_filter_grid = exp(-pi*R**2/(2*Lambda**2))

	end function Gaussian_filter_grid	


	function VonKarman_filter(R,TKE,Lambda)

		implicit none

		real :: temp
		real :: TKE,Lambda,R,VonKarman_filter

		temp = R/(2*VonKarman_alpha_zeta*Lambda)

		VonKarman_filter = VonKarman_constante_factor*sqrt(TKE)*(VonKarman_B*EvalPoly(HyperGeom1_VK,temp**2) &
						   & - VonKarman_C*temp**(1./3.)*EvalPoly(HyperGeom2_VK,temp**2))

	end function VonKarman_filter


	function VonKarman_filter_grid(R,Lambda)

		implicit none

		real :: temp
		real :: Lambda,R,VonKarman_filter_grid

		temp = R/(2*VonKarman_alpha_zeta*Lambda)

		VonKarman_filter_grid = VonKarman_constante_factor*(VonKarman_B*EvalPoly(HyperGeom1_VK,temp**2) &
						   & - VonKarman_C*temp**(1./3.)*EvalPoly(HyperGeom2_VK,temp**2))

	end function VonKarman_filter_grid



	function Liepmann_filter(R,TKE,Lambda)

		implicit none

		real :: temp
		real :: TKE,Lambda,R,Liepmann_filter

		temp = R/(2*Lambda)

		Liepmann_filter = Liepmann_A*sqrt(TKE*2/3)*(Liepmann_B*EvalPoly(HyperGeom1_Liep,temp**2) &
						   & - Liepmann_C*sqrt(4*temp)*EvalPoly(HyperGeom2_Liep,temp**2))

	end function Liepmann_filter


	function Liepmann_filter_grid(R,Lambda)

		implicit none

		real :: temp
		real :: Lambda,R,Liepmann_filter_grid

		temp = R/(2*Lambda)

		Liepmann_filter_grid = Liepmann_A*(Liepmann_B*EvalPoly(HyperGeom1_Liep,temp**2) &
						   & - Liepmann_C*sqrt(4*temp)*EvalPoly(HyperGeom2_Liep,temp**2))

	end function Liepmann_filter_grid

end Module ModuleFilter