module ConstantsMod
  use TypesMod

  implicit none

  private

  integer, parameter, public      :: unit_log   = 2001
  
  real (DP), parameter, public    :: pi         = 3.141592653589793238462643383279_dp
  real (DP), parameter, public    :: tpi        = 2.0_dp * pi 
  real (DP), parameter, public    :: twopi      = 2.0_dp * pi 
  real (DP), parameter, public    :: ry2ev      = 13.605698066_dp
  real (DP), parameter, public    :: b2ang      = 0.529177249_dp
  real (DP), parameter, public    :: b2cm       = b2ang * 1.0e-8_dp
  real (DP), parameter, public    :: fineconst  = 1.0_dp / 137.0359895
  real (DP), parameter, public    :: lightspeed = 2.0_dp / fineconst
  real (DP), parameter, public    :: ang2bohr   = 1.0_dp / 0.529177249_dp
  complex (DP), parameter, public :: cmplx_i    = dcmplx(0.0_dp, 1.0_dp)
  complex (DP), parameter, public :: cmplx_0    = dcmplx(0.0_dp, 0.0_dp)
  complex (DP), parameter, public :: cmplx_1    = dcmplx(1.0_dp, 0.0_dp)

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !                                                                          !  
  !                   numerical convergence constants                        !
  !                                                                          ! 
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  real (DP), parameter, public    :: eps1       = 1.0e-1_dp
  real (DP), parameter, public    :: eps2       = 1.0e-2_dp
  real (DP), parameter, public    :: eps3       = 1.0e-3_dp
  real (DP), parameter, public    :: eps4       = 1.0e-4_dp
  real (DP), parameter, public    :: eps5       = 1.0e-5_dp
  real (DP), parameter, public    :: eps6       = 1.0e-6_dp
  real (DP), parameter, public    :: eps7       = 1.0e-7_dp
  real (DP), parameter, public    :: eps8       = 1.0e-8_dp
  real (DP), parameter, public    :: eps9       = 1.0e-9_dp
  real (DP), parameter, public    :: eps10      = 1.0e-10_dp
  
end module ConstantsMod
