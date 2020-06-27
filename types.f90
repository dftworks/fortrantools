module TypesMod

  implicit none

  private
        
  integer, public, parameter :: SP = selected_real_kind(6,37)
  integer, public, parameter :: DP = selected_real_kind(15,307)
  integer, public, parameter :: QP = selected_real_kind(33,4931)

end module TypesMod
