!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
! This module provides generic interfaces for saving                       !
! and loading binary files.                                                !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
module FileMod
  use TypesMod
  use ConstantsMod
  
  implicit none

  private

  public :: save_bin, load_bin

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !                                                                          !
  !             Generic interface of saving binary files                     ! 
  !                                                                          !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  interface save_bin
     ! double-precision data
     module procedure save_bin_dbl_1   ! 1-dim 
     module procedure save_bin_dbl_2   ! 2-dim
     
     ! double-precision complex data
     module procedure save_bin_cmplx_2 ! 2-dim
     module procedure save_bin_cmplx_4 ! 4-dim
     module procedure save_bin_cmplx_5 ! 5-dim
     module procedure save_bin_cmplx_6 ! 6-dim
     
  end interface
  
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !                                                                          !
  !             Generic interface of loading binary files                    ! 
  !                                                                          !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++! 
  interface load_bin
     ! double-precision data
     module procedure load_bin_dbl_1   ! 1-dim
     module procedure load_bin_dbl_2   ! 2-dim
     
     ! double-precision complex data
     module procedure load_bin_cmplx_2 ! 2-dim
     module procedure load_bin_cmplx_4 ! 4-dim
     module procedure load_bin_cmplx_5 ! 5-dim
     module procedure load_bin_cmplx_6 ! 6-dim
     
  end interface

  public :: dump_bin

  interface dump_bin
     module procedure dump_bin_cmplx_4
  end interface

contains
  ! 
  ! Save and load 1-dim double-precision array
  ! 
  subroutine save_bin_dbl_1(filename, buf)
    character (len=*), intent(in)       :: filename
    real (DP), dimension(:), intent(in) :: buf

    integer :: unit

    unit = 30

    open ( unit, FILE = filename, FORM='UNFORMATTED', ACCESS='stream', status='replace' )
    write ( unit ) size(buf)
    write ( unit ) buf
    close ( unit )

  end subroutine save_bin_dbl_1

  subroutine load_bin_dbl_1(filename, buf)
    character (len=*), intent(in)                   :: filename
    real (DP), dimension(:), pointer, intent(inout) :: buf

    integer :: unit
    integer :: n1

    unit = 30

    open ( unit, FILE = filename, FORM='UNFORMATTED', ACCESS='stream', status='old' )
    read ( unit ) n1
    if ( .not. associated(buf) ) then
       allocate ( buf(n1) )
    end if
    read ( unit ) buf
    close ( unit )

  end subroutine load_bin_dbl_1
  ! 
  ! Save and load 2-dim double-precision array
  ! 
  subroutine save_bin_dbl_2(filename, buf)
    character (len=*), intent(in)         :: filename
    real (DP), dimension(:,:), intent(in) :: buf
    ! 
    integer :: unit
    ! 
    unit = 30
    ! 
    open ( unit, FILE = filename, FORM='UNFORMATTED', ACCESS='stream', status='replace' )
    write ( unit ) size(buf,1), size(buf,2)
    write ( unit ) buf
    close ( unit )

  end subroutine save_bin_dbl_2

  subroutine load_bin_dbl_2(filename, buf)
    character (len=*), intent(in)                     :: filename
    real (DP), dimension(:,:), pointer, intent(inout) :: buf

    integer :: unit
    integer :: n1,n2

    unit = 30

    open ( unit, FILE = filename, FORM='UNFORMATTED', ACCESS='stream', status='old' )

    read ( unit ) n1, n2

    if ( .not. associated(buf) ) then
       allocate ( buf(n1,n2) )
    end if

    read ( unit ) buf

    close ( unit )

  end subroutine load_bin_dbl_2
  ! 
  ! Save and load 2-dim double-precision complex array
  ! 
  subroutine save_bin_cmplx_2(filename, buf)
    character (len=*), intent(in)            :: filename
    complex (DP), dimension(:,:), intent(in) :: buf

    integer :: unit

    unit = 30

    open ( unit, FILE = filename, FORM='UNFORMATTED', ACCESS='stream', status='replace' )
    write ( unit ) size(buf,1), size(buf,2)
    write ( unit ) buf
    close ( unit )

  end subroutine save_bin_cmplx_2

  subroutine load_bin_cmplx_2(filename, buf)
    character (len=*), intent(in)                      :: filename
    complex (DP), dimension(:,:), pointer, intent(out) :: buf

    integer :: unit
    integer :: n1,n2

    unit = 30

    open ( unit, FILE = filename, FORM='UNFORMATTED', ACCESS='stream', status='old' )

    read ( unit ) n1, n2

    if ( .not. associated(buf) ) then
       allocate ( buf(n1,n2) )
    end if

    read ( unit ) buf

    close ( unit )

  end subroutine load_bin_cmplx_2
  ! 
  ! Save and load 4-dim double-precision complex array
  ! 
  subroutine save_bin_cmplx_4(filename, buf)
    character (len=*), intent(in)                :: filename
    complex (DP), dimension(:,:,:,:), intent(in) :: buf

    integer :: unit

    unit = 30

    open ( unit, FILE = filename, FORM='UNFORMATTED', ACCESS='stream', status='replace' )

    write ( unit ) size(buf,1), size(buf,2), size(buf,3), size(buf,4)
    write ( unit ) buf

    close ( unit )
    
  end subroutine save_bin_cmplx_4

  subroutine load_bin_cmplx_4(filename, buf)
    character (len=*), intent(in)                          :: filename
    complex (DP), dimension(:,:,:,:), pointer, intent(out) :: buf

    integer :: unit
    integer :: n1,n2,n3,n4

    unit = 30

    open ( unit, FILE = filename, FORM='UNFORMATTED', ACCESS='stream', status='old' )

    read ( unit ) n1, n2, n3, n4

    if ( .not. associated(buf) ) then
       allocate ( buf(n1,n2,n3,n4) )
    end if

    read ( unit ) buf

    close ( unit )

  end subroutine load_bin_cmplx_4
  ! 
  ! Save and load 5-dim double precision complex array
  ! 
  subroutine save_bin_cmplx_5(filename, buf)
    character (len=*), intent(in)                  :: filename
    complex (DP), dimension(:,:,:,:,:), intent(in) :: buf

    integer :: unit

    unit = 30

    open ( unit, FILE = filename, FORM='UNFORMATTED', ACCESS='stream', status='replace' )

    write ( unit ) size(buf,1), size(buf,2), size(buf,3), size(buf,4), size(buf,5)
    write ( unit ) buf

    close ( unit )

  end subroutine save_bin_cmplx_5

  subroutine load_bin_cmplx_5(filename, buf)
    character (len=*), intent(in)                            :: filename
    complex (DP), dimension(:,:,:,:,:), pointer, intent(out) :: buf

    integer :: unit
    integer :: n1,n2,n3,n4,n5

    unit = 30

    open ( unit, FILE = filename, FORM='UNFORMATTED', ACCESS='stream', status='old' )

    read ( unit ) n1, n2, n3, n4, n5

    if ( .not. associated(buf) ) then
       allocate ( buf(n1,n2,n3,n4,n5) )
    end if

    read ( unit ) buf

    close ( unit )

  end subroutine load_bin_cmplx_5
  ! 
  ! Save and load 6-dim double precision complex array
  ! 
  subroutine save_bin_cmplx_6(filename, buf)
    character (len=*), intent(in)                    :: filename
    complex (DP), dimension(:,:,:,:,:,:), intent(in) :: buf

    integer :: unit

    unit = 30

    open ( unit, FILE = filename, FORM='UNFORMATTED', ACCESS='stream', status='replace' )

    write ( unit ) size(buf,1), size(buf,2), size(buf,3), size(buf,4), size(buf,5), size(buf,6)
    write ( unit ) buf

    close ( unit )

  end subroutine save_bin_cmplx_6

  subroutine load_bin_cmplx_6(filename, buf)
    character (len=*), intent(in)                              :: filename
    complex (DP), dimension(:,:,:,:,:,:), pointer, intent(out) :: buf

    integer :: unit
    integer :: n1,n2,n3,n4,n5,n6

    unit = 30

    open ( unit, FILE = filename, FORM='UNFORMATTED', ACCESS='stream', status='old' )

    read ( unit ) n1, n2, n3, n4, n5, n6

    if ( .not. associated(buf) ) then
       allocate ( buf(n1,n2,n3,n4,n5,n6) )
    end if

    read ( unit ) buf

    close ( unit )

  end subroutine load_bin_cmplx_6
  ! 
  ! Dump 4-dim double precision complex array to the file unit
  ! 
  subroutine dump_bin_cmplx_4(unit,buf)
    integer, intent(in)                          :: unit
    complex (DP), dimension(:,:,:,:), intent(in) :: buf

    write ( unit ) buf

  end subroutine dump_bin_cmplx_4

end module FileMod
