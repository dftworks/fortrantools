!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
! This module provides a MPI class to encapsulate the mpi subroutines      !
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
module MpiMod
  use TypesMod  
  implicit none
  include "mpif.h"

  private
  
  integer, parameter :: ROOT = 0
  
  type, public :: MPI

     private

     integer :: ierr, nproc, rank
     integer :: status(MPI_STATUS_SIZE)

   contains

     procedure :: barrier


     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
     !                                                                          !  
     !   Generic interface of sending data of different types and dimensions    !
     !                                                                          ! 
     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
     generic :: send => send_int, &
          send_dcmplx_4d

     ! Send integer data
     procedure :: send_int         ! single number

     ! Send double-precision complex data
     procedure :: send_dcmplx_4d   ! 4-dim array


     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
     !                                                                          !   
     !  Generic interface of receiving data of different types and dimensions   !
     !                                                                          !
     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
     generic :: recv => recv_int, &
          recv_dcmplx_4d

     ! Receiv integer data
     procedure :: recv_int         ! single number
     procedure :: recv_dcmplx_4d   ! 4-dim array
     
     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
     !                                                                          !
     ! Generic interface of broadcasting data of different types and dimensions ! 
     !                                                                          !
     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
     generic :: bcast => bcast_int, bcast_int_1d, bcast_int_2d, & 
          bcast_dbl, bcast_dbl_1d, bcast_dbl_2d, & 
          bcast_dcmplx, bcast_dcmplx_1d, bcast_dcmplx_2d, bcast_dcmplx_3d, bcast_dcmplx_4d, &
          bcast_char_1d

     ! Broadcast integer data
     procedure :: bcast_int        ! single number
     procedure :: bcast_int_1d     ! 1-dim array 
     procedure :: bcast_int_2d     ! 2-dim array

     ! Broadcast double-precision data
     procedure :: bcast_dbl        ! single number
     procedure :: bcast_dbl_1d     ! 1-dim array
     procedure :: bcast_dbl_2d     ! 2-dim array

     ! Broadcast double-precision complex data
     procedure :: bcast_dcmplx     ! single number
     procedure :: bcast_dcmplx_1d  ! 1-dim array
     procedure :: bcast_dcmplx_2d  ! 2-dim array
     procedure :: bcast_dcmplx_3d  ! 3-dim array
     procedure :: bcast_dcmplx_4d  ! 4-dim array

     ! Broadcast a 1-dim character array 
     procedure :: bcast_char_1d

     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
     !                                                                          !
     !  Generic interface of reducing data of different types and dimensions    ! 
     !                                                                          !  
     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
     generic :: reduce => reduce_int, &
          reduce_dbl_2d,  &
          reduce_dcmplx_1d, reduce_dcmplx_2d, reduce_dcmplx_4d, reduce_dcmplx_6d


     ! Reduce integer data
     procedure :: reduce_int       ! single number
     
     ! Reduce double-precision data
     procedure :: reduce_dbl_2d    ! 2-dim array
     
     ! Reduce double-precision complex data
     procedure :: reduce_dcmplx_1d ! 1-dim array
     procedure :: reduce_dcmplx_2d ! 2-dim array
     procedure :: reduce_dcmplx_4d ! 4-dim array
     procedure :: reduce_dcmplx_6d ! 6-dim array

     
     procedure :: is_master
     procedure :: get_rank
     procedure :: size => get_nproc
     ! 
  end type MPI

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !                                                                          !
  !             Interfaces for constructors and destructors                  ! 
  !                                                                          !  
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  public :: create, init, release, destroy
  
  interface create
     module procedure create_single
  end interface

  interface init
     module procedure init_single
  end interface

  interface release
     module procedure release_single
  end interface

  interface destroy
     module procedure destroy_single
  end interface

  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  !                                                                          ! 
  !            Interfaces of external mpi_* subroutines                      !
  !                                                                          !
  !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
  interface 
     
     subroutine mpi_abort(comm, errcode, ierror)
       integer, intent(in)  :: comm, errcode
       integer, intent(out) :: ierror
     end subroutine mpi_abort
     
     subroutine mpi_init(ierror)
       integer, intent(out) :: ierror
     end subroutine mpi_init

     subroutine mpi_comm_size(comm, nnodes, ierror)
       integer, intent(in)  :: comm
       integer, intent(out) :: nnodes, ierror
     end subroutine mpi_comm_size
     
     subroutine mpi_comm_rank(comm, rank, ierror)
       integer, intent(in)  :: comm
       integer, intent(out) :: rank, ierror
     end subroutine mpi_comm_rank
     
     subroutine mpi_type_struct(count, block, disp, types, newtype, ierror)
       integer, intent(in)               :: count
       integer, dimension(*), intent(in) :: block, disp, types
       integer, intent(out)              :: newtype, ierror
     end subroutine mpi_type_struct
     
     subroutine mpi_type_commit(datatype, ierror)
       integer, intent(in)  :: datatype
       integer, intent(out) :: ierror
     end subroutine mpi_type_commit
     
     subroutine mpi_type_free(datatype, ierror)
       integer, intent(in)  :: datatype
       integer, intent(out) :: ierror
     end subroutine mpi_type_free

  end interface

contains
  ! 
  ! A simple encapsulation for mpi_barrier for future expansion
  ! 
  subroutine barrier(this)
    class (MPI), intent(in) :: this
    
    call mpi_barrier(MPI_COMM_WORLD, this%ierr)
    
  end subroutine barrier
  ! 
  ! Determine if "myself" is the ROOT
  ! 
  function is_master(this) 
    class (MPI), intent(in) :: this
    logical                 :: is_master

    is_master = .false.
    if (this%rank == ROOT) then
       is_master = .true.
    end if

  end function is_master
  !
  ! Return the rank of this process
  ! 
  function get_rank(this) result(rank)
    class (MPI), intent(in) :: this
    integer                 :: rank

    rank = this%rank    
    
  end function get_rank
  ! 
  ! Return the total processes
  ! 
  function get_nproc(this) result(nsize)
    class (MPI), intent(in) :: this
    integer                 :: nsize

    nsize = this%nproc

  end function get_nproc
  ! 
  ! Broadcast for integer
  ! 
  subroutine bcast_int(this, buf)
    class (MPI), intent(in) :: this
    integer, intent(in)     :: buf

    call mpi_bcast(buf, 1, MPI_INTEGER, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine bcast_int

  subroutine bcast_int_1d(this, buf)
    class (MPI), intent(in)           :: this
    integer, dimension(:), intent(in) :: buf
    integer                           :: nsize

    nsize = size(buf)
    call mpi_bcast(buf, nsize, MPI_INTEGER, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine bcast_int_1d

  subroutine bcast_int_2d(this, buf)
    class (MPI), intent(in)             :: this
    integer, dimension(:,:), intent(in) :: buf
    integer                             :: nsize

    nsize = size(buf)
    call mpi_bcast(buf, nsize, MPI_INTEGER, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine bcast_int_2d
  ! 
  ! Broadcast for double precision
  ! 
  subroutine bcast_dbl(this, buf)
    class (MPI), intent(in) :: this
    real (DP), intent(in)   :: buf

    call mpi_bcast(buf, 1, MPI_DOUBLE_PRECISION, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine bcast_dbl

  subroutine bcast_dbl_1d(this, buf)
    class (MPI), intent(in)             :: this
    real (DP), dimension(:), intent(in) :: buf
    integer                             :: nsize

    nsize = size(buf)
    call mpi_bcast(buf, nsize, MPI_DOUBLE_PRECISION, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine bcast_dbl_1d

  subroutine bcast_dbl_2d(this, buf)
    class (MPI), intent(in)               :: this
    real (DP), dimension(:,:), intent(in) :: buf
    integer                               :: nsize

    nsize = size(buf)
    call mpi_bcast(buf, nsize, MPI_DOUBLE_PRECISION, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine bcast_dbl_2d
  !
  ! Broadcast for double complex 
  ! 
  subroutine bcast_dcmplx(this, buf)
    class (MPI), intent(in)  :: this
    complex (DP), intent(in) :: buf
    integer                  :: nsize

    nsize = 1
    call mpi_bcast(buf, nsize, MPI_DOUBLE_COMPLEX, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine bcast_dcmplx

  subroutine bcast_dcmplx_1d(this, buf)
    class (MPI), intent(in)                :: this
    complex (DP), dimension(:), intent(in) :: buf
    integer                                :: nsize

    nsize = size(buf)
    call mpi_bcast(buf, nsize, MPI_DOUBLE_COMPLEX, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine bcast_dcmplx_1d

  subroutine bcast_dcmplx_2d(this, buf)
    class (MPI), intent(in)                  :: this
    complex (DP), dimension(:,:), intent(in) :: buf
    integer                                  :: nsize

    nsize = size(buf)
    call mpi_bcast(buf, nsize, MPI_DOUBLE_COMPLEX, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine bcast_dcmplx_2d

  subroutine bcast_dcmplx_3d(this, buf)
    class (MPI), intent(in)                    :: this
    complex (DP), dimension(:,:,:), intent(in) :: buf
    integer                                    :: nsize

    nsize = size(buf)
    call mpi_bcast(buf, nsize, MPI_DOUBLE_COMPLEX, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine bcast_dcmplx_3d

  subroutine bcast_dcmplx_4d(this, buf)
    class (MPI), intent(in)                      :: this
    complex (DP), dimension(:,:,:,:), intent(in) :: buf
    integer                                      :: nsize

    nsize = size(buf)
    call mpi_bcast(buf, nsize, MPI_DOUBLE_COMPLEX, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine bcast_dcmplx_4d
  ! 
  ! Broadcast for character
  ! 
  subroutine bcast_char_1d(this, buf)
    class (MPI), intent(in)       :: this
    character (len=*), intent(in) :: buf
    integer                       :: nsize

    nsize = len(buf)
    call mpi_bcast(buf, nsize, MPI_CHARACTER, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine bcast_char_1d
  ! 
  ! Reduce for integer variable
  ! 
  subroutine reduce_int(this, sv, dv)
    class (MPI), intent(in) :: this
    integer, intent(in)     :: sv,dv

    call mpi_reduce(sv,dv,1,MPI_INTEGER, MPI_SUM, ROOT, MPI_COMM_WORLD, this%ierr)    

  end subroutine reduce_int
  ! 
  ! Reduce for double precision
  ! 
  subroutine reduce_dbl_2d(this, sbuf, dbuf)
    class (MPI), intent(in)               :: this
    real (DP), dimension(:,:), intent(in) :: sbuf, dbuf
    integer                               :: nsize

    nsize = size(sbuf)
    call mpi_reduce(sbuf, dbuf, nsize, MPI_DOUBLE_PRECISION, MPI_SUM, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine reduce_dbl_2d
  ! 
  ! Reduce for double complex
  ! 
  subroutine reduce_dcmplx_1d(this, sbuf, dbuf)
    class (MPI), intent(in)                :: this
    complex (DP), dimension(:), intent(in) :: sbuf, dbuf
    integer                                :: nsize

    nsize = size(sbuf)
    call mpi_reduce(sbuf, dbuf, nsize, MPI_DOUBLE_COMPLEX, MPI_SUM, ROOT, MPI_COMM_WORLD, this%ierr)
    
  end subroutine reduce_dcmplx_1d

  subroutine reduce_dcmplx_2d(this, sbuf, dbuf)
    class (MPI), intent(in)                  :: this
    complex (DP), dimension(:,:), intent(in) :: sbuf, dbuf
    integer                                  :: nsize

    nsize = size(sbuf)
    call mpi_reduce(sbuf, dbuf, nsize, MPI_DOUBLE_COMPLEX, MPI_SUM, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine reduce_dcmplx_2d

  subroutine reduce_dcmplx_4d(this, sbuf, dbuf)
    class (MPI), intent(in)                      :: this
    complex (DP), dimension(:,:,:,:), intent(in) :: sbuf, dbuf
    integer                                      :: nsize

    nsize = size(sbuf)
    call mpi_reduce(sbuf, dbuf, nsize, MPI_DOUBLE_COMPLEX, MPI_SUM, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine reduce_dcmplx_4d

  subroutine reduce_dcmplx_6d(this, sbuf, dbuf)
    class (MPI), intent(in)                          :: this
    complex (DP), dimension(:,:,:,:,:,:), intent(in) :: sbuf, dbuf
    integer                                          :: nsize

    nsize = size(sbuf)
    call mpi_reduce(sbuf, dbuf, nsize, MPI_DOUBLE_COMPLEX, MPI_SUM, ROOT, MPI_COMM_WORLD, this%ierr)

  end subroutine reduce_dcmplx_6d

  subroutine send_int(this, buf, tag)
    class (MPI), intent(in) :: this
    integer, intent(in)     :: buf
    integer, intent(in)     :: tag

    call mpi_send(buf,1,MPI_INTEGER, ROOT, tag, MPI_COMM_WORLD, this%ierr)
    
  end subroutine send_int

  subroutine recv_int(this, buf, tag, rank)
    class (MPI), intent(in) :: this
    integer, intent(out)    :: buf
    integer, intent(in)     :: tag, rank
    
    call mpi_recv(buf,1,MPI_INTEGER,rank,tag,MPI_COMM_WORLD,this%status,this%ierr)

  end subroutine recv_int

  subroutine send_dcmplx_4d(this,buf,tag)
    class (MPI), intent(in)                      :: this
    complex (DP), dimension(:,:,:,:), intent(in) :: buf
    integer, intent(in)                          :: tag

    call mpi_send(buf,size(buf),MPI_DOUBLE_COMPLEX,ROOT,tag,MPI_COMM_WORLD,this%ierr)

  end subroutine send_dcmplx_4d

  subroutine recv_dcmplx_4d(this,buf,tag,rank)
    class (MPI), intent(in)                       :: this
    complex (DP), dimension(:,:,:,:), intent(out) :: buf
    integer, intent(in)                           :: tag, rank
    
    call mpi_recv(buf,size(buf),MPI_DOUBLE_COMPLEX,rank,tag,MPI_COMM_WORLD,this%status,this%ierr)
    
  end subroutine recv_dcmplx_4d
  ! 
  ! Constructor
  ! 
  subroutine create_single(this)
    class (MPI), pointer :: this
    integer              :: alloc_status
    
    allocate(this,stat=alloc_status)

    if ( alloc_status > 0 ) then
       print *, "unable to allocate MPI Object"
    end if
    
    call init_single(this)
    
  end subroutine create_single
  ! 
  ! Initialization
  ! 
  subroutine init_single(this)
    class (MPI), intent(inout) :: this    

    call mpi_init(this%ierr)
    call mpi_comm_size(MPI_COMM_WORLD, this%nproc, this%ierr)
    call mpi_comm_rank(MPI_COMM_WORLD, this%rank, this%ierr)
    
  end subroutine init_single
  ! 
  ! Release allocated memory resources
  ! 
  subroutine release_single(this)
    class (MPI), intent(inout) :: this
    
    call mpi_finalize(this%ierr)
    
  end subroutine release_single
  ! 
  ! Delete MPI pointer
  ! 
  subroutine destroy_single(this)
    class (MPI), pointer :: this
    
    if ( associated(this) ) then
       call release_single(this)
       deallocate(this)
       nullify(this)
    end if
    
  end subroutine destroy_single

end module MpiMod
