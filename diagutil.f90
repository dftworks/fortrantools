Module DiagUtilMod
  use TypesMod
  use ConstantsMod

  implicit none

  private

  public :: diagonalize
  
  interface diagonalize
     module procedure diag_complex
     module procedure diag_complex_zhpevx
     module procedure diag_complex_eigvec
  end interface

  ! 
  ! Interface for external BLAS/LAPACK subroutine
  ! 
  interface
     subroutine zgemm(transa,transb,m,n,k,alpha,a,lda,b,ldb,beta,c,ldc)
       character (len=*), intent(in)                   :: transa
       character (len=*), intent(in)                   :: transb
       integer, intent(in)                             :: m
       integer, intent(in)                             :: n
       integer, intent(in)                             :: k
       double complex, intent(in)                      :: alpha
       double complex, dimension(lda,*), intent(in)    :: a
       integer, intent(in)                             :: lda
       double complex, dimension(ldb,*), intent(in)    :: b
       integer, intent(in)                             :: ldb
       double complex, intent(in)                      :: beta
       double complex, dimension(ldc,*), intent(inout) :: c
       integer, intent(in)                             :: ldc
     end subroutine zgemm
  end interface
  
contains

  subroutine diag_complex_zhpevx(Hk,eigen,eigvec,nbnd)
    complex (DP), dimension(:,:), intent(in)  :: Hk
    real (DP), dimension(:), intent(out)      :: eigen
    complex (DP), dimension(:,:), intent(out) :: eigvec
    integer                                   :: nbnd

    integer                                   :: neig, info, ifail( nbnd ), iwork( 5*nbnd )
    real(DP)                                  :: w( nbnd ), rwork( 7*nbnd )
    complex(DP)                               :: champ( nbnd*(nbnd+1)/2 )
    complex(DP)                               :: cwork( 2*nbnd )
    complex(DP)                               :: cz( nbnd, nbnd)
    

    integer                                   :: ibnd,jbnd

    
    do jbnd = 1, nbnd
       do ibnd = 1, jbnd
          champ (ibnd + (jbnd - 1) * jbnd/2 ) = &
               ( Hk ( ibnd, jbnd) + conjg ( Hk ( jbnd, ibnd) ) ) / 2.d0
       end do
    end do
  
    call zhpevx('V','A','U',nbnd,champ, 0.0, 0.0, &
         0, 0, -1.0, neig, w, cz, nbnd, cwork, &
         rwork, iwork, ifail, info)

    eigen = w
    eigvec = cz
    
  end subroutine diag_complex_zhpevx

  subroutine diag_complex(Hk,eigen)
    complex (DP), dimension(:,:), intent(in)  :: Hk
    real (DP), dimension(:), intent(out)      :: eigen

    complex (DP), dimension(:,:), allocatable :: Haux, Saux
    integer                                   :: lwork, lrwork, liwork, info
    real (DP), dimension(:), allocatable      :: work, rwork
    integer, dimension(:), allocatable        :: iwork
    complex (DP), dimension(:,:), allocatable :: Sk

    integer                                   :: no_u, i

    no_u = size(Hk,1)

    allocate(Sk(no_u,no_u))

    Sk = 0.0_dp

    do i=1,no_u
       Sk(i,i) = 1.0_dp
    end do

    lwork  = max(no_u+1,2*no_u-1)
    lrwork = max(no_u,3*no_u-2)
    liwork = no_u

    allocate(work(2*lwork))
    allocate(rwork(lrwork))
    allocate(iwork(liwork))

    allocate(Haux(no_u,no_u))
    allocate(Saux(no_u,no_u))

    Haux = Hk
    Saux = Sk
    
    call zhegvd(1,"N","U",no_u,Haux,no_u,Saux,no_u,eigen,work,lwork, &
            rwork,lrwork,iwork,liwork,info)
    
    if (info /= 0) then
       print *, "info: ", info, size(eigen)
       stop
    end if

    deallocate(Sk)
    deallocate(Haux)
    deallocate(Saux)
    deallocate(work)
    deallocate(rwork)
    deallocate(iwork)

  end subroutine diag_complex

  subroutine diag_complex_eigvec(Hk,eigen,eigvec)
    complex (DP), dimension(:,:), intent(in)  :: Hk
    real (DP), dimension(:), intent(out)      :: eigen
    complex (DP), dimension(:,:), intent(out) :: eigvec

    complex (DP), dimension(:,:), allocatable :: Haux, Saux
    integer                                   :: lwork, lrwork, liwork, info
    real (DP), dimension(:), allocatable      :: work, rwork
    integer, dimension(:), allocatable        :: iwork
    complex (DP), dimension(:,:), allocatable :: Sk

    integer                                   :: no_u, i

    no_u = size(Hk,1)

    allocate(Sk(no_u,no_u))

    Sk=0.0_dp

    do i=1,no_u
       Sk(i,i) = 1.0_dp
    end do

    lwork  = 2*no_u + no_u*no_u
    lrwork = 1 + 5*no_u + 2*no_u*no_u
    liwork = 3 + 5*no_u

    allocate(work(2*lwork))
    allocate(rwork(lrwork))
    allocate(iwork(liwork))

    allocate(Haux(no_u,no_u))
    allocate(Saux(no_u,no_u))

    Haux = Hk
    Saux = Sk
    
    call zhegvd(1,"V","L",no_u,Haux,no_u,Saux,no_u,eigen,work,lwork, &
         rwork,lrwork,iwork,liwork,info)

    eigvec = Haux
    
    if (info /= 0) then
       print *, "info: ", info, size(eigen)
       stop
    end if

    deallocate(Sk)
    deallocate(Haux)
    deallocate(Saux)
    deallocate(work)
    deallocate(rwork)
    deallocate(iwork)

    call mkl_free_buffers()
    
  end subroutine diag_complex_eigvec

end Module DiagUtilMod
