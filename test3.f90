program test3
    implicit none
    integer(8)              :: nelem, nnode, npoin, niter
    integer(8)              :: ielem, inode, ipoin, iter
    integer(8), allocatable :: table(:,:)
    real(4)   , allocatable :: u(:), R(:), R_ACC(:), R_OMP(:)
    real(8)   , parameter   :: pi=3.141592653589793238462643383279502884197169399375105820974944592307816406286d0

    niter = 100
    nelem = 10
    nnode = 64
    npoin = nelem*nnode
    allocate(u(npoin), R(npoin),R_ACC(npoin),R_OMP(npoin))
    allocate(table(nelem,nnode))

    !$acc parallel loop collapse(2)
    do ielem = 1,nelem
        do inode = 1,nnode
            table(ielem,inode) = (ielem-1)*nnode+inode
        end do
    end do
    !$acc end parallel loop

    !$acc parallel loop
    do ipoin = 1,npoin
        u(ipoin) = sin(2.0*pi*real(ipoin,kind=4)/real(npoin,kind=4))
        R(ipoin) = 0.0
        R_ACC(ipoin) = 0.0
        R_OMP(ipoin) = 0.0
    end do
    !$acc end parallel loop

    do iter = 1,niter
        call elemOps(nelem,nnode,npoin,table,u,R)
    end do
    do iter = 1,niter
        call elemOps_ACC(nelem,nnode,npoin,table,u,R_ACC)
    end do
    do iter = 1,niter
        call elemOps_OMP(nelem,nnode,npoin,table,u,R_OMP)
    end do

    write(*,*) "ipoin  |  R(ipoin)  |  R_ACC(ipoin)  |  R_OMP(ipoin)"
    do ipoin = 1,npoin
        write(*,*) ipoin, R(ipoin), R_ACC(ipoin), R_OMP(ipoin)
    end do

    deallocate(u, R, table)

end program test3

subroutine elemOps(nelem,nnode,npoin,table,u,R)
    implicit none
    integer(8), intent(in)                          :: nelem, nnode, npoin
    integer(8), intent(in) , dimension(nelem,nnode) :: table
    real(4)   , intent(in) , dimension(npoin)       :: u
    real(4)   , intent(out), dimension(npoin)       :: R
    integer(8)                                      :: ielem, inode, ipoin
    real(4)                , dimension(nnode)       :: u_shared
    real(4)                                         :: aux
    do ielem = 1, nelem
        do inode = 1,nnode
            u_shared(inode) = u(table(ielem,inode))
        end do
        aux = dot_product(u_shared,u_shared)
        do inode = 1,nnode
            R(table(ielem,inode)) = aux
        end do
    end do
end subroutine elemOps

subroutine elemOps_ACC(nelem,nnode,npoin,table,u,R)
    implicit none
    integer(8), intent(in)                          :: nelem, nnode, npoin
    integer(8), intent(in) , dimension(nelem,nnode) :: table
    real(4)   , intent(in) , dimension(npoin)       :: u
    real(4)   , intent(out), dimension(npoin)       :: R
    integer(8)                                      :: ielem, inode, ipoin
    real(4)                , dimension(nnode)       :: u_shared
    real(4)                                         :: aux
    !$acc parallel loop gang private(u_shared,aux)
    do ielem = 1, nelem
        !$acc loop vector
        do inode = 1,nnode
            u_shared(inode) = u(table(ielem,inode))
        end do
        aux = dot_product(u_shared,u_shared)
        !$acc loop vector
        do inode = 1,nnode
            R(table(ielem,inode)) = aux
        end do
    end do
    !$acc end parallel loop
end subroutine elemOps_ACC

subroutine elemOps_OMP(nelem,nnode,npoin,table,u,R)
    implicit none
    integer(8), intent(in)                          :: nelem, nnode, npoin
    integer(8), intent(in) , dimension(nelem,nnode) :: table
    real(4)   , intent(in) , dimension(npoin)       :: u
    real(4)   , intent(out), dimension(npoin)       :: R
    integer(8)                                      :: ielem, inode, ipoin
    real(4)                , dimension(nnode)       :: u_shared
    real(4)                                         :: aux
    !$omp target teams distribute private(u_shared,aux)
    do ielem = 1, nelem
        !$omp parallel do simd
        do inode = 1,nnode
            u_shared(inode) = u(table(ielem,inode))
        end do
        aux = dot_product(u_shared,u_shared)
        !$omp parallel do simd
        do inode = 1,nnode
            R(table(ielem,inode)) = aux
        end do
    end do
    !$omp end target teams distribute
end subroutine elemOps_OMP
