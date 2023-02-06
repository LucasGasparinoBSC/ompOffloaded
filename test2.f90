program test2
    implicit none
    integer(8)           :: n, i, j, iter, maxIter
    real(4), allocatable :: a(:,:), b(:,:), c(:,:)

    maxIter = 100
    n = 512
    allocate(a(n,n), b(n,n), c(n,n))

    !$omp target teams loop collapse(2)
    do i = 1,n
        do j = 1,n
            a(i,j) = 1.0
            b(i,j) = 2.0
            c(i,j) = 0.0
        end do
    end do
    !$omp end target teams loop

    do iter = 1,maxIter
        call matProd_omp(n,a,b,c)
    end do

    print *, "C(1,1) := ", c(1,1)
    deallocate(a,b,c)

end program test2

subroutine matProd_omp(n,A,B,C)
    implicit none
    integer(8), intent(in)  :: n
    real(4)   , intent(in)  :: A(n,n), B(n,n)
    real(4)   , intent(out) :: C(n,n)
    integer(8)              :: i, j, k
    real(4)                 :: aux

    !$omp target teams distribute parallel do simd
    do i = 1,n
        do j = 1,n
            C(i,j) = A(i,j)*B(i,j)
        end do
    end do
    !$omp end target teams distribute parallel do simd

end subroutine matProd_omp