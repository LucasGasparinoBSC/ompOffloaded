program test1
    implicit none
    integer(8)           :: n, i, iter, maxIter
    real(4), allocatable :: a(:), b(:), c(:)

    maxIter = 100
    n = 512*512
    allocate(a(n), b(n), c(n))

    !$omp target teams distribute parallel do
    do i = 1,n
        a(i) = 1.0
        b(i) = 2.0
        c(i) = 0.0
    end do
    !$omp end target teams distribute parallel do

    do iter = 1,maxIter
        call vecAdd_omp(n,a,b,c)
    end do

    print *, "sum(c) := ", sum(c)
    deallocate(a,b,c)

end program test1

subroutine vecAdd_omp(n,a,b,c)
    implicit none
    integer(8), intent(in)  :: n
    real(4)   , intent(in)  :: a(n), b(n)
    real(4)   , intent(out) :: c(n)
    integer(8)              :: i

    ! GPU offloaded omp parallel loop
    !$omp target loop
    do i = 1,n
        c(i) = a(i) + b(i)
    end do
    !$omp end target loop
    
end subroutine vecAdd_omp