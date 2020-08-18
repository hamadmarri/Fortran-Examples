program calc_prime
implicit none

	integer :: i, j

	! try comment out !$omp parallel do and !$omp end parallel do
	! then recompile, and compare the execution time
!$omp parallel do
numbers:do i = 980000,1000000
		do j = i - 1, 2, -1
			if ( mod(i, j) == 0 ) then
				cycle numbers
			end if
		end do
		
		print *, i, "is prime"
	end do numbers
!$omp end parallel do


end program calc_prime
