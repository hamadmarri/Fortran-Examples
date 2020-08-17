program test
implicit none

	integer, dimension(10) :: marks	! array of 1 dimansion of size 10 elements
	integer :: i				! used for do loop
	integer :: time				! used to get unix time for seed
	integer, dimension(2) :: seed		! seed should be 1D array of 2 elements
	real :: num				! float variable used to hold float values
	real :: average				! a declaration of average function in array_operation.f95
	integer :: maximum, minimum		! to hold the maximum and minimum marks

	! set the seed array
	seed = (/time(), time()/)

	! set the seed for random (== C, srand(seed))
	call random_seed(PUT=seed)

	! for loop from i == 1 to i == 10 (inclusive)
	do i=1,10
		! get random number and set num to it
		! num would be in the form 0.#####
		call random_number(num)

		! multiply num by 100, then convert it to integer, then
		! set marks[i] to it
		marks(i) = nint(num * 100)
	end do

	! calling a subroutine print_array
	call print_array(marks)

	! format:
	!	1/	means insert 1 blank line
	!	A	means print string
	!	F4.1	means format float where the number of chars size is 4 ("35.1" is 4 chars)
	!		and show 1 decimal point
	print '(1/A, F4.1)', "average: ", average(marks)

	! calling subroutine in array_operation
	! maximum and minimum are passed by reference since intent(out)
	call max_and_min(marks, maximum, minimum)

	print *, "Max: ", maximum
	print *, "Min: ", minimum


end program test

