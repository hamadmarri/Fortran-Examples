
!print array marks
subroutine print_array(marks, array_size)
implicit none

	integer, intent(in) :: array_size
	integer, intent(in), dimension(array_size) :: marks
	integer :: i

	do i = 1,array_size
		print *, i, ": ", marks(i)
	end do
end subroutine


!return average of marks
function average(marks, array_size) result(ave)
implicit none
	
	integer, intent(in) :: array_size
	integer, intent(in), dimension(array_size) :: marks
	real :: ave

	ave = sum(marks) / real(array_size)
	
end function

