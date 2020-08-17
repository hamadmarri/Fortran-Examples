
!print array marks
subroutine print_array(marks)
implicit none
	integer, intent(in), dimension(10) :: marks
	integer :: i

	do i=1,10
		print *, i, ": ", marks(i)
	end do
end subroutine


!return average of marks
function average(marks) result(ave)
implicit none

	integer, intent(in), dimension(10) :: marks
	real :: ave
	integer :: i
	real :: sum = 0.0

	do i=1,10
		sum = sum + marks(i)
	end do

	ave = sum / 10
end function


subroutine max_and_min(marks, maximum, minimum)
implicit none
	integer, intent(in), dimension(10) :: marks
	integer, intent(out) :: maximum
	integer, intent(out) :: minimum
	integer :: i

	maximum = marks(1)
	minimum = marks(1)

	do i=2,10
		if (marks(i) > maximum) then
			maximum = marks(i)
		end if

		if (marks(i) < minimum) then
			minimum = marks(i)
		end if
	end do

end subroutine
