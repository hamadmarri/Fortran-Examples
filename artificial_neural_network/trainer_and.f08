
subroutine train(layers, layers_config, NUM_LAYERS, learningRate, momentum)
use ANN_types
implicit none
	integer, intent(in) :: NUM_LAYERS
	integer, dimension(NUM_LAYERS), intent(in) :: layers_config
	type(layer), dimension(NUM_LAYERS), intent(inout) :: layers
	real, intent(in) :: learningRate
	real, intent(in) :: momentum
	integer a, b, c, i, iterations
	
	iterations = 50000
		
	do i = 1, iterations
		do a = 0, 1
			do b = 0, 1
				c = a .and. b
				! set input
				call set_input(layers(1), (/ real(a), real(b) /), layers_config(1))
				call feedForward(layers, layers_config, NUM_LAYERS)
				call backpropagate(layers, layers_config, NUM_LAYERS, (/ real(c) /), learningRate, momentum)
				
!				print 100, "expected", c, "output", layers(NUM_LAYERS)%neurons(1)%output, &
!					"error", layers(NUM_LAYERS)%neurons(1)%error
			end do
		end do
	end do
	
!	100 format(A, I2, 5xA, F20.15, 5xA, F20.15)
	
end subroutine
