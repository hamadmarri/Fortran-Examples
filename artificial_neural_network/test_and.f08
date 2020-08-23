subroutine test(layers, layers_config, NUM_LAYERS)
use ANN_types
implicit none
	integer, intent(in) :: NUM_LAYERS
	integer, dimension(NUM_LAYERS), intent(in) :: layers_config
	type(layer), dimension(NUM_LAYERS), intent(inout) :: layers
	integer i, a, b
	
	do i = 1, 5
		print *, "test: "
		read (*,*), a, b
		
		call set_input(layers(1), (/ real(a), real(b) /), layers_config(1))
		call feedForward(layers, layers_config, NUM_LAYERS)
		
		print *, "output:", nint(layers(NUM_LAYERS) % neurons(1) % output)
	end do
	
end subroutine
