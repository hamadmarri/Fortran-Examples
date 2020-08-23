program ANN
use ANN_types
implicit none

	integer, parameter :: NUM_LAYERS = 3
	integer, dimension(NUM_LAYERS) :: layers_config = (/ 2, 3, 1 /)
	real :: learningRate
	real :: momentum
	type(layer), dimension(NUM_LAYERS) :: layers
	
	learningRate = 0.1
	momentum = 0.15

	call initialize_ANN(layers, layers_config, NUM_LAYERS)
	!		call print_ANN(layers, layers_config, NUM_LAYERS)

	call print_ANN(layers, layers_config, NUM_LAYERS)
	call train(layers, layers_config, NUM_LAYERS, learningRate, momentum)
	call print_ANN(layers, layers_config, NUM_LAYERS)
	
!	call test(layers, layers_config, NUM_LAYERS)

end program ANN

! bais in output layer need to be removed

