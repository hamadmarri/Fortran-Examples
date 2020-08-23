
subroutine feedForward(layers, layers_config, NUM_LAYERS)
use ANN_types
implicit none

	integer, intent(in) :: NUM_LAYERS
	integer, dimension(NUM_LAYERS), intent(in) :: layers_config
	type(layer), dimension(NUM_LAYERS), target, intent(inout) :: layers
	type(layer), pointer :: curr_layer, prev_layer
	type(neuron), pointer :: curr_neuron
	integer i, j, k, neurons_size, left_edges_size
	real :: net
	real :: sigmoid
	
	do i = 2, NUM_LAYERS
		curr_layer => layers(i)
		prev_layer => layers(i - 1)
		neurons_size = layers_config(i) + 1
		
		! - 1 so bias neuron doesn't need to calculate its output
		do j = 1, neurons_size - 1
			curr_neuron => curr_layer % neurons(j)
			left_edges_size = layers_config(i - 1) + 1
			net = curr_neuron % threshold
			
			do k = 1, left_edges_size
				net = net + &
					(prev_layer % neurons(k) % output &
					* curr_neuron % left_edges_weights(k))
					
			end do

			curr_neuron % output = sigmoid(net)
		end do
	end do
	
	
end subroutine


function sigmoid(net) result(_sigmoid)
implicit none
	real, intent(in) :: net
	real, _sigmoid
	
	_sigmoid = 1.0 / (1.0 + exp(-net))
	
end function


subroutine set_input(input_layer, input, input_size)
use ANN_types
implicit none
	integer, intent(in) :: input_size
	type(layer), intent(inout) :: input_layer
	real, dimension(input_size), intent(in) :: input
	integer i
	
	do i = 1, input_size
		input_layer % neurons(i) % output = input(i)
	end do
	
end subroutine
