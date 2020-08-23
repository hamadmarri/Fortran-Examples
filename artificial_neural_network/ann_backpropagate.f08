

subroutine backpropagate(layers, layers_config, NUM_LAYERS, expected_output, learningRate, momentum)
use ANN_types
implicit none
	integer, intent(in) :: NUM_LAYERS
	integer, dimension(NUM_LAYERS), intent(in) :: layers_config
	type(layer), dimension(NUM_LAYERS), intent(inout) :: layers
	real, dimension(layers_config(NUM_LAYERS)), intent(in) :: expected_output
	real, intent(in) :: learningRate
	real, intent(in) :: momentum
	integer i

	call backpropagate_output_layer(layers, layers_config, NUM_LAYERS, expected_output, learningRate, momentum)

	! Calculate error for all neurons in the hidden layers
	do i = NUM_LAYERS - 1, 2, -1
		call backpropagate_hidden_layer(layers, i, layers_config, NUM_LAYERS, learningRate, momentum)
	end do
end subroutine



subroutine backpropagate_output_layer(layers, layers_config, NUM_LAYERS, expected_output, learningRate, momentum)
use ANN_types
implicit none
	integer, intent(in) :: NUM_LAYERS
	integer, dimension(NUM_LAYERS), intent(in) :: layers_config
	type(layer), dimension(NUM_LAYERS), target, intent(inout) :: layers
	type(layer), pointer :: output_layer
	real, dimension(layers_config(NUM_LAYERS)), intent(in) :: expected_output
	real, intent(in) :: learningRate
	real, intent(in) :: momentum
	integer :: neurons_number
	type(neuron), pointer :: curr_neuron
	real, pointer :: curr_output
	integer i, j
	real :: derivativeOfSigmoid

	output_layer => layers(NUM_LAYERS)

	! get the number of neurons in output layer
	neurons_number = layers_config(NUM_LAYERS)

	do i = 1, neurons_number
		curr_neuron => output_layer % neurons(i)
		curr_output => curr_neuron % output

		! update error
		derivativeOfSigmoid = (expected_output(i) - curr_output) &
					* curr_output * (1 - curr_output)

		! set error
		curr_neuron % error = derivativeOfSigmoid

		! update delta threshold
		call update_threshold(output_layer % neurons(i), learningRate, momentum)

		! update input edges weights
		call update_edges_weight(layers(NUM_LAYERS - 1), output_layer % neurons(i), learningRate, momentum)

	end do
end subroutine



subroutine backpropagate_hidden_layer(layers, curr_layer_index, layers_config, NUM_LAYERS, learningRate, momentum)
use ANN_types
implicit none
	integer, intent(in) :: NUM_LAYERS
	integer, dimension(NUM_LAYERS), intent(in) :: layers_config
	type(layer), dimension(NUM_LAYERS), target, intent(inout) :: layers
	type(layer), pointer :: curr_hidden_layer, next_layer
	integer, intent(in) :: curr_layer_index
	real, intent(in) :: learningRate
	real, intent(in) :: momentum
	integer :: neurons_number
	type(neuron), pointer :: curr_neuron, right_neuron
	real, pointer :: curr_output
	integer i, j
	real :: error_sum

	curr_hidden_layer => layers(curr_layer_index)
	next_layer => layers(curr_layer_index + 1)

	! get the number of neurons in the current hidden layer
	neurons_number = layers_config(curr_layer_index)

	do i = 1, neurons_number
		curr_neuron => curr_hidden_layer % neurons(i)
		curr_output => curr_neuron % output

		! get erro sum from right connected neurons
		error_sum = 0.0
		do j = 1, layers_config(curr_layer_index + 1)
			right_neuron => next_layer % neurons(j)
			
			error_sum = error_sum + (right_neuron % left_edges_weights(i)) * (right_neuron % error)
		end do

		! update error
		curr_neuron % error = (error_sum * (curr_neuron % output) * (1 - (curr_neuron % error)))

		! update delta threshold
		call update_threshold(curr_hidden_layer % neurons(i), learningRate, momentum)

		! update input edges weights
		call update_edges_weight(layers(curr_layer_index - 1), curr_hidden_layer % neurons(i), learningRate, momentum)

	end do
end subroutine



subroutine update_threshold(curr_neuron, learningRate, momentum)
use ANN_types
implicit none
	type(neuron), intent(inout) :: curr_neuron
	real, intent(in) :: learningRate
	real, intent(in) :: momentum

	! update delta threshold
	curr_neuron % deltaThreshold = (learningRate * curr_neuron % error) + (momentum * curr_neuron % deltaThreshold)

	! update threshold
	curr_neuron % threshold = (curr_neuron % threshold) + (curr_neuron % deltaThreshold)

end subroutine



subroutine update_edges_weight(prev_layer, curr_neuron, learningRate, momentum)
use ANN_types
implicit none
	type(layer), intent(in) :: prev_layer
	type(neuron), intent(inout) :: curr_neuron
	real, intent(in) :: learningRate
	real, intent(in) :: momentum
	integer i, left_edges_number

	left_edges_number = size(curr_neuron % left_edges_weights)

	! update all left edges weights
	do i = 1, left_edges_number
		! update delta weight	
		curr_neuron % left_edges_delta_weights(i) = (learningRate * curr_neuron % error) &
				* (prev_layer % neurons(i) % output) &
				+ (momentum * (curr_neuron % left_edges_delta_weights(i)))
				
		! update weight
		curr_neuron % left_edges_weights(i) = (curr_neuron % left_edges_weights(i)) &
				+ (curr_neuron % left_edges_delta_weights(i))
				
	end do
end subroutine
