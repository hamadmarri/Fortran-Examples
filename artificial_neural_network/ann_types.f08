module ANN_types
implicit none
	type neuron
		real :: threshold
		real :: output
		real :: error
		real :: deltaThreshold = 0.0
		real, dimension(:), allocatable :: left_edges_weights
		real, dimension(:), allocatable :: left_edges_delta_weights
	end type neuron
	
	type layer
		type(neuron), dimension(:), allocatable :: neurons
	end type layer
end module ANN_types
