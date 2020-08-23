subroutine set_random_weights(left_edges_weights, num_left_edges)
use ANN_types
implicit none
	integer, intent(in) :: num_left_edges
	real, dimension(num_left_edges), intent(inout) :: left_edges_weights
	integer :: i
	real :: num
	
	do i = 1, num_left_edges
		! get random number and set num to it
		! num would be in the form 0.#####
		call random_number(num)
		
		left_edges_weights(i) = -1 + (2 * num)
	end do
	
end subroutine


subroutine initialize_left_edges(alayer, layers_config_curr, layers_config_prev)
use ANN_types
implicit none
	type(layer), intent(inout) :: alayer
	integer, intent(in) :: layers_config_curr
	integer, intent(in) :: layers_config_prev
	integer :: i
	
	do i = 1, layers_config_curr
		allocate(alayer % neurons(i) % left_edges_weights(layers_config_prev + 1))
		allocate(alayer % neurons(i) % left_edges_delta_weights(layers_config_prev + 1))
		
		! set left_edges_weights
		call set_random_weights(alayer % neurons(i) % left_edges_weights, layers_config_prev + 1)
		
		! set delta weights to zero
		alayer % neurons(i) % left_edges_delta_weights = 0.0
	end do
	
end subroutine


subroutine initialize_seed()
implicit none
	integer :: time					! used to get unix time for seed
	integer, dimension(2) :: seed			! seed should be 1D array of 2 elements
	
	! set the seed array
	seed = (/time(), time()/)

	! set the seed for random (== C, srand(seed))
	call random_seed(PUT=seed)
end subroutine


subroutine initialize_ANN(layers, layers_config, NUM_LAYERS)
use ANN_types
implicit none
	integer, intent(in) :: NUM_LAYERS
	integer, dimension(NUM_LAYERS), intent(in) :: layers_config
	type(layer), dimension(NUM_LAYERS), intent(inout) :: layers
	integer :: i, j
	real :: num
	
	call initialize_seed()
	
	do i = 1, NUM_LAYERS
		! allocate neurons size
		allocate(layers(i) % neurons(layers_config(i) + 1))
		
		! initialize threshold neurons with a random
		! number between -1 and 1
		do j = 1, layers_config(i) + 1
			call random_number(num)
			layers(i) % neurons(j) % threshold = -1 + (2 * num)
		end do
		
		! make the bias neuron's output = 1
		layers(i) % neurons(layers_config(i) + 1) % output = 1.0
		
		! no left edges for input layer
		if (i == 1) cycle
		
		! initialize left_edges_weights size
		call initialize_left_edges(layers(i), layers_config(i), layers_config(i - 1))
	end do

	
end subroutine


subroutine print_ANN(layers, layers_config, NUM_LAYERS)
use ANN_types
implicit none
	integer, intent(in) :: NUM_LAYERS
	integer, dimension(NUM_LAYERS), intent(in) :: layers_config
	type(layer), dimension(NUM_LAYERS), intent(inout) :: layers
	integer :: i, j, k
	
!	print *, layers_config
	print *, "----------- ANN ------------"
	
	do i = 2, NUM_LAYERS
		if (allocated(layers(i) % neurons) == .false.) then
			print *, "erroorrr"
		end if
		
		print *, "LAYER", i
		
		do j = 1, layers_config(i) + 1
			print *, layers(i) % neurons(j) % left_edges_weights
		end do
	end do
	
end subroutine

