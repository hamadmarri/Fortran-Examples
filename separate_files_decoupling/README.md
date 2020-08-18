# Description
This example is to show you how to decouple your code in two or more separate files. Initiate 1D array with
10 elements, and fill the array with random integers. Get average, max, and min.


## What is covered in this example
* Array 1D
* Random numbers
* Do loop
* Subroutines
* Functions
* Dealing with one main file and another separate helper file for array operations
* Converting from real to int (`nint`)

## Compile

```
# compile object files separately
flang -c main.f08
flang -c array_operation.f08

# link
flang main.o array_operation.o

# run
./a.out

```

or

```
# compile all files at once
flang main.f08 array_operation.f08

# run
./a.out

```
