# Description

This example is to show how to use open omp to use parallel computation on calculating prime numbers.
On my machine, it takes `1m6s` without parallel, and `29s` with parallel threads.

## What is covered in this example

* !$omp parallel do

## Compile

```
# compile
flang main.f08 -fopenmp

# run
time ./a.out

```
