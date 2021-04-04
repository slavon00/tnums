# The tnums Library
## Introduction
Tnums (True Numbers) provide arbitrary precission computation of real numbers. It supports mathematical operations and functions of abstract strucutures 'tnums' representing numbers.

After building a structure corresponding with the number, it can be represented as ratio using the function 'tnum-to-num'. This does not coerce to floating numbers, but is as precise as you wish. The result can be used in other functions as a number, so that one does not need to copy-paste this.

## Usage
As said above, the computing of numbers is made in two steps:
1. Building a tnum (this is fast and lazy)
2. Computing a structure using 'tnum-to-num' (this can take a while, but will end)

### Creating tnum
There is sort of functions to create a tnum. You can use translation from lisp number using 'num-to-tnum' or you can use constants such as 'tnum-pi', 'tnum-e', 'tnum-phi' meaning Ludolf`s number, Euler`s number and Golden Ratio. Than you can use operation such as 'tnum+', '-tnum', 'tnum-', 'tnum*', '/tnum', 'tnum/', 'tnum-expt', 'tnum-log' and 'tnum-root'. Than there are functions of tnums such as 'tnum-exp', 'tnum-ln', 'tnum-sin', 'tnum-cos', 'tnum-tan', 'tnum-ctan', 'tnum-sec', 'tnum-csc', 'tnum-sqrt', 'tnum-1-' and 'tnum-1+'.
#### Constants
#### Operation
#### Function

### Calculation tnum