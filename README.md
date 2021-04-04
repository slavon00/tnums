# The tnums Library
## Introduction
Tnums (True Numbers) provide arbitrary precission computation of real numbers. It supports mathematical operations and functions of abstract strucutures `tnums` representing numbers.

After building a structure corresponding with the number, it can be represented as ratio using the function `tnum-to-num`. This does not coerce to floating numbers, but is as precise as you wish. The result can be used in other functions as a number, so that one does not need to copy-paste this.

## Usage
As said above, the computing of numbers is made in two steps:
1. Building a tnum (this is fast and lazy)
2. Computing a structure using 'tnum-to-num' (this can take a while, but will end)

### Creating tnum
There is sort of functions to create a tnum. You can use translation from lisp number using `num-to-tnum` or you can use constants such as `tnum-pi`, `tnum-e`, `tnum-phi`. Than you can use operation such as `tnum+`, `-tnum`, `tnum-`, `tnum*`, `/tnum`, `tnum/`, `tnum-expt`, `tnum-log` and `tnum-root`. Than there are functions of tnums such as `tnum-exp`, `tnum-ln`, `tnum-sin`, `tnum-cos`, `tnum-tan`, `tnum-ctan`, `tnum-sec`, `tnum-csc`, `tnum-sqrt`,  `tnum-1-` and `tnum-1+`. One special function is called `tnum*num` and it multiplies tnum and number.
#### Constants
For creating constant from **Lisp number** there is function `num-to-tnum`. This function also coerces floating-point numbers to ratio to be arbitrary precise.

    tnum of number x = (num-to-tnum x)

From mathematical constants there is **Ludolf's number** as function `tnum-pi`, **Euler's number** as function `tnum-e` and **Golden Ratio** as `tnum-phi`.

    pi = (tnum-pi)
    e = (tnum-e)
    phi = (tnum-phi)

#### Operation
All operations below are for tnums, so `x` and `y` in examples assumes tnums, not numbers. To translate numbers to tnums, use `num-to-tnum`.

**Addition** is provided by function `tnum+`. It takes 0+ tnums and returns tnum meaning the addition of tnums in arguments. Return tnum of 0 if none given.

    x+y=(tnum+ x y)

**Negation** of tnum is provided by function `-tnum`. It takes 1 tnum and returns -tnum.

    -x = (-tnum x)

**Substraction** is provided by function `tnum-`. It needs 1+ arguments and returns negation of one tnum or substract of more tnums.

    x-y = (tnum- x y)

**Multiplication** is provided by function `tnum*`. It takes 0+ arguments and returns tnum of 1 if none given, otherwise return tnum of the product of arguments.

    x*y = (tnum* x y)

**Reversion** of tnum is provided by funcion `/tnum`. For 1 given tnum returns 1/tnum.

    1/x = (/tnum x)

**Fraction** of tnums is implemented by function `tnum/`. It needs 1+ arguments and for one returns the reversion of this and for more their division.
    
    x/y = (tnum/ x y)

**Exponentiation** of two tnums is provided by function `tnum-expt`. It takes exactly 2 tnums and returns tnum1^tnum2.

    x^y = (tnum-expt x y)

**Logarithm** of two tnums is provided by `tnum-log`. It takes exactly 2 tnums and first argument is the base and second is argument of algorithm.

    log_x(y) = (tnum-log x y)

**Extraction** of two tnums is provided by `tnum-root`. It takes exactly two arguments and first is the argument and second is degree of root.

    x^(1/y) = (tnum-root x y)

#### Functions
All functions take exactly one argument.
**Natural exponentiation** is provided by function `tnum-exp`.

    e^x = (tnum-exp x)

**Natural logarithm** is provided by `tnum-ln`.

    ln(x) = (tnum-ln x)

**Incrementation by 1** is provided by `tnum-1+`.
    
    x+1 = (tnum-1+ x)

**Decrementation by 1** is provided by `tnum-1-`.

    x-1 = (tnum-1- x)

**Square root** is provided by `tnum-sqrt`.

    x^(1/2) = (tnum-sqrt x)

**Goniometric functions** are provided by `tnum-sin`, `tnum-cos`, `tnum-tan`, `tnum-ctan`, `tnum-sec` and `tnum-csc`.

    sine(x) = (tnum-sin x)
    cosine(x) = (tnum-cos x)
    tangent(x) = (tnum-tan x)
    cotangent(x) = (tnum-ctan x)
    secant(x) = (tnum-sec x)
    cosecant(x) = (tnum-cos x)

### Calculation tnum
To calculate tnum with precission, call `tnum-to-num` with tnum and presission. The precission eps should be 0<eps<=1. The non-positive value of eps is meant as 10^eps.

    Value of x at precission eps = (tnum-to-num x eps)

The function returns ratio as that is the only precise representation value with many decimal places. If you want something unprecise but readable by human, use `tnum-to-float` which returns tnum calculated with precission coerced to `float`.

    Long-float of x at precission eps = (tnum-to-float x eps)

## Examples
Tnums do not always return exact values, but you can use the precission to make them as exact as you need. This means the arbitrary precission. So |(tnum-to-num T(x) eps) -x | < eps, denoting T(x) as a tnum representating x.

    e^pi at precission 0.001 = (tnum-to-num (tnum-expt (tnum-e) (tnum-pi)) 0.001)
or

    (tnum-to-num (tnum-exp (tnum-pi)) -3).

If you use tnum-to-num with non-positive argument -n, you get n-1 decimal places (if the last is not 0 or 9).

    First 7 decimal places of pi = (tnum-to-float (tnum-pi) -8)

But tnums are strong in returning `ratio`s. So you can use the results in other applications. You get the precise number.

    First 15 decimal places of phi = (coerce (tnum-to-num (tnum-phi) -16) 'long-float)

And if you wish, you can build a tnum and evaluate it afterwards.

    * (tnum-csc (tnum*num (tnum-pi) (/ 2 3)))
    #<FUNCTION (LAMBDA (EPS) :IN /TNUM) {100212091B}>
    * (tnum-to-num * -100)
    ...VERY LONG RATIO...

Note that tnums library is not meant to be a calculator. Of course there are ways to do so, but I do not feel about it and rather build and improve this library so the user can write his own functions to work with precise numbers. Or anybody can write a calculator or other programs based on tnums and share it with others and this would be much better than creating a calculator from tnums.

The usage of all operations follows.
### Adition
    * (tnum-to-num (tnum+) -20)
    0
    * (tnum-to-num (tnum+ (num-to-tnum -200)) -20)
    -200
    * (tnum-to-num (tnum+ (num-to-tnum 6/5) (num-to-tnum 299/3)) -20)
    1513/15
    * (coerce (tnum-to-num (tnum+ (tnum-pi) (tnum-e) (tnum-phi) (num-to-tnum 1)) -20) 'long-float)
    8.477908470798733d0

### Negation
    * (princ (tnum-to-num (-tnum (tnum-pi)) -5))
    -16071212445820879/5115625817702400 
    -16071212445820879/5115625817702400

### Substraction
    * (tnum-to-num (tnum- (num-to-tnum -200)) -20)
    200
    * (tnum-to-num (tnum- (num-to-tnum 6/5) (num-to-tnum 299/3)) -20)
    -1477/15
    * (coerce (tnum-to-num (tnum- (tnum-pi) (tnum-e) (tnum-phi) (num-to-tnum 1)) -20) 'long-float)
    -2.194723163619147d0

### Multiplication
    * (tnum-to-num (tnum*) -20)
    1
    * (tnum-to-num (tnum* (num-to-tnum -200)) -20)
    -200
    * (tnum-to-num (tnum* (num-to-tnum 6/5) (num-to-tnum 299/3)) -20)
    598/5
    * (coerce (tnum-to-num (tnum* (tnum-pi) (tnum-e) (tnum-phi) (num-to-tnum 1)) -20) 'long-float)
    13.817580227176494d0

### Reversion
    * (tnum-to-num (/tnum (tnum-e)) -20)
    25545471085854720000/69439789852104840011
    * (float *)
    0.36787945

### Division
    * (tnum-to-num (tnum/ (num-to-tnum -200)) -20)
    -1/200
    * (tnum-to-num (tnum/ (num-to-tnum 6/5) (num-to-tnum 299/3)) -20)
    18/1495
    * (coerce (tnum-to-num (tnum/ (tnum-pi) (tnum-e) (tnum-phi) (num-to-tnum 1)) -20) 'long-float)
    0.7142787838986283d0

### Exponentiation
    * (setq r (num-to-tnum 100))
    #<FUNCTION (LAMBDA (EPS) :IN NUM-TO-TNUM) {10023D570B}>
    * (tnum-to-float (tnum* (tnum-pi) (tnum-expt r (num-to-tnum 2))) -7)
    31415.926
    * (tnum-to-float (tnum-expt r (tnum-pi)) -7)
    1919487.6

### Logarithm
    * (tnum-to-float (tnum-log (num-to-tnum 2) (num-to-tnum 16)) -20)
    4.0
    * (tnum-to-num (tnum-log (tnum-phi) (tnum-phi)) -20)
    ...VERY VERY LONG RATIO...
    * (* 1.0d0 *)
    1.0d0
    * (tnum-to-float (tnum-log (tnum-pi) (tnum-e)) -20)
    0.87356853

### Root
    * (tnum-to-float (tnum-root (num-to-tnum 4) (num-to-tnum 16)) -20)
    2.0
    * (tnum-to-float (tnum-root (tnum-pi) (tnum-expt (tnum-e) (num-to-tnum 2))) -7)
    1.8900812

And functions.

### Exponential
    * (tnum-to-float (tnum-exp (num-to-tnum 2.1)) -20)
    8.16617
    * (tnum-to-float (tnum-exp (tnum-phi)) -20)
    5.0431657

### NatLog
    * (tnum-to-float (tnum-ln (num-to-tnum 10)) -20)
    2.3025851
    * (coerce (tnum-to-num (tnum-ln (num-to-tnum 2)) -20) 'long-float)
    0.6931471805599453d0

### Incrementation
    * (setq n (num-to-tnum 1000000))
    #<FUNCTION (LAMBDA (EPS) :IN NUM-TO-TNUM) {10041B488B}>
    * (tnum-to-float (tnum-expt (tnum-1+ (/tnum n)) n) -20)
    2.7182806

### Decrementation
    * (tnum-to-float (tnum-1- (tnum-1- (tnum-1- (tnum-pi)))) -20)
    0.14159265

### Square root
    * (coerce (tnum-to-num (tnum-sqrt (num-to-tnum 2)) -20) 'long-float)
    1.4142135623730951d0
    * (tnum-to-num (tnum-sqrt (num-to-tnum 2)) -4)
    194363389812315075599657859994834246/137435994817044508577304502085109375

### Sine
    * (tnum-to-float (tnum-sin (num-to-tnum (/ 2))) -20)
    0.47942555

### Cosine
    * (tnum-to-float (tnum-cos (num-to-tnum (/ 2))) -20)
    0.87758255

### Tangent
    * (tnum-to-float (tnum-tan (num-to-tnum 1)) -20)
    1.5574077

### Cotangent
    * (tnum-to-float (tnum-ctan (num-to-tnum 1)) -20)
    0.64209265

### Secant
    * (tnum-to-float (tnum-sec (tnum-pi)) -20)
    -1.0

### Cosecant
    * (tnum-to-float (tnum-csc (tnum- (tnum-pi) (num-to-tnum .000001))) -7)
    1000000.0
