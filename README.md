# The tnums Library
## Introduction
Tnums (True Numbers) provide arbitrary precission computation of real numbers. It supports mathematical operations and functions of abstract strucutures `tnums` representing numbers.

After building a structure corresponding with the number, it can be represented as ratio using the function `tnum-to-num`. This does not coerce to floating numbers, but is as precise as you wish. The result can be used in other functions as a number, so that one does not need to copy-paste this.

## Running
Open you favourite LISP compilator and evaluate file `load.lisp`. For example in my SBCL I can navigate to the project folder and type

    (load "load.lisp")

and that is it. In GUI editors with in-built interpreteurs (LispWorks etc.) you can just open the file and evaluate it by clicking.

## Usage
As said above, the computing of numbers is made in two steps:
1. Building a tnum (this is fast and lazy)
2. Computing a structure using 'tnum-to-num' (this can take a while, but will end)

### Creating tnum
There is sort of functions to create a tnum. You can use translation from lisp number using `num-to-tnum` or you can use constants such as `tnum-pi`, `tnum-e`, `tnum-phi`. Than you can use operation such as `tnum+`, `-tnum`, `tnum-`, `tnum*`, `/tnum`, `tnum/`, `tnum-expt`, `tnum-log` and `tnum-root`. Than there are functions of tnums such as `tnum-exp`, `tnum-ln`, `tnum-sin`, `tnum-cos`, `tnum-tan`, `tnum-ctan`, `tnum-sec`, `tnum-csc`, `tnum-sqrt`,  `tnum-1-` and `tnum-1+`.

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

### Calculating tnum
#### As number
To calculate tnum with precission, call `tnum-to-num` with tnum and presission.

    Value of T^x at precission eps = (tnum-to-num T^x eps)

The relationship between number `x` and its tnum `T^x` is as follows: |(tnum-to-num T^x eps) - x| < eps. The precission eps should be 0<eps<=1. The other value of eps is meant as 10^(-|eps|).

#### As string
To get value of tnum in string, call `tnum-to-string` with tnum and count of decimal places.

    Value of T^x as a string output with n decimal places = (tnum-to-string T^x n)

## Examples
Tnums do not always return exact values, but you can use the precission to make them as exact as you need. This means the arbitrary precission. So |(tnum-to-num T^x eps) -x | < eps, denoting T^x as a tnum representating x.

    e^pi at precission 0.001 = (tnum-to-num (tnum-expt (tnum-e) (tnum-pi)) 0.001)
or in shorter form

    (tnum-to-num (tnum-exp (tnum-pi)) 3).

Tnums are strong in returning `ratio`s. So you can use the results in other applications. You get the precise number.

    First 15 decimal places of phi = (coerce (tnum-to-num (tnum-phi) -16) 'long-float)

Or if you do not need to use output as a number, but for example to print it to the file, you can use tnum-to-string.

    (tnum-to-string (tnum-pi) 1000)

And if you wish, you can build a tnum and evaluate it afterwards.

    * (tnum-csc (tnum*num (tnum-pi) (/ 2 3)))
    #<FUNCTION (LAMBDA (EPS) :IN /TNUM) {100212091B}>
    * (tnum-to-num * 100)
    ...VERY LONG RATIO...

Note that tnums library is not meant to be a calculator. Of course there are ways to do so, but I do not feel about it and rather build and improve this library so the user can write his own functions to work with precise numbers. Or anybody can write a calculator or other programs based on tnums and share it with others and this would be much better than my creation of a calculator for tnums.

The usage of all operations follows.
### Adition
    * (tnum-to-string (tnum+) 50)
    "0.00000000000000000000000000000000000000000000000000..."
    * (tnum-to-string (tnum+ (num-to-tnum -200)) 50)
    "-200.00000000000000000000000000000000000000000000000000..."
    * (tnum-to-num (tnum+ (num-to-tnum 6/5) (num-to-tnum 299/3)) 50)
    1513/15
    * (tnum-to-string (tnum+ (num-to-tnum 6/5) (num-to-tnum 299/3)) 50)
    "100.86666666666666666666666666666666666666666666666666..."
    * (tnum-to-string (tnum+ (tnum-pi) (tnum-e) (tnum-phi) (num-to-tnum 1)) 50)
    "8.47790847079873332202751768899780349967472567288082..."

### Negation
    * (tnum-to-string (-tnum (tnum-pi)) 50)
    "-3.14159265358979323846264338327950288419716939937510..."


### Substraction
    * (tnum-to-string (tnum- (num-to-tnum -200)) 50)
    "200.00000000000000000000000000000000000000000000000000..."
    * (tnum-to-string (tnum- (num-to-tnum 6/5) (num-to-tnum 299/3)) 50)
    "-98.46666666666666666666666666666666666666666666666666..."
    * (tnum-to-string (tnum- (tnum-pi) (tnum-e) (tnum-phi) (num-to-tnum 1)) 50)
    "-2.19472316361914684510223092243879773128038687413061..."

### Multiplication
    * (tnum-to-string (tnum*) 50)
    "1.00000000000000000000000000000000000000000000000000..."
    * (tnum-to-string (tnum* (num-to-tnum -200)) 50)
    "-200.00000000000000000000000000000000000000000000000000..."
    * (tnum-to-string (tnum* (num-to-tnum 6/5) (num-to-tnum 299/3)) 50)
    "119.60000000000000000000000000000000000000000000000000..."
    * (tnum-to-string (tnum* (tnum-pi) (tnum-e) (tnum-phi) (num-to-tnum 1)) 50)
    "13.81758022717649443973675620120759565921921254251536..."

### Reversion
    * (tnum-to-string (/tnum (tnum-e)) 50)
    "0.36787944117144232159552377016146086744581113103176..."

### Division
    * (tnum-to-string (tnum/ (num-to-tnum -200)) 50)
    "-0.00500000000000000000000000000000000000000000000000..."
    * (tnum-to-string (tnum/ (num-to-tnum 6/5) (num-to-tnum 299/3)) 50)
    "0.01204013377926421404682274247491638795986622073578..."
    * (tnum-to-string (tnum/ (tnum-pi) (tnum-e) (tnum-phi) (num-to-tnum 1)) 50)
    "0.71427878389862830105313858884996215912911202055654..."

### Exponentiation
    * (setq r (num-to-tnum 100))
    #<FUNCTION (LAMBDA (EPS) :IN NUM-TO-TNUM) {10023D570B}>
    * (float (tnum-to-num (tnum* (tnum-pi) (tnum-expt r (num-to-tnum 2))) 7))
    31415.926
    * (float (tnum-to-num (tnum-expt r (tnum-pi)) 7))
    1919487.6

### Logarithm
    * (tnum-to-string (tnum-log (num-to-tnum 2) (num-to-tnum 16)) 50)
    "4.00000000000000000000000000000000000000000000000000..."
    * (tnum-to-num (tnum-log (tnum-phi) (tnum-phi)) 10)
    1
    * (tnum-to-string (tnum-log (tnum-pi) (tnum-e)) 50)
    "0.87356852683023186835397746476334273882072986617613..."

### Root
    * (tnum-to-string (tnum-root (num-to-tnum 4) (num-to-tnum 16)) 50)
    "1.99999999999999999999999999999999999999999999999999..."
    * (float (tnum-to-num (tnum-root (tnum-pi) (tnum-expt (tnum-e) (num-to-tnum 2))) 7))
    1.8900812

Note that the string is precise only if the last digit is not 9 or 0.

### Exponential
    * (tnum-to-string (tnum-exp (num-to-tnum 2.1)) 50)
    "8.16616991256765007344972741047863128518315260430523..."
    * (tnum-to-string (tnum-exp (tnum-phi)) 50)
    "5.04316564336002865131188218928542471032359017541384..."

### NatLog
    * (tnum-to-string (tnum-ln (num-to-tnum 10)) 50)
    "2.30258509299404568401799145468436420760110148862877..."
    * (tnum-to-string (tnum-ln (num-to-tnum 2)) 50)
    "0.69314718055994530941723212145817656807550013436025..."

### Incrementation
    * (setq n (num-to-tnum 100000000000000000000000000000000))
    #<FUNCTION (LAMBDA (EPS) :IN NUM-TO-TNUM) {10021AFEEB}>
    * (float (tnum-to-num (tnum-expt (tnum-1+ (/tnum n)) n) 50))
    2.7182817

### Decrementation
    * (tnum-to-string (tnum-1- (tnum-1- (tnum-1- (tnum-pi)))) 50)
    "0.14159265358979323846264338327950288419716939937510..."

### Square root
    * (tnum-to-string (tnum-sqrt (num-to-tnum 2)) 50)
    "1.41421356237309504880168872420969807856967187537694..."
    * (tnum-to-num (tnum-sqrt (num-to-tnum 2)) 4)
    194363389812315075599657859994834246/137435994817044508577304502085109375

### Sine
    * (tnum-to-string (tnum-sin (num-to-tnum (/ 2))) 50)
    "0.47942553860420300027328793521557138808180336794060..."

### Cosine
    * (tnum-to-string (tnum-cos (num-to-tnum (/ 2))) 50)
    "0.87758256189037271611628158260382965199164519710974..."

### Tangent
    * (tnum-to-string (tnum-tan (num-to-tnum 1)) 50)
    "1.55740772465490223050697480745836017308725077238152..."

### Cotangent
    * (tnum-to-string (tnum-ctan (num-to-tnum 1)) 50)
    "0.64209261593433070300641998659426562023027811391817..."

### Secant
    * (tnum-to-string (tnum-sec (tnum-pi)) 50)
    "-1.00000000000000000000000000000000000000000000000000..."

### Cosecant
    * (tnum-to-string (tnum-csc (tnum- (tnum-pi) (num-to-tnum (expt 10 -25)))) 25)
    "10000000000000000000000000.0000000000000000000000000..."
