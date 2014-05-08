module Complex where
import MathUtils (..)

{-| The Complex type. Used to represent a complex number
with a real part and an imaginary part.
-}
type Complex = { real : Float
               , imaginary : Float }


{-| Adds two complex numbers and returns the result.

      (Complex 1 2) `cAdd` (Complex 3 4) == Complex 4 6
-}
cAdd : Complex -> Complex -> Complex
cAdd z w = Complex (z.real + w.real)
                   (z.imaginary + w.imaginary)

{-| Subtracts two complex numbers and returns the result.

      (Complex 1 2) `cSub` (Complex 3 4) == Complex -2 -2
-}
cSub : Complex -> Complex -> Complex
cSub z w = Complex (z.real - w.real)
                   (z.imaginary - w.imaginary)

{-| Negates a complex number
      
      cNeg (Complex 1 2) == Complex -1 -2
-}
cNeg : Complex -> Complex 
cNeg z = Complex (-z.real) (-z.imaginary)


{-| Multiplies two complex numbers and returns the result

      (Complex 2 3) `cMul` (Complex 4 1) == Complex 5 14

-}
cMul : Complex -> Complex -> Complex
cMul z w = 
  let a = z.real
      b = z.imaginary
      c = w.real
      d = w.imaginary
  in Complex (a * c - b * d)
             (b * c + a * d)

{-| Divides two complex numbers and returns the result

      (Complex 1 2) `cDiv` (Complex 2 2) == Complex 0.75 0.25 

-}
cDiv : Complex -> Complex -> Complex
cDiv z w =
  let a = z.real
      b = z.imaginary
      c = w.real
      d = w.imaginary
      denom = c * c + d * d 
  in Complex ((a * c + b * d) / denom)
             ((b * c - a * d) / denom)

{-| Returns the conjugate of a Complex number

      conjugate (Complex 1 2) == Complex 1 -2

-}
conjugate : Complex -> Complex
conjugate z = Complex z.real (-z.imaginary)


{-| Returns the absolute value of a Complex number 
as a Float.

      cAbs (Complex 9 12) == 15

-}
cAbs : Complex -> Float
cAbs z = 
  let a = z.real
      b = z.imaginary
  in sqrt (a * a + b * b)

{-| Returns the argument or phase of a complex number.
The argument is the angle of the radius (line segment
from the origin to the complex number) with the positive
real axis.

      cArg (Complex 1 1) == pi / 4

-}
cArg : Complex -> Float
cArg z = 
  let a = z.real
      b = z.imaginary
  in if | a > 0            -> atan2 b a
        | a < 0  && b >= 0 -> atan2 b a + pi
        | a < 0  && b < 0  -> atan2 b a - pi 
        | a == 0 && b > 0  -> pi / 2
        | a == 0 && b < 0  -> -pi / 2
        | otherwise        -> b / a  -- NaN

{-| Returns the principal square root of a complex number

      cSqrt (Complex 3 4) == Complex 2 1

-}
cSqrt : Complex -> Complex
cSqrt z = 
  let a = z.real
      b = z.imaginary
      c = sqrt ((a + sqrt (a * a + b * b)) / 2)
      d = (signum b) * sqrt ((-a + sqrt (a * a + b * b)) / 2)
  in Complex c d


{-| Return the natural logarithm of a complex number
-}
cLog : Complex -> Complex 
cLog z = Complex (logBase e (cAbs z)) (cArg z)

{-| Return e raised to a complex power
      
      cExp (Complex 0 pi) == Complex -1 0
      -- If you don't think that the above is beautiful
      -- then you have no emotion
-}
cExp : Complex -> Complex
cExp z = 
    let a = z.real
        b = z.imaginary
    in Complex (e^a * cos b) (e^a * sin b)

{-| Return the result of raising a complex number to a 
complex power
-}
cPow : Complex -> Complex -> Complex
cPow base exponent =  cExp ( exponent `cMul` (cLog base) )

{-| Converts a Float to the Complex type by setting the 
real part.

      toComplex 3 == Complex 3 0
-}
toComplex : Float -> Complex 
toComplex x = Complex x 0


{-| Converts a Float to the Complex type by setting the
imaginary part.

      toComplex 3 == Complex 0 3
-}
toImaginary : Float -> Complex
toImaginary x = Complex 0 x

{-| Imaginary number
-}
i : Complex 
i = Complex 0 1

{-| Imaginary number
-}
j : Complex
j = i 