module Complex where
import MathUtils (..)

type Complex = { real : Float
               , imaginary : Float }


cAdd : Complex -> Complex -> Complex
cAdd z w = Complex (z.real + w.real)
                   (z.imaginary + w.imaginary)

cSub : Complex -> Complex -> Complex
cSub z w = Complex (z.real - w.real)
                   (z.imaginary - w.imaginary)

cMul : Complex -> Complex -> Complex
cMul z w = 
  let a = z.real
      b = z.imaginary
      c = w.real
      d = z.imaginary
  in Complex (a * c - b * d)
             (b * c + a * d)


cDiv : Complex -> Complex -> Complex
cDiv z w =
  let a = z.real
      b = z.imaginary
      c = w.real
      d = w.imaginary
      denom = c * c + d * d 
  in Complex ((a * c + b * d) / denom)
             ((b * c - a * d) / denom)

conjugate : Complex -> Complex
conjugate z = Complex z.real (-z.imaginary)

cAbs : Complex -> Float
cAbs z = 
  let a = z.real
      b = z.imaginary
  in sqrt (a * a + b * b)

cArg : Complex -> Float
cArg z = 
  let a = z.real
      b = z.imaginary
  in if | a > 0            -> atan2 b a
        | a < 0  && b >= 0 -> atan2 b a + pi
        | a < 0  && b < 0  -> atan2 b a - pi 
        | a == 0 && y > 0  -> pi / 2
        | a == 0 && y < 0  -> -pi / 2
        | otherwise        -> b / a  -- NaN

cSqrt : Complex -> Complex
cSqrt z = 
  let a = z.real
      b = z.imaginary
      c = sqrt ((a + sqrt (a * a + b * b)) / 2)
      d = (signum b) * sqrt ((-a + sqrt (a * a + b * b)) / 2)
  in Complex c d


cLog : Complex -> Complex 
cLog z = Complex (logBase e (cAbs z)) (cArg z)

cExp : Complex -> Complex
cExp z = 
    let a = z.real
        b = z.imaginary
    in Complex (e^a * cos b) (e^a * sin b)

cPow : Complex -> Complex 
cPow base exponent =  cExp ( base `cMul` (cLog exponent) )

toComplex : Float -> Complex 
toComplex x = Complex x 0

toImaginary : Float -> Complex
toImaginary x = Complex 0 x

i : Complex 
i = Complex 0 1

j : Complex
j = i 