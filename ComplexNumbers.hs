module ComplexNumbers (Complex, conjugate, abs, real, imaginary, mul, add, sub, div, complex)
  where

import Prelude hiding (div, abs)

data Complex a = Complex a a deriving (Eq, Show)

complex :: (a, a) -> Complex a
complex (r, i) = Complex r i

conjugate :: Num a => Complex a -> Complex a
conjugate (Complex r i) = Complex r (-i)

abs :: Floating a => Complex a -> a
abs (Complex r i) = sqrt $ r^2 + i^2

real, imaginary :: Num a => Complex a -> a
real      (Complex r _) = r
imaginary (Complex _ i) = i

add, sub, mul :: Num a => Complex a -> Complex a -> Complex a
add (Complex a b) (Complex c d) = Complex (a+c) (b+d)
sub (Complex a b) (Complex c d) = Complex (a-c) (b-d)
mul (Complex a b) (Complex c d) = Complex (a*c - b*d) (b*c + a*d)

div :: Fractional a => Complex a -> Complex a -> Complex a
div (Complex a b) (Complex c d) = Complex r i
                                    where r = (a*c + b*d) / squares
                                          i = (b*c - a*d) / squares
                                          squares = c^2 + d^2
