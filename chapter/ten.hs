import Data.Ratio

egypt :: Rational -> [Rational]
egypt 0 = []
egypt fraction =
  (1%denom):(remainders) where
  x = numerator fraction
  y = denominator fraction
  denom = ceiling (y%x)
  remx = (-y) `mod` x
  remy = y*denom
  remainders = egypt (remx%remy)

vulgar :: [Rational] -> Rational
vulgar = foldl (+) 0
