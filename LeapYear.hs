module LeapYear (isLeapYear) where

-- A leap year in the Gregorian calendar occurs:
--   on every year that is evenly divisible by 4
--   except every year that is evenly divisible by 100
--   unless the year is also evenly divisible by 400

isLeapYear :: Integer -> Bool
isLeapYear year = if byFour year
                  then by100 year
                  else False

byFour :: Integer -> Bool
byFour year = year `mod` 4 == 0

by100 :: Integer -> Bool
by100 year = if year `mod` 100 == 0 then
               if year `mod` 400 == 0 then True else False
             else True

