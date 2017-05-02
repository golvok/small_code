import Text.Printf (printf)

square :: Double -> Double
square x = x**2

polyeval :: [Double] -> [Double] -> Double -> Double
polyeval as bs x = sum $ zipWith (\aa b -> aa*(x**b)) as bs

squarepolyeval :: [Double] -> [Double] -> Double -> Double
squarepolyeval as bs x = square $ polyeval as bs x

integrate :: Double -> Double -> [Double] -> [Double] -> Double
integrate l r a b = 0.001 * (sum $ map (polyeval a b) (map (/1000.0) [(l*1000)..(r*1000)]))

integrateSquared :: Double -> Double -> [Double] -> [Double] -> Double
integrateSquared l r a b = pi * 0.001 * (sum $ map (squarepolyeval a b) (map (/1000.0) [(l*1000)..(r*1000)]))

-- This function should return a list [area, volume].
solve :: Double -> Double -> [Double] -> [Double] -> [Double]
solve l r a b = [area, revolution]
    where area = integrate l r a b
          revolution = integrateSquared l r a b

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
