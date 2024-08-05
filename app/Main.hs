module Main where

-- perceptron code --

{-
return if the sum of conditions multiplied
by the weights if bigger than the treshold
-}

p_conditions = [1, 1, 1] -- playing around
p_weights    = [6, 2, 2] -- weights

perceptron :: [Int] -> [Int] -> Int -> Bool
perceptron inputs weights treshold = (foldr (+) 0 $ zipWith (*) inputs weights) <= treshold -- one line lol

-- perceptron code --

main :: IO ()
main = do
  putStrLn $ show $ perceptron p_conditions p_weights 5

  return ()
