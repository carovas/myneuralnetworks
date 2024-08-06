module Main where

-- perceptron code --

{-
return if the sum of conditions multiplied
by the weights if bigger than the treshold

the -treshold is the "perceptron's biad"
-}

p_conditions = [0, 1, 1] -- playing around
p_weights    = [6, 2, 2] -- weights

perceptron :: [Int] -> [Int] -> Int -> Bool
perceptron inputs weights treshold = (foldr (+) 0 $ zipWith (*) inputs weights) - treshold > 0 -- one line lol

-- BIT ADDER USING PERCEPTRONS
bitadder :: [Int] -> (Bool, Bool)-- first is result then carry bit
bitadder x = 
  let 
    w1 = [(-2), (-2)]
    tresh = (-3)
    l1 = fromEnum $ perceptron x w1 tresh
    l2 = [fromEnum $ perceptron [l1, x !! 0] w1 tresh, fromEnum $ perceptron [l1, x !! 1] w1 tresh]
  in (perceptron l2 w1 tresh, perceptron [l1, l1] [(-2), (-2)] tresh)

-- perceptron code --

-- sigmoid code --

-- sigmoid function σ(x)
sig :: Float -> Float
sig x = 1 / (1 + 2.718282 ** (-x))

-- one sigmoid neuron
sign :: [Float] -> [Float] -> Float -> Float
sign i w t = sig ((foldr (+) 0 $ zipWith (*) i w) - t)

{- 
done this function simulate a whole layer of neurons
pseudo code
for weigths, treshold in weightslist, tresholdlist
  (sign input weigths treshold) : output
-}
layersig :: [Float] -> [[Float]] -> [Float] -> [Float]
layersig i w t = zipWith (sign i) w t

-- sigmoid code --

main :: IO ()
main = do
  putStrLn $ show $ layersig [0.0] [[0.0], [0.0]] [0.0, 0.0]

  return ()
