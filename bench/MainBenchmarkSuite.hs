module Main where

import Criterion.Main
import Language.Lojban.Parser
import Control.Monad
import Language.Lojban.Jboi.Mekso
import Language.Lojban.Jboi.Constant
import Language.Lojban.Jboi.CSTtoAST

calcNum :: String -> Integer
calcNum s = case parse s of
  Left _   -> error "parse error!" --Nothing -- fail (show err)
  Right v  -> case ast v of
    Nothing                 -> error "No AST could be made!"
    Just (Meks x)           -> ex $ e x
    Just (Seq ((Meks x):_)) -> ex $ e x
    _                       -> error "unsupported AST type in bench"

ex (Lit (JboNum v)) = toInteger v
  
mekSum :: [Mekso] -> JboNum
mekSum []                    = JboInt 0
mekSum ((Lit (JboNum n)):xs) = n + mekSum xs
mekSum _                     = error "Unsummable number"

main :: IO ()
main = defaultMain [
  bgroup "parsing"
  [ bench "number"   $ nf (map calcNum) numbers
  , bench "number2"  $ nf (map calcNum) numbers2
  , bench "numbers3" $ nf (map calcNum) numbers3
  ]
  ]

numbers  = ["li pa", "li no", ".i li mu", ".ui li so", "li ze"]
numbers2 = ["li pano re", "li nono nonono", ".i li mureso", ".ui li somuciso", "li zebiremu"]
numbers3 = ["li pe'o fa'ai vei su'i mu ve'o vei te'a ci ve'o re",
            "li vei re pi'i mu bi'e su'i vei mu te'a ci ve'o",
            "li vei vei ci ve'o su'i vei mu pi'i re"]
