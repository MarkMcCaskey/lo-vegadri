module Main where

import Criterion.Main
import Language.Lojban.Parser
import Control.Monad
import Language.Lojban.Jboi.Mekso
import Language.Lojban.Jboi.Constant
import Language.Lojban.Jboi.CSTtoAST

calcNum :: String -> Maybe Mekso
calcNum s = case parse s of
  Left _   -> Nothing -- fail (show err)
  Right v  -> case ast v of
    Nothing                 -> Nothing
    Just (Meks x)           -> Just $ e x
    Just (Seq ((Meks x):_)) -> Just $ e x
    _                       -> Nothing -- fail "unsupported AST type in bench"

mekSum :: [Mekso] -> JboNum
mekSum []           = JboNum 0
mekSum ((Lit n):xs) = n + mekSum xs

main :: IO ()
main = defaultMain [
  bgroup "parsing"
  [ bench "number"  $! whnf $! (mekSum . sequence (fmap calcNum  numbers))
  , bench "number2" $! whnf $! (mekSum . sequence (fmap calcNum numbers2))
  ]
  ]

numbers  = ["li pa", "li no", ".i li mu", ".ui li so", "li ze"]
numbers2 = ["li pano re", "li nono nonono", ".i li mureso", ".ui li somuciso", "li zebiremu"]
