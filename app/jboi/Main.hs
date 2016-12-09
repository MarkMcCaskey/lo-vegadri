module Main where

import Repl (repl, replWithDict)
import Options.Applicative
import NLP.Dictionary.StarDict.InMemory
import NLP.Dictionary.StarDict hiding (mkDictionary)
import System.IO
import System.Directory (doesFileExist)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T

jboEngFileLocations@((eDictF:eIdxF:[eIfoF])) = (fmap ("/usr/share/stardict/dic/jbo-eng/"++) ["jbo-eng.dict.dz", "jbo-eng.idx", "jbo-eng.ifo"])
  
main :: IO ()
main = do
  dictReady <- and <$> mapM doesFileExist jboEngFileLocations
  if dictReady
    then do 
      eDict <- mkDictionary eIfoF (\(UTF8Text t) -> t)
      replWithDict eDict
    else repl
