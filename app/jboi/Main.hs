{-# LANGUAGE TupleSections #-}

module Main where

import Repl (repl, replWithDict)
import Options.Applicative
import NLP.Dictionary.StarDict.InMemory
import NLP.Dictionary.StarDict hiding (mkDictionary, sdIndex, StarDict)
import NLP.Dictionary
import System.IO
import System.Directory (doesFileExist)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy.Encoding as EL
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Trie.Convenience
import Data.Trie
import qualified Data.Map as Map
import Control.Monad.IO.Class (liftIO)
-- This needs to be removed:
import System.IO.Unsafe (unsafePerformIO)

jboJboFileLocations@((jDictF:jIdxF:[jIfoF])) =
  (fmap ("/usr/share/stardict/dic/jbo-jbo/"++)
   ["jbo-jbo.dict.dz", "jbo-jbo.idx", "jbo-jbo.ifo"])

jboEngFileLocations@((jeictF:eIdxF:[eIfoF])) =
  (fmap ("/usr/share/stardict/dic/jbo-eng/"++)
   ["jbo-eng.dict.dz", "jbo-eng.idx", "jbo-eng.ifo"])

  
main :: IO ()
main = do
  dictReady <- and <$> mapM doesFileExist jboJboFileLocations
  dictReady' <- and <$> mapM doesFileExist jboEngFileLocations
  if dictReady && dictReady'
    then do 
      jDict <- mkDictionary jIfoF (\(UTF8Text t) -> t)
      jTrie <- makeTrie jDict
      eDict <- mkDictionary eIfoF (\(UTF8Text t) -> t)
      replWithDict eDict jTrie
    else repl

{-|
This needs to be rewritten.  well actually most of this needs to be rewritten
Especially the IO, but the overall design needs to be changed in a fairly major
way as  well.
-}
makeTrie eDict = do
  let indices :: [TL.Text]
      indices = Map.keys $ sdIndex eDict
  let a :: ([(BS.ByteString, TL.Text)])
      a = map (\a -> (BSL.toStrict . EL.encodeUtf8 $ a,
                      TL.unlines ((unsafePerformIO . (getEntries a))
                                  eDict))) indices
  return $ fromListS a
