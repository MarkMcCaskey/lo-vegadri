{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module     : Jboi
Description: REPL for semantic Lojban
-}

module Repl where

import Control.Monad.Trans
import System.Console.Repline
import System.Console.Haskeline
import Data.List (isPrefixOf)
import qualified Language.Lojban.Parser as L
import Database.Datalog
import Language.Lojban.Jboi.Mekso
import Language.Lojban.Jboi.CSTtoAST
import Language.Lojban.Jboi.DB
import Development.IncludeFile
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
-- import qualified Data.ByteString.Internal.Lazy as LBS
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TLE
import NLP.Dictionary.StarDict.InMemory
import NLP.Dictionary.StarDict hiding (StarDict)
import NLP.Dictionary
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Trie
import Data.Trie.Convenience


$(includeFileInSource "data/Lojban_logo.txt" "logo")

type Repl a = HaskelineT IO a

runReplIO = runInputT defaultSettings 

loop :: Command (HaskelineT IO)
loop []    = return ()
loop instr = do
  let  mcst = L.parse instr
  case mcst of
    Left  err -> runReplIO $ outputStrLn (show err)
    Right cst -> do
      let  a  =  ast cst
      case a of
        Left  err -> runReplIO $ outputStrLn err
        Right val -> handler val
  where handler val = case val of
          (Meks m) -> runReplIO $ outputStrLn (show m) >> outputStrLn (show (e m))
          (Sumt s) -> runReplIO $ outputStrLn ("Sumti " ++ show s) 
          (Selbr a b) -> runReplIO $ do
            case b of
              []    -> return ()
              ([x]) -> liftIO $ (putStrLn . T.unpack . T.unlines . concat) =<<
                ((\d -> unQ d ((_name a)) (processSumti x)) =<< (liftIO db))
              ([x,y]) -> liftIO $ (putStrLn . T.unpack . T.unlines . concat) =<<
                ((\d -> binQ d ((_name a)) (processSumti x) (processSumti y)) =<< (liftIO db))
              _ -> outputStrLn ("other: " ++ show a ++ "\n" ++ show b)
          v@(Sel _ _ _) -> handler =<< applySe v
            
          (Seq  l) -> mapM_ handler l
          f        -> runReplIO $ outputStrLn ("unsupported AST path. Found: " ++ show f)


          
processSumti x = T.pack $ fmap(\y -> if y=='h' then '\'' else y) (fmap toLower (show x))
  
cmd :: String -> Repl ()
cmd input = liftIO $ print input

completer :: Monad m => WordCompleter m
completer n = do
  let words = ["pa","no","re","ci","vo","su'i","li","du"]
  return $ filter (isPrefixOf n) words

completerWithDict :: Monad m => Trie LT.Text -> WordCompleter m
completerWithDict tri n = do
 -- let words = fmap (\(a,_,_) -> (T.unpack . TE.decodeUtf8 $ a)) $ matches tri (LBS.toStrict . TLE.encodeUtf8 . LT.pack $ n)
  let words = fmap (T.unpack . TE.decodeUtf8) (keys (submap (TE.encodeUtf8 . T.pack $ n) tri))
  return words



help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

options :: [(String, [String] -> Repl ())]
options =
  [("help", help)
  ,("quit", const quit)
  ]

optionsWithDict :: (LT.Text -> IO [LT.Text]) -> [(String, [String] -> Repl ())]
optionsWithDict dict =
  [("help", help)
  ,("quit", const quit)
  ,("lookup", printDef dict)
  ]

printDef :: (LT.Text -> IO [LT.Text]) -> [String] -> Repl ()
printDef dict args = do
  let d = mapM (dict . LT.pack) args
  _ <- liftIO $ putStrLn . LT.unpack . LT.unlines . concat =<< d
  return ()
  
quit :: Repl ()
quit = abort >> return ()

ini :: Repl ()
ini = liftIO $ do
  BS.putStrLn logo

repl :: IO ()
repl = evalRepl ".i " loop options (Word0 completer) ini

replWithDict :: StarDict -> Trie LT.Text -> IO ()
replWithDict sd tr = do
  let entLookup = (`getEntries` sd)
  evalRepl ".i " loop (optionsWithDict entLookup) (Word (completerWithDict tr)) ini
