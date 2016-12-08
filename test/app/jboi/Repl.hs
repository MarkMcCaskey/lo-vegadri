{-# LANGUAGE TemplateHaskell #-}
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
import Development.IncludeFile
import qualified Data.ByteString as BS

$(includeFileInSource "app/jboi/Lojban_logo.txt" "logo")

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
          (Selbr a b) -> runReplIO $ outputStrLn (show a ++ "\n" ++ show b)
          (Seq  l) -> mapM_ handler l
          f        -> runReplIO $ outputStrLn ("unsupported AST path. Found: " ++ show f)


          

cmd :: String -> Repl ()
cmd input = liftIO $ print input

completer :: Monad m => WordCompleter m
completer n = do
  let words = ["pa","no","re","ci","vo","su'i","li","du"]
  return $ filter (isPrefixOf n) words

help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

options :: [(String, [String] -> Repl ())]
options =
  [("help", help)
  ,("quit", const quit)
  ]

quit :: Repl ()
quit = abort >> return ()

ini :: Repl ()
ini = liftIO $ do
  BS.putStrLn logo

repl :: IO ()
repl = evalRepl ".i " loop options (Word0 completer) ini
