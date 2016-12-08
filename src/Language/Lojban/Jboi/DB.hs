{-# LANGUAGE OverloadedStrings #-}

module Language.Lojban.Jboi.DB where

import Database.Datalog
import Database.Datalog.Rules
import Data.Text hiding (zipWith)
import qualified Data.Map as Map
import Language.Lojban.Jboi.CSTtoAST
import Control.Lens

selbriList :: [Text]
selbriList = (Map.toList selbriMap)^..traverse._1

kohaList :: [Text]
kohaList = (Map.toList kohamap)^..traverse._1

unaryRelList    :: [(Text,Text)]
unaryRelList    = [("prenu","mi")]
binaryRelList   :: [(Text,Text,Text)]
binaryRelList   = [("prami","mi","do")]
ternaryRelList  :: [(Text,Text,Text,Text)]
ternaryRelList  = []
quadnaryRelList :: [(Text,Text,Text,Text,Text)]
quadnaryRelList = []
quintaryRelList :: [(Text,Text,Text,Text,Text,Text)]
quintaryRelList = []

db :: IO (Database Text)
db = makeDatabase $ do
  sumt   <- addRelation "sumti"     1
  selb   <- addRelation "selbri"    1
  unary  <- addRelation "unaryRel"  2
  binary <- addRelation "binaryRel" 3
  mapM_ (assertFact selb)   (fmap return selbriList)
  mapM_ (assertFact sumt)   (fmap return kohaList)
  mapM_ (assertFact unary)  (fmap (\(a,b) -> [a,b])
                             unaryRelList)
  mapM_ (assertFact binary) (fmap (\(a,b,c) -> [a,b,c])
                             binaryRelList)

  

--t1 :: Database Text -> 
binQ db1 sel x1 x2 = do
  queryDatabase db1 q
  where
    q = do
      su     <- relationPredicateFromName "sumti"
      se     <- relationPredicateFromName "selbri"
      unary  <- relationPredicateFromName "unaryRel"
      binary <- relationPredicateFromName "binaryRel"
      binRel <- inferencePredicate "binRel"
      let x = LogicVar "X"
          y = LogicVar "Y"
          z = LogicVar "Z"
          sel' = if sel == "mo" then x else (Atom sel)
          x1'  = if x1  == "ma" then y else (Atom x1 )
          x2'  = if x2  == "ma" then z else (Atom x2 )
      (binRel, [x,y,z]) |- [ lit se [x],
                             lit su [y],
                             lit su [z],
                             lit binary [x,y,z]]
      issueQuery binRel [sel', x1', x2']
          
