{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Language.Lojban.Jboi.CSTtoAST where

import Data.Typeable
import Language.Lojban.Jboi.Constant
import Language.Lojban.Jboi.Mekso
import Language.Lojban.Parser hiding (parse, many1)
import Control.Lens
import Text.Parsec
import Text.Parsec.Combinator
import Data.Maybe (fromMaybe, catMaybes, fromJust, isNothing, mapMaybe)
import Control.Monad 
import Control.Monad.Logic
import Data.Ratio ((%), approxRational)
import Control.Applicative hiding ((<|>))
import Control.Arrow ((&&&), (|||))
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.List as DL
import qualified Control.Applicative as A


data SelbriInfo = SelbriInfo
  { _arity       :: !Integer
  , _name        :: !T.Text }
  deriving (Show, Eq)

makeLenses ''SelbriInfo


data AST
  = Meks !Mekso
  | Command 
  | Selb !SelbriD
  | Sumt !SumtiD
  | Selbr !SelbriInfo ![SumtiD]
  | Sel !SelbriInfo ![SumtiD] ![Se]
  | Seq ![AST]
  deriving (Show)

data Se = Se | Te | Xe
  deriving (Show, Eq)

data ProSumtiMi
  = MI | DO | MIhO | MIhA | MAhA | DOhO | KO
  deriving (Show, Eq)

data ProSumtiDemonstrative
  = TI | TA | TU
  deriving (Show, Eq)

data ProSumtiUtterance
  = DIhU | DEhU | DAhU | DIhE | DEhE | DAhE | DEI | DOhI
  deriving (Show, Eq)

data ProSumtiAssignable
  = KOhAs { _ref :: !(Maybe T.Text) } | KOhE { _ref :: !(Maybe T.Text) }
  | KOhI  { _ref :: !(Maybe T.Text) } | KOhO { _ref :: !(Maybe T.Text) }
  | KOhU  { _ref :: !(Maybe T.Text) } | FOhA { _ref :: !(Maybe T.Text) }
  | FOhE  { _ref :: !(Maybe T.Text) } | FOhI { _ref :: !(Maybe T.Text) }
  | FOhO  { _ref :: !(Maybe T.Text) } | FOhU { _ref :: !(Maybe T.Text) }
  deriving (Show, Eq)

data ProSumtiQ
  = MA 
  deriving(Show,Eq)

kohamap :: Map.Map T.Text ProSumti
kohamap = Map.fromList (foldl1 (++) [assignable,mi,demonstrative,utterance,anaphoric,[("ma",M MA)]])
  where
    assignable =
      (zip ["fo'a","fo'e","fo'i","fo'o","fo'u",
             "ko'a","ko'e","ko'i","ko'o","ko'u","mi"]
        (fmap (\a -> Psas (a Nothing))
          [KOhAs,KOhE,KOhI,KOhO,KOhU,
           FOhA,FOhE,FOhI,FOhO,FOhU]))
    mi = zip ["mi","do","mi'o","mi'a","ma'a","do'o","ko"]
         (fmap Psm [MI, DO, MIhO, MIhA, MAhA, DOhO, KO])
    demonstrative = zip ["ti","ta","tu"] (fmap Psd [TI, TA, TU])
    utterance = zip ["di'u","de'u","da'u","di'e","de'e","da'e","dei","do'i"]
                (fmap Psu [DIhU, DEhU, DAhU, DIhE, DEhE, DAhE, DEI, DOhI])
    anaphoric = zip ["ri","ra","ru"] (fmap Psan [RI, RA, RU])

data ProBridiAssignable
  = BRODA { _ref :: !T.Text }
  | BRODE { _ref :: !T.Text }
  | BRODI { _ref :: !T.Text }
  | BRODO { _ref :: !T.Text }
  | BRODU { _ref :: !T.Text }
  deriving (Show, Eq)

data ProSumtiAnaphoric = RI | RA | RU
  deriving (Show, Eq)

data ProBridiAnaphoric = GOhI | GOhAs | GOhU | GOhE | GOhO | NEI | NOhA
  deriving (Show, Eq)

data ProSumti
  = Psas { _psas :: !ProSumtiAssignable }
  | Psan { _psan :: !ProSumtiAnaphoric }
  | Psm  { _psm  :: !ProSumtiMi }
  | Psd  { _psd  :: !ProSumtiDemonstrative }
  | Psu  { _psu  :: !ProSumtiUtterance }
  | M    { _m    :: !ProSumtiQ }
  deriving(Eq)

instance Show ProSumti where
  show (Psas a) = show a
  show (Psan a) = show a
  show (Psm a)  = show a
  show (Psd a)  = show a
  show (Psu a)  = show a
  show (M a)    = show a


data SumtiD
  = Quote {_content :: !T.Text}
  | Psa   {_psa     :: !ProSumti}
  | ZOhE 
  deriving (Eq)

instance Show SumtiD where
  show (Quote c) = show c
  show (Psa   c) = show c
  show (ZOhE)    = "zo'e"

data SelbriD
  = S1 { _selbriName :: !T.Text
       , _fa         :: !SumtiD}
  | S2 { _selbriName :: !T.Text
       , _fa         :: !SumtiD
       , _fe         :: !SumtiD}
  | S3 { _selbriName :: !T.Text
       , _fa         :: !SumtiD
       , _fe         :: !SumtiD
       , _fi         :: !SumtiD}
  | S4 { _selbriName :: !T.Text
       , _fa         :: !SumtiD
       , _fe         :: !SumtiD
       , _fi         :: !SumtiD
       , _fo         :: !SumtiD}
  | S5 { _selbriName :: !T.Text
       , _fa         :: !SumtiD
       , _fe         :: !SumtiD
       , _fi         :: !SumtiD
       , _fo         :: !SumtiD
       , _fu         :: !SumtiD}
  deriving (Show, Eq)

makeLenses ''SumtiD
makeLenses ''SelbriD
makeLenses ''AST

{-

LA/LE/LO (probably consider all equal for now)


Selbri

Brivla

Sumti
-}



{-|
Top level function for construction 'AST's
-}
ast
  :: (Monad m, Alternative m)
  => Text  -- ^ 'Language.Lojban.Parser.Text'
  -> m AST -- ^ Higher level semantic representation
ast (FTermsVAU l _ _)            =
  case sequence ((uncurry (zipWith(A.<|>))
                   (unzip ((((Sumt <$>) . astSumti) &&&
                             ((Meks <$>) . meksoSumti))
                            <$> l)))^..traverse.filtered(not . isNothing)) of
    Nothing -> fail "no valid terms"
    Just x  -> return (Seq x)
                                     
ast (HeadNIhO _ _ Nothing)       = fail "Empty paragraph"
ast (HeadNIhO _ _ (Just p))      = ast p -- @@Ignore paragraphs
ast (IText_1 _ _ _ _ Nothing)    = fail "Empty Sentence"
ast (IText_1 _ _ _ _ (Just p))   = ast p -- @@Ignore sentence start
ast (TopText _ _ _ _ Nothing _)  = fail "Empty top level expression"
ast (TopText _ _ _ _ (Just p) _) = ast p
ast (Selbri b)                   = astSelbri b 
ast (TermsBridiTail s _ _ t)     = case ast t of
  (Just (Selb v)) -> case (lookupSelbri (v^.selbriName.to T.unpack)) of
    (Just si) -> Selbr si <$> (sequence (fmap astSumti s)) 
    Nothing   -> fail "selbri unsupported"
  (Just (Selbr v _))  -> Selbr v <$> (sequence (fmap astSumti s)) 
  (Just (Sel v _ se)) -> (\b -> Sel v b se) <$> (sequence (fmap astSumti s))
  x               -> fail ("invalid selbri in tbt: " ++ show x)
ast (SelbriTailTerms s t _ _)    = case (astSelbri s) of
  Left err          -> fail err
  Right (Selbr i _)  -> (Selbr i) <$> (sequence (fmap astSumti t)) 
  Right (Sel i _ se) -> (\b -> Sel i b se) <$> (sequence (fmap astSumti t))


  
{-|
Dictionary of 

Note: remember that this needs to be sorted by keys, for linear
insertion time (fromAscList)
-}
selbriMap :: Map.Map T.Text SelbriInfo
selbriMap = Map.fromList
  [s "pendo" 2,
   s "prami" 2,
   s "nelci" 2,
   s "dunli" 3,
   s "prenu" 2,
   s "mroka'e" 2,
   s "mo"      3]
  where s n a = (n,SelbriInfo a n)

lookupSelbri :: (Monad m, Alternative m) => String -> m SelbriInfo
lookupSelbri s = case Map.lookup (T.pack s) selbriMap of
  Nothing -> fail   ("Selbri " ++ s ++ " is currently not defined")
  Just x  -> return x


astSumti :: (Monad m, Alternative m) => Sumti -> m SumtiD
astSumti l@(LI _ _ _ _ _)     = fail (show l ++ " is a mekso sumti")
astSumti (KOhA (_,n,_) _)     = case Map.lookup (T.pack n) kohamap of
  Nothing -> fail ("error: " ++ n ++ " is not a KOhA")
  Just  k -> return (Psa k)
astSumti (ZO _ _ q _ _)       = return (Quote (T.pack q))
astSumti (ZOI _ _ t _ _)      = return . Quote $ T.pack (foldl1 (++) t)
astSumti (LOhU _ _ t _ _ _)   = return . Quote . T.pack . unwords $ t
astSumti _ = fail "this type of sumti is currently unhandeled"

astSelbri :: (Monad m, Alternative m) => Selbri -> m AST
astSelbri (Brivla (_,n,_) _)    = (`Selbr` []) <$> lookupSelbri n
astSelbri (GOhA (_,"mo",_) _ _) = (`Selbr` []) <$> lookupSelbri "mo"
astSelbri (SE (_,mod,_) _ s)    = (astSelbri s) >>= \a -> case a of
  (Sel s args ses) -> return $ Sel s args ((modse mod):ses)
  (Selbr s args)   -> return (Sel s args [modse mod])
  _                -> fail "Error in SE, unsupported AST selbri"
  where modse str = case str of {"se" -> Se; "Te" -> Te; "Xe" -> Xe;}

    {-(astSelbri s) >>= \a -> case (a,mod) of
  (Selbr s args, "se") -> case args of
    (a:b:c)   -> return (Selbr s (b:a:(drop 2 c)))
    errArgs   -> fail ("Error! selbri: " ++ show s ++ " does not have enough sumti arguments ( " ++ show errArgs ++ ")")
  (Selbr s args, "te") -> case args of
    (a:b:c:d)  -> return (Selbr s (c:b:a:(drop 3 d)))
    _          -> fail ("Error! selbri: " ++ show s ++ " does not have enough sumti arguments")
  (Selbr s args, "xe") -> case args of
    _ -> undefined

  _            -> fail "Unsupported SE" -}
  
meksoSumti :: (Monad m, Alternative m) => Sumti -> m Mekso
meksoSumti (LI _ _ m mc _) = mex m
meksoSumti _ = fail "this type of mekso sumti not handeled" 

{-|
Processing of 'Mex' ('Operand') into 'Mekso'
-}
mex
  :: (Monad m, Alternative m)
  => Mex
  -> m Mekso
mex (Number [] _ _) = fail "no number could be read"
mex (Number xs _ _) = (xs^..traverse._2) `numberConverter` Nothing
mex (OLerfuString [(_,l,_)] _ _) = return $ uvar l
mex (MexOperatorMex m op) = foldr1 App <$> sequence (reverse (fmap mexOp op) ++ [mex m])
  where insert_snd []  _    = []
        insert_snd (x:xs) y = x:y:xs
mex (PEhO _ _ mo (l:ls) _ _) = liftM2 (foldl App) (mexOp (mo, l)) (mapM mex ls)
mex (VEI _ _ m _ _)      = mex m
mex (BIhE m _ _ mo m')    = liftM2 App (mexOp (mo, m)) (mex m')

{-|
Unary function ('MexOperator') application to 'Mex' construction

Note: Every function is unary if partially applied
-}
mexOp
  :: (Monad m, Alternative m)
  => (MexOperator, Mex)
  -> m Mekso
mexOp (VUhU (_,c,_) _,m) = case Map.lookup c vuhuMap of
  Nothing  -> fail ("Unrecognized VUhU: " ++ c)
  Just v   -> do
    rm <- mex m
    return (App v rm)
mexOp (JoikJekJoikKEOperator mo _, m) = mexOp (mo, m) --ignore for now
mexOp _          = fail "unrecognized mexOp"

{-|
Handles all atoms of the Mekso system.

Currently it does not support repeating decimals, irrational numbers,
complex numbers, or real numbers.

The syntax of what is acceptable in this version of Lojban is not the
same as the CLL.  See the standard for information about these
differences.
-}
numberConverter
  :: (Monad m, Alternative m)
  => [String]      -- ^ PA "numbers"
  -> Maybe Integer -- ^ Optional base of the number
  -> m Mekso       -- ^ Mekso constant
numberConverter [] _        = fail "no numbers found"
numberConverter n@(x:xs) mb =
  case (DL.elemIndices "fi'u" n, DL.elemIndices "pi" n) of
    ([],[])  -> return . Lit . JboNum . JboInt $ signval n * numval n
    ([a],[]) -> return . Lit . JboNum . JboRational $
                signval c * (let nv = numval c
                                 in (if nv == 0 then 1
                                    else nv) % numval d)
      where (c,d) = DL.splitAt a n
    ([],[b]) -> return . Lit . JboNum . JboFloat $
                signval c * read (show c ++ "." ++ show d) 
      where (c,d) = DL.splitAt b n
    _        -> fail "invalid pattern (fractions and decimals are mutually exclusive and cannot be nested (i.e. numerators/denomerators must be Integers"
  where numMap = Map.fromList (zip
                           ["no","pa","re","ci","vo","mu","xa","ze","bi",
                            "so","dau","fei","gai","jau","rei","vai"]
                           [0..15])
        signval l = product (mapMaybe sign l)
        numval  l = sum $ zipWith
                    (\a b -> a* fromMaybe 10 mb ^b)
                    (reverse (mapMaybe (`Map.lookup` numMap)
                                 l)) [0..]
          
        sign "ma'u" = return 1
        sign "ni'u" = return (-1)
        sign x      = fail (show x ++ " sign not recognized")
        fraction "fi'u" = True
        fraction _      = False
        decimal  "pi"   = True
        decimal  _      = False
  


{-|
Turn 'Selbr' 'AST' into meaning!

friend(Graham1642,Andurilfromnarsil)
Tiger(Tim_apple)
Loves(Zombie_brand, tigers)

lo tirxu (the thing described by x1 of tirxu)



-}
  {-
evalSelbri :: (Monad m, Alternative m) => AST -> m
evalSelbri = undefined

-}

applySe :: (Monad m) => AST -> m AST
applySe (Sel a b [])    = return $ Selbr a b
applySe (Sel a b (x:xs)) = case x of
  Se -> applySe (Sel a ((take 1 (drop 1 b)) ++ (take 1 b) ++ (drop 2 b)) xs)
  Te -> applySe (Sel a ((take 1 (drop 2 b)) ++ (take 1 (drop 1 b))
                        ++ (take 1 b) ++ (drop 3 b)) xs)
  _ -> undefined
applySe other           = fail ("Error: no SE to apply in " ++ show other)
