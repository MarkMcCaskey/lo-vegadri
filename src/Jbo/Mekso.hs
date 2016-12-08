{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}


{-|
Module     : Jbo.Data.Mekso
Description: Representation of mathematical expressions
-}
module Jbo.Mekso where

import Jbo.Constant

import Prelude hiding ((.), id)
import Control.Lens
import Control.Monad.Error
import Data.String
import Data.Ratio
import Data.Type.Equality
import Control.Arrow ((&&&))
import Data.Typeable
import qualified Data.Map.Strict as Map
import qualified Unbound.LocallyNameless as NL
import qualified Unbound.LocallyNameless.Alpha as NLA
import qualified Unbound.LocallyNameless.Subst as NLS
import qualified Unbound.LocallyNameless.Types as NLT
import qualified Data.Set as S
import Unbound.LocallyNameless hiding (Subst, compose)
import Control.Category
import NumericPrelude ((^/))
import qualified NumericPrelude as N
import qualified Algebra.Algebraic as AA
import qualified Algebra.Field as AF
import qualified Algebra.Ring as AR
import qualified Algebra.Additive as AAD
import qualified Algebra.ToRational as ATR
import qualified Algebra.ZeroTestable as AZT
import qualified Algebra.Absolute as AAB

  
data MeksoType
  = TVar (Name MeksoType)
  | TArr MeksoType MeksoType
  | TFunc MeksoType MeksoType
  | TVal 
  deriving (Show)

{-
data Mekso
  = Constant {_const   :: Constant}
  | Unary    {_unary   :: (Mekso -> Mekso)
             ,_arg     :: Mekso}
  | Binary   {_binary  :: (Mekso -> Mekso -> Mekso)
             ,_bargs   :: (Mekso, Mekso)}
  | Ternary  {_ternary :: (Mekso -> Mekso -> Mekso -> Mekso)
             ,_targs   :: (Mekso, Mekso, Mekso)}
  | Nary     {_nary    :: [Mekso] -> Mekso
             ,_args    :: [Mekso]}


instance Show Mekso where
  show (Constant n)  = show n
  show (Unary _ a)   = "Unary function with args: " ++ show a
  show (Binary _ a)  = "Unary function with args: " ++ show a
  show (Ternary _ a) = "Unary function with args: " ++ show a
  show (Nary _ a)    = "Unary function with args: " ++ show a
-}


data Mekso
  = Var (Name Mekso)
  | Lam (Bind (Name Mekso) Mekso)
  | App Mekso Mekso
  | Let (Bind (Name Mekso) Mekso)
  | UnaryFunc   (Mekso -> Mekso)
  | BinaryFunc  (Mekso -> Mekso -> Mekso)
  | TernaryFunc (Mekso -> Mekso -> Mekso -> Mekso)
  | NaryFunc    ([Mekso] -> Mekso)
  | Bottom
  | Lit Constant 

instance Show Mekso where
  show (Var n)          = show n
  show (Lam bnd)        = let (n,v) = runFreshM (unbind bnd) in "(λ" ++ show n ++ ". " ++ show v ++ ")"
  show (App f m)        = show f ++ "(" ++ show m ++ ")"
  show (Let bnd)        = let (n,m) = runFreshM (unbind bnd) in "let " ++ show n ++ " = " ++ show m
  show (UnaryFunc _)    = "(λ_a. f _a)"
  show (BinaryFunc _)   = "(λ_a. λ_b. f _a _b)"
  show (TernaryFunc _)  = "(λ_a. λ_b. λ_c. f _a _b _c)"
  show (NaryFunc _)     = "λ... . f ..."
  show (Lit c)          = show c
  show Bottom           = "."
   


         
$(derive [''JboNum, ''Constant, ''MeksoType, ''Mekso])

instance IsString Mekso where
  fromString = Var . fromString
instance IsString MeksoType where
  fromString = TVar . fromString
instance IsString (Name Mekso) where
  fromString = string2Name
instance IsString (Name MeksoType) where
  fromString = string2Name

instance Eq MeksoType where
  (==) = eqType

com :: Mekso -> Mekso -> Mekso
com d e = App (App (lam "a" (lam "b" (lam "c" (App (Var "a") (App (Var "b") (Var "c")))))) d) e

{-lsucc :: Mekso -> Either TypeError Mekso
lsucc (Lit (JboNum m)) = return (Lit (JboNum (m + 1))) 
lsucc _                = throwError GenericTypeError
-}

instance Num Mekso where
  (Lit (JboNum m)) + (Lit (JboNum n)) = (Lit (JboNum (n+m)))
  (Lit (JboNum m)) - (Lit (JboNum n)) = (Lit (JboNum (m - n))) --because it's constructed backwards
  (Lit (JboNum m)) * (Lit (JboNum n)) = (Lit (JboNum (m*n)))
  negate (Lit (JboNum m))             = (Lit (JboNum (negate m)))
  abs (Lit (JboNum m))                = (Lit (JboNum (abs m)))
  signum (Lit (JboNum m))             = (Lit (JboNum (signum m)))
  fromInteger m                       = (Lit (JboNum (JboInt m)))
--  a + b = (ConstFunc (a+) b)
  

lam :: Name Mekso -> Mekso -> Mekso
lam x y = Lam (bind x y)
  
eqType :: MeksoType -> MeksoType -> Bool
eqType (TVar v1) (TVar v2) = v1 == v2
eqType TVal  TVal          = True
eqType _ _                 = False

uvar :: String -> Mekso
uvar x = Var (s2n x)

tvar :: String -> MeksoType
tvar x = TVar (s2n x)

instance Alpha Rational
instance Alpha JboNum
instance Alpha MeksoType
instance Alpha Constant
instance Alpha Mekso
instance Alpha (Mekso -> Mekso)
instance Alpha (Mekso -> Mekso -> Mekso)
instance Alpha (Mekso -> Mekso -> Mekso -> Mekso)
instance Alpha ([Mekso] -> Mekso)

instance NL.Subst MeksoType MeksoType where
  isvar (TVar v) = Just (SubstName v)
  isvar _        = Nothing

instance NL.Subst Mekso Mekso where
  isvar (Var v) = Just (SubstName v)
  isvar _       = Nothing

instance NL.Subst Mekso (Mekso -> Mekso) where

instance NL.Subst Mekso (Mekso -> Mekso -> Mekso) where

instance NL.Subst Mekso (Mekso -> Mekso -> Mekso -> Mekso) where

instance NL.Subst Mekso ([Mekso] -> Mekso) where

instance NL.Subst Mekso MeksoType where

instance NL.Subst Mekso Rational where
  
instance NL.Subst Mekso JboNum where
  
instance NL.Subst Mekso Constant where

--isvar (Var v) = Just (SubstName v)
--  isvar _       = Nothing

data TypeError
  = UnboundVariable (Name Mekso)
  | GenericTypeError
  deriving (Show)

instance Error TypeError where
  noMsg = GenericTypeError

type Env        = Map.Map (Name Mekso) MeksoType
type Constraint = (MeksoType, MeksoType)
type Infer      = ErrorT TypeError FreshM

instance Fresh Infer where
  fresh n = return n

empty :: Env
empty = Map.empty

freshtv :: Infer MeksoType
freshtv = do
  x <- fresh "_t"
  return $ TVar x

infer :: Env -> Mekso -> Infer (MeksoType, [Constraint])
infer env expr = case expr of
  Lam b -> do
    (n,e)    <- unbind b
    tv       <- freshtv
    let env' = Map.insert n tv env
    (t,cs)   <- infer env' e
    return (TArr tv t, cs)
  App e1 e2 -> do
    (t1, cs1) <- infer env e1
    (t2, cs2) <- infer env e2
    tv        <- freshtv
    return (tv, (t1, TArr t2 tv) : cs1 ++ cs2)
  Var n -> do
    case Map.lookup n env of
      Nothing -> throwError $ UnboundVariable n
      Just t  -> return (t, [])
  Let b -> do
    (n, e)          <- unbind b
    (tBody, csBody) <- infer env e
    let env'        = Map.insert n tBody env
    (t, cs)         <- infer env' e
    return (t, cs ++ csBody)
  Lit c -> do
    return (TVal, [])

fvSet :: (Alpha a, Typeable b, Rep b) => a -> S.Set (Name b)
fvSet = fv-- S.fromList . toListOf fv

type M a = FreshM a

red :: Mekso -> M Mekso
red (App e1 e2) = do
  e1' <- red e1
  e2' <- red e2
  case e1' of
    Lam bnd -> do
      (x, e1'') <- unbind bnd
      return $ subst x e2' e1''
    (UnaryFunc f) -> do
      red $ f e2'
    (BinaryFunc f) -> do
      return (UnaryFunc (`f`  e2'))
    (TernaryFunc f) -> do
      return (BinaryFunc (\a b -> f a b e2' ))

    otherwise -> return $ App e1' e2'
red (Lam bnd) = do
  (x, e) <- unbind bnd
  e' <- red e
  case e of
    App e1 (Var y) | y == x && x `S.notMember` fvSet e1
                     -> return e1
    otherwise        -> return (Lam (bind x e'))
red (Var x) = return $ (Var x)
red (Lit x) = return $ (Lit x)
red (Let bnd) = do
  (x, e) <- unbind bnd
  undefined
red (UnaryFunc x) = return (UnaryFunc x)--red $ (lam "a" (App (UnaryFunc x) "a"))
red (BinaryFunc x) = return (BinaryFunc x) --red $ (lam "a" (lam "b" (App (App (BinaryFunc x) "a") "b")))
red (TernaryFunc x) = return (TernaryFunc x)
red (NaryFunc x) = return (NaryFunc x)


data MeksoFunction
  = UnaryF   {_unary   :: (Mekso -> Mekso)}
  | BinaryF  {_binary  :: (Mekso -> Mekso -> Mekso)}
  | TernaryF {_ternary :: (Mekso -> Mekso -> Mekso -> Mekso)}
  | NaryF    {_nary    :: [Mekso] -> Mekso}
  
--makeFields ''Mekso
makeFields ''MeksoFunction
  
  {-
eval :: Mekso -> Mekso
eval e@(Constant n) = e
eval e@(Unary f a)  = f (eval a)
eval e@(Binary f (a,b))  = f (eval a) (eval b)
eval e@(Ternary f (a,b,c))  = f (eval a) (eval b) (eval c)
eval e@(Nary f as)  = f (fmap eval as)
-}
{-|
Function for converting binary haskell functions to N-ary `Mekso`s

The evaluation order is through foldl1
-}
hsBinToN
  :: (JboNum -> JboNum -> JboNum) -- ^ Binary function
  -> Mekso                        -- ^ Jbo function
hsBinToN f = NaryFunc $
  foldl1 (\(Lit (JboNum a))
           (Lit (JboNum b))
           -> (Lit (JboNum (f a b))))

  {-
{-|
Function for converting unary Haskell functions to unary `MeksoFunction`s
-}
hsUn
  :: (JboNum -> JboNum) -- ^ Unary function
  -> MeksoFunction      -- ^ Jbo function
hsUn f = UnaryF $
  (\(Constant (JboNum a)) -> (Constant (JboNum (f a))))

-}
vuhuMap :: Map.Map String Mekso
vuhuMap = Map.fromAscList
  [ ("cu'a", UnaryFunc abs)
  , ("de'o", UnaryFunc (\(Lit (JboNum n)) -> Lit (JboNum (log n))))
  , ("fa'ai", BinaryFunc (\(UnaryFunc a)
                           (UnaryFunc b) -> UnaryFunc (b . a)))
  , ("fe'a", BinaryFunc (\(Lit (JboNum a))
                          (Lit (JboNum b))
                          -> (Lit (JboNum (a ** (1/b))))))
  , ("gei", TernaryFunc (\(Lit (JboNum a))
                          (Lit (JboNum b))
                          (Lit (JboNum c)) -> Lit (JboNum (b * (c ^ a)))))
  , ("ne'o", UnaryFunc (\(Lit (JboNum n)) -> Lit (JboNum (product [1..n]))))
  {-, ("pa'i", BinaryFunc
      (\(Lit (JboNum a))
        (Lit (JboNum b))
        -> Lit (JboNum
                 (case (a,b) of
                   (JboRational a, JboRational b) -> ((numerator a) * (denominator b) % (numerator b) * (denominator a))
                   (JboRational a, b)             -> (numerator a % (denominator a) * b)
                   (a, JboRational b)             -> (a * (denominator b) % (numerator a))
                   (a,b)                          ->  (a %b)

               )))) -}
  , ("pi'i", BinaryFunc (*))
  , ("su'i", BinaryFunc (+))
  , ("te'a", BinaryFunc (\(Lit (JboNum a))
                          (Lit (JboNum b))
                          -> Lit (JboNum (a ^ b)))) -- TODO: fix this
  , ("va'a", UnaryFunc (negate))
  , ("vu'u", BinaryFunc (-))]

buildMeksoTree :: [Mekso] -> Mekso
buildMeksoTree []      = Bottom
buildMeksoTree [x]     = x
buildMeksoTree (x:y:s) = App (buildMeksoTree s) (App x y)

e = runFreshM . red

 
