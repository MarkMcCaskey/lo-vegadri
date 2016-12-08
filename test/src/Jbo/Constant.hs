{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module     : Jbo.Data.Constant
Description: Representation of constant values (for mekso and logic)
-}
module Jbo.Constant where

import Control.Lens
import Data.Ratio
import Data.Type.Equality
import Control.Arrow ((&&&))
import Data.Typeable
import GHC.Generics


data JboNum = JboInt {_int :: Integer}
            | JboRational {_rational :: Rational}
            | JboFloat {_float :: Double}
  deriving (Eq, Ord, Typeable, Generic)

instance Show JboNum where
  show (JboInt x)      = show x
  show (JboRational x) = show x
  show (JboFloat x)    = show x

deriving instance Read JboNum 
  

makeLenses ''JboNum

data Constant = JboNum {_num :: JboNum}
              | JboBool {_bool :: Bool}
  deriving (Eq, Ord, Typeable, Generic)

instance Show Constant where
  show (JboNum x)   = show x
  show (JboBool x)  = show x

deriving instance Read Constant
  

makeLenses ''Constant

instance Num JboNum where
  (JboInt a)      + (JboInt b)      = JboInt      (a + b)
  (JboInt a)      + (JboFloat b)    = JboFloat    ((fromInteger a) + b)
  (JboFloat a)    + (JboInt b)      = JboFloat    (a + (fromInteger b))
  (JboFloat a)    + (JboFloat b)    = JboFloat    (a + b)
  (JboRational a) + (JboFloat b)    = JboRational (a + approxRational b 0)
  (JboFloat a)    + (JboRational b) = JboRational (b + approxRational a 0)
  (JboRational a) + (JboInt b)      = JboRational (a + (b%1))
  (JboInt a)      + (JboRational b) = JboRational ((a%1) + b)
  (JboRational a) + (JboRational b) = JboRational (a + b)

  (JboInt a)      * (JboInt b)      = JboInt      (a * b)
  (JboInt a)      * (JboFloat b)    = JboFloat    ((fromInteger a) * b)
  (JboFloat a)    * (JboInt b)      = JboFloat    (a * (fromInteger b))
  (JboFloat a)    * (JboFloat b)    = JboFloat    (a * b)
  (JboRational a) * (JboFloat b)    = JboRational (a * approxRational b 0)
  (JboFloat a)    * (JboRational b) = JboRational (b * approxRational a 0)
  (JboRational a) * (JboInt b)      = JboRational (a * (b%1))
  (JboInt a)      * (JboRational b) = JboRational ((a%1) * b)
  (JboRational a) * (JboRational b) = JboRational (a * b)

  abs (JboInt a)      = JboInt      (abs a)
  abs (JboRational a) = JboRational (abs a)
  abs (JboFloat a)    = JboFloat    (abs a)

  signum (JboInt a)      = JboInt      (signum a)
  signum (JboRational a) = JboRational (signum a)
  signum (JboFloat a)    = JboFloat    (signum a)

  fromInteger = JboInt

  negate (JboInt a)      = JboInt      (-a)
  negate (JboRational a) = JboRational (-a)
  negate (JboFloat a)    = JboFloat    (-a)

instance Enum JboNum where
  toEnum n = JboInt (toInteger n)
  fromEnum (JboInt n)      = fromEnum n
  fromEnum (JboFloat n)    = fromEnum n
  fromEnum (JboRational n) =
    (fromEnum . numerator $ n) `div` (fromEnum . denominator $ n)

instance Real JboNum where
  toRational (JboInt n)      = toRational n
  toRational (JboRational n) = toRational ((numerator ar) `div` (denominator ar))
    where
      ar = approxRational n 0
  toRational (JboFloat n)    = toRational n

instance Integral JboNum where
  toInteger (JboInt a)          = a
  toInteger (JboRational b)     = round b
  toInteger (JboFloat c)        = round c
  quotRem (JboInt a) (JboInt b) = (JboInt c, JboInt d)
    where
      (c, d) = quotRem a b
  quotRem _ _ = error "Error fractions and floats not yet integrals" -- (JboInt c, JboInt d)
  --   where (c,d) = quotRem (toInteger a) (toInteger b)

deriving instance Floating JboNum
deriving instance Fractional JboNum
