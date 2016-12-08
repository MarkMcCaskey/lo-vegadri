{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings #-}

import Test.Hspec
import Language.Lojban.Parser
import Language.Lojban.Jboi.CSTtoAST
import Language.Lojban.Jboi.Mekso
import Language.Lojban.Jboi.Constant
import Data.Ratio
import Test.QuickCheck
import GHC.Generics

jboNumberStrings = elements ["no","pa","re","ci","mu","xa","ze","bi","so"]
jboNumberOpStrings = elements ["su'i", "vu'u", "pi'i", "te'a"]

newtype MeksoString = MeksoString String
{-instance Arbitrary MeksoString where
  arbitrary = do
    return $ "li " ++ oneof []-}


  
main :: IO ()
main = hspec $ do
  meksoParse
  experimentalVuhu

parseIntoAST :: String -> Maybe AST
parseIntoAST s = case parse s of
  Left _  -> Nothing
  Right x -> ast x


experimentalVuhu = describe "semantic parse of experimental vu'u" $ do
  describe "unary" $ do
    describe "factorial" $ do
      it "f x = product [1..x]" $ do
        evalTest "li pe'o ne'o re" 2
        evalTest "li pe'o ne'o ci" 6
        evalTest "li pe'o ne'o ma'u nono" 1
        evalTest "li pe'o ne'o paboi" 1
        evalTest "li pe'o ne'o vei re pi'i mu ve'o" (product [1..10])
    --    evalTest "li pe'o ne'o re bi'e pi'i mu" (product [1..10])
  
    describe "Absolute value" $ do
      it "makes numbers non-negative" $ do
        evalTest "li pe'o cu'a ma'u re" 2
        evalTest "li pe'o cu'a vei ni'u re ve'o" 2
        evalTest "li pe'o cu'a ma'u ma'u no" 0
        evalTest "li pe'o cu'a ni'u ni'u dau" 10
        evalTest "li pe'o cu'a ni'u ma'u ma'u ni'u ni'u soso" 99

    describe "Scientific notation" $ do
      it "= b * c^a" $ do
        evalTest "li pe'o gei no boi ciboi dau" 3
 --       evalTest "li pe'o gei no boi ciboi" 3 -- Currently disabling because variadic functions are being excluded from v0.0.1.0
   --     evalTest "li pe'o gei no boi ci" 3
        evalTest "li pe'o gei biboi muboi re" 1280

    describe "Function composition" $ do
      it "composes functions right to left" $ do
        evalTest "li pe'o fa'ai vei su'i pa ve'o vei pi'i mu ve'o ci" ((3 * 5) + 1)
        evalTest "li fa'ai vei su'i pa ve'o vei pi'i ni'u no ve'o dau" 1
      
    
  
meksoParse = describe "semantic parse of Mekso" $ do
  {-describe "Constant parsing" $ do
    describe "Integer parsing" $ do
      it "returns the number in the Mekso constant wrapper, JboInt" $ do
        parseIntoMeksoTree "li pa" `shouldBe` (Just (Constant (JboInt 1)))

    describe "Fraction parsing" $ do
      it "returns num1 over num2" $ do
        parseIntoMeksoTree "li pamu fi'u reno" `shouldBe`
          (Just (Constant (JboIntFrac (3%4))))
-}

  describe "Basic math" $ do
    describe "addition" $ do
      it "adds numbers" $ do
        evalTest "li pano su'i remu" 35
        evalTest "li ni'u pa su'i pa" 0
        evalTest "li rerereso su'i pa no mu" 2334 
        evalTest "li no su'i no" 0
        evalTest "li no su'i ni'u pa" (-1)
        evalTest "li re su'i pa pa su'i ci" 16
        evalTest "li re su'i pa pa bi'e su'i ci" 16
        evalTest "li re bi'e su'i pa pa bi'e su'i ci" 16

      it "adds int fractions" $ do
        evalTest "li pa fi'u re su'i pa fi'u re" (JboRational (1%1))
        evalTest "li fi'u re su'i fi'u re" (JboRational (1%1))
        evalTest "li fi'u mu su'i ci fi'u re" (JboRational (17%10))
        evalTest "li ni'u re fi'u so su'i ci" (JboRational (25%9))
                                              

    describe "multiplication" $ do
      it "multiplies numbers -- boi fix" $ do
        evalTest "li no boi pi'i pa" 0
        evalTest "li no boi pi'i ni'u pare" 0
        evalTest "li ni'u no boi pi'i sosomu" 0
        evalTest "li ma'u no boi pi'i so" 0
        evalTest "li ma'u re boi pi'i ma'u re" 4
        evalTest "li ni'u re boi pi'i ma'u re" (-4)
        evalTest "li ma'u re boi pi'i ni'u re" (-4)
        evalTest "li ni'u re boi pi'i ni'u re" 4
        evalTest "li ma'u ni'u re boi pi'i ni'u ni'u ni'u muno" 100

      it "multiplies numbers" $ do
        evalTest "li no pi'i pa" 0
        evalTest "li no pi'i ni'u pare" 0
        evalTest "li ni'u no pi'i sosomu" 0
        evalTest "li ma'u no pi'i so" 0

    describe "parens" $ do
      it "uses vei" $ do
        evalTest "li vei pa su'i mu" 6
        evalTest "li vei pa su'i mu ve'o" 6
        evalTest "li vei pa su'i mu ve'o pi'i mu" 30
        evalTest "li vei pa su'i mu boi pi'i mu" 30
        evalTest "li vei pa su'i mu boi pi'i mu ve'o" 30
        evalTest "li pa su'i vei mu boi pi'i mu" 26
        evalTest "li pa su'i vei mu boi pi'i mu ve'o" 26

 {-   describe "Polish notation" $ do
      it "applies functions called first" $ do
        evalTest "li su'i paboi reboi ciboi" 6
        evalTest "li pe'o pi'i paboi reboi ciboi" 6 
        evalTest "li te'a reboi reboi reboi" 16
        evalTest "li su'i vei re boi pi'i re boi vei ci boi pi'i ci boi" 13
        evalTest "li su'i vei re boi pi'i re boi ve'o vei ci boi pi'i mu boi ve'o ku'e" 19
        evalTest "li pe'o vu'u soboi muboi voboi paboi reboi" (-3)

Disabled because variadic functions are not enabled in v0.0.1.0
-}
        


evalTest str val = do case e m' of
                        (Lit trueval) -> trueval `shouldBe` (JboNum val)
                        _             -> False `shouldBe` True
  where (Just m) = (parseIntoAST str)
        m'       = case m of
          (Meks m) -> m
          (Seq ((Meks s):_))  -> s
          e        -> error (show e ++ " Error in test")
  
