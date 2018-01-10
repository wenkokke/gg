{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

import Lib
import qualified Rewrite as Ref
import GHC.Generics (Generic)
import Test.QuickCheck

newtype Engine = Engine { rules :: [Rule] }

deriving instance Generic Rule
deriving instance Generic Engine
deriving instance Show Engine

arbitraryABC :: Gen String
arbitraryABC = listOf1 $ elements ['a'..'z']

arbitraryRule :: String -> Gen Rule
arbitraryRule abc = rule
  where
    char = elements abc
    side = listOf1 char
    rule = Rule <$> side <*> side

instance Arbitrary Engine where
  arbitrary = arbitraryEngine

arbitraryEngine :: Gen Engine
arbitraryEngine = do
  abc   <- arbitraryABC
  rules <- listOf1 $ arbitraryRule abc
  return $ Engine rules

comp_Rule :: Rule -> Ref.Rule
comp_Rule Rule{..} = Ref.Rule lhs rhs

comp_Engine :: Engine -> Ref.Engine
comp_Engine Engine{..} = map comp_Rule rules

rewrite :: String -> Engine -> String
rewrite str Engine{..} = apply (compile rules) str

prop_DecompileCorrect str e1@Engine{..} =
  rewrite str e1 == rewrite str e2
  where
    e2 = Engine . decompile . compile $ rules

prop_EqualToRef str e =
  rewrite str e == Ref.rewrite str (comp_Engine e)

main :: IO ()
main = do
  qc prop_DecompileCorrect
  qc prop_EqualToRef
  where
    qc = quickCheckWith stdArgs { maxSize = 10 }
