{-# LANGUAGE RecordWildCards #-}

import Lib
import qualified Lib.Ref as Ref
import Test.QuickCheck

newtype Engine = Engine { rules :: [Rule] }

instance Show Engine where
  showsPrec _ = showRules . rules

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

rewrite :: String -> [Rule] -> String
rewrite str rules = apply (compile rules) str

prop_DecompileCorrect str Engine{..} =
  rewrite str rules == rewrite str rules'
  where
    rules' = decompile . compile $ rules

prop_ShowReadId Engine{..} =
  read . show $ rs == rs
  where
    rs = compile rules

prop_EqualToRef str Engine{..} =
  rewrite str rules == Ref.rewrite str rules

main :: IO ()
main = do
  qc prop_DecompileCorrect
  qc prop_EqualToRef
  where
    qc = quickCheckWith stdArgs { maxSize = 10 }
