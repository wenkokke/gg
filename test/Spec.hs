{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

import Lib
import qualified Lib.Ref as Ref
import Test.QuickCheck


main :: IO ()
main = do
  quickCheck prop_DecompileCorrect
  quickCheck prop_ShowReadId
  quickCheck prop_EqualToRef


prop_DecompileCorrect str Engine{..} =
  rewrite str rules == rewrite str rules'
  where
    rules' = decompile . compile $ rules

prop_ShowReadId Engine{..} =
  rs1 == rs2
  where
    rs1, rs2 :: RuleTree
    rs1 = compile rules
    rs2 = read (show rs1)

prop_EqualToRef str Engine{..} =
  rewrite str rules == Ref.rewrite str rules


-- |Implementation of 'rewrite' with an identical type to the reference function
rewrite :: String -> [Rule] -> String
rewrite str rules = apply (compile rules) str


-- * The 'Engine' type and its 'Show' instance

newtype Engine = Engine { rules :: [Rule] }

instance Show Engine where
  showsPrec p = showsPrec p . rules


-- * Implementation of 'Arbitrary' for 'Engine'

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
  shrink = shrinkEngine

shrinkEngine :: Engine -> [Engine]
shrinkEngine Engine{..} = Engine <$> dropOne rules

arbitraryEngine :: Gen Engine
arbitraryEngine = do
  abc   <- arbitraryABC
  rules <- listOf1 $ arbitraryRule abc
  return $ Engine rules

dropOne :: [a] -> [[a]]
dropOne xs =
  [ [ x | (l, x) <- xsWithLabel, not l ] | xsWithLabel <- labelOne xs ]

labelOne :: [a] -> [[(Bool, a)]]
labelOne []     = []
labelOne (x:xs) =
  ((True,x) : map (False,) xs) : (map ((False,x):) (labelOne xs))

