{-# LANGUAGE RecordWildCards #-}
module Lib where

import Control.Arrow (second)
import Control.Applicative ((<*>), (<|>), pure)
import Data.Function ((&), on)
import Data.List (groupBy, sort, span)
import Data.List.Ordered (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, listToMaybe)
import Data.Monoid ((<>))
import Data.Ord (Ordering(..), comparing)

data Rule = Rule
            { lhs :: String
            , rhs :: String
            }

instance Eq Rule where
  r1 == r2 = compare r1 r2 == EQ

instance Ord Rule where
  compare r1 r2 = comparing lhs r1 r2 <>
                  comparing (length . rhs) r1 r2

instance Show Rule where
  showsPrec _ (Rule lhs rhs) = showString lhs . showString " => " . showString rhs

data RuleTree = RuleTree
                { next   :: Map Char RuleTree
                , output :: Maybe String
                } deriving (Show)

-- |Compile a list of rules into a 'RuleTree'
compile :: [Rule] -> RuleTree
compile = go . nub . sort
  where
    go :: [Rule] -> RuleTree
    go rs = RuleTree next output
      where
        -- Split the rules into those where the left-hand side is empty and those where it isn't
        (outputs, rules) = span (null . lhs) rs

        next :: Map Char RuleTree
        next = rules                   -- Take the rules
             & map splitFstChar        -- Split off the first char from the left-hand side
             & groupBy ((==) `on` fst) -- Group them by the first char
             & map takeFstChar         -- Take the first char from each group as representative
             & map (second go)         -- Recursively turn the remainder of the rules into rule trees
             & M.fromList              -- And construct a map from first chars to rule trees
          where
            takeFstChar :: [(Char, Rule)] -> (Char, [Rule])
            takeFstChar ((x, ys) : zzs) = (x, ys : map snd zzs)
            splitFstChar :: Rule -> (Char, Rule)
            splitFstChar (Rule (c : lhs) rhs) = (c, Rule lhs rhs)

        output :: Maybe String
        output = outputs               -- Take the rules with empty left-hand sides
               & map rhs               -- Take their right-hand sides
               & listToMaybe           -- Take the first of these right-hand sides

apply :: RuleTree -> String -> String
apply top = fromJust . go top
  where
    go :: RuleTree -> String -> Maybe String
    go _            ""      = return ""
    go RuleTree{..} (c:str) = carryOn <|> stopNow <|> noMatch
      where
        carryOn = do rs <- M.lookup c next   -- Follow the branch labeled with c
                     go rs str               -- Recursively apply the rules

        stopNow = do str1 <- output          -- Take the current output
                     str2 <- go top (c:str)  -- Start again from the root of the rule tree
                     return (str1 ++ str2)   -- Concatenate the resulting strings

        noMatch = do str' <- go top str      -- Start again from the root of the rule tree
                     return (c:str')         -- Concatenate the character which didn't match any rule

