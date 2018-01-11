{-# LANGUAGE RecordWildCards #-}
module Lib where

import Control.Arrow (second)
import Control.Applicative ((<|>))
import Data.Char (isAlphaNum)
import Data.Function ((&), on)
import Data.List (groupBy, sortBy, span)
import Data.List.Ordered (nubBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, listToMaybe, maybeToList)
import Data.Monoid ((<>))
import Data.Ord (Ordering(..), comparing)
import Text.ParserCombinators.ReadP(ReadP, char, skipSpaces, many1, munch1, string, readP_to_S)


data Rule = Rule { lhs :: String, rhs :: String }
          deriving (Eq)

data RuleTree = RuleTree { next :: Map Char RuleTree, output :: Maybe String }
              deriving (Eq)


-- |Compare two rules based on their lhs and the length of their rhs
cmpRule :: Rule -> Rule -> Ordering
cmpRule r1 r2 = comparing lhs r1 r2 <>
                comparing (length . rhs) r1 r2

-- |Check if two rules are equal according to 'cmpRule'
eqRule :: Rule -> Rule -> Bool
eqRule r1 r2 = cmpRule r1 r2 == EQ


-- |Compile a list of rules into a 'RuleTree'
compile :: [Rule] -> RuleTree
compile = go . nubBy eqRule . sortBy cmpRule
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


-- |Rewrite a string using a 'RuleTree'
apply :: RuleTree -> String -> String
apply top = fromJust . go top []
  where
    go :: RuleTree -> String -> String -> Maybe String
    go _            _   ""      = return ""
    go RuleTree{..} mem (c:str) = carryOn <|> stopNow <|> noMatch
      where
        carryOn = do rs <- M.lookup c next      -- Follow the branch labeled with c
                     go rs (c:mem) str          -- Recursively apply the rules

        stopNow = do str1 <- output             -- Take the current output
                     str2 <- go top [] (c:str)  -- Start again from the root of the rule tree
                     return (str1 ++ str2)      -- Concatenate the resulting strings

        noMatch = do str2 <- go top [] str      -- Start again from the root of the rule tree
                     let str1 = reverse (c:mem) -- Add up all the character's we've skipped
                     return (str1 ++ str2)      -- And concatenate them to the resulting string

-- |Decompile a 'RuleTree' into a list of rules
decompile :: RuleTree -> [Rule]
decompile = go ""
  where
    go :: String -> RuleTree -> [Rule]
    go lhs RuleTree{..} = now ++ later
      where
        now   = maybeToList (Rule (reverse lhs) <$> output)
        later = concat [ go (c:lhs) rs | (c,rs) <- M.toList next ]


-- * Instances of 'Show'

showRule :: Rule -> ShowS
showRule (Rule lhs rhs) =
  showString lhs . showString " => " . showString rhs

instance Show Rule where
  showsPrec _ = showRule

showRules :: [Rule] -> ShowS
showRules rs = foldr1 (.) [ shows r . showString ";\n" | r <- rs ]

instance Show RuleTree where
  showsPrec _ = showRules . decompile


-- * Instances of 'Read'

instance Read Rule where
  readsPrec _ = readP_to_S readRule

readRule :: ReadP Rule
readRule = Rule <$> chars <* arrow <*> chars
  where
    token = (skipSpaces *>)
    arrow = token $ string "=>"
    chars = token $ munch1 isAlphaNum

instance Read RuleTree where
  readsPrec _ = readP_to_S readRuleTree

readRules :: ReadP [Rule]
readRules = many1 (rule <* semi)
  where
    token = (skipSpaces *>)
    semi  = token $ char ';'
    rule  = token $ readRule

readRuleTree :: ReadP RuleTree
readRuleTree = compile <$> readRules
