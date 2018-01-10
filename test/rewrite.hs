-- This program was made by Panu Kalliokoski and is licensed under
-- CC-BY-4.0 (https://creativecommons.org/licenses/by/4.0/)

module Rewrite where
import Data.List (isPrefixOf, sortOn, dropWhile)

data Rule = Rule String String deriving Show
type Engine = [Rule]

applicable input (Rule lhs _) = isPrefixOf lhs input

applicableRules input engine = filter (applicable input) engine

-- smaller values mean higher precedence of rules
precedence (Rule lhs rhs) = (0 - length lhs, length rhs)

equalPrecedence r1 r2 = precedence r1 == precedence r2

hasPrecedenceClash (r1 : r2 : rs) = equalPrecedence r1 r2
hasPrecedenceClash _ = False

dropClashingRules engine@(r : rs) | hasPrecedenceClash engine =
 dropClashingRules (dropWhile (equalPrecedence r) engine)
dropClashingRules engine = engine

maybeHead [] = Nothing
maybeHead (h:_) = Just h

ruleToApply input engine =
 maybeHead $ dropClashingRules $ sortOn precedence
 $ applicableRules input engine

rewrite input@(c : cs) engine =
 case ruleToApply input engine of
  Nothing -> c : rewrite cs engine
  Just (Rule lhs rhs) -> rhs ++ rewrite (drop (length lhs) input) engine
rewrite [] engine = []

