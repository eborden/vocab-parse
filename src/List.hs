{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module List (parse) where

import Prelude

import Data.Foldable (toList)
import Data.List (inits)
import Data.Monoid
import qualified Data.Set as Set
import Data.Set (Set)
import SentenceFragment

type Glossary = Set String

parse :: Glossary -> Int -> String -> [SentenceFragment String]
parse glossary limit = collapse . toList . go [] . words
 where
  go acc [] = acc
  go acc tokens = case mayTerm of
    Just (len, term) -> go (acc <> [Vocab term]) $ drop len tokens
    Nothing -> go (acc <> [NoVocab $ head tokens]) $ tail tokens
   where
    mayTerm =
      getFirst . foldMap (First . termAndLength) . take limit . drop 1 $ inits
        tokens
    termAndLength xs = (length xs, ) <$> isGloss glossary (unwords xs)

isGloss :: Glossary -> String -> Maybe String
isGloss glossary terms
  | terms `Set.member` glossary = Just terms
  | otherwise = Nothing

collapse :: [SentenceFragment String] -> [SentenceFragment String]
collapse = \case
  [] -> []
  NoVocab x : NoVocab y : rest -> collapse $ (NoVocab $ x <> " " <> y) : rest
  x : xs -> x : collapse xs
