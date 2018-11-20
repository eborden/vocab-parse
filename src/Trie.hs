{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Trie (parse, mkGlossary) where

import Prelude

import qualified Data.ByteString as BS
import Data.Monoid
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Trie (Trie, match)
import qualified Data.Trie as Trie
import SentenceFragment

mkGlossary :: [Text] -> Trie ()
mkGlossary = Trie.fromList . map ((, ()) . encodeUtf8)

parse :: Trie a -> Text -> [SentenceFragment Text]
parse glossary =
  fmap (fmap decodeUtf8) . collapse . go . encodeUtf8
 where
  go str
    | BS.null str = mempty
    | otherwise = case match glossary str of
      Nothing ->
        let
          novocab = BS.take 1 str
          rest = BS.drop 1 str
        in NoVocab novocab : go rest
      Just (vocab, _, rest) -> Vocab vocab : go rest

collapse :: Monoid a => [SentenceFragment a] -> [SentenceFragment a]
collapse = \case
  [] -> []
  NoVocab x : NoVocab y : rest -> collapse (NoVocab (x <> y) : rest)
  x : xs -> x : collapse xs
