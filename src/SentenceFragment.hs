{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}

module SentenceFragment where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data SentenceFragment a
  = NoVocab a
  | Vocab a
  deriving stock (Show, Generic, Functor)
  deriving anyclass (NFData)

