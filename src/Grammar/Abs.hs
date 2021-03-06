-- File generated by the BNF Converter (bnfc 2.9.3).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language Grammar.

module Grammar.Abs where

import Prelude (String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Formula
    = Top
    | Bot
    | Atom Prop
    | Not Formula
    | And Formula Formula
    | Or Formula Formula
    | Imp Formula Formula
    | Iff Formula Formula
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Prop = Prop String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

