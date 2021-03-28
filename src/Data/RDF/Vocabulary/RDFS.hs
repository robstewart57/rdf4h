{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Data.RDF.Vocabulary.RDFS where

import qualified Data.RDF.Namespace (mkPrefixedNS)
import qualified Data.RDF.Types (unode)
import Data.RDF.Vocabulary.Generator.VocabularyGenerator (genVocabulary)
import qualified Data.Text (pack)

$(genVocabulary "resources/rdfs.ttl")
