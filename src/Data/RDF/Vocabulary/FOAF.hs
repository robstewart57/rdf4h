{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Data.RDF.Vocabulary.FOAF where

import qualified Data.RDF.Namespace (mkPrefixedNS)
import qualified Data.RDF.Types (unode)
import Data.RDF.Vocabulary.Generator.VocabularyGenerator (genVocabulary)
import qualified Data.Text (pack)

$(genVocabulary "resources/foaf.ttl")
