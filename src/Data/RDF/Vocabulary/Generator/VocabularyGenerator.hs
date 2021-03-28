{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.RDF.Vocabulary.Generator.VocabularyGenerator
  ( genVocabulary,
  )
where

import Data.Char (isLower)
import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.RDF
  ( AdjHashMap,
    Node (UNode),
    PrefixMappings (PrefixMappings),
    RDF,
    Rdf,
    TurtleParser (TurtleParser),
    parseFile,
    prefixMappings,
    subjectOf,
    triplesOf,
  )
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH

-- | Generates 'Node' values for concepts and properties, and
-- 'Namespace' values, for a given schema in the Haskell module in
-- which 'genVocabulary' is used.
--
-- Concepts in the schema are prepended with "_", the names of
-- properties are unchanged.
--
-- For example:
--
-- >>> $(genVocabulary "resources/shacl.ttl")
--
-- creates many 'Node' values including
--
-- @
--     _SPARQLConstraint  :: Node
--     annotationProperty :: Node
-- @
--
-- This is used to auto-generate all modules in Data.RDF.Vocabulary.* at
-- compile time with Template Haskell.
genVocabulary ::
  -- | the filepath of the file containing the schema in RDF Turtle format.
  String ->
  Q [Dec]
genVocabulary file = vocabulary <$> runIO (loadGraph file)

loadGraph :: String -> IO (RDF AdjHashMap)
loadGraph file =
  parseFile (TurtleParser Nothing Nothing) file >>= \result -> case result of
    Left err -> error $ show err
    Right rdfGraph -> return rdfGraph

vocabulary :: Rdf a => RDF a -> [Dec]
vocabulary graph =
  let nameDecls = do
        subject <- nub $ subjectOf <$> triplesOf graph
        iri <- maybeToList $ toIRI subject
        name <- maybeToList $ iriToName iri
        return (name, declareIRI name iri)
      (PrefixMappings prefixMappings') = prefixMappings graph
      namespaceDecls = do
        (prefix, iri) <- M.toList prefixMappings'
        let name = mkName . T.unpack . escape $ prefix <> "NS"
        return $ declarePrefix name prefix iri
      iriDecls = snd <$> nameDecls
      irisDecl = declareIRIs $ fst <$> nameDecls
   in irisDecl : namespaceDecls <> iriDecls

toIRI :: Node -> Maybe Text
toIRI (UNode iri) = Just iri
toIRI _ = Nothing

packFun :: Exp
packFun = VarE $ mkName "Data.Text.pack"

unodeFun :: Exp
unodeFun = VarE $ mkName "Data.RDF.Types.unode"

mkPrefixedNSFun :: Exp
mkPrefixedNSFun = VarE $ mkName "Data.RDF.Namespace.mkPrefixedNS"

declareIRI :: Name -> Text -> Dec
declareIRI name iri =
  let iriLiteral = LitE . StringL $ T.unpack iri
      unodeLiteral = AppE unodeFun $ AppE packFun iriLiteral
   in FunD name [Clause [] (NormalB unodeLiteral) []]

declareIRIs :: [Name] -> Dec
declareIRIs names =
  let iriList = ListE (VarE <$> names)
   in FunD (mkName "iris") [Clause [] (NormalB iriList) []]

-- namespace = mkPrefixedNS "ogit" "http://www.purl.org/ogit/"
declarePrefix :: Name -> Text -> Text -> Dec
declarePrefix name prefix iri =
  let prefixLiteral = AppE packFun . LitE . StringL . T.unpack $ prefix
      iriLiteral = AppE packFun . LitE . StringL . T.unpack $ iri
      namespace = AppE (AppE mkPrefixedNSFun prefixLiteral) iriLiteral
   in FunD name [Clause [] (NormalB namespace) []]

iriToName :: Text -> Maybe Name
iriToName iri = mkName . T.unpack . escape <$> (lastMay . filter (not . T.null) . T.split (`elem` separators)) iri
  where
    separators = ['/', '#']
    lastMay :: [a] -> Maybe a
    lastMay [] = Nothing
    lastMay xs = Just (last xs)

escape :: Text -> Text
escape name = escapeKeywords $ T.map escapeOperators name
  where
    escapeOperators c | c `elem` operators = escapeChar
    escapeOperators c = c
    escapeKeywords name' | not (isLower $ T.head name') = escapeChar `T.cons` name'
    escapeKeywords name' | name' `elem` keywords = escapeChar `T.cons` name'
    escapeKeywords name' = name'
    operators = ['!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '\\', '^', '|', '-', '~']
    keywords =
      [ "as",
        "case",
        "of",
        "class",
        "data",
        "data family",
        "data instance",
        "default",
        "deriving",
        "deriving instance",
        "do",
        "forall",
        "foreign",
        "hiding",
        "if",
        "then",
        "else",
        "import",
        "infix",
        "infixl",
        "infixr",
        "instance",
        "let",
        "in",
        "mdo",
        "module",
        "newtype",
        "proc",
        "qualified",
        "rec",
        "type",
        "type family",
        "type instance",
        "where"
      ]
    escapeChar = '_'
