{-# LANGUAGE OverloadedStrings #-}

module Text.RDF.RDF4H.QuickCheck where

import           Control.Monad
import qualified Data.Map as Map
import           Data.RDF hiding (empty)
import qualified Data.Text as T
import           Test.QuickCheck

newtype SingletonGraph rdf = SingletonGraph
  { rdfGraph :: (RDF rdf)
  }

instance (Rdf rdf) =>
         Arbitrary (SingletonGraph rdf) where
  arbitrary = do
    pref <- arbitraryPrefixMappings
    baseU' <- arbitraryBaseUrl
    baseU <- oneof [return (Just baseU'), return Nothing]
    t <- liftM3 triple arbitraryS arbitraryP arbitraryO
    return SingletonGraph {rdfGraph = (mkRdf [t] baseU pref)}

instance (Rdf rdf) =>
         Show (SingletonGraph rdf) where
  show singletonGraph = showGraph (rdfGraph singletonGraph)

instance Arbitrary BaseUrl where
  arbitrary = arbitraryBaseUrl

instance Arbitrary PrefixMappings where
  arbitrary = arbitraryPrefixMappings

arbitraryBaseUrl :: Gen BaseUrl
arbitraryBaseUrl =
  oneof $
  fmap
    (return . BaseUrl . T.pack)
    ["http://example.org/", "http://example.com/a", "http://asdf.org/b", "http://asdf.org/c"]

arbitraryPrefixMappings :: Gen PrefixMappings
arbitraryPrefixMappings =
  oneof
    [ return $ PrefixMappings Map.empty
    , return $
      PrefixMappings $
      Map.fromAscList
        [ (T.pack "ex", T.pack "ex:")
        , (T.pack "eg1", T.pack "http://example.org/1")
        , (T.pack "eg2", T.pack "http://example.org/2")
        , (T.pack "eg3", T.pack "http://example.org/3")
        ]
    ]

languages :: [T.Text]
languages = [T.pack "fr", T.pack "en"]

datatypes :: [T.Text]
datatypes = fmap (mkUri xsd . T.pack) ["string", "int", "token"]

uris :: T.Text -> [T.Text]
uris type' =
  [mkUri ex (type' <> "/" <> n <> T.pack (show (i :: Int))) | n <- ["foo", "bar", "quz", "zak"], i <- [0 .. 2]]

plainliterals :: [LValue]
plainliterals = [plainLL lit lang | lit <- litvalues, lang <- languages]

typedliterals :: [LValue]
typedliterals = [typedL lit dtype | lit <- litvalues, dtype <- datatypes]

litvalues :: [T.Text]
litvalues = fmap T.pack ["hello", "world", "peace", "earth", "", "haskell"]

unodes :: T.Text -> [Node]
unodes type' = fmap UNode (uris type')

bnodes :: [ Node]
bnodes = fmap (BNode . \i -> T.pack ":_genid" <> T.pack (show (i::Int))) [1..5]

lnodes :: [Node]
lnodes = [LNode lit | lit <- plainliterals <> typedliterals]

-- maximum number of triples
maxN :: Int
maxN = 10

instance (Rdf rdf) => Arbitrary (RDF rdf) where
  arbitrary = do
    prefix <- arbitraryPrefixMappings
    baseU' <- arbitraryBaseUrl
    baseU <- oneof [return (Just baseU'), return Nothing]
    ts <- arbitraryTs
    return $ mkRdf ts baseU prefix

instance Arbitrary Triple where
  arbitrary = do
    s <- arbitraryS
    p <- arbitraryP
    triple s p <$> arbitraryO

instance Arbitrary Node where
  arbitrary = do
    type' <- elements ["sub", "pred", "obj"]
    --let typeText = T.pack type'
    oneof $ fmap return (unodes type')

arbitraryTs :: Gen Triples
arbitraryTs = do
  n <- sized (\_ -> choose (0, maxN))
  sequence [arbitrary | _ <- [1 .. n]]

arbitraryS, arbitraryP, arbitraryO :: Gen Node
arbitraryS = oneof $ fmap return $ (unodes "sub") <> bnodes
arbitraryP = oneof $ fmap return (unodes "pred")
arbitraryO = oneof $ fmap return $ (unodes "obj") <> bnodes <> lnodes
