{-# LANGUAGE CPP #-}

-- | An RDF serializer for Turtle
--  <http://www.w3.org/TeamSubmission/turtle/>.
module Text.RDF.RDF4H.TurtleSerializer
  (TurtleSerializer (TurtleSerializer))
where

#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#else
#endif
#else
#endif

import Control.Monad
import Data.List (groupBy, sort)
import qualified Data.Map as Map
import Data.RDF.Namespace hiding (rdf)
import Data.RDF.Query
import Data.RDF.Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import Text.RDF.RDF4H.TurtleSerializer.Internal

data TurtleSerializer = TurtleSerializer (Maybe T.Text) PrefixMappings

instance RdfSerializer TurtleSerializer where
  hWriteRdf (TurtleSerializer docUrl pms) h rdf = _writeRdf h docUrl (addPrefixMappings rdf pms False)
  writeRdf s = hWriteRdf s stdout
  hWriteH (TurtleSerializer _ pms) h rdf = writeHeader h (baseUrl rdf) (prefixMappings rdf <> pms)
  writeH s = hWriteRdf s stdout

  -- TODO: should use mdUrl to render <> where appropriate
  hWriteTs (TurtleSerializer docUrl pms) h = writeTriples h docUrl pms
  writeTs s = hWriteTs s stdout
  hWriteT (TurtleSerializer docUrl pms) h = writeTriple h docUrl pms
  writeT s = hWriteT s stdout
  hWriteN (TurtleSerializer docUrl pms) h n = writeNode h docUrl n pms
  writeN s = hWriteN s stdout

-- TODO: writeRdf currently merges standard namespace prefix mappings with
-- the ones that the RDF already contains, so that if the RDF has none
-- (e.g., was parsed from ntriples RDF) the output still uses prefix for
-- common mappings like rdf, owl, and the like. This behavior should be
-- configurable somehow, so that if the user really doesn't want any extra
-- prefix declarations added, that is possible.

_writeRdf :: Rdf a => Handle -> Maybe T.Text -> RDF a -> IO ()
_writeRdf h mdUrl rdf =
  writeHeader h bUrl pms' >> writeTriples h mdUrl pms' ts >> hPutChar h '\n'
  where
    bUrl = baseUrl rdf
    -- a merged set of prefix mappings using those from the standard_ns_mappings
    -- that are not defined already (union is left-biased).
    pms' = PrefixMappings $ (asMap $ prefixMappings rdf)
    asMap (PrefixMappings x) = x
    ts = triplesOf rdf

writeHeader :: Handle -> Maybe BaseUrl -> PrefixMappings -> IO ()
writeHeader h bUrl pms = writeBase h bUrl >> writePrefixes h pms

writeBase :: Handle -> Maybe BaseUrl -> IO ()
writeBase _ Nothing =
  return ()
writeBase h (Just (BaseUrl bUrl)) =
  hPutStr h "@base " >> hPutChar h '<' >> T.hPutStr h bUrl >> hPutStr h "> ." >> hPutChar h '\n'

writePrefixes :: Handle -> PrefixMappings -> IO ()
writePrefixes h pms = mapM_ (writePrefix h) (toPMList pms) >> hPutChar h '\n'

writePrefix :: Handle -> (T.Text, T.Text) -> IO ()
writePrefix h (pre, uri) =
  hPutStr h "@prefix " >> T.hPutStr h pre >> hPutStr h ": "
    >> hPutChar h '<'
    >> T.hPutStr h uri
    >> hPutStr h "> ."
    >> hPutChar h '\n'

writeTriples :: Handle -> Maybe T.Text -> PrefixMappings -> Triples -> IO ()
writeTriples h mdUrl (PrefixMappings pms) ts =
  mapM_ (writeSubjGroup h mdUrl revPms) (groupBy equalSubjects (sort ts))
  where
    revPms = PrefixMappings . Map.fromList $ (\(k, v) -> (v, k)) <$> Map.toList pms

writeTriple :: Handle -> Maybe T.Text -> PrefixMappings -> Triple -> IO ()
writeTriple h mdUrl pms t =
  w subjectOf >> space >> w predicateOf >> space >> w objectOf
  where
    w :: (Triple -> Node) -> IO ()
    w f = writeNode h mdUrl (f t) pms
    space = hPutChar h ' '

-- Write a group of triples that all have the same subject, with the subject only
-- being output once, and comma or semi-colon used as appropriate.
writeSubjGroup :: Handle -> Maybe T.Text -> PrefixMappings -> Triples -> IO ()
writeSubjGroup _ _ _ [] = return ()
writeSubjGroup h dUrl pms ts@(t : _) =
  writeNode h dUrl (subjectOf t) pms >> hPutChar h ' '
    >> writePredGroup h dUrl pms (head ts')
    >> mapM_ (\t' -> hPutStr h ";\n\t" >> writePredGroup h dUrl pms t') (tail ts')
    >> hPutStrLn h " ."
  where
    ts' = groupBy equalPredicates ts

-- Write a group of triples that all have the same subject and the same predicate,
-- assuming the subject has already been output and only the predicate and objects
-- need to be written.
writePredGroup :: Handle -> Maybe T.Text -> PrefixMappings -> Triples -> IO ()
writePredGroup _ _ _ [] = return ()
writePredGroup h docUrl pms (t : ts) =
  -- The doesn't rule out <> in either the predicate or object (as well as subject),
  -- so we pass the docUrl through to writeNode in all cases.
  writeNode h docUrl (predicateOf t) pms >> hPutChar h ' '
    >> writeNode h docUrl (objectOf t) pms
    >> mapM_ (\t' -> hPutStr h ", " >> writeNode h docUrl (objectOf t') pms) ts

writeNode :: Handle -> Maybe T.Text -> Node -> PrefixMappings -> IO ()
writeNode h mdUrl node pms =
  case node of
    (UNode bs) ->
      let currUri = bs
       in case mdUrl of
            Nothing -> writeUNodeUri h currUri pms
            Just url -> if url == currUri then hPutStr h "<>" else writeUNodeUri h currUri pms
    (BNode gId) -> T.hPutStr h gId
    (BNodeGen i) -> putStr "_:genid" >> hPutStr h (show i)
    (LNode n) -> writeLValue h n pms

-- Print prefix mappings to stdout for debugging.
_debugPMs :: PrefixMappings -> IO ()
_debugPMs (PrefixMappings pms) = mapM_ (\(k, v) -> T.putStr k >> putStr "__" >> T.putStrLn v) (Map.toList pms)

writeLValue :: Handle -> LValue -> PrefixMappings -> IO ()
writeLValue h lv pms =
  case lv of
    (PlainL lit) -> writeLiteralString h lit
    (PlainLL lit lang) ->
      writeLiteralString h lit
        >> hPutStr h "@"
        >> T.hPutStr h lang
    (TypedL lit dtype) ->
      writeLiteralString h lit
        >> hPutStr h "^^"
        >> writeUNodeUri h dtype pms

-- writeUNodeUri h (T.reverse dtype) pms

writeLiteralString :: Handle -> T.Text -> IO ()
writeLiteralString h bs =
  do
    hPutChar h '"'
    void (T.foldl' writeChar (return True) bs)
    hPutChar h '"'
  where
    writeChar :: IO Bool -> Char -> IO Bool
    writeChar b c =
      case c of
        '\n' -> b >>= \b' -> when b' (hPutChar h '\\' >> hPutChar h 'n') >> return True
        '\t' -> b >>= \b' -> when b' (hPutChar h '\\' >> hPutChar h 't') >> return True
        '\r' -> b >>= \b' -> when b' (hPutChar h '\\' >> hPutChar h 'r') >> return True
        '"' -> b >>= \b' -> when b' (hPutChar h '\\' >> hPutChar h '"') >> return True
        '\\' -> b >>= \b' -> when b' (hPutChar h '\\' >> hPutChar h '\\') >> return True
        _ -> b >>= \b' -> when b' (hPutChar h c) >> return True
