module ParserUtils where

import RDF
import Namespace
import Network.URI
import Network.HTTP.Simple
import Text.ParserCombinators.Parsec(ParseError)

-- A convenience function for terminating a parse with a parse failure, using 
-- the given error message as the message for the failure.
errResult :: Graph gr => String -> Either ParseFailure gr
errResult msg = Left (ParseFailure msg)

-- Keep the (Just t) triples (eliminating the Nothing comments), and unbox the
-- triples, leaving a list of triples.
justTriples :: [Maybe(Triple)] -> [Triple]
justTriples = map (maybe (error "ParserUtils.justTriples") id) . 
              filter (/= Nothing)


handleParse :: Graph gr => (Triples -> Maybe BaseUrl -> PrefixMappings -> gr) -> 
                           Either ParseError ([Maybe Triple], Maybe BaseUrl, PrefixMappings) ->
                           Either ParseFailure gr
handleParse _mkGraph result =
  case result of
    (Left err)                      -> Left (ParseFailure $ show err)
    (Right (ts, baseUrl, prefixes)) -> Right $ _mkGraph (justTriples ts) baseUrl prefixes


_parseURL :: Graph gr => (String -> Either ParseFailure gr) 
                         -> String 
                         -> IO (Either ParseFailure gr)
_parseURL parseFunc url =
  return (parseURI url) >>=  
    maybe (return (errResult $ "Unable to parse URL: " ++ url)) p
  where
    p url =     
      httpGet url >>= \result ->
        case result of
          Nothing  -> return (errResult $ "couldn't retrieve from URL: " ++ show url)
          Just str -> return $ parseFunc str