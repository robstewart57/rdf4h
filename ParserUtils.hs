module ParserUtils where

import RDF
import Network.URI
import Network.HTTP.Simple

-- A convenience function for terminating a parse with a parse failure, using 
-- the given error message as the message for the failure.
errResult :: Graph gr => String -> Either ParseFailure gr
errResult msg = Left (ParseFailure msg)

-- Keep the (Just t) triples (eliminating the Nothing comments), and unbox the
-- triples, leaving a list of triples.
justTriples :: [Maybe(Triple)] -> [Triple]
justTriples = map (maybe (error "ParserUtils.justTriples") id) . 
              filter (/= Nothing)


_parseURL :: Graph gr => (String -> IO (Either ParseFailure gr)) 
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
          Just str -> parseFunc str