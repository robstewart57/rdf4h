{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | An implementation of the RFC3987
--  [RFC3987]: http://www.ietf.org/rfc/rfc3987.txt
module Data.RDF.IRI
  ( IRI (..),
    IRIRef (..),
    Scheme (..),
    Authority (..),
    UserInfo (..),
    Host (..),
    Port (..),
    Path (..),
    IRIQuery (..),
    Fragment (..),
    IRIError (..),
    SchemaError (..),
    mkIRI,
    serializeIRI,
    parseIRI,
    parseRelIRI,
    validateIRI,
    resolveIRI,
    removeIRIFragment,
  )
where

#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#else
#endif
#else
#endif

#if MIN_VERSION_base(4,13,0)
import Data.Maybe (isJust)
#else
import Data.Maybe (maybe, isJust)
#endif

import Control.Applicative
import Control.Arrow (first, (&&&), (>>>))
import Control.Monad (guard)
import Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as P
import Data.Char (isAlpha, isAlphaNum, isDigit, toLower, toUpper)
import Data.Functor
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T

-- | A serialized IRI representation.
newtype IRI = IRI {getIRI :: Text}
  deriving (Show, Eq)

-- | A detailed IRI representation with its components.
data IRIRef
  = IRIRef
      !(Maybe Scheme)
      !(Maybe Authority)
      !Path
      !(Maybe IRIQuery)
      !(Maybe Fragment)
  deriving (Show, Eq, Ord)

newtype Scheme = Scheme Text
  deriving (Show, Eq, Ord)

data Authority
  = Authority
      !(Maybe UserInfo)
      !Host
      !(Maybe Port)
  deriving (Show, Eq, Ord)

newtype UserInfo = UserInfo Text
  deriving (Show, Eq, Ord)

newtype Host = Host Text
  deriving (Show, Eq, Ord)

newtype Port = Port Int
  deriving (Show, Eq, Ord)

newtype Path = Path Text
  deriving (Show, Eq, Semigroup, Monoid, Ord)

newtype IRIQuery = IRIQuery Text
  deriving (Show, Eq, Semigroup, Ord)

instance Monoid IRIQuery where
  mempty = IRIQuery mempty

#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

newtype Fragment = Fragment Text
  deriving (Show, Eq, Semigroup, Ord)

instance Monoid Fragment where
  mempty = Fragment mempty

#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

data IRIError = InvalidIRI
  deriving (Show, Eq)

data SchemaError
  = -- | Scheme must start with an alphabet character
    NonAlphaLeading
  | -- | Subsequent characters in the schema were invalid
    InvalidChars
  | -- | Schemas must be followed by a colon
    MissingColon
  deriving (Show, Eq)

removeIRIFragment :: IRIRef -> IRIRef
removeIRIFragment (IRIRef s a p q _) = IRIRef s a p q Nothing

-- [TODO] use Builder
serializeIRI :: IRIRef -> Text
serializeIRI (IRIRef s a p q f) =
  mconcat
    [ maybe mempty scheme s,
      maybe mempty authority a,
      path p,
      maybe mempty query q,
      maybe mempty fragment f
    ]
  where
    scheme (Scheme s') = s' <> ":"
    authority (Authority u (Host h) p') =
      mconcat
        [ "//",
          maybe mempty userInfo u,
          h,
          maybe mempty port p'
        ]
    userInfo (UserInfo u) = u <> "@"
    port (Port p') = (":" <>) . T.pack . show $ p'
    path (Path p') = p'
    query (IRIQuery q') = "?" <> q'
    fragment (Fragment f') = "#" <> f'

mkIRI :: Text -> Either String IRI
mkIRI t = IRI . serializeIRI <$> parseIRI t

parseIRI :: Text -> Either String IRIRef
parseIRI = P.parseOnly $ iriParser <* (P.endOfInput <?> "Unexpected characters at the end")

parseRelIRI :: Text -> Either String IRIRef
parseRelIRI = P.parseOnly $ irelativeRefParser <* (P.endOfInput <?> "Unexpected characters at the end")

validateIRI :: Text -> Either String Text
validateIRI t = t <$ parseIRI t

-- | IRI parsing and resolution according to algorithm 5.2 from RFC3986
-- See: http://www.ietf.org/rfc/rfc3986.txt
-- [FIXME] Currently, this is a correct but naive implementation.
resolveIRI ::
  -- | Base URI
  Text ->
  -- | URI to resolve
  Text ->
  Either String Text
resolveIRI baseIri iri = serializeIRI <$> resolvedIRI
  where
    resolvedIRI = either (const resolvedRelativeIRI) resolveAbsoluteIRI (parseIRI iri)
    resolveAbsoluteIRI (IRIRef s a (Path p) q f) = return $ IRIRef s a (removeDotSegments p) q f
    resolvedRelativeIRI = do
      -- Parse as a relative IRI
      (IRIRef _ ra rp@(Path rp') rq rf) <- parseRelIRI iri
      -- Parse base IRI
      (IRIRef bs ba bp bq _) <- parseIRI baseIri
      let rIriWithoutAuth = resolveIriWithoutAuth rp rq rf bs ba bp bq
          rIriWithAuth = return (IRIRef bs ra (removeDotSegments rp') rq rf)
      maybe rIriWithoutAuth (const rIriWithAuth) ra
    resolveIriWithoutAuth rp rq rf bs ba bp bq =
      return
        $! if (rp == mempty)
          then maybe (IRIRef bs ba bp bq rf) (const (IRIRef bs ba bp rq rf)) rq
          else
            let (Path rp') = rp
             in if (T.head rp' == '/')
                  then IRIRef bs ba (removeDotSegments rp') rq rf
                  else IRIRef bs ba (removeDotSegments (merge ba bp rp)) rq rf
    removeDotSegments p = removeDotSegments' (T.split (== '/') p) mempty
    removeDotSegments' [] os = Path $ mconcat (intersperse "/" os)
    removeDotSegments' ["."] os = removeDotSegments' mempty (os <> [mempty])
    removeDotSegments' [".."] [] = removeDotSegments' mempty mempty
    removeDotSegments' [".."] os = removeDotSegments' mempty (init os <> [mempty])
    removeDotSegments' ss@[_] os = removeDotSegments' mempty (os <> ss)
    removeDotSegments' ("." : ss) os = removeDotSegments' ss os
    removeDotSegments' (".." : ss) [] = removeDotSegments' ss mempty
    removeDotSegments' (".." : ss) os@[""] = removeDotSegments' ss os
    removeDotSegments' (".." : ss) os = removeDotSegments' ss (init os)
    removeDotSegments' (s : ss) os = removeDotSegments' ss (os <> [s])
    merge ba (Path bp) (Path rp)
      | isJust ba && bp == mempty = "/" <> rp
      | otherwise = T.dropWhileEnd (/= '/') bp <> rp

-- IRI = scheme ":" ihier-part [ "?" iquery ] [ "#" ifragment ]
iriParser :: Parser IRIRef
iriParser = do
  scheme <- Just <$> schemeParser
  _ <- P.string ":" <?> "Missing colon after scheme"
  (authority, path) <- ihierPartParser
  query <- optional iqueryParser
  fragment <- optional ifragmentParser
  return (IRIRef scheme authority path query fragment)

-- ihier-part = "//" iauthority ipath-abempty
--            / ipath-absolute
--            / ipath-rootless
--            / ipath-empty
ihierPartParser :: Parser (Maybe Authority, Path)
ihierPartParser =
  iauthWithPathParser
    <|> ipathAbsoluteParser
    <|> ipathRootlessParser
    <|> ipathEmptyParser

-- IRI-reference = IRI / irelative-ref
-- [TODO]

-- absolute-IRI = scheme ":" ihier-part [ "?" iquery ]
-- [TODO]

-- irelative-ref = irelative-part [ "?" iquery ] [ "#" ifragment ]
irelativeRefParser :: Parser IRIRef
irelativeRefParser = do
  (authority, path) <- irelativePartParser
  query <- optional iqueryParser
  fragment <- optional ifragmentParser
  return (IRIRef Nothing authority path query fragment)

-- irelative-part = "//" iauthority ipath-abempty
--                / ipath-absolute
--                / ipath-noscheme
--                / ipath-empty
irelativePartParser :: Parser (Maybe Authority, Path)
irelativePartParser =
  iauthWithPathParser
    <|> ipathAbsoluteParser
    <|> ipathNoSchemeParser
    <|> ipathEmptyParser

-- iauthority = [ iuserinfo "@" ] ihost [ ":" port ]
iauthorityParser :: Parser Authority
iauthorityParser =
  Authority <$> optional (iuserInfoParser <* P.string "@")
    <*> ihostParser
    <*> optional (P.string ":" *> portParser)
    <?> "Authority"

-- iuserinfo = *( iunreserved / pct-encoded / sub-delims / ":" )
iuserInfoParser :: Parser UserInfo
iuserInfoParser = UserInfo . mconcat <$> P.many1 iuserInfoP
  where
    iuserInfoP = iunreservedP <|> pctEncodedParser <|> subDelimsP <|> P.string ":"

-- ihost = IP-literal / IPv4address / ireg-name
ihostParser :: Parser Host
ihostParser =
  Host <$> (ipLiteralParser <|> ipV4AddressParser <|> iregNameParser)
    <?> "Host"

-- ireg-name = *( iunreserved / pct-encoded / sub-delims )
iregNameParser :: Parser Text
iregNameParser = mconcat <$> P.many' (iunreservedP <|> pctEncodedParser <|> subDelimsP)

{-
ipath          = ipath-abempty   ; begins with "/" or is empty
                  / ipath-absolute  ; begins with "/" but not "//"
                  / ipath-noscheme  ; begins with a non-colon segment
                  / ipath-rootless  ; begins with a segment
                  / ipath-empty     ; zero characters
-}
-- [TODO]

-- ipath-abempty = *( "/" isegment )
ipathAbEmptyParser :: Parser Path
ipathAbEmptyParser = Path <$> ipathAbEmptyParser'

ipathAbEmptyParser' :: Parser Text
ipathAbEmptyParser' = mconcat <$> P.many' (mconcat <$> sequence [P.string "/", isegmentParser])

-- ipath-absolute = "/" [ isegment-nz *( "/" isegment ) ]
ipathAbsoluteParser :: Parser (Maybe Authority, Path)
ipathAbsoluteParser = (Nothing,) <$> (Path <$> ipathAbsoluteParser')

ipathAbsoluteParser' :: Parser Text
ipathAbsoluteParser' = mconcat <$> sequence [P.string "/", ipathRootlessParser']

-- ipath-noscheme = isegment-nz-nc *( "/" isegment )
ipathNoSchemeParser :: Parser (Maybe Authority, Path)
ipathNoSchemeParser = (Nothing,) <$> (Path <$> ipathNoSchemeParser')

ipathNoSchemeParser' :: Parser Text
ipathNoSchemeParser' = mconcat <$> sequence [isegmentNzNcParser, ipathAbEmptyParser']

-- ipath-rootless = isegment-nz *( "/" isegment )
ipathRootlessParser :: Parser (Maybe Authority, Path)
ipathRootlessParser = (Nothing,) <$> (Path <$> ipathRootlessParser')

ipathRootlessParser' :: Parser Text
ipathRootlessParser' = mconcat <$> sequence [isegmentNzParser, ipathAbEmptyParser']

-- ipath-empty = 0<ipchar>
ipathEmptyParser :: Parser (Maybe Authority, Path)
ipathEmptyParser = (Nothing, mempty) <$ ipathEmptyParser'

ipathEmptyParser' :: Parser Text
ipathEmptyParser' = P.string mempty <?> "Empty path"

-- isegment = *ipchar
isegmentParser :: Parser Text
isegmentParser = mconcat <$> (P.many' ipcharParser)

-- isegment-nz = 1*ipchar
isegmentNzParser :: Parser Text
isegmentNzParser = mconcat <$> (P.many1 ipcharParser)

-- isegment-nz-nc = 1*( iunreserved / pct-encoded / sub-delims / "@" )
--                ; non-zero-length segment without any colon ":"
isegmentNzNcParser :: Parser Text
isegmentNzNcParser = mconcat <$> (P.many1 _isegmentNzNcParser)
  where
    _isegmentNzNcParser = iunreservedP <|> pctEncodedParser <|> subDelimsP <|> P.string "@"

-- ipchar = iunreserved / pct-encoded / sub-delims / ":" / "@"
ipcharParser :: Parser Text
ipcharParser = iunreservedP <|> pctEncodedParser <|> subDelimsP <|> P.string ":" <|> P.string "@"

-- iquery = *( ipchar / iprivate / "/" / "?" )
iqueryParser :: Parser IRIQuery
iqueryParser = IRIQuery <$> iqueryParser'

iqueryParser' :: Parser Text
iqueryParser' =
  P.char '?' *> (mconcat <$> P.many' (ipcharParser <|> iprivateParser <|> P.string "/" <|> P.string "?"))
    <?> "Query"

-- ifragment = *( ipchar / "/" / "?" )
ifragmentParser :: Parser Fragment
ifragmentParser = Fragment <$> ifragmentParser'

ifragmentParser' :: Parser Text
ifragmentParser' =
  P.char '#' *> (mconcat <$> P.many' (ipcharParser <|> P.string "/" <|> P.string "?"))
    <?> "Fragment"

-- iunreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
iunreservedP :: Parser Text
iunreservedP = T.singleton <$> P.satisfy isIunreserved

isIunreserved :: Char -> Bool
isIunreserved c = isUnreserved c || isUcsChar c

-- ucschar = %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
--         / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
--         / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
--         / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
--         / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
--         / %xD0000-DFFFD / %xE1000-EFFFD
isUcsChar :: Char -> Bool
isUcsChar c =
  ('\x000A0' <= c && c <= '\x0D7FF')
    || ('\x0F900' <= c && c <= '\x0FDCF')
    || ('\x0FDF0' <= c && c <= '\x0FFEF')
    || ('\x10000' <= c && c <= '\x1FFFD')
    || ('\x20000' <= c && c <= '\x2FFFD')
    || ('\x30000' <= c && c <= '\x3FFFD')
    || ('\x40000' <= c && c <= '\x4FFFD')
    || ('\x50000' <= c && c <= '\x5FFFD')
    || ('\x60000' <= c && c <= '\x6FFFD')
    || ('\x70000' <= c && c <= '\x7FFFD')
    || ('\x80000' <= c && c <= '\x8FFFD')
    || ('\x90000' <= c && c <= '\x9FFFD')
    || ('\xA0000' <= c && c <= '\xAFFFD')
    || ('\xB0000' <= c && c <= '\xBFFFD')
    || ('\xC0000' <= c && c <= '\xCFFFD')
    || ('\xD0000' <= c && c <= '\xDFFFD')
    || ('\xE1000' <= c && c <= '\xEFFFD')

-- iprivate = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
iprivateParser :: Parser Text
iprivateParser = T.singleton <$> P.satisfy isIPrivate

isIPrivate :: Char -> Bool
isIPrivate c =
  ('\x00E000' <= c && c <= '\x00F8FF')
    || ('\x0F0000' <= c && c <= '\x0FFFFD')
    || ('\x100000' <= c && c <= '\x10FFFD')

-- scheme = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
schemeParser :: Parser Scheme
schemeParser =
  -- Force lower case (RFC page 25)
  Scheme . T.map toLower <$> (T.cons <$> schemeHead <*> schemeRest)
  where
    schemeHead = P.satisfy isAlpha <?> "Scheme head"
    schemeRest = P.takeWhile isSchemeTailChar <?> "Scheme tail"
    isSchemeTailChar c =
      isAlphaNum c
        || c == '+'
        || c == '.'
        || c == '_'
        || c == '-'

-- port = *DIGIT
portParser :: Parser Port
portParser = Port <$> portParser'

portParser' :: Parser Int
portParser' = P.decimal <?> "Port"

-- IP-literal = "[" ( IPv6address / IPvFuture  ) "]"
ipLiteralParser :: Parser Text
ipLiteralParser = P.string "[" *> (ipV6AddressParser <|> ipFutureParser) <* P.string "]"

-- IPvFuture = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )
ipFutureParser :: Parser Text
ipFutureParser =
  mconcat
    <$> sequence
      [ P.string "v",
        P.takeWhile1 isHexaDigit,
        P.string ".",
        P.takeWhile1 isValidFinalChar
      ]
  where
    isValidFinalChar c = isUnreserved c || isSubDelims c || c == ':'

-- IPv6address =                            6( h16 ":" ) ls32
--             /                       "::" 5( h16 ":" ) ls32
--             / [               h16 ] "::" 4( h16 ":" ) ls32
--             / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
--             / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
--             / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
--             / [ *4( h16 ":" ) h16 ] "::"              ls32
--             / [ *5( h16 ":" ) h16 ] "::"              h16
--             / [ *6( h16 ":" ) h16 ] "::"
ipV6AddressParser :: Parser Text
ipV6AddressParser =
  do
    l <- leadingP
    t <- trailingP l
    joinParts l t
    <?> "IPV6"
  where
    leadingP = h16 `P.sepBy` ":"
    trailingP = (id &&& length) >>> \l -> ipNotElided l <|> ipElided l
    joinParts leading trailing = pure $ (T.intercalate ":" leading) <> trailing
    h16 = parseBetween 1 4 (P.takeWhile isHexaDigit)
    ipNotElided (leading, lengthL) =
      guard (lengthL == 7 && isDecOctet (last leading)) *> partialIpV4
        <|> (guard (lengthL == 8) $> mempty)
    ipElided (_, lengthL) = do
      guard $ lengthL <= 8
      elision <- P.string "::"
      trailing <- h16 `P.sepBy` ":"
      let lengthT = length trailing
      let lengthTotal = lengthL + lengthT
      guard $ lengthT < 8
      embeddedIpV4 <-
        guard (lengthT > 0 && lengthTotal < 7 && isDecOctet (last trailing)) *> partialIpV4
          <|> pure mempty
      pure $ mconcat [elision, (T.intercalate ":" trailing), embeddedIpV4]
    partialIpV4 = mconcat <$> sequence [dotP, decOctetP, dotP, decOctetP, dotP, decOctetP]

-- h16 = 1*4HEXDIG
-- [TODO]

-- ls32 = ( h16 ":" h16 ) / IPv4address
-- [TODO]

-- IPv4address = dec-octet "." dec-octet "." dec-octet "." dec-octet
ipV4AddressParser :: Parser Text
ipV4AddressParser = mconcat <$> sequence [decOctetP, dotP, decOctetP, dotP, decOctetP, dotP, decOctetP]

-- dec-octet = DIGIT                 ; 0-9
--           / %x31-39 DIGIT         ; 10-99
--           / "1" 2DIGIT            ; 100-199
--           / "2" %x30-34 DIGIT     ; 200-249
--           / "25" %x30-35          ; 250-255
decOctetP :: Parser Text
decOctetP = do
  -- [TODO] 1-liner ?
  s <- P.takeWhile1 isDigit
  guard (isDecOctet s)
  pure s

isDecOctet :: Text -> Bool
isDecOctet s = len > 0 && T.all isDigit s && (len < 3 || (len == 3 && s <= "255"))
  where
    len = T.length s

-- pct-encoded = "%" HEXDIG HEXDIG
pctEncodedParser :: Parser Text
pctEncodedParser =
  T.cons <$> P.char '%'
    <*> (T.pack . fmap toUpper <$> (P.count 2 (P.satisfy isHexaDigit)))
    <?> "Percent encoding"

-- unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
isUnreserved :: Char -> Bool
isUnreserved c =
  isAlphaNum c
    || c == '-'
    || c == '.'
    || c == '_'
    || c == '~'

-- reserved = gen-delims / sub-delims
-- [TODO]

-- gen-delims = ":" / "/" / "?" / "#" / "[" / "]" / "@"

-- sub-delims     = "!" / "$" / "&" / "'" / "(" / ")"
--                / "*" / "+" / "," / ";" / "="
subDelimsP :: Parser Text
subDelimsP = T.singleton <$> P.satisfy isSubDelims

isSubDelims :: Char -> Bool
isSubDelims c = c `elem` ("!$&'()*+,;=" :: String)

-- "//" iauthority ipath-abempty
iauthWithPathParser :: Parser (Maybe Authority, Path)
iauthWithPathParser = do
  void (P.string "//")
  curry (first Just) <$> iauthorityParser <*> ipathAbEmptyParser

isHexaDigit :: Char -> Bool
isHexaDigit c =
  (isDigit c)
    || (c >= 'a' && c <= 'f')
    || (c >= 'A' && c <= 'F')

dotP :: Parser Text
dotP = P.string "."

parseBetween :: Int -> Int -> Parser Text -> Parser Text
parseBetween i j p = do
  s <- p
  let len = T.length s
  guard $ len >= i && len <= j
  return s
