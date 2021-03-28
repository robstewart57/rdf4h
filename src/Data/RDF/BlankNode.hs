module Data.RDF.BlankNode
  ( mkBNode,
  )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text (Parser, (<?>))
import qualified Data.Attoparsec.Text as P
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Text.Parser.Char
import Text.Parser.Combinators (option, try, unexpected)

-- Note: the NTriples parser combinators and Turtle parser combinators
-- should be shared, there's some duplication. This module could be
-- the place for those functions to be moved to, to reduce the size of
-- the NTriplesParser and TurtleParser modules.

-- mkBNode :: Text -> Either String String
-- mkBNode t = IRI . serializeIRI <$> parseIRI t

mkBNode :: Text -> Maybe String
mkBNode t =
  case parseBNodeLabel t of
    Left _e -> Nothing
    Right bString -> Just bString

parseBNodeLabel :: Text -> Either String String
parseBNodeLabel = P.parseOnly $ t_blank_node_label <* (P.endOfInput <?> "Unexpected characters at the end")

-- taken from TurtleParser (TurtleParser and NTriplesParser could
--possibly share this blank node label parser combinator?)
--
-- TODO replicate the recursion technique from [168s] for ((..)* something)?
-- [141s] BLANK_NODE_LABEL ::= '_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
-- t_blank_node_label :: (CharParsing m, Monad m) => m String
t_blank_node_label :: Parser String
t_blank_node_label = do
  void (string "_:")
  firstChar <- t_pn_chars_u <|> satisfy isDigit
  try $ (firstChar :) <$> otherChars
  where
    otherChars = option "" $ do
      xs <- many (t_pn_chars <|> char '.')
      if null xs
        then pure xs
        else
          if last xs == '.'
            then unexpected "'.' at the end of a blank node label"
            else pure xs

-- [163s] PN_CHARS_BASE
t_pn_chars_base :: CharParsing m => m Char
t_pn_chars_base = nt_pn_chars_base

-- [164s] PN_CHARS_U ::= PN_CHARS_BASE | '_'
t_pn_chars_u :: CharParsing m => m Char
t_pn_chars_u = t_pn_chars_base <|> char '_'

-- [166s] PN_CHARS ::= PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
t_pn_chars :: CharParsing m => m Char
t_pn_chars = t_pn_chars_u <|> char '-' <|> char '\x00B7' <|> satisfy f
  where
    f = flip in_range [('0', '9'), ('\x0300', '\x036F'), ('\x203F', '\x2040')]

-- [157s] PN_CHARS_BASE ::= [A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
nt_pn_chars_base :: CharParsing m => m Char
nt_pn_chars_base = try $ satisfy isBaseChar
  where
    isBaseChar c =
      (isAsciiUpper c)
        || (isAsciiLower c)
        || (c >= '\x00C0' && c <= '\x00D6')
        || (c >= '\x00D8' && c <= '\x00F6')
        || (c >= '\x00F8' && c <= '\x02FF')
        || (c >= '\x0370' && c <= '\x037D')
        || (c >= '\x037F' && c <= '\x1FFF')
        || (c >= '\x200C' && c <= '\x200D')
        || (c >= '\x2070' && c <= '\x218F')
        || (c >= '\x2C00' && c <= '\x2FEF')
        || (c >= '\x3001' && c <= '\xD7FF')
        || (c >= '\xF900' && c <= '\xFDCF')
        || (c >= '\xFDF0' && c <= '\xFFFD')
        || (c >= '\x10000' && c <= '\xEFFFF')

{-# INLINE in_range #-}
in_range :: Char -> [(Char, Char)] -> Bool
in_range c = any (\(c1, c2) -> c >= c1 && c <= c2)
