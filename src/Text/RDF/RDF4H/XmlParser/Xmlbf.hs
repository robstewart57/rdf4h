{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-
  Files Xmlbf and Xeno have been taken from:
  https://gitlab.com/k0001/xmlbf

  Which is licensed under Apache License 2.0.

  Justification
  ~~~~~~~~~~~~~

  The monad transformer in xmlbf introduced by:
    https://gitlab.com/k0001/xmlbf/merge_requests/5

  Enabled rdf4h to adopt the xmlbf library in favour of the existing
  arrows based XML RDF parser, which failed many of the tests in
  the W3C rdf-tests repository.

  It was later decided to remove the monad transformer:
    https://gitlab.com/k0001/xmlbf/issues/25

  But this happened before a release was made, so rdf4h could not
  depend on any version of xmlbf for this monad transformer.

  Future plans
  ~~~~~~~~~~~~

  Ideally, rdf4h should depend on the xmlbf and xmlbf-xeno libraries,
  rather than having this file and and the Xeno file in this rdf4h
  repository. For that, either:

  1. the monad transformer should be re-added to xmlbf

  2. use the StateT transformer on top of xmlbf as suggested in
     https://gitlab.com/k0001/xmlbf/issues/25#note_178094971

     This has been tried, but the resulting implementation fails many
     rdf-tests W3C tests. See:
     https://github.com/robstewart57/rdf4h/tree/statet-rdfxml
-}

-- | XML back and forth!
--
-- @xmlbf@ provides high-level tools for encoding and decoding XML.
--
-- @xmlbf@ provides tools like 'dfpos' and 'dfposM' for finding a fixpoint
-- of an XML fragment.
--
-- @xmlbf@ provides 'FromXml' and 'ToXml' typeclasses intended to be used as the
-- familiar 'Data.Aeson.FromJSON' and 'Data.Aeson.ToXml' from the @aeson@
-- package.
--
-- @xmlbf@ doesn't do any parsing of raw XML on its own. Instead, one should
-- use @xmlbf@ together with libraries like
-- [xmlbf-xeno](https://hackage.haskell.org/package/xmlbf-xeno) or
-- [xmlbf-xmlhtml](https://hackage.haskell.org/package/xmlbf-xmlhtml) for
-- this.
module Text.RDF.RDF4H.XmlParser.Xmlbf {--}
 ( -- * Parsing
   parse
 , parseM
   -- ** Low-level
 , ParserT
 , parserT
 , runParserT
 , ParserState
 , initialParserState

   -- * Parsers
 , pElement
 , pAnyElement
 , pName
 , pAttr
 , pAttrs
 , pChildren
 , pText
 , pEndOfInput

    -- * Rendering
 , encode

   -- * Nodes
 , Node
 , node

 , pattern Element
 , element
 , element'

 , pattern Text
 , text
 , text'

   -- * Fixpoints
 , dfpos
 , dfposM
 , dfpre
 , dfpreM

   -- * Typeclasses
 , FromXml(fromXml)
 , ToXml(toXml)
 ) --}
 where

import Control.Applicative (Alternative(empty, (<|>)), liftA2)
import Control.DeepSeq (NFData(rnf))
import Control.Monad (MonadPlus(mplus, mzero), join, when, ap)
import qualified Control.Monad.Catch as Ex
import Control.Monad.Error.Class (MonadError(catchError, throwError))
import Control.Monad.Cont (MonadCont(callCC))
import qualified Control.Monad.Fail
import Control.Monad.Fix (MonadFix(mfix))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Reader.Class (MonadReader(local, ask))
import Control.Monad.State.Class (MonadState(state))
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Zip (MonadZip(mzipWith))
import Control.Selective (Selective(select))
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Prim as BBP
import qualified Data.Char as Char
import Data.Foldable (for_, toList)
import Data.Functor.Identity (Identity(Identity), runIdentity)
import qualified Data.HashMap.Strict as HM
import Data.Kind (Type)
#if MIN_VERSION_base(4,9,0)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#else
#endif
#else
#endif
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Traversable (for)
import Data.Word (Word8)

--------------------------------------------------------------------------------

-- | Either a text or an element node in an XML fragment body.
--
-- Construct with 'text' or 'element'. Destruct with 'Text' or 'Element'.
data Node
  = Element' !T.Text !(HM.HashMap T.Text T.Text) ![Node]
  | Text' !TL.Text
  deriving (Eq)

instance NFData Node where
  rnf = \case
    Element' n as cs -> rnf n `seq` rnf as `seq` rnf cs `seq` ()
    Text' t -> rnf t `seq` ()
  {-# INLINABLE rnf #-}

instance Show Node where
  showsPrec n = \x -> showParen (n > 10) $ case x of
    Text' t -> showString "Text " . showsPrec 0 t
    Element' t as cs ->
      showString "Element " .
      showsPrec 0 t . showChar ' ' .
      showsPrec 0 (HM.toList as) . showChar ' ' .
      showsPrec 0 cs

-- | Destruct an element 'Node'.
pattern Element :: T.Text -> HM.HashMap T.Text T.Text -> [Node] -> Node
pattern Element t as cs <- Element' t as cs
#if MIN_VERSION_base(4,10,0)
{-# COMPLETE Element #-} -- TODO this leads to silly pattern matching warnings
#endif
  
-- | Destruct a text 'Node'.
pattern Text :: TL.Text -> Node
pattern Text t <- Text' t
#if MIN_VERSION_base(4,10,0)
{-# COMPLETE Text #-} -- TODO this leads to silly pattern matching warnings
#endif
  
-- | Case analysis for a 'Node'.
node
  :: (T.Text -> HM.HashMap T.Text T.Text -> [Node] -> a)
  -- ^ Transform an 'Element' node.
  -> (TL.Text -> a)
  -- ^ Transform a 'Text' node.
  -> Node
  -> a
{-# INLINE node #-}
node fe ft = \case
  Text' t -> ft t
  Element' t as cs -> fe t as cs

-- | Normalizes 'Node's by concatenating consecutive 'Text' nodes.
normalize :: [Node] -> [Node]
{-# INLINE normalize #-}
normalize = \case
   -- Note that @'Text' ""@ is forbidden by construction, actually. But we do
   -- take care of it in case the 'Node' was constructed unsafely somehow.
   Text' "" : ns -> normalize ns
   Text' a : Text' b : ns -> normalize (text (a <> b) <> ns)
   Text' a : ns -> Text' a : normalize ns
   Element' t as cs : ns -> Element' t as (normalize cs) : normalize ns
   [] -> []

-- | Construct a XML fragment body containing a single 'Text' 'Node', if
-- possible.
--
-- This function will return empty list if it is not possible to construct the
-- 'Text' with the given input. To learn more about /why/ it was not possible to
-- construct it, use 'text'' instead.
--
-- Using 'text'' rather than 'text' is recommended, so that you are forced to
-- acknowledge a failing situation in case it happens. However, 'text' is at
-- times more convenient to use. For example, when you know statically the input
-- is valid.
text
  :: TL.Text  -- ^ Lazy 'TL.Text'.
  -> [Node]
{-# INLINE text #-}
text t = case text' t of
  Right x -> [x]
  Left _  -> []

-- | Construct a 'Text' 'Node', if possible.
--
-- Returns 'Left' if the 'Text' 'Node' can't be created, with an explanation
-- of why.
text'
  :: TL.Text  -- ^ Lazy 'TL.Text'.
  -> Either String Node
{-# INLINE text' #-}
text' = \case
  "" -> Left "Empty text"
  t  -> Right (Text' t)

-- | Construct a XML fragment body containing a single 'Element' 'Node', if
-- possible.
--
-- This function will return empty list if it is not possible to construct the
-- 'Element' with the given input. To learn more about /why/ it was not possible
-- to construct it, use 'element' instead.
--
-- Using 'element'' rather than 'element' is recommended, so that you are forced
-- to acknowledge a failing situation in case it happens. However, 'element' is
-- at times more convenient to use, whenever you know the input is valid.
element
  :: T.Text                   -- ^ Element' name as a strict 'T.Text'.
  -> HM.HashMap T.Text T.Text -- ^ Attributes as strict 'T.Text' pairs.
  -> [Node]                   -- ^ Children.
  -> [Node]
{-# INLINE element #-}
element t hm ns = case element' t hm ns of
  Right x -> [x]
  Left  _ -> []

-- | Construct an 'Element' 'Node'.
--
-- Returns 'Left' if the 'Element' 'Node' can't be created, with an explanation
-- of why.
element'
  :: T.Text                   -- ^ Element' name as a strict 'T.Text'.
  -> HM.HashMap T.Text T.Text -- ^ Attributes as strict 'T.Text' pairs.
  -> [Node]                   -- ^ Children.
  -> Either String Node
element' t0 hm0 ns0 = do
  when (t0 /= T.strip t0)
     (Left ("Element name has surrounding whitespace: " ++ show t0))
  when (T.null t0)
     (Left ("Element name is blank: " ++ show t0))
  for_ (HM.keys hm0) $ \k -> do
     when (k /= T.strip k)
        (Left ("Attribute name has surrounding whitespace: " ++ show k))
     when (T.null k)
        (Left ("Attribute name is blank: " ++ show k))
  Right (Element' t0 hm0 (normalize ns0))

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Parsing

class FromXml a where
  -- | Parses an XML fragment body into a value of type @a@.
  --
  -- If a 'ToXml' instance for @a@ exists, then:
  --
  -- @
  -- 'parseM' 'fromXml' ('toXml' a) == pure ('Right' a)
  -- @
  fromXml :: ParserT m a

-- | Internal parser state.
data ParserState
  = STop ![Node]
    -- ^ Parsing the top-level nodes.
  | SReg !T.Text !(HM.HashMap T.Text T.Text) ![Node]
    -- ^ Parsing a particular root element.

-- | Construct an initial 'ParserState' to use with 'runParserT' from zero or
-- more top-level 'Node's.
initialParserState :: [Node] -> ParserState
initialParserState = STop . normalize
{-# INLINE initialParserState #-}

-- | XML parser for a value of type @a@.
--
-- This parser runs on top of some 'Monad' @m@,
-- making 'ParserT' a suitable monad transformer.
--
-- You can build a 'ParserT' using 'pElement', 'pAnyElement', 'pName',
-- 'pAttr', 'pAttrs', 'pChildren', 'pText', 'pEndOfInput', any of the
-- 'Applicative', 'Alternative' or 'Monad' combinators, or you can
-- use 'parserT' directly.
--
-- Run a 'ParserT' using 'parse', 'parseM' or 'runParserT'
newtype ParserT (m :: Type -> Type) (a :: Type)
  = ParserT (ParserState -> m (ParserState, Either String a))

-- | 'parserT' is the most general way or building a 'ParserT'.
parserT
  :: (ParserState -> m (ParserState, Either String a))
  -- ^ Given a parser's internal state, obtain an @a@ if possible, otherwise
  -- return a 'String' describing the parsing failure. A new state with
  -- leftovers is returned.
  -> ParserT m a
parserT = ParserT
{-# INLINE parserT #-}

-- | 'runParserT' is the most general way or running a 'ParserT'.
runParserT
  :: ParserT m a
  -- ^ Parser to run.
  -> ParserState
  -- ^ Initial parser state. You can obtain this from
  -- 'initialParserState' or from a previous execution of 'runParserT'.
  -> m (ParserState, Either String a)
  -- ^ Returns the leftover parser state, as well as an @a@ in case parsing was
  -- successful, or a 'String' with an error message otherwise.
runParserT (ParserT f) = f
{-# INLINE runParserT #-}

-- | Run a 'ParserT' on an XML fragment body.
--
-- Notice that this function doesn't enforce that all input is consumed. If you
-- want that behavior, then please use 'pEndOfInput' in the given 'ParserT'.
--
-- As a simpler alternative to 'runParserT', consider using 'parse' if you don't
-- need transformer functionality. 'parseM' is implemented on top of the more
-- general 'runParserT'.
parseM
  :: Applicative m
  => ParserT m a
  -- ^ Parser to run.
  -> [Node]
  -- ^ XML fragment body to parse. That is, top-level XML 'Node's.
  -> m (Either String a)
  -- ^ If parsing fails, a 'String' with an error message is returned.
  -- Otherwise, we the parser output @a@ is returned.
parseM p = fmap snd . runParserT p . initialParserState
{-# INLINE parseM #-}

-- | Pure version of 'parseM' running on top of 'Identity'.
parse
  :: ParserT Identity a
  -- ^ Parser to run.
  -> [Node]
  -- ^ XML fragment body to parse. That is, top-level XML 'Node's.
  -> Either String a
  -- ^ If parsing fails, a 'String' with an error message is returned.
  -- Otherwise, we the parser output @a@ is returned.
parse p = runIdentity . parseM p
{-# INLINE parse #-}

#if MIN_VERSION_base(4,9,0)
instance (Monad m, Semigroup a) => Semigroup (ParserT m a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}
#endif

#if MIN_VERSION_base(4,9,0)
instance (Monad m, Monoid a, Semigroup a) => Monoid (ParserT m a) where
#else
instance (Monad m, Monoid a) => Monoid (ParserT m a) where
#endif
  mempty = pure mempty
  {-# INLINE mempty #-}
#if MIN_VERSION_base(4,9,0)
  mappend = (<>)
#else
  mappend = liftA2 mappend
#endif
  {-# INLINE mappend #-}

instance Functor m => Functor (ParserT m) where
  fmap f = \pa -> ParserT (\s -> fmap (fmap (fmap f)) (runParserT pa s))
  {-# INLINE fmap #-}

-- | The 'Monad' superclass is necessary because 'ParserT' shortcircuits like
-- 'Control.Monad.Trans.Except.ExceptT'.
instance Monad m => Applicative (ParserT m) where
  pure = \a -> ParserT (\s -> pure (s, Right a))
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

-- | @ma '<|>' mb@ backtracks the internal parser state before running @mb@.
instance Monad m => Alternative (ParserT m) where
  empty = pFail "empty"
  {-# INLINE empty #-}
  pa <|> pb = ParserT (\s0 -> do
    (s1, ea) <- runParserT pa s0
    case ea of
      Right a -> pure (s1, Right a)
      Left _ -> runParserT pb s0)
  {-# INLINABLE (<|>) #-}

instance Monad m => Selective (ParserT m) where
  select pe pf = ParserT (\s0 -> do
    (s1, eeab) <- runParserT pe s0
    case eeab of
      Right (Right b) -> pure (s1, Right b)
      Right (Left a) -> runParserT (pf <*> pure a) s1
      Left msg -> pure (s1, Left msg))
  {-# INLINABLE select #-}

instance Monad m => Monad (ParserT m) where
  return = pure
  {-# INLINE return #-}
  pa >>= kpb = ParserT (\s0 -> do
    (s1, ea) <- runParserT pa s0
    case ea of
      Right a -> runParserT (kpb a) s1
      Left msg -> pure (s1, Left msg))
  {-# INLINABLE (>>=) #-}
#if !MIN_VERSION_base(4,13,0)
  fail = pFail
  {-# INLINE fail #-}
#endif

#if MIN_VERSION_base(4,9,0)
instance Monad m => Control.Monad.Fail.MonadFail (ParserT m) where
  fail = pFail
  {-# INLINE fail #-}
#endif

-- | A 'ParserT' that always fails with the given error message.
pFail :: Applicative m => String -> ParserT m a
pFail = \msg -> ParserT (\s -> pure (s, Left msg))
{-# INLINE pFail #-}

-- | @'mzero' ma mb@ backtracks the internal parser state before running @mb@.
instance Monad m => MonadPlus (ParserT m) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus = (<|>)
  {-# INLINE mplus #-}

instance MonadFix m => MonadFix (ParserT m) where
  mfix f =
    let die = \msg -> error ("mfix (ParserT): " <> msg)
    in ParserT (\s0 ->
         mfix (\ ~(_s1, ea) -> runParserT (f (either die id ea)) s0))

instance MonadZip m => MonadZip (ParserT m) where
  mzipWith f pa pb = ParserT (\s0 -> do
    (s1, ea) <- runParserT pa s0
    case ea of
      Right a0 ->
        mzipWith (\a1 (s2, eb) -> (s2, fmap (f a1) eb))
                 (pure a0) (runParserT pb s1)
      Left msg -> pure (s1, Left msg))
  {-# INLINABLE mzipWith #-}

instance MonadTrans ParserT where
  lift = \ma -> ParserT (\s -> ma >>= \a -> pure (s, Right a))
  {-# INLINE lift #-}

instance MFunctor ParserT where
  hoist nat = \p -> ParserT (\s -> nat (runParserT p s))
  {-# INLINE hoist #-}

instance MonadIO m => MonadIO (ParserT m) where
  liftIO = lift . liftIO
  {-# INLINE liftIO #-}

instance MonadReader r m => MonadReader r (ParserT m) where
  ask = lift ask
  {-# INLINE ask #-}
  local f = \p -> ParserT (\s -> local f (runParserT p s))
  {-# INLINE local #-}

instance MonadState s m => MonadState s (ParserT m) where
  state = lift . state
  {-# INLINE state #-}

instance MonadError e m => MonadError e (ParserT m) where
  throwError = lift . throwError
  {-# INLINABLE throwError #-}
  catchError ma h = ParserT (\s ->
    catchError (runParserT ma s)
               (\e -> runParserT (h e) s))
  {-# INLINABLE catchError #-}

instance Ex.MonadThrow m => Ex.MonadThrow (ParserT m) where
  throwM = lift . Ex.throwM
  {-# INLINABLE throwM #-}

instance Ex.MonadCatch m => Ex.MonadCatch (ParserT m) where
  catch ma h = ParserT (\s ->
    Ex.catch (runParserT ma s)
             (\e -> runParserT (h e) s))
  {-# INLINABLE catch #-}

instance Ex.MonadMask m => Ex.MonadMask (ParserT m) where
  mask f = ParserT (\s ->
    Ex.mask (\u ->
      runParserT (f (\p -> ParserT (u . runParserT p))) s))
  {-# INLINABLE mask #-}
  uninterruptibleMask f = ParserT (\s ->
    Ex.uninterruptibleMask (\u ->
      runParserT (f (\p -> ParserT (u . runParserT p))) s))
  {-# INLINABLE uninterruptibleMask #-}
  generalBracket acq rel use = ParserT (\s0 -> do
    ((_sb,eb), (sc,ec)) <- Ex.generalBracket
      (runParserT acq s0)
      (\(s1, ea) ec -> case ea of
          Right a -> case ec of
            Ex.ExitCaseSuccess (s2, Right b) ->
              runParserT (rel a (Ex.ExitCaseSuccess b)) s2
            Ex.ExitCaseSuccess (s2, Left msg) ->
              -- Result of using mzero or similar on release
              pure (s2, Left msg)
            Ex.ExitCaseException e ->
              runParserT (rel a (Ex.ExitCaseException e)) s1
            Ex.ExitCaseAbort ->
              runParserT (rel a Ex.ExitCaseAbort) s1
          Left msg ->
            -- acq failed, nothing to release
            pure (s1, Left msg))
      (\(s1, ea) -> case ea of
          Right a -> runParserT (use a) s1
          Left msg ->
            -- acq failed, nothing to use
            pure (s1, Left msg))
    -- We run ec first because its error message, if any, has priority
    pure (sc, flip (,) <$> ec <*> eb))

-- | This version uses the current state on entering the continuation. See
-- 'liftCallCC''.  It does not satisfy the uniformity property (see
-- 'Control.Monad.Signatures.CallCC').
instance MonadCont m => MonadCont (ParserT m) where
  callCC f = ParserT (\s0 ->
    callCC (\c -> runParserT (f (\a -> ParserT (\s1 -> c (s1, Right a)))) s0))

--------------------------------------------------------------------------------
-- Some parsers

-- | @'pElement' "foo" p@ runs a 'ParserT' @p@ inside a 'Element' node named
-- @"foo"@. This parser __fails__ if such element does not exist at the current
-- position.
--
-- Leading whitespace is ignored. If you need to preserve that whitespace for
-- some reason, capture it using 'pText' before using 'pElement'.
--
-- __Consumes the matched element__ from the parser state.
pElement
  :: Monad m
  => T.Text       -- ^ Element name as strict 'T.Text'.
  -> ParserT m a  -- ^ 'ParserT' to run /inside/ the matched 'Element'.
  -> ParserT m a
pElement t0 p0 = ParserT $ \case
  SReg t1 as0 (Element' t as cs : cs0) | t == t0 ->
    runParserT p0 (SReg t as cs) >>= \case
      (_, Right a) -> pure (SReg t1 as0 cs0, Right a)
      (s1, Left msg) -> pure (s1, Left msg)
  STop (Element' t as cs : cs0) | t == t0 ->
    runParserT p0 (SReg t as cs) >>= \case
      (_, Right a) -> pure (STop cs0, Right a)
      (s1, Left msg) -> pure (s1, Left msg)
  -- skip leading whitespace
  SReg t as (Text' x : cs) | TL.all Char.isSpace x ->
    runParserT (pElement t0 p0) (SReg t as cs)
  STop (Text' x : cs) | TL.all Char.isSpace x ->
    runParserT (pElement t0 p0) (STop cs)
  s0 -> pure (s0, Left ("Missing element " <> show t0))
{-# INLINABLE pElement #-}

-- | @'pAnyElement' p@ runs a 'ParserT' @p@ inside the 'Element' node at the
-- current position, if any. Otherwise, if no such element exists, this parser
-- __fails__.
--
-- You can recover the name of the matched element using 'pName' inside the
-- given 'ParserT'. However, if you already know beforehand the name of the
-- element that you want to match, it's better to use 'pElement' rather than
-- 'pAnyElement'.
--
-- Leading whitespace is ignored. If you need to preserve that whitespace for
-- some reason, capture it using 'pText' before using 'pAnyElement'.
--
-- __Consumes the matched element__ from the parser state.
pAnyElement
  :: Monad m
  => ParserT m a  -- ^ 'ParserT' to run /inside/ any matched 'Element'.
  -> ParserT m a
pAnyElement p0 = ParserT $ \case
  SReg t0 as0 (Element' t as cs : cs0) ->
    runParserT p0 (SReg t as cs) >>= \case
      (_, Right a) -> pure (SReg t0 as0 cs0, Right a)
      (s1, Left msg) -> pure (s1, Left msg)
  STop (Element' t as cs : cs0) ->
    runParserT p0 (SReg t as cs) >>= \case
      (_, Right a) -> pure (STop cs0, Right a)
      (s1, Left msg) -> pure (s1, Left msg)
  -- skip leading whitespace
  SReg t as (Text' x : cs) | TL.all Char.isSpace x ->
    runParserT (pAnyElement p0) (SReg t as cs)
  STop (Text' x : cs) | TL.all Char.isSpace x ->
    runParserT (pAnyElement p0) (STop cs)
  s0 -> pure (s0, Left "Missing element")
{-# INLINABLE pAnyElement #-}

-- | Returns the name of the currently selected 'Element'.
--
-- This parser __fails__ if there's no currently selected 'Element' (see
-- 'pElement', 'pAnyElement').
--
-- Doesn't modify the parser state.
pName
  :: Applicative m
  => ParserT m T.Text -- ^ Element name as strict 'T.Text'.
pName = ParserT (\s -> case s of
  SReg t _ _ -> pure (s, Right t)
  _ -> pure (s, Left "Before selecting an name, you must select an element"))
{-# INLINABLE pName #-}

-- | Return the value of the requested attribute, if defined. Returns an
-- 'T.empty' 'T.Text' in case the attribute is defined but no value was given to
-- it.
--
-- This parser __fails__ if there's no currently selected 'Element' (see
-- 'pElement', 'pAnyElement').
--
-- __Consumes the matched attribute__ from the parser state.
pAttr
  :: Applicative m
  => T.Text
  -- ^ Attribute name as strict 'T.Text'.
  -> ParserT m T.Text
  -- ^ Attribute value as strict 'T.Text', possibly 'T.empty'.
pAttr n = ParserT (\s -> case s of
  SReg t as cs -> case HM.lookup n as of
    Just x -> pure (SReg t (HM.delete n as) cs, Right x)
    Nothing -> pure (s, Left ("Missing attribute " <> show n))
  _ -> pure (s, Left "Before selecting an attribute, you must select an element"))
{-# INLINABLE pAttr #-}

-- | Returns all of the available element attributes.
--
-- Returns 'T.empty' 'T.Text' as values in case an attribute is defined but no
-- value was given to it.
--
-- This parser __fails__ if there's no currently selected 'Element' (see
-- 'pElement', 'pAnyElement').
--
-- __Consumes all the attributes__ for this element from the parser state.
pAttrs
  :: Applicative m
  => ParserT m (HM.HashMap T.Text T.Text)
  -- ^ Pairs of attribute names and possibly 'T.empty' values, as strict
  -- 'T.Text'.
pAttrs = ParserT (\s -> case s of
  SReg t as cs -> pure (SReg t mempty cs, Right as)
  _ -> pure (s, Left "Before selecting an attribute, you must select an element"))
{-# INLINABLE pAttrs #-}

-- | Returns all of the immediate children of the current element.
--
-- If parsing top-level nodes rather than a particular element (that is, if
-- 'pChildren' is /not/ being run inside 'pElement'), then all of the top level
-- 'Node's will be returned.
--
-- __Consumes all the returned nodes__ from the parser state.
pChildren
  :: Applicative m
  => ParserT m [Node] -- ^ 'Node's in their original order.
pChildren = ParserT (\case
  STop cs -> pure (STop mempty, Right cs)
  SReg t as cs -> pure (SReg t as mempty, Right cs))
{-# INLINABLE pChildren #-}

-- | Returns the contents of a 'Text' node.
--
-- Surrounidng whitespace is not removed, as it is considered to be part of the
-- text node.
--
-- If there is no text node at the current position, then this parser __fails__.
-- This implies that 'pText' /never/ returns an empty 'TL.Text', since there is
-- no such thing as a text node without text.
--
-- Please note that consecutive text nodes are always concatenated and returned
-- together.
--
-- @
-- 'parseT' 'pText' ('text' \"Ha\" <> 'text' \"sk\" <> 'text' \"ell\")
--     == 'pure' ('Right' ('text' "Haskell"))
-- @
--
-- __Consumes the text__ from the parser state. This implies that if you
-- perform two consecutive 'pText' calls, the second will always fail.
--
-- @
-- 'parseT' ('pText' >> 'pText') ('text' \"Ha\" <> 'text' \"sk\" <> 'text' \"ell\")
--     == 'pure' ('Left' "Missing text node")
-- @
pText
  :: Applicative m
  => ParserT m TL.Text
  -- ^ Content of the text node as a lazy 'TL.Text'.
pText = ParserT (\case
  -- Note: this works only because we asume 'normalize' has been used.
  STop (Text x : ns) -> pure (STop ns, Right x)
  SReg t as (Text x : cs) -> pure (SReg t as cs, Right x)
  s0 -> pure (s0, Left "Missing text node"))
{-# INLINABLE pText #-}

-- | Succeeds if all of the elements, attributes and text nodes have
-- been consumed.
pEndOfInput :: Applicative m => ParserT m ()
pEndOfInput = ParserT (\s -> case isEof s of
  True -> pure (s, Right ())
  False -> pure (s, Left "Not end of input yet"))
{-# INLINABLE pEndOfInput #-}

isEof :: ParserState -> Bool
isEof = \case
  SReg _ as cs -> HM.null as && null cs
  STop ns -> null ns
{-# INLINE isEof #-}

--------------------------------------------------------------------------------
-- Rendering

class ToXml a where
  -- | Renders a value of type @a@ into an XML fragment body.
  --
  -- If a 'FromXml' instance for @a@ exists, then:
  --
  -- @
  -- 'parseM' 'fromXml' ('toXml' a) == 'pure' ('Right' a)
  -- @
  toXml :: a -> [Node]

-- | Encodes a list of XML 'Node's, representing an XML fragment body, to an
-- UTF8-encoded and XML-escaped bytestring.
--
-- This function doesn't render self-closing elements. Instead, all
-- elements have a corresponding closing tag.
--
-- Also, it doesn't render CDATA sections. Instead, all text is escaped as
-- necessary.
encode :: [Node] -> BB.Builder
encode xs = mconcat (map encodeNode xs)
  where
    encodeNode :: Node -> BB.Builder
    encodeNode = \case
      Text x -> encodeXmlUtf8Lazy x
      Element t as cs ->
         -- This ugly code is so that we make sure we always bind concatenation
         -- to the right with as little effort as possible, using (<>).
         "<" <> encodeUtf8 t
             <> encodeAttrs (">" <> encode cs <> "</" <> encodeUtf8 t <> ">") as
    {-# INLINE encodeNode #-}
    encodeAttrs :: BB.Builder -> HM.HashMap T.Text T.Text -> BB.Builder
    encodeAttrs = HM.foldlWithKey'
      (\o k v -> " " <> encodeUtf8 k <> "=\"" <> encodeXmlUtf8 v <> "\"" <> o)
    {-# INLINE encodeAttrs #-}

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Node fixpoint

-- | Post-order depth-first replacement of 'Node' and all of its children.
--
-- This function works like 'Data.Function.fix', but the given function is
-- trying to find a fixpoint for the individual children nodes, not for the root
-- node.
--
-- For example, the following function renames every node named @"w"@ to @"y"@,
-- and every node named @"y"@ to @"z"@. It accomplishes this by first renaming
-- @"w"@ nodes to @"x"@, and then, by using @k@ recursively to further rename
-- all @"x"@ nodes (including the ones that were just created) to @"y"@ in a
-- post-order depth-first manner. After renaming an @"x"@ node to @"y"@, the
-- recursion stops (i.e., @k@ is not used), so our new @"y"@ nodes won't be
-- further renamed to @"z"@. However, nodes that were named @"y"@ initially will
-- be renamed to @"z"@.
--
-- In our example we only replace one node with another, but a node can be
-- replaced with zero or more nodes, depending on the length of the resulting
-- list.
--
-- @
-- foo :: 'Node' -> ['Node']
-- foo = 'dfpos' $ \\k -> \\case
--     'Element' "w" as cs -> 'element' "x" as cs >>= k
--     'Element' "x" as cs -> 'element' "y" as cs
--     'Element' "y" as cs -> 'element' "z" as cs >>= k
-- @
--
-- See 'dfpre' for pre-orderd depth-first replacement.
--
-- /WARNING/ If you call @k@ in every branch, then 'dfpos' will never terminate.
-- Make sure the recursion stops at some point by simply returning a list of
-- nodes instead of calling @k@.
dfpos :: ((Node -> [Node]) -> Node -> [Node]) -> Node -> [Node]
dfpos f = runIdentity . dfposM (\k -> Identity . f (runIdentity . k))

-- | Monadic version of 'dfpos'.
dfposM :: Monad m => ((Node -> m [Node]) -> Node -> m [Node]) -> Node -> m [Node]
dfposM f = \n0 -> do
  c1 <- traverseChildren (dfposM f) (cursorFromNode n0)
  c2 <- traverseRightSiblings (dfposM f) c1
  fmap (normalize . join)
       (traverse (f (dfposM f)) (cursorSiblings c2))


-- | Pre-order depth-first replacement of 'Node' and all of its children.
--
-- This is just like 'dfpos' but the search proceeds in a different order.
dfpre :: ((Node -> [Node]) -> Node -> [Node]) -> Node -> [Node]
dfpre f = runIdentity . dfpreM (\k -> Identity . f (runIdentity . k))

-- | Monadic version of 'dfpre'.
dfpreM :: Monad m => ((Node -> m [Node]) -> Node -> m [Node]) -> Node -> m [Node]
dfpreM f = \n0 -> do
  ns <- f (dfpreM f) n0
  fmap (normalize . join) $ for ns $ \n -> do
     c1 <- traverseChildren (dfpreM f) (cursorFromNode n)
     cursorSiblings <$> traverseRightSiblings (dfpreM f) c1


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- INTERNAL: Cursor
--
-- Most of this comes from Chris Smith's xmlhtml, BSD3 licensed
-- https://hackage.haskell.org/package/xmlhtml

-- | Zipper into a 'Node' tree.
data Cursor = Cursor
  { _cursorCurrent :: !Node
    -- ^ Retrieves the current node of a 'Cursor'.
  , _cursorLefts :: !(Seq Node)
    -- ^ Nodes to the left (ordered right to left).
  , _cursorRights :: !(Seq Node)
    -- ^ Nodes to the right (ordered left to right).
  , _cursorParents :: !(Seq (Seq Node, T.Text, HM.HashMap T.Text T.Text, Seq Node))
    -- ^ Parents' name, attributes, and siblings.
  }

------------------------------------------------------------------------------

-- | The cursor if left where it starts.
traverseChildren :: Monad m => (Node -> m [Node]) -> Cursor -> m Cursor
{-# INLINABLE traverseChildren #-}
traverseChildren f c0 = case _cursorCurrent c0 of
  Text _ -> pure c0
  Element t as cs -> do
     n1s <- fmap (normalize . join) (traverse f cs)
     pure (c0 {_cursorCurrent = Element' t as n1s})

-- | The cursor if left in the rightmost sibling.
traverseRightSiblings :: Monad m => (Node -> m [Node]) -> Cursor -> m Cursor
{-# INLINABLE traverseRightSiblings #-}
traverseRightSiblings f c0 = case cursorRemoveRight c0 of
   Nothing -> pure c0
   Just (n1, c1) -> do
      n2s <- fmap normalize (f n1)
      traverseRightSiblings f (cursorInsertManyRight n2s c1)

-- | Builds a 'Cursor' for navigating a tree. That is, a forest with a single
-- root 'Node'.
cursorFromNode :: Node -> Cursor
{-# INLINE cursorFromNode #-}
cursorFromNode n = Cursor n mempty mempty mempty

-- | Retrieves a list of the 'Node's at the same level as the current position
-- of a cursor, including the current node.
cursorSiblings :: Cursor -> [Node]
{-# INLINE cursorSiblings #-}
cursorSiblings (Cursor cur ls rs _) =
  toList (Seq.reverse ls <> (cur Seq.<| rs))

-- | Removes the node to the right and return it.
cursorRemoveRight :: Cursor -> Maybe (Node, Cursor)
{-# INLINABLE cursorRemoveRight #-}
cursorRemoveRight = \case
  Cursor n ls rs0 ps | not (Seq.null rs0) ->
     case Seq.viewl rs0 of
        r Seq.:< rs -> Just (r, Cursor n ls rs ps)
        _ -> undefined -- unreachable, rs0 is not empty
  _ -> Nothing

-- | Inserts a list of new 'Node's to the right of the current position.
cursorInsertManyRight :: [Node] -> Cursor -> Cursor
{-# INLINE cursorInsertManyRight #-}
cursorInsertManyRight ns (Cursor nn ls rs ps) =
  Cursor nn ls (Seq.fromList ns <> rs) ps

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Miscellaneous

encodeUtf8 :: T.Text -> BB.Builder
{-# INLINE encodeUtf8 #-}
encodeUtf8 = T.encodeUtf8Builder

encodeXmlUtf8 :: T.Text -> BB.Builder
{-# INLINE encodeXmlUtf8 #-}
encodeXmlUtf8 = T.encodeUtf8BuilderEscaped xmlEscaped

encodeXmlUtf8Lazy :: TL.Text -> BB.Builder
{-# INLINE encodeXmlUtf8Lazy #-}
encodeXmlUtf8Lazy = TL.encodeUtf8BuilderEscaped xmlEscaped

xmlEscaped :: BBP.BoundedPrim Word8
{-# INLINE xmlEscaped #-}
xmlEscaped =
   BBP.condB (== 38) (fixed5 (38,(97,(109,(112,59))))) $  -- '&'  ->  "&amp;"
   BBP.condB (== 60) (fixed4 (38,(108,(116,59)))) $       -- '<'  ->  "&lt;"
   BBP.condB (== 62) (fixed4 (38,(103,(116,59)))) $       -- '>'  ->  "&gt;"
   BBP.condB (== 34) (fixed5 (38,(35,(51,(52,59))))) $    -- '"'  ->  "&#34;"
   BBP.liftFixedToBounded BBP.word8
 where
   {-# INLINE fixed4 #-}
   fixed4 :: (Word8, (Word8, (Word8, Word8))) -> BBP.BoundedPrim Word8
   fixed4 x = BBP.liftFixedToBounded
     (const x BBP.>$< BBP.word8 BBP.>*< BBP.word8
              BBP.>*< BBP.word8 BBP.>*< BBP.word8)
   {-# INLINE fixed5 #-}
   fixed5 :: (Word8, (Word8, (Word8, (Word8, Word8)))) -> BBP.BoundedPrim Word8
   fixed5 x = BBP.liftFixedToBounded
     (const x BBP.>$< BBP.word8 BBP.>*< BBP.word8
              BBP.>*< BBP.word8 BBP.>*< BBP.word8 BBP.>*< BBP.word8)
