module Data.Parse
  ( class Parse, class Read
  , parse
  , readEither
  , readMaybe
  , read
  , primaryParser
  ) where

import Prelude

import Control.Alt (map, (<|>))
import Control.Alternative (pure)
import Control.Category ((<<<))
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Array (many)
import Data.Bifunctor (lmap)
import Data.DateTime (Date, DateTime(..), Day, Hour, Millisecond, Minute, Second, Time(..), Year, canonicalDate, date)
import Data.Either (Either(..), hush, note)
import Data.Enum (toEnum)
import Data.Int as I
import Data.Maybe (Maybe(..), fromJust)
import Data.Number as N
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.CodeUnits (fromCharArray)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser (Parser, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Language (emptyDef, haskellDef)
import Text.Parsing.Parser.String (char, string)
import Text.Parsing.Parser.Token (GenTokenParser, digit, makeTokenParser)

-----------------------------------------------------

class Read a where
  readEither :: String -> Either String a

read :: forall a. Read a => Partial => String -> a
read s = case readEither s of
  Right val -> val

readMaybe :: forall a. Read a => String -> Maybe a
readMaybe = hush <<< readEither

instance parseRead :: Parse a => Read a where
  readEither str = lmap show $ runParser str parse

-----------------------------------------------------

class Parse a where
  parse :: Parser String a

primaryParser = makeTokenParser emptyDef

instance readString :: Parse String where
  parse = primaryParser.stringLiteral

instance readInt :: Parse Int where
  parse = primaryParser.integer

instance readNumber :: Parse Number where
  parse = do
    sign <- (char '-' $> negate)
            <|> (char '+' $> identity)
            <|> pure identity
    f <- primaryParser.float
    pure $ sign f

instance readBool :: Parse Boolean where
  parse = try (string "true" *> pure true) <|> (string "false" *> pure false)

foreign import _readDateTime :: forall a. String -> (Year -> Int -> Day -> Hour -> Minute -> Second -> Millisecond -> DateTime) -> (a -> Maybe a) -> Maybe a -> Maybe DateTime

dateTimeFromString :: String -> Maybe DateTime
dateTimeFromString str = _readDateTime str mkDateTime Just Nothing
  where
    mkDateTime = unsafePartial \y mo d h mi s ms ->
      DateTime (canonicalDate y (fromJust (toEnum mo)) d) (Time h mi s ms)

instance readDateTime :: Parse DateTime where
  parse = do
    str <- map fromCharArray $ many (digit <|> char '.' <|> char 'T' <|> char 'Z' <|> char '-' <|> char ':')
    case dateTimeFromString str of
      Nothing -> fail ("Cannot parse DateTime from " <> str)
      Just d -> pure d

instance readDate :: Parse Date where
  parse = map date parse
