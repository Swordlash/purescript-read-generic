module Data.Parse.Generic where

import Control.Alt ((<|>))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..), to)
import Data.Parse (class Parse, parse, readEither)
import Data.String.CodeUnits (fromCharArray)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prelude (bind, map, pure, ($), (<>))
import Text.Parsing.Parser (Parser, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (between, try, (<?>))
import Text.Parsing.Parser.String (anyChar, string, whiteSpace)
import Type.Proxy (Proxy(..))

class GenericParse a where
  genericParse' :: Parser String a

instance genericParseNoConstructors :: GenericParse NoConstructors where
  genericParse' = fail "Parsing NoConstructors"

instance genericParseNoArguments :: GenericParse NoArguments where
  genericParse' = pure NoArguments

instance genericParseSum :: (GenericParse a, GenericParse b) => GenericParse (Sum a b) where
  genericParse' = try (map Inl genericParse') <|> map Inr genericParse'

instance genericParseArgument :: Parse arg => GenericParse (Argument arg) where
  genericParse' = map Argument parse

instance genericParseProduct :: (GenericParse a, GenericParse b) => GenericParse (Product a b) where
  genericParse' = do
    x <- genericParse' <?> "first argument of product"
    _ <- whiteSpace
    y <- genericParse' <?> "second argument of product"
    pure (Product x y)

instance genericParseConstructor :: IsSymbol name => GenericParse (Constructor name NoArguments) where
  genericParse' = do
    _ <- string (reflectSymbol (Proxy :: Proxy name))
    pure (Constructor NoArguments)

else instance genericParseConstructor' :: (IsSymbol name, GenericParse arg) => GenericParse (Constructor name arg) where
  genericParse' = between (string "(" <?> "opening bracket") (string ")" <?> "closing bracket") $ do
    _ <- string label <?> ("type name " <> label)
    _ <- whiteSpace
    arg <- genericParse' <?> "constructor argument"
    pure (Constructor arg)
    where
      label = reflectSymbol (Proxy :: Proxy name)

genericParse :: forall a rep. Generic a rep => GenericParse rep => Parser String a
genericParse = map to genericParse'
