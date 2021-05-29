module Data.Parse.Generic where

import Control.Alt ((<|>))
import Control.Applicative (unless, ($>))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), NoConstructors, Product(..), Sum(..), to)
import Data.Parse (class Parse, parse, primaryParser)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Unit (Unit)
import Prelude (bind, map, pure, ($), (<>), discard, (==))
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, try, (<?>))
import Text.Parsing.Parser.String (string, whiteSpace)
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
  genericParse' = parseConstr (Proxy :: Proxy name) $> (Constructor NoArguments)

else instance genericParseConstructor' :: (IsSymbol name, GenericParse arg) => GenericParse (Constructor name arg) where
  genericParse' = between (string "(" <?> "opening bracket") (string ")" <?> "closing bracket") $ do
    parseConstr (Proxy :: Proxy name)
    _ <- whiteSpace
    arg <- genericParse' <?> "constructor argument"
    pure (Constructor arg)

parseConstr :: forall name. IsSymbol name => Proxy name -> Parser String Unit
parseConstr lab = do
  ident <- primaryParser.identifier <?> ("type name " <> label)
  unless (ident == label) $ fail ("Unknown identifier: " <> ident <> ",  " <> label <> " expected.")

  where label = reflectSymbol lab


genericParse :: forall a rep. Generic a rep => GenericParse rep => Parser String a
genericParse = map to genericParse'
