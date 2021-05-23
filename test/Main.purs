module Test.Main where

import Data.Either
import Data.Generic.Rep
import Data.Parse
import Data.Parse.Generic
import Data.Show
import Data.Show.Generic
import Prelude
import Test.Assert

import Control.Lazy (fix)
import Data.Function (const)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console (log, logShow)

data X = Y X Boolean | X Int Int Number

derive instance eqX :: Eq X
derive instance genX :: Generic X _

instance showX :: Show X where
  show x = genericShow x

instance parseX :: Parse X where
  parse = fix $ \_ -> genericParse

testCase :: forall a. Eq a => Show a => Parse a => a -> Effect Unit
testCase x = do
  log $ "Testing " <> show x
  log $ "Result: " <> show (readEither (show x) :: Either String a)
  assert (readEither (show x) == Right x)

main :: Effect Unit
main = do
  testCase (X 5 5 7.0)
  testCase (Y (X 5 5 (-6.0)) true)
