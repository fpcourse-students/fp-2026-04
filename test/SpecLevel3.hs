module SpecLevel3 where

import Level3
import Test.Prelude
import TypeCheck

tests :: NamedTests
tests = nameTests 3
  [ testTypes
  , testDeepFirst
  ]

-- | Типы сравниваются по форме (см. "TypeCheck"): имена переменных не важны,
-- число параметров синонима и порядок аргументов — важны.
testTypes :: Test
testTypes = TestList
  [ assertShape "3.1 Pair" "SHAPE_PAIR" $(synonymShape ''Pair)
  , assertShape "3.1 Variant" "SHAPE_VARIANT" $(synonymShape ''Variant)
  , assertShape "3.1 Nat" "SHAPE_NAT" $(synonymShape ''Nat)
  ]

testDeepFirst :: Test
testDeepFirst = TestList
  [ propertyToTest "deepFirst applies the function to the innermost first component"
      \(Fun _ f :: Fun Int Char, x :: Int, y :: Bool, z :: Char) ->
        deepFirst f ((x, y), z) === ((f x, y), z)
  , TestCase $ assertEqual "deepFirst (+ 1)" ((2 :: Int, 'a'), True) $ deepFirst (+ 1) ((1, 'a'), True)
  ]
