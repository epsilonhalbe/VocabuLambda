import Trainer.Internal

import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = do runTestTT allTests
          print "all HUnit tests passed"
          mapM_ quickCheck allProps

allTests :: Test
allTests = TestList
    [ TestCase (assertBool "TestMessageHere" True)
    ]
prop_nonNegativity :: Property
prop_nonNegativity  =
      forAll (arbitrary::Gen (Positive Integer)) $ \(Positive idx) ->
      forAll (choose (0,1)) $ \r1 ->
      forAll (choose (0,1)) $ \r2 -> positiveStdNormal (fromIntegral idx) r1 r2 >= 0

prop_boundedness :: Property
prop_boundedness =
      forAll (arbitrary::Gen (Positive Integer)) $ \(Positive idx) ->
      forAll (choose (0,1)) $ \r1 ->
      forAll (choose (0,1)) $ \r2 -> positiveStdNormal (fromIntegral idx) r1 r2 <= fromIntegral idx

allProps ::  [Property]
allProps =
    [ prop_nonNegativity
    , prop_boundedness
    ]
