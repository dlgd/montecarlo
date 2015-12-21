import Control.Applicative
import Montecarlo
import System.Random
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

mcPi :: Montecarlo StdGen [Double]
mcPi =  dist <$> mkValue <*> mkValue
    where mkValue = montecarlo $ randomR (0, 1)
          dist x y = [sqrt $ x**2 + y**2]

calcPi :: [Double] -> Double
calcPi xs = 4 * fromIntegral (length (filter (<= 1) xs))
             / fromIntegral (length xs)

evalPi :: Double
evalPi = calcPi $ evalMontecarlo 100000 mcPi (mkStdGen 42)

evalPiPar :: Double
evalPiPar = calcPi $ evalMontecarloPar 100 1000 mcPi (mkStdGen 42)

testEq :: Double -> Double -> Bool
testEq x y = abs (x - y) < 0.1

unitTests :: TestTree
unitTests = testGroup "Unit tests" [
             testCase "montecarlo: pi computation" $
             assertBool "" $ testEq evalPi pi
           , testCase "montecarlo: pi parallel computation" $
             assertBool "" $ testEq evalPiPar pi
            ]
