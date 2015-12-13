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
    where mkValue = liftMontecarlo $ randomR (0, 1)
          dist x y = [sqrt $ x**2 + y**2]

evalPi :: Double
evalPi = 4 * fromIntegral (length (filter (<=1) xs)) / fromIntegral (length xs)
    where xs = evalMontecarlo 100000 mcPi (mkStdGen 42)

unitTests :: TestTree
unitTests = testGroup "Unit tests" [
             testCase "montecarlo: pi computation" $
             assertBool "" (abs (evalPi - pi) < 0.01)
            ]
