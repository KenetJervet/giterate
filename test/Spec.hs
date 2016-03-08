{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Giterate.Internal
import           Test.HUnit

hutest1 :: Test
hutest1 = TestCase $ do
  (exitCode, out, err) <- runGitCmd ["status"] ""
  putStrLn "==== exitCode ===="
  print exitCode
  putStrLn "==== out ===="
  putStrLn out
  putStrLn "==== err ===="
  putStrLn err
  putStrLn "==== FIN ===="

main :: IO ()
-- main = test1
-- main = quickCheck ((\s -> s == s) :: Char -> Bool)
main = void $ runTestTT $ TestList [TestLabel "hutest1" hutest1]
