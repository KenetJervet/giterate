{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Giterate
import           System.Directory
import           Test.HUnit
import           TestUtil

testInit :: Test
testInit = testWithTmpDir $ \tmpDir -> do
  putStrLn tmpDir
  setCurrentDirectory tmpDir
  (exitCode, out, err) <- execute GtrInit
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
main = void $ runTestTT $ TestList [TestLabel "testInit" testInit]
