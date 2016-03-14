module TestUtil where

import           Control.Monad.IO.Class
import           System.IO.Temp
import           Test.HUnit

testWithTmpDir :: (FilePath -> IO ()) -> Test
testWithTmpDir testFunc = TestCase $ withSystemTempDirectory "giterate" testFunc
