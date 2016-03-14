module Giterate.Util where

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Giterate.Internal

getCurrentTS :: IO NominalDiffTime
getCurrentTS = getPOSIXTime
