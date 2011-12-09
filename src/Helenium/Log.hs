module Helenium.Log (
	logMsg,
	logGenerator
) where

-------------------------------------------------------------------------------

import qualified Helenium.Base as H
import Data.Maybe
import Control.Monad.Error
import Control.Monad.RWS.Strict
import qualified Data.Time as Time

-------------------------------------------------------------------------------

logMsg :: H.HeleniumWriterLevel -> H.HeleniumWriterMsg -> H.HeleniumM ()
logMsg level msg = do
	t <- liftIO $ Time.getCurrentTime
	tell [(t, level, msg)]

logGenerator :: H.HeleniumWriter -> [String]
logGenerator logs = map f logs where
	f (t, l, m) = (showLogTime t) ++ " - " ++ (showLogLevel l) ++ " - " ++ m

showLogTime :: Time.UTCTime -> String
showLogTime t = show t
	
showLogLevel H.Info = "INFO"
showLogLevel H.Debug = "DEBUG" 
showLogLevel H.DebugRequest = "REQUEST"
showLogLevel H.DebugResponse = "RESPONSE"

