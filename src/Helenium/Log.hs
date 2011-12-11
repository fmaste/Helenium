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

logMsg :: H.HeleniumWriterMsg -> H.HeleniumM ()
logMsg msg = do
	t <- liftIO $ Time.getCurrentTime
	tell [(t, msg)]

logGenerator :: H.HeleniumWriter -> [String]
logGenerator logs = map f logs where
	f (t, m) = (showLogTime t) ++ " - " ++ (showLogMsg m)

showLogTime :: Time.UTCTime -> String
-- TODO: Sometimes the miliseconds has 3 numbers and sometimes 4.
showLogTime t = show t

showLogMsg :: H.HeleniumWriterMsg -> String
showLogMsg (H.Info msg) = "INFO - " ++ msg
showLogMsg (H.HttpRequest req) = "REQUEST - " ++ req
showLogMsg (H.HttpResponse res) = "RESPONSE - " ++ res
showLogMsg (H.Screenshot s) = "SCREENSHOT - ..."

