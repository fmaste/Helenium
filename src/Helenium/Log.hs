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

logGenerator :: H.HeleniumWriter -> String
logGenerator logs = htmlPrefix ++ (concat $ map f logs) ++ htmlSuffix where
	f (t, m) = "<tr>" ++ (showLogTime t) ++ (showLogMsg m) ++ "</tr>"

htmlPrefix = "<html><body><table>"
htmlSuffix = "</table></body></html>"

showLogTime :: Time.UTCTime -> String
-- TODO: Sometimes the miliseconds has 3 numbers and sometimes 4.
showLogTime t = "<td>" ++ (show t) ++ "</td>"

showLogMsg :: H.HeleniumWriterMsg -> String
showLogMsg (H.Info msg) = "<td>INFO</td>" ++ msg ++ "</td>"
showLogMsg (H.HttpRequest req) = "<td>REQUEST</td><td>" ++ req ++ "</td>"
showLogMsg (H.HttpResponse res) = "<td>RESPONSE</td><td>" ++ res ++ "</td>"
showLogMsg (H.Screenshot s) = "<td>SCREENSHOT</td><td>...</td>"


