{-# OPTIONS_HADDOCK hide #-}

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
	f (t, m) = "<tr><td>" ++ (showLogTime t) ++ "</td><td>" ++ (showLogType m) ++ "</td><td>" ++ (showLogMsg m) ++ "</td></tr>"

htmlPrefix = "<html><head><style>td{border: 1px solid #000;}</style></head><body><table>"
htmlSuffix = "</table></body></html>"

showLogTime :: Time.UTCTime -> String
-- TODO: Sometimes the miliseconds has 3 numbers and sometimes 4.
showLogTime t = show t

showLogType :: H.HeleniumWriterMsg -> String
showLogType (H.Info msg) = "INFO"
showLogType (H.HttpRequest req) = "REQUEST"
showLogType (H.HttpResponse res) = "RESPONSE"
showLogType (H.Screenshot s) = "SCREENSHOT"

showLogMsg :: H.HeleniumWriterMsg -> String
showLogMsg (H.Info msg) = msg
showLogMsg (H.HttpRequest req) = req
showLogMsg (H.HttpResponse res) = res
showLogMsg (H.Screenshot s) = "<img src=\"data:image/png;base64," ++ s ++ "\" />"

