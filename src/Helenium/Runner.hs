module Helenium.Runner (
	runTest
) where

import qualified Helenium.Base as H
import qualified Helenium.Log as HL
import qualified Helenium.Network as HN
import Control.Monad.Error
import Control.Monad.RWS.Strict
import System (getArgs)
import Data.List (find, isPrefixOf, isSuffixOf, isInfixOf)
import qualified Text.JSON as JSON

-- Create a new session.
connect :: H.HeleniumM ()
connect = do
	state <- get
	reader <- ask
	let b = H.heleniumBrowserNameKey $ H.browserName $ H.browser reader
	let capabilities = H.serverCapabilities state
	let capabilitiesArray = map 
		(\c -> (H.heleniumCapabilityKey c, JSON.JSBool True)) 
		capabilities
	let capabilitiesArray' = capabilitiesArray ++ [("browserName", JSON.showJSON b)]
	let capabilitiesJson = JSON.makeObj capabilitiesArray'
	let bodyJson = JSON.makeObj [("desiredCapabilities", capabilitiesJson)]
	ans <- HN.callSelenium $ HN.Request False (HN.Post $ JSON.encode bodyJson) "/session"
	-- Return a redirect with header:
	-- Location: /session/bf8deb5adc6e61e87c09913f78e5c82c
	let location = HN.responseHTTPHeaderLocation $ HN.responseHTTPHeaders ans
	case location of
		Just location -> do
			let sessId = reverse $ takeWhile (/= '/') $ reverse location
			put $ state {H.serverSessionId = (Just sessId)}
		Nothing -> throwError "Response has no Location header with the new session."
	return () 

-- Set the amount of time the driver should wait when searching for elements.
-- When searching for an element, the driver should poll the page until the 
-- element is found or the timeout expires, whichever occurs first.
setElementTimeout :: Int -> H.HeleniumM ()
setElementTimeout ms = do
	let body = JSON.makeObj [("ms", JSON.showJSON ms)]
	HN.callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/timeouts/implicit_wait"
	return ()

-- Delete the session.
disconnect :: H.HeleniumM ()
disconnect = do
	HN.callSelenium $ HN.Request True HN.Delete "/"
	return ()

runTest :: H.HeleniumM () -> IO ()
runTest t = do
	args <- getArgs
	when (null args) (error $ "No paramaters. Server and browser are needed.")
	when (not $ any (isPrefixOf "--server=") args) (error $ "No --server parameter.")
	when (not $ any (isPrefixOf "--browser=") args) (error $ "No --browser parameter.")
	let config = H.HeleniumReader {
		H.server = "http://127.0.0.1:9515",
		H.browser = H.HeleniumBrowser H.Chrome "16" H.Linux,
		H.logTime = True,
		H.debugHttp = False,
		H.debugTime = False,
		H.timeoutTest = 0,
		H.timeoutElement = 5000,
		H.screenshotPath = "/home/developer"
	}
	let config' = foldl updateConfig config args
	when (not $ HN.isUriValid $ H.server config') (error "Not a valid server URL.")
	-- TODO: Check valid browser name! The browser in config is lazy (not evaluated).
	runTest' config' t

browserStringToBrowserName :: String -> H.HeleniumBrowserName
browserStringToBrowserName "chrome" = H.Chrome
browserStringToBrowserName "firefox" = H.Firefox
browserStringToBrowserName "htmlunit" = H.HtmlUnit
browserStringToBrowserName "internet explorer" = H.IE
browserStringToBrowserName "iphone" = H.IPhone
browserStringToBrowserName b = error $ b ++ " is not a valid browser."

updateConfig :: H.HeleniumReader -> String -> H.HeleniumReader
updateConfig r c
	| isPrefixOf "--server=" c && length c > 9 = 
		r {H.server = drop 9 c}
	| isPrefixOf "--browser=" c && length c > 10 = 
		r {H.browser = H.HeleniumBrowser (browserStringToBrowserName $ drop 10 c) "16" H.Linux}
	| c == "--debugHttp" = 
		r {H.debugHttp = True}
	| otherwise = error $ c ++ " is not a valid parameter."

runTest' :: H.HeleniumReader -> H.HeleniumM () -> IO ()
runTest' r t = do
	-- If the URI has a trailing '/', remove it..
	let r' = if (last $ H.server r) == '/'
		then r {H.server = (init $ H.server r)}
		else r
	let s = H.HeleniumState {
		H.serverCapabilities = [
			H.JavascriptEnabled,
			H.TakesScreenshot,
			H.ApplicationCacheEnabled,
			H.BrowserConnectionEnabled,
			H.HandlesAlerts,
			H.CssSelectorsEnabled
		],
		H.serverSessionId = Nothing
	}
	t' <- wrapTest t
	(eitherAns, s', w) <- H.runHeleniumM t' r' s
	showWriter w
	case eitherAns of
		Left err -> showError err
		Right _ -> showOk

showOk :: IO ()
showOk = putStrLn "Test finished successfully!"

showError :: H.HeleniumError -> IO ()
showError e = putStrLn $ "An error ocurred: " ++ e

showWriter :: H.HeleniumWriter -> IO ()
-- TODO: Do something with message type!
showWriter w = mapM_ putStrLn (HL.logGenerator w)

wrapTest :: H.HeleniumM () -> IO (H.HeleniumM ())
wrapTest t = do
	return $ do
		reader <- ask
		connect
		do {
			setElementTimeout $ H.timeoutElement reader;
			t
		} `catchError` (\e -> do {disconnect; throwError e})
		disconnect

