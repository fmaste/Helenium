module Test.Google where

import Helenium

main :: IO ()
main = do
	let reader = HeleniumReader {
		debugHttp = True,
		debugTime = False, -- TODO: Debug how long it takes to run the test.
		logTime = True,
		screenshotPath = "/home/developer"
	}
	let state = HeleniumState {
		serverHost = "http://127.0.0.1",
		serverPort = 9515,
		serverPath = "",
		serverBrowser = HeleniumBrowser Chrome "16" Linux,
		serverCapabilities = [
			JavascriptEnabled,
			TakesScreenshot,
			ApplicationCacheEnabled,
			BrowserConnectionEnabled,
			HandlesAlerts,
			CssSelectorsEnabled
		],
		serverSessionId = Nothing
	}
	runTest reader state test

-- Test to search google on Google!
test :: HeleniumM ()
test = do
	goTo "http://www.google.com"
	searchInput <- getElementById "lst-ib"
	sendKeysToElement searchInput "google"
	sleep 5
	submitElement searchInput
	back
	forward
	refresh
	url <- getUrl
	title <- getTitle
	return ()

