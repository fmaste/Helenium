module Test.Google where

import Helenium

main :: IO ()
main = do
	let reader = HeleniumReader {
		name = "Google",
		logTime = True,
		debugHttp = True,
		debugTime = False, -- TODO: Debug how long it takes to run the test.
		timeoutTest = 0,
		timeoutElement = 3000,
		screenshotPath = "/home/developer"
	}
	let state = HeleniumState {
		serverHost = "http://127.0.0.1",
		--serverPort = 4444,
		--serverPath = "/wd/hub",
		serverPort = 9515,
		serverPath = "",
		serverBrowser = HeleniumBrowser Firefox "16" Linux,
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

