module Test.Login where

import Helenium

main :: IO ()
main = do
	let config = HeleniumReader {
		name = "Login",
		logTime = True,
		debugHttp = False,
		debugTime = False,
		timeoutTest = 0,
		timeoutElement = 3000,
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
        runTest config state test

test = do
	goTo "http://www.aaa.com"
	-- Not supported on linux.
	-- takeScreenshot "login"
	login
	assertProfile
	logout
	login
	refresh
	assertProfile
	logout
	login
	deleteAllCookies
	refresh
	login
	deleteCookieByName "login"
	refresh
	login
	
login = do
	echo "Click on the Sign in link."
	signInLink <- getElementByText "Sign in"
	clickElement signInLink
	echo "Fill the login form."
	userInput <- getElementById "username"
	passInput <- getElementById "password"
	sendKeysToElement userInput "fmaste"
	sendKeysToElement passInput "123456"
	echo "Submit login form."
	submitElement passInput
	cookieValue <- getCookieValue "login"
	assertEq cookieValue "1"

logout = do
	echo "Click on the Sign out link."
	signOutLink <- getElementByText "Sign out"
	clickElement signOutLink

assertProfile = do
	echo "Look for the username link that should be on the header."
	userLink <- getElementByText "fmaste"
	echo "Go to the user profile by clicking the link."
	clickElement userLink
	echo "Assert the profile title."
	profileTitleElement <- getElementByXPath "//div[@id='item-top']//p[@id='olx_item_title']"
	profileTitleText <- getElementText profileTitleElement
	assertEq profileTitleText "fmaste's Profile"

