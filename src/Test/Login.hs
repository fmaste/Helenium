module Test.Login where

import Helenium

main = runTest test

test = do
	goTo "http://www.olx.com"
	login
	assertUSHomePage
	assertLogoutLink
	assertCookieLogin
	echo "Wait for 5 minutes and refresh."
	sleep (60 * 5) -- Wait 5 minutes
	refresh
	assertLogoutLink
	assertCookieLogin

login = do
	echo "Click on the Sign in link."
	signInLink <- getElementByText "Sign in"
	clickElement signInLink
	echo "Fill the login form."
	userInput <- getElementById "username"
	passInput <- getElementById "password"
	sendKeysToElement userInput "user"
	sendKeysToElement passInput "123456"
	echo "Submit login form."
	submitElement passInput

assertUSHomePage = do
	url <- getUrl
	assertPrefix "http://www.olx.com" url

assertLogoutLink = do
	echo "Assert if the logout button exists."
	getElementByText "Sign out"

assertCookieLogin = do 
	echo "Assert if the login cookie exists and has value 1."
	cookieValue <- getCookieValue "login"
	assertEqual cookieValue "1"

assertProfile = do
	echo "Look for the username link that should be on the header."
	userLink <- getElementByText "fmaste"
	echo "Go to the user profile by clicking the link."
	clickElement userLink
	echo "Assert the profile title."
	profileTitleElement <- getElementByXPath "//div[@id='item-top']//p[@id='olx_item_title']"
	profileTitleText <- getElementText profileTitleElement
	assertEqual profileTitleText "fmaste's Profile"

