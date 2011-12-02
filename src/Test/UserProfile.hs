module Test.UserProfile where

import Helenium
import Test.Login (login)

main = runTest test

test = do
	goTo "http://www.olx.com"
	login
	assertProfile

assertProfile = do
	echo "Look for the username link that should be on the header."
	userLink <- getElementByText "fmaste"
	echo "Go to the user profile by clicking the link."
	clickElement userLink
	echo "Assert the profile title."
	profileTitleElement <- getElementByXPath "//div[@id='item-top']//p[@id='olx_item_title']"
	profileTitleText <- getElementText profileTitleElement
	assertEqual profileTitleText "fmaste's Profile"

