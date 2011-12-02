module Test.Logout where

import Helenium
import Test.Login (login)

main = runTest test

test = do
	goTo "http://www.olx.com"
	login
	refresh
	logout
	assertCookieLogout
	login

logout = do
	echo "Click on the Sign out link."
	signOutLink <- getElementByText "Sign out"
	clickElement signOutLink

assertCookieLogout = do
	echo "Test if the login cookie does not exists."
	assertCookieDoesNotExists "login"

