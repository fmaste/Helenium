module Test.Google where

import Helenium

main = runTest test

-- Test to search google on Google!
test = do
	goTo "http://www.google.com/ncr"
	searchInput <- getElementById "lst-ib"
	sendKeysToElement searchInput "google"
	sleep 5
	goTo "http://www.amazon.com"
	back
	forward
	refresh
	url <- getUrl
	assertEqual "http://www.amazon.com/" url
	title <- getTitle
	assertPrefix "Amazon.com" title
	return ()

