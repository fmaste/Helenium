module Test.Google where

import Helenium

main = runTest test

-- Test to search google on Google!
test = do
	goTo "http://www.google.com/ncr"
	searchInput <- getElementById "lst-ib"
	sendKeysToElement searchInput "google"
	sleep 5
	assertElementDoesNotExistsById "UNKNOWN-DOM-NODE"
	return ()

