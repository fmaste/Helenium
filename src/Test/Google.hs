module Test.Google where

import Helenium

main = runTest test

-- Test to search google on Google!
test = do
	goTo "http://www.google.com/ncr"
	searchInput <- getElementById "lst-ib"
	sendKeysToElement searchInput "google"
	sleep 5
	takeScreenshot
	assertElementDoesNotExistsById "UNKNOWN-DOM-NODE"
	echo "About to finish."
	refresh
	assertElementDoesNotExistsByXPath "/html/body/div"
	return ()

