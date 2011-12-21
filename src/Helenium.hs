module Helenium (
	runTest,
	echo,
	getEpoch,
	getCurrentTime,
	for,
	times,
	sleep,
	assertEqual,
	assertLess,
	assertGreater,
	assertLessOrEqual,
	assertGreaterOrEqual,
	substr,
	assertPrefix,
	assertSuffix,
	assertInfix,
	goTo,
	getUrl,
	getTitle,
	refresh,
	back,
	forward,
	takeScreenshot,
	changeFocusToIframeById,
	changeFocusToIframeByNumber,
	changeFocusToDefaultIframe,
	getActiveElement,
	getElementById,
	getElementByName,
	getElementByClassName,
	getElementByCssSelector,
	getElementByText,
	getElementByPartialText,
	getElementByXPath,
	assertElementDoesNotExistsById,
	assertElementDoesNotExistsByName,
	assertElementDoesNotExistsByClassName,
	assertElementDoesNotExistsByCssSelector,
	assertElementDoesNotExistsByText,
	assertElementDoesNotExistsByPartialText,
	assertElementDoesNotExistsByXPath,
	clickElement,
	clearElement,
	submitElement,
	getElementText,
	sendKeys,
	sendKeysToElement,
	assertElementIsEnabled,
	assertElementIsNotEnabled,
	assertElementIsDisplayed,
	assertElementIsNotDisplayed,
	assertElementIsSelected,
	assertElementIsNotSelected,
	getCookieValue,
	getCookieExpiresEpoch,
	deleteAllCookies,
	deleteCookieByName,
	assertCookieDoesNotExists
) where

-------------------------------------------------------------------------------

import qualified Helenium.Base as H
import qualified Helenium.Log as HL
import qualified Helenium.Runner as HR
import qualified Helenium.Commands as HC
import Data.Maybe
import Data.Either
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as TimePosix
import Data.List (find, isPrefixOf, isSuffixOf, isInfixOf)
import Control.Monad.Error
import Control.Monad.RWS.Strict
import qualified Text.JSON as JSON
import qualified System.Posix.Unistd as Sys

-- Runner
-------------------------------------------------------------------------------

-- |This function starts running the test passed as parameter.
runTest :: H.HeleniumM () -> IO ()
runTest = HR.runTest

-- Log commands
-------------------------------------------------------------------------------

-- |Adds a message to the test output.
echo :: String -> H.HeleniumM ()
echo m = do
	HL.logMsg $ H.Info m

-- Time commands
-------------------------------------------------------------------------------

-- |Gets the current UNIX timestamp.
getEpoch :: H.HeleniumM String
getEpoch = do
	t <- liftIO TimePosix.getPOSIXTime
	-- The show returns something like "123456.1234s"
	return $ takeWhile (/= '.') (show t)

-- |Gets the current UTC time.
getCurrentTime :: H.HeleniumM String
getCurrentTime = do
	t <- liftIO Time.getCurrentTime
	return $ show t

-- Flow control commands
-------------------------------------------------------------------------------

-- |Execute the given HeleniumM action with each element of the array as a parameter.
for :: [a] -> (a -> H.HeleniumM b) -> H.HeleniumM ()
for as f = forM_ as f

-- |Do a HeleniunM action a defined number of times.
times :: Int -> H.HeleniumM () -> H.HeleniumM ()
times n t = replicateM_ n t

-- |Suspends the test for a given number of seconds.
sleep :: Int -> H.HeleniumM ()
sleep ms = do
	liftIO $ Sys.sleep ms
	return ()

-- Assert commands
-------------------------------------------------------------------------------

assert :: Show x => String -> (x -> x -> Bool) -> x -> x -> H.HeleniumM ()
assert p f a b =
	if f a b
		then return ()
		else throwError $ H.Assert ("Not " ++ p ++ ": " ++ show a ++ " with " ++ show b)

-- |Fails if the elements are not equal. 
assertEqual :: (Eq x, Show x) => x -> x -> H.HeleniumM ()
assertEqual a b = assert "equal" (==) a b

-- |Fails if the first element is greater or equal.
assertLess :: (Ord x, Show x) => x -> x -> H.HeleniumM ()
assertLess a b = assert "less" (<) a b

-- |Fails if the first element is less or equal.
assertGreater :: (Ord x, Show x) => x -> x -> H.HeleniumM ()
assertGreater a b = assert "greater" (>) a b

-- |Fails if the first element is greater.
assertLessOrEqual :: (Ord x, Show x) => x -> x -> H.HeleniumM ()
assertLessOrEqual a b = assert "less" (<) a b

-- |Fails if the first element is less.
assertGreaterOrEqual :: (Ord x, Show x) => x -> x -> H.HeleniumM ()
assertGreaterOrEqual a b = assert "greater" (>) a b

-- |Fails if the first string is not equal or prefix.
assertPrefix :: String -> String -> H.HeleniumM ()
assertPrefix a b = assert "prefix" isPrefixOf a b

-- |Fails if the first string is not equal or suffix.
assertSuffix :: String -> String -> H.HeleniumM ()
assertSuffix a b = assert "suffix" isSuffixOf a b

-- |Fails if the first string is not contained anywhere on the second one.
assertInfix :: String -> String -> H.HeleniumM ()
assertInfix a b = assert "infix" isInfixOf a b

-- String manipulation
-------------------------------------------------------------------------------

substr :: String -> Int -> Int -> H.HeleniumM String
substr s from to = do
-- TODO: Check errors and assert when needed!
	let s' = drop from s
	let s'' = if to == 0
			then s'
			else if to > 0
				then take to s'
				else take ((length s') + to) s'
	return s''

-- Client-server commands!
-------------------------------------------------------------------------------

-- Browser commands.
-------------------------------------------------------------------------------

-- |Navigate to a new URL.
goTo :: String -> H.HeleniumM ()
goTo = HC.goTo

-- |Retrieve the URL of the current page.
getUrl :: H.HeleniumM String
getUrl = HC.getUrl

getTitle :: H.HeleniumM String
getTitle = HC.getTitle

-- |Refresh the current page.
refresh :: H.HeleniumM ()
refresh = HC.refresh

-- |Navigate backwards in the browser history, if possible.
back :: H.HeleniumM ()
back = HC.back

-- |Navigate forwards in the browser history, if possible.
forward :: H.HeleniumM ()
forward = HC.forward

-- |Take a screenshot of the current page.
takeScreenshot :: H.HeleniumM ()
takeScreenshot = do
	screen <- HC.getScreenshot
	HL.logMsg $ H.ScreenshotMsg screen

-- Frame commands.
-------------------------------------------------------------------------------

changeFocusToIframeById :: String -> H.HeleniumM ()
changeFocusToIframeById = HC.changeFocusToIframeById

changeFocusToIframeByNumber :: Int -> H.HeleniumM ()
changeFocusToIframeByNumber = HC.changeFocusToIframeByNumber

changeFocusToDefaultIframe :: H.HeleniumM ()
changeFocusToDefaultIframe = HC.changeFocusToDefaultIframe

-- Find element commands.
-------------------------------------------------------------------------------

-- |Get the element on the page that currently has focus.
getActiveElement :: H.HeleniumM String
getActiveElement = HC.getActiveElement

getElementById :: String -> H.HeleniumM String
getElementById = HC.getElementById

getElementByName :: String -> H.HeleniumM String
getElementByName = HC.getElementByName

getElementByClassName :: String -> H.HeleniumM String
getElementByClassName = HC.getElementByClassName

getElementByCssSelector :: String -> H.HeleniumM String
getElementByCssSelector = HC.getElementByCssSelector

-- |Returns an anchor element whose visible text matches the search value.
getElementByText :: String -> H.HeleniumM String
getElementByText = HC.getElementByText

-- |Returns an anchor element whose visible text partially matches the search value.
getElementByPartialText :: String -> H.HeleniumM String
getElementByPartialText = HC.getElementByPartialText

getElementByXPath :: String -> H.HeleniumM String
getElementByXPath = HC.getElementByXPath

processElementDoesNotExistsResponse :: (a -> H.HeleniumM String) -> a -> H.HeleniumM ()
processElementDoesNotExistsResponse getResponseElement a =
	do {
		getResponseElement a;
		-- If no exception, the element was found!
		throwError $ H.Assert "Element exists."
	} `catchError` (\e -> 
		case e of
			(H.FailedCommand 7 _ _) -> return ()
			_ -> throwError e
	)

assertElementDoesNotExistsById :: String -> H.HeleniumM ()
assertElementDoesNotExistsById id = do
	processElementDoesNotExistsResponse getElementById id

assertElementDoesNotExistsByName :: String -> H.HeleniumM ()
assertElementDoesNotExistsByName name = do
	processElementDoesNotExistsResponse getElementByName name

assertElementDoesNotExistsByClassName :: String -> H.HeleniumM ()
assertElementDoesNotExistsByClassName className = do
	processElementDoesNotExistsResponse getElementByClassName className

assertElementDoesNotExistsByCssSelector :: String -> H.HeleniumM ()
assertElementDoesNotExistsByCssSelector css = do
	processElementDoesNotExistsResponse getElementByCssSelector css

assertElementDoesNotExistsByText :: String -> H.HeleniumM ()
assertElementDoesNotExistsByText text = do
	processElementDoesNotExistsResponse getElementByText text

assertElementDoesNotExistsByPartialText :: String -> H.HeleniumM ()
assertElementDoesNotExistsByPartialText text = do
	processElementDoesNotExistsResponse getElementByPartialText text

assertElementDoesNotExistsByXPath :: String -> H.HeleniumM ()
assertElementDoesNotExistsByXPath x = do
	processElementDoesNotExistsResponse getElementByXPath x

-- Manipulate element commands.
-------------------------------------------------------------------------------

-- |Click on an element.
clickElement :: String -> H.HeleniumM ()
clickElement = HC.clickElement

-- |Clear a TEXTAREA or text INPUT element's value.
clearElement :: String -> H.HeleniumM ()
clearElement = HC.clearElement

-- |Submit a FORM element. The submit command may also be applied to any element 
-- that is a descendant of a FORM element.
submitElement :: String -> H.HeleniumM ()
submitElement = HC.submitElement

getElementText :: String -> H.HeleniumM String
getElementText = HC.getElementText

sendKeys :: [Char] -> H.HeleniumM ()
sendKeys = HC.sendKeys

sendKeysToElement :: String -> [Char] -> H.HeleniumM ()
sendKeysToElement = HC.sendKeysToElement

assertElementIsEnabled :: String -> H.HeleniumM ()
assertElementIsEnabled e = do
	enabled <- HC.getElementIsEnabled e
	if enabled
		then return ()
		else throwError $ H.Assert "Assert element is enabled failed."
	
assertElementIsNotEnabled :: String -> H.HeleniumM ()
assertElementIsNotEnabled e = do
	enabled <- HC.getElementIsEnabled e
	if enabled
		then throwError $ H.Assert "Assert element is not enabled failed."
		else return ()

assertElementIsDisplayed :: String -> H.HeleniumM ()
assertElementIsDisplayed e = do
	displayed <- HC.getElementIsDisplayed e
	if displayed
		then return ()
		else throwError $ H.Assert "Assert element is displayed failed."

assertElementIsNotDisplayed :: String -> H.HeleniumM ()
assertElementIsNotDisplayed e = do
	displayed <- HC.getElementIsDisplayed e
	if displayed
		then throwError $ H.Assert "Assert element is not displayed failed."
		else return ()

assertElementIsSelected :: String -> H.HeleniumM ()
assertElementIsSelected e = do
	selected <- HC.getElementIsSelected e
	if selected
		then return ()
		else throwError $ H.Assert "Assert element is selected failed."

assertElementIsNotSelected :: String -> H.HeleniumM ()
assertElementIsNotSelected e = do
	selected <- HC.getElementIsSelected e
	if selected
		then throwError $ H.Assert "Assert element is not selected failed."
		else return ()

getCookieByName :: String -> H.HeleniumM (Maybe JSON.JSValue)
getCookieByName name = do
	cookies <- HC.getCookies
	return $ find (cookieFinder name) cookies

cookieFinder :: String -> JSON.JSValue -> Bool
cookieFinder name json = do
	case json of
		JSON.JSObject obj -> case JSON.valFromObj "name" obj of
			JSON.Ok (JSON.JSString jsName) -> (name == (JSON.fromJSString jsName))
			_ -> False
		_ -> False

getCookieValue :: String -> H.HeleniumM String
getCookieValue name = do
	cookie <- getCookieByName name
	case cookie of
		Just (JSON.JSObject obj) -> case JSON.valFromObj "value" obj of
			JSON.Ok (JSON.JSString value) -> return $ JSON.fromJSString value
			_ -> throwError $ H.Unknown "Error reading cookie value, not a valid JSON response."
		Nothing -> throwError $ H.Unknown $ "Cookie does not exists: " ++ name ++ "."
		_ -> throwError $ H.Unknown "Error reading cookie value, not a valid JSON response."

getCookieExpiresEpoch :: String -> H.HeleniumM Int
getCookieExpiresEpoch name = do
	cookie <- getCookieByName name
	case cookie of
		Just (JSON.JSObject obj) -> case JSON.valFromObj "expiry" obj of
			JSON.Ok (JSON.JSRational False e) -> return $ fromEnum e
			_ -> throwError $ H.Unknown "Error reading cookie expires, not a valid JSON response."
		Nothing -> throwError $ H.Unknown $ "Cookie does not exists: " ++ name ++ "."
		_ -> throwError $ H.Unknown "Error reading cookie expires, not a vlaid JSON response."

deleteAllCookies :: H.HeleniumM ()
deleteAllCookies = do HC.deleteAllCookies

deleteCookieByName :: String -> H.HeleniumM ()
deleteCookieByName = HC.deleteCookieByName

assertCookieDoesNotExists :: String -> H.HeleniumM ()
assertCookieDoesNotExists name = do
	cookie <- getCookieByName name
	case cookie of
		Nothing -> return ()
		_ -> throwError $ H.Assert $ "Cookie exists: " ++ name ++ "."

