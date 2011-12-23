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
	assertElementIsEnabled,
	assertElementIsNotEnabled,
	assertElementIsSelected,
	assertElementIsNotSelected,
	sendKeys,
	sendKeysToElement,
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

-- |Do a HeleniunM action a predefined number of times.
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

-- |Create a substring.
-- First param is the string to process.
-- The first number it from where to start (first letter is 0).
-- The second number if positive is how many chars to take.
-- If negative is how many chars to drop from the end of the string.
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

-- Browser commands.
-------------------------------------------------------------------------------

-- |Navigate to a new URL.
goTo :: String -> H.HeleniumM ()
goTo = HC.goTo

-- |Retrieve the URL of the current page.
getUrl :: H.HeleniumM String
getUrl = HC.getUrl

-- |Get the browser title.
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

-- Element helper functions.
-------------------------------------------------------------------------------

-- To assert if elements are displayed before using them.
assertElementIsDisplayed :: String -> H.HeleniumM ()
assertElementIsDisplayed element = do
	isDisplayed <- HC.getElementIsDisplayed element
	if isDisplayed
		then return ()
		else throwError $ H.HeleniumFailed "Element exists but is not displayed."

processElementExistsResponse :: (a -> H.HeleniumM String) -> a -> H.HeleniumM String
processElementExistsResponse getElement a = do
	element <- getElement a
	assertElementIsDisplayed element
	return element

processElementDoesNotExistsResponse :: (a -> H.HeleniumM String) -> a -> H.HeleniumM ()
processElementDoesNotExistsResponse getResponseElement a =
	do {
		element <- getResponseElement a;
		isDisplayed <- HC.getElementIsDisplayed element;
		if isDisplayed
			then throwError $ H.Assert "Element exists.";
			else HL.logMsg $ H.Warn "Element exists but is not displayed.";
	} `catchError` (\e ->
		case e of
			(H.FailedCommand 7 _ _) -> return ()
			_ -> throwError e
	)

-- Find element commands.
-------------------------------------------------------------------------------

-- |Get the element on the page that currently has focus.
getActiveElement :: H.HeleniumM String
-- TODO: An active element can be not displayed?
getActiveElement = HC.getActiveElement

getElementById :: String -> H.HeleniumM String
getElementById = processElementExistsResponse HC.getElementById

getElementByName :: String -> H.HeleniumM String
getElementByName = processElementExistsResponse HC.getElementByName

getElementByClassName :: String -> H.HeleniumM String
getElementByClassName = processElementExistsResponse HC.getElementByClassName

getElementByCssSelector :: String -> H.HeleniumM String
getElementByCssSelector = processElementExistsResponse HC.getElementByCssSelector

-- |Returns an anchor element whose visible text matches the search value.
getElementByText :: String -> H.HeleniumM String
getElementByText = processElementExistsResponse HC.getElementByText

-- |Returns an anchor element whose visible text partially matches the search value.
getElementByPartialText :: String -> H.HeleniumM String
getElementByPartialText = processElementExistsResponse HC.getElementByPartialText

getElementByXPath :: String -> H.HeleniumM String
getElementByXPath = processElementExistsResponse HC.getElementByXPath

assertElementDoesNotExistsById :: String -> H.HeleniumM ()
assertElementDoesNotExistsById id = do
	processElementDoesNotExistsResponse HC.getElementById id

assertElementDoesNotExistsByName :: String -> H.HeleniumM ()
assertElementDoesNotExistsByName name = do
	processElementDoesNotExistsResponse HC.getElementByName name

assertElementDoesNotExistsByClassName :: String -> H.HeleniumM ()
assertElementDoesNotExistsByClassName className = do
	processElementDoesNotExistsResponse HC.getElementByClassName className

assertElementDoesNotExistsByCssSelector :: String -> H.HeleniumM ()
assertElementDoesNotExistsByCssSelector css = do
	processElementDoesNotExistsResponse HC.getElementByCssSelector css

assertElementDoesNotExistsByText :: String -> H.HeleniumM ()
assertElementDoesNotExistsByText text = do
	processElementDoesNotExistsResponse HC.getElementByText text

assertElementDoesNotExistsByPartialText :: String -> H.HeleniumM ()
assertElementDoesNotExistsByPartialText text = do
	processElementDoesNotExistsResponse HC.getElementByPartialText text

assertElementDoesNotExistsByXPath :: String -> H.HeleniumM ()
assertElementDoesNotExistsByXPath x = do
	processElementDoesNotExistsResponse HC.getElementByXPath x

-- Manipulate element commands.
-------------------------------------------------------------------------------

-- |Click on an element.
clickElement :: String -> H.HeleniumM ()
clickElement element = do
	assertElementIsDisplayed element
	HC.clickElement element

-- |Clear a TEXTAREA or text INPUT element's value.
clearElement :: String -> H.HeleniumM ()
clearElement element = do
	assertElementIsDisplayed element
	HC.clearElement element

-- |Submit a FORM element. The submit command may also be applied to any element 
-- that is a descendant of a FORM element.
submitElement :: String -> H.HeleniumM ()
submitElement element = do
	assertElementIsDisplayed element
	HC.submitElement element

-- Element properties
-------------------------------------------------------------------------------

getElementText :: String -> H.HeleniumM String
getElementText e = do
	assertElementIsDisplayed e
	HC.getElementText e

assertElementIsEnabled :: String -> H.HeleniumM ()
assertElementIsEnabled e = do
	assertElementIsDisplayed e
	enabled <- HC.getElementIsEnabled e
	if enabled
		then return ()
		else throwError $ H.Assert "Assert element is enabled failed."
	
assertElementIsNotEnabled :: String -> H.HeleniumM ()
assertElementIsNotEnabled e = do
	assertElementIsDisplayed e
	enabled <- HC.getElementIsEnabled e
	if enabled
		then throwError $ H.Assert "Assert element is not enabled failed."
		else return ()

assertElementIsSelected :: String -> H.HeleniumM ()
assertElementIsSelected e = do
	assertElementIsDisplayed e
	selected <- HC.getElementIsSelected e
	if selected
		then return ()
		else throwError $ H.Assert "Assert element is selected failed."

assertElementIsNotSelected :: String -> H.HeleniumM ()
assertElementIsNotSelected e = do
	assertElementIsDisplayed e
	selected <- HC.getElementIsSelected e
	if selected
		then throwError $ H.Assert "Assert element is not selected failed."
		else return ()

-- Keyboard
-------------------------------------------------------------------------------

sendKeys :: [Char] -> H.HeleniumM ()
sendKeys = HC.sendKeys

sendKeysToElement :: String -> [Char] -> H.HeleniumM ()
sendKeysToElement element chars = do
	assertElementIsDisplayed element
	HC.sendKeysToElement element chars

-- Cookies
-------------------------------------------------------------------------------

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

