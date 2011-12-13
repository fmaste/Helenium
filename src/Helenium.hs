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
	getElementText,
	submitElement,
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
	assertCookieDoesNotExists,
	setTimeoutAsyncScript
	-- TODO: execute,
	-- TODO: executeAsync,
) where

-------------------------------------------------------------------------------

import qualified Helenium.Base as H
import qualified Helenium.Log as HL
import qualified Helenium.Network as HN
import qualified Helenium.Runner as HR
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

runTest :: H.HeleniumM () -> IO ()
runTest = HR.runTest

-- Log commands
-------------------------------------------------------------------------------

echo :: String -> H.HeleniumM ()
echo m = do
	HL.logMsg $ H.Info m

-- Time commands
-------------------------------------------------------------------------------

getEpoch :: H.HeleniumM String
getEpoch = do
	t <- liftIO TimePosix.getPOSIXTime
	-- The show returns something like "123456.1234s"
	return $ takeWhile (\c -> c /= '.') (show t)

getCurrentTime :: H.HeleniumM String
getCurrentTime = do
	t <- liftIO Time.getCurrentTime
	return $ show t

-- Flow control commands
-------------------------------------------------------------------------------

for :: [a] -> (a -> H.HeleniumM b) -> H.HeleniumM ()
for as f = forM_ as f

times :: Int -> H.HeleniumM () -> H.HeleniumM ()
times n t = replicateM_ n t

-- Suspends the current thread for a given number of seconds.
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
		else throwError ("Not " ++ p ++ ": " ++ show a ++ " with " ++ show b)

assertEqual :: (Eq x, Show x) => x -> x -> H.HeleniumM ()
assertEqual a b = assert "equal" (==) a b

assertLess :: (Ord x, Show x) => x -> x -> H.HeleniumM ()
assertLess a b = assert "less" (<) a b

assertGreater :: (Ord x, Show x) => x -> x -> H.HeleniumM ()
assertGreater a b = assert "greater" (>) a b

assertLessOrEqual :: (Ord x, Show x) => x -> x -> H.HeleniumM ()
assertLessOrEqual a b = assert "less" (<) a b

assertGreaterOrEqual :: (Ord x, Show x) => x -> x -> H.HeleniumM ()
assertGreaterOrEqual a b = assert "greater" (>) a b

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

assertPrefix :: String -> String -> H.HeleniumM ()
assertPrefix a b = assert "prefix" isPrefixOf a b

assertSuffix :: String -> String -> H.HeleniumM ()
assertSuffix a b = assert "suffix" isSuffixOf a b

assertInfix :: String -> String -> H.HeleniumM ()
assertInfix a b = assert "infix" isInfixOf a b

-- Client-server commands
-------------------------------------------------------------------------------

type ResponseStatus = Int

type ResponseValue = JSON.JSValue

-- If the remote server must return a 4xx response, the response body shall 
-- have a Content-Type of text/plain and the message body shall be a descriptive 
-- message of the bad request. For all other cases, if a response includes a 
-- message body, it must have a Content-Type of application/json;charset=UTF-8 
-- and will be a JSON object.
processResponseHttp :: HN.Response -> H.HeleniumM ()
processResponseHttp res = do
	case HN.responseHTTPCode res of
		(2, 0, 0) -> return () -- An Ok response with content.
		(2, 0, 4) -> return () -- An Ok response with no content.
		(3, 0, 2) -> return () -- The new session redirect.
		-- There are two levels of error handling specified by the wire
		-- protocol: invalid requests and failed commands.
		-- Note this is the only error in the Invalid Request category
		-- that does not return a 4xx status code.
		(5, 0, 1) -> processResponseFailedCommand res
		-- Invalid requests.
		(4, _, _) -> processResponseInvalidRequest res
		-- Failed commands.
		(5, _, _) -> processResponseFailedCommand res
		-- TODO: More descriptive message.
		(_, _, _) -> throwError $ "Unknown server response."

-- All invalid requests should result in the server returning a 4xx HTTP response.
-- The response Content-Type should be set to text/plain and the message body 
-- should be a descriptive error message.
processResponseInvalidRequest :: HN.Response -> H.HeleniumM ()
processResponseInvalidRequest res = do
	-- TODO: Send it to the log!
	throwError $ HN.responseHTTPBody res 

-- If a request maps to a valid command and contains all of the expected
-- parameters in the request body, yet fails to execute successfully, then the
-- server should send a 500 Internal Server Error. This response should have a
-- Content-Type of application/json;charset=UTF-8 and the response body should
-- be a well formed JSON response object.
processResponseFailedCommand :: HN.Response -> H.HeleniumM ()
processResponseFailedCommand res = do
	(msg, maybeScreen) <- processResponseFailedCommandBody $ HN.responseHTTPBody res
	if isJust maybeScreen
		then HL.logMsg $ H.Screenshot (fromJust maybeScreen)
		else return ()
	-- TODO: Send it to the log!
	throwError msg

type FailedCommandMessage = String

type FailedCommandScreen = Maybe String

processResponseFailedCommandBody :: String -> H.HeleniumM (FailedCommandMessage, FailedCommandScreen)
processResponseFailedCommandBody body = do
	-- Remove trailing nuls.
	let body' = reverse $ dropWhile (== '\0') $ reverse body
	let jsonResult = processResponseFailedCommandBodyJson body'
	case jsonResult of
		JSON.Error msg -> throwError $ "Error parsing JSON response: " ++ msg
		JSON.Ok ans -> return ans

processResponseFailedCommandBodyJson :: String -> JSON.Result (FailedCommandMessage, FailedCommandScreen)
processResponseFailedCommandBodyJson body = do
	-- The response must be a JSON object with a "message" and "screen" property.
	json <- (JSON.decode body :: (JSON.Result (JSON.JSObject JSON.JSValue)))
	msgJson <- JSON.valFromObj "message" json
	msg <- case JSON.readJSON msgJson of
		JSON.Ok (JSON.JSString msg') -> return $ JSON.fromJSString msg'
		_ -> JSON.Error "Invalid failed command response, it has no error message."
	screenJson <- JSON.valFromObj "screen" json
	maybeScreen <- case JSON.readJSON screenJson of
		JSON.Ok (JSON.JSString screen') -> return $ Just (JSON.fromJSString screen')
		_ -> return Nothing
	return (msg, maybeScreen)

processResponseBody :: String -> H.HeleniumM (ResponseStatus, ResponseValue)
processResponseBody body = do
	-- Remove trailing nuls.
	let body' = reverse $ dropWhile (== '\0') $ reverse body
	let jsonResult = processResponseBodyJson body'
	case jsonResult of
		JSON.Error msg -> throwError $ "Error parsing JSON response: " ++ msg
		-- TODO: Check error status codes!
		JSON.Ok ans -> return ans

processResponseBodyJson :: String -> JSON.Result (ResponseStatus, ResponseValue)
processResponseBodyJson body = do
	-- The response must be a JSON object with a "status" and "value" property.
	json <- (JSON.decode body :: (JSON.Result (JSON.JSObject JSON.JSValue)))
	statusJson <- JSON.valFromObj "status" json
	status <- case JSON.readJSON statusJson of
		JSON.Ok (JSON.JSRational False e) -> return $ fromEnum e
		JSON.Error msg -> JSON.Error "Error parsing JSON reponse: Invalid status property."
	value <- JSON.valFromObj "value" json
	return (status, value)

-- Navigate to a new URL.
goTo :: String -> H.HeleniumM ()
goTo url = do
	let body = JSON.toJSObject [("url", JSON.toJSString url)]
	HN.callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/url"
	return ()

-- Retrieve the URL of the current page.
getUrl :: H.HeleniumM String
getUrl = do
	ans <- HN.callSelenium $ HN.Request True HN.Get "/url"
	(status, value) <- processResponseBody $ HN.responseHTTPBody ans
	case value of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError "Error reading url, not a valid JSON response."

getTitle :: H.HeleniumM String
getTitle = do
	ans <- HN.callSelenium $ HN.Request True HN.Get "/title"
	(status, value) <- processResponseBody $ HN.responseHTTPBody ans
	case value of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError "Error reading title, not a valid JSON response."

-- Refresh the current page.
refresh :: H.HeleniumM ()
refresh = do
	HN.callSelenium $ HN.Request True (HN.Post "") "/refresh"
	return ()

-- Navigate backwards in the browser history, if possible.
back :: H.HeleniumM ()
back = do
	HN.callSelenium $ HN.Request True (HN.Post "") "/back"
	return ()

-- Navigate forwards in the browser history, if possible.
forward :: H.HeleniumM ()
forward = do
	HN.callSelenium $ HN.Request True (HN.Post "") "/forward"
	return ()

-- Take a screenshot of the current page.
-- Returns the screenshot as a base64 encoded PNG.
takeScreenshot :: H.HeleniumM ()
takeScreenshot = do
	ans <- HN.callSelenium $ HN.Request True HN.Get "/screenshot"
	(status, value) <- processResponseBody $ HN.responseHTTPBody ans
	case value of
		JSON.JSString jsString -> HL.logMsg $ H.Screenshot (JSON.fromJSString jsString)
		_ -> throwError "Error reading screenshot, not a valid JSON response."

changeFocusToIframeById :: String -> H.HeleniumM ()
changeFocusToIframeById iframeName = do
	let body = JSON.toJSObject [("id", JSON.toJSString iframeName)]
	ans <- HN.callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/frame"
	return ()

changeFocusToIframeByNumber :: Int -> H.HeleniumM ()
changeFocusToIframeByNumber iframeNumber = do
	let body = JSON.toJSObject [("id", JSON.showJSON iframeNumber)]
	ans <- HN.callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/frame"
	return ()

changeFocusToDefaultIframe :: H.HeleniumM ()
changeFocusToDefaultIframe = do
	let body = JSON.toJSObject [("id", JSON.JSNull)]
	ans <- HN.callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/frame"
	return ()

-- Get the element on the page that currently has focus.
getActiveElement :: H.HeleniumM String
getActiveElement = do
	ans <- HN.callSelenium $ HN.Request True (HN.Post "") "/element/active"
	processElementResponse ans

-- Search for an element on the page, starting from the document root.
-- Each locator must return the first matching element located in the DOM.
getResponseElementBy :: String -> String -> H.HeleniumM HN.Response
getResponseElementBy using value = do
	let body = JSON.toJSObject [
		("using", JSON.toJSString using),
		("value", JSON.toJSString value)]
	-- Return {"ELEMENT":":wdc:1322198176445"}
	ans <- HN.callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/element"
	return ans

getResponseElementById :: String -> H.HeleniumM HN.Response
getResponseElementById id = getResponseElementBy "id" id

getResponseElementByName :: String -> H.HeleniumM HN.Response
getResponseElementByName name = getResponseElementBy "name" name

getResponseElementByClassName :: String -> H.HeleniumM HN.Response
getResponseElementByClassName className = getResponseElementBy "class name" className

getResponseElementByCssSelector :: String -> H.HeleniumM HN.Response
getResponseElementByCssSelector css = getResponseElementBy "css selector" css

-- Returns an anchor element whose visible text matches the search value.
getResponseElementByText :: String -> H.HeleniumM HN.Response
getResponseElementByText text = getResponseElementBy "link text" text

-- Returns an anchor element whose visible text partially matches the search value.
getResponseElementByPartialText :: String -> H.HeleniumM HN.Response
getResponseElementByPartialText text = getResponseElementBy "partial link text" text

getResponseElementByXPath :: String -> H.HeleniumM HN.Response
getResponseElementByXPath x = getResponseElementBy "xpath" x

processElementResponse :: HN.Response -> H.HeleniumM String
processElementResponse ans = do
	(status, value) <- processResponseBody $ HN.responseHTTPBody ans 
	case value of
		(JSON.JSObject obj) -> case JSON.valFromObj "ELEMENT" obj of
			JSON.Ok (JSON.JSString element) -> return $ JSON.fromJSString element
			_ -> throwError "Error reading element, not a valid JSON response."
		_ -> throwError "Error reading element, not a valid JSON response."

getElementById :: String -> H.HeleniumM String
getElementById id = do
	ans <- getResponseElementById id
	processElementResponse ans

getElementByName :: String -> H.HeleniumM String
getElementByName name = do
	ans <- getResponseElementByName name
	processElementResponse ans

getElementByClassName :: String -> H.HeleniumM String
getElementByClassName className = do
	ans <- getResponseElementByClassName className
	processElementResponse ans

getElementByCssSelector :: String -> H.HeleniumM String
getElementByCssSelector css = do
	ans <- getResponseElementByCssSelector css
	processElementResponse ans

-- Returns an anchor element whose visible text matches the search value.
getElementByText :: String -> H.HeleniumM String
getElementByText text = do
	ans <- getResponseElementByText text
	processElementResponse ans

-- Returns an anchor element whose visible text partially matches the search value.
getElementByPartialText :: String -> H.HeleniumM String
getElementByPartialText text = do
	ans <- getResponseElementByPartialText text
	processElementResponse ans

getElementByXPath :: String -> H.HeleniumM String
getElementByXPath x = do
	ans <- getResponseElementByXPath x
	processElementResponse ans

processElementDoesNotExistsResponse :: HN.Response -> H.HeleniumM ()
processElementDoesNotExistsResponse ans = do
	(status, value) <- processResponseBody $ HN.responseHTTPBody ans
	if status == 7 -- TODO: Use status codes!!!
		then return ()
		else throwError "Response was not NoSushElement"

assertElementDoesNotExistsById :: String -> H.HeleniumM ()
assertElementDoesNotExistsById id = do
	ans <- getResponseElementById id
	processElementDoesNotExistsResponse ans

assertElementDoesNotExistsByName :: String -> H.HeleniumM ()
assertElementDoesNotExistsByName name = do
	ans <- getResponseElementById name
	processElementDoesNotExistsResponse ans

assertElementDoesNotExistsByClassName :: String -> H.HeleniumM ()
assertElementDoesNotExistsByClassName className = do
	ans <- getResponseElementByClassName className
	processElementDoesNotExistsResponse ans

assertElementDoesNotExistsByCssSelector :: String -> H.HeleniumM ()
assertElementDoesNotExistsByCssSelector css = do
	ans <- getResponseElementByCssSelector css
	processElementDoesNotExistsResponse ans

assertElementDoesNotExistsByText :: String -> H.HeleniumM ()
assertElementDoesNotExistsByText text = do
	ans <- getResponseElementByText text
	processElementDoesNotExistsResponse ans

assertElementDoesNotExistsByPartialText :: String -> H.HeleniumM ()
assertElementDoesNotExistsByPartialText text = do
	ans <- getResponseElementByPartialText text
	processElementDoesNotExistsResponse ans

assertElementDoesNotExistsByXPath :: String -> H.HeleniumM ()
assertElementDoesNotExistsByXPath x = do
	ans <- getResponseElementByXPath x
	processElementDoesNotExistsResponse ans

-- Click on an element.
clickElement :: String -> H.HeleniumM ()
clickElement e = do
	HN.callSelenium $ HN.Request True (HN.Post "") $ "/element/" ++ e ++ "/click"
	return ()

-- Clear a TEXTAREA or text INPUT element's value.
clearElement :: String -> H.HeleniumM ()
clearElement e = do
	HN.callSelenium $ HN.Request True (HN.Post "") $ "/element/" ++ e ++ "/clear"
	-- TODO: Check error. See the procotol!
	return ()

getElementText :: String -> H.HeleniumM String
getElementText e = do
	ans <- HN.callSelenium $ HN.Request True HN.Get ("/element/" ++ e ++ "/text")
	(status, value) <- processResponseBody $ HN.responseHTTPBody ans
	case value of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError "Error reading element text, not a valid JSON response."

-- Submit a FORM element. The submit command may also be applied to any element 
-- that is a descendant of a FORM element.
submitElement :: String -> H.HeleniumM ()
submitElement e = do
	HN.callSelenium $ HN.Request True (HN.Post "") $ "/element/" ++ e ++ "/submit"
	return ()

sendKeys :: [Char] -> H.HeleniumM ()
sendKeys ks = do
	let body = JSON.toJSObject [
		("value", JSON.JSArray $ map JSON.showJSON ks)]
	HN.callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/keys"
	return ()

sendKeysToElement :: String -> [Char] -> H.HeleniumM ()
sendKeysToElement e ks = do
	let body = JSON.toJSObject [
                ("value", JSON.JSArray $ map JSON.showJSON ks)]
	HN.callSelenium $ HN.Request True (HN.Post $ JSON.encode body) $ "/element/" ++ e ++ "/value"
	return ()

getElementIsEnabled :: String -> H.HeleniumM Bool
getElementIsEnabled e = do
	ans <- HN.callSelenium $ HN.Request True HN.Get $ "/element/" ++ e ++ "/enabled"
	(status, value) <- processResponseBody $ HN.responseHTTPBody ans
	case value of
		JSON.JSBool bool -> return bool
		_ -> throwError "Error reading element enabled property, not a valid JSON response."

assertElementIsEnabled :: String -> H.HeleniumM ()
assertElementIsEnabled e = do
	enabled <- getElementIsEnabled e
	if enabled
		then return ()
		else throwError "Assert element is enabled failed."
	
assertElementIsNotEnabled :: String -> H.HeleniumM ()
assertElementIsNotEnabled e = do
	enabled <- getElementIsEnabled e
	if enabled
		then throwError "Assert element is not enabled failed."
		else return ()

getElementIsDisplayed :: String -> H.HeleniumM Bool
getElementIsDisplayed e = do
	ans <- HN.callSelenium $ HN.Request True HN.Get $ "/element/" ++ e ++ "/displayed"
	(status, value) <- processResponseBody $ HN.responseHTTPBody ans
	case value of
		JSON.JSBool bool -> return bool
		_ -> throwError "Error reading element displayed property, not a valid JSON response."	

assertElementIsDisplayed :: String -> H.HeleniumM ()
assertElementIsDisplayed e = do
	displayed <- getElementIsDisplayed e
	if displayed
		then return ()
		else throwError "Assert element is displayed failed."

assertElementIsNotDisplayed :: String -> H.HeleniumM ()
assertElementIsNotDisplayed e = do
	displayed <- getElementIsDisplayed e
	if displayed
		then throwError "Assert element is not displayed failed."
		else return ()

getElementIsSelected :: String -> H.HeleniumM Bool
getElementIsSelected e = do
	ans <- HN.callSelenium $ HN.Request True HN.Get $ "/element/" ++ e ++ "/selected"
	(status, value) <- processResponseBody $ HN.responseHTTPBody ans
	case value of
		JSON.JSBool bool -> return bool
		_ -> throwError "Error reading element selected property, not a valid JSON response."

assertElementIsSelected :: String -> H.HeleniumM ()
assertElementIsSelected e = do
	selected <- getElementIsSelected e
	if selected
		then return ()
		else throwError "Assert element is selected failed."

assertElementIsNotSelected :: String -> H.HeleniumM ()
assertElementIsNotSelected e = do
	selected <- getElementIsSelected e
	if selected
		then throwError "Assert element is not selected failed."
		else return ()

getCookies :: H.HeleniumM [JSON.JSValue]
getCookies = do
	ans <- HN.callSelenium $ HN.Request True HN.Get "/cookie"
	(status, value) <- processResponseBody $ HN.responseHTTPBody ans
	case value of
		JSON.JSArray cookies -> return cookies
		_ -> throwError "Error reading cookies, not a valid JSON response."

getCookieByName :: String -> H.HeleniumM (Maybe JSON.JSValue)
getCookieByName name = do
	cookies <- getCookies
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
			_ -> throwError $ "Error reading cookie value, not a valid JSON response."
		Nothing -> throwError $ "Cookie does not exists: " ++ name ++ "."
		_ -> throwError $ "Error reading cookie value, not a valid JSON response."

getCookieExpiresEpoch :: String -> H.HeleniumM Int
getCookieExpiresEpoch name = do
	cookie <- getCookieByName name
	case cookie of
		Just (JSON.JSObject obj) -> case JSON.valFromObj "expiry" obj of
			JSON.Ok (JSON.JSRational False e) -> return $ fromEnum e
			_ -> throwError $ "Error reading cookie expires, not a valid JSON response."
		Nothing -> throwError $ "Cookie does not exists: " ++ name ++ "."
		_ -> throwError $ "Error reading cookie expires, not a vlaid JSON response."

deleteAllCookies :: H.HeleniumM ()
deleteAllCookies = do
	HN.callSelenium $ HN.Request True HN.Delete "/cookie"
	return ()

deleteCookieByName :: String -> H.HeleniumM ()
deleteCookieByName name = do
	HN.callSelenium $ HN.Request True HN.Delete ("/cookie/" ++ name)
	return ()

assertCookieDoesNotExists :: String -> H.HeleniumM ()
assertCookieDoesNotExists name = do
	cookie <- getCookieByName name
	case cookie of
		Nothing -> return ()
		_ -> throwError $ "Cookie exists: " ++ name ++ "."

-- Set the amount of time, in milliseconds, that asynchronous scripts executed 
-- by /session/:sessionId/execute_async are permitted to run before they are 
-- aborted and a |Timeout| error is returned to the client.
setTimeoutAsyncScript :: Int -> H.HeleniumM ()
setTimeoutAsyncScript ms = do
	let body = JSON.toJSObject [("ms", JSON.showJSON ms)]
	HN.callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/timeouts/async_script"
	return ()

-- TODO:
-- Inject a snippet of JavaScript into the page for execution in the context of the currently selected frame.
execute :: String -> H.HeleniumM ()
execute s = do
	HN.callSelenium $ HN.Request True (HN.Post s) "/execute"
	return ()

-- TODO:
-- Inject a snippet of JavaScript into the page for execution in the context of the currently selected frame.
executeAsync :: String -> H.HeleniumM ()
executeAsync s = do
	HN.callSelenium $ HN.Request True (HN.Post s) "/execute_async"
	return ()

