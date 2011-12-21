{-# OPTIONS_HADDOCK hide #-}

module Helenium.Commands (
	ResponseValue,
	goTo,
	getUrl,
	getTitle,
	refresh,
	back,
	forward,
	getScreenshot,
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
	sendKeys,
	sendKeysToElement,
	clickElement,
	clearElement,
	submitElement,
	getElementText,
	getElementIsEnabled,
	getElementIsSelected,
	getElementIsDisplayed,
	getCookies,
	deleteAllCookies,
	deleteCookieByName
) where

-------------------------------------------------------------------------------

import qualified Helenium.Base as H
import qualified Helenium.Network as HN
import Data.Maybe
import Control.Monad.Error
import Control.Monad.RWS.Strict
import qualified Text.JSON as JSON

-- Client server communication
-------------------------------------------------------------------------------

type ResponseValue = JSON.JSValue

callSelenium :: HN.Request -> H.HeleniumM ResponseValue
callSelenium req = do
	ans <- HN.callSelenium req
	processResponseHttp ans

-- If the remote server must return a 4xx response, the response body shall 
-- have a Content-Type of text/plain and the message body shall be a descriptive 
-- message of the bad request. For all other cases, if a response includes a 
-- message body, it must have a Content-Type of application/json;charset=UTF-8 
-- and will be a JSON object.
processResponseHttp :: HN.Response -> H.HeleniumM ResponseValue
processResponseHttp res = do
	case HN.responseHTTPCode res of
		-- Ok response with content.
		(2, 0, 0) -> do
			(status, value) <- processResponseBody $ HN.responseHTTPBody res
			-- TODO: Is it valid a response with HTTP 200 and a status that is not 0 ??
			-- Maybe throw a failed command exception instead.
			when (status /= 0) $ throwError $ H.Unknown $ 
				"Response has an error status code: " ++ (show status)
			return value
		-- Ok response with no content.
		(2, 0, 4) -> return JSON.JSNull
		-- There are two levels of error handling specified by the wire
		-- protocol: invalid requests and failed commands:
		-- Command has not been implemented on the server.
		-- Note this is the only error in the Invalid Request category
		-- that does not return a 4xx status code.
		(5, 0, 1) -> processResponseInvalidRequest res
		-- Invalid requests.
		(4, _, _) -> processResponseInvalidRequest res
		-- Failed commands.
		(5, _, _) -> processResponseFailedCommand res
		-- TODO: More descriptive message.
		(_, _, _) -> throwError $ H.Unknown "Unknown server response."

-- All invalid requests should result in the server returning a 4xx HTTP response.
-- The response Content-Type should be set to text/plain and the message body 
-- should be a descriptive error message.
processResponseInvalidRequest :: HN.Response -> H.HeleniumM ResponseValue
processResponseInvalidRequest res = do
	-- TODO: Send it to the log!
	throwError $ H.InvalidRequest $ HN.responseHTTPBody res 

-- If a request maps to a valid command and contains all of the expected
-- parameters in the request body, yet fails to execute successfully, then the
-- server should send a 500 Internal Server Error. This response should have a
-- Content-Type of application/json;charset=UTF-8 and the response body should
-- be a well formed JSON response object.
processResponseFailedCommand :: HN.Response -> H.HeleniumM ResponseValue
processResponseFailedCommand res = do
	(status, value) <- processResponseBody $ HN.responseHTTPBody res
	(msg, maybeScreen) <- processResponseFailedCommandJson value
	throwError $ H.FailedCommand status msg maybeScreen

type FailedCommandMessage = String

type FailedCommandScreen = Maybe String

processResponseFailedCommandJson :: 
	ResponseValue -> H.HeleniumM (FailedCommandMessage, FailedCommandScreen)
processResponseFailedCommandJson json = case json of
	-- The response must be a JSON object with a "message" and "screen" property.
	(JSON.JSObject jsonObj) -> do
		msg <- case (JSON.valFromObj "message" jsonObj) of
			JSON.Ok (JSON.JSString msg') -> return $ JSON.fromJSString msg'
			_ -> throwError $ H.Unknown "Invalid failed command response, it has no error message."
		maybeScreen <- case (JSON.valFromObj "screen" jsonObj) of
			JSON.Ok (JSON.JSString screen') -> return $ Just (JSON.fromJSString screen')
			_ -> return Nothing
		return (msg, maybeScreen)
	_ -> throwError $ H.Unknown "Invalid failed command response, it is not a JSON object."

processResponseBody :: String -> H.HeleniumM (H.Status, ResponseValue)
processResponseBody body = do
	-- Remove trailing nuls.
	let body' = reverse $ dropWhile (== '\0') $ reverse body
	let jsonResult = processResponseBodyJson body'
	case jsonResult of
		JSON.Error msg -> throwError $ H.Unknown $ "Error parsing JSON response: " ++ msg
		JSON.Ok (status, value) -> return (status, value)

processResponseBodyJson :: String -> JSON.Result (H.Status, ResponseValue)
processResponseBodyJson body = do
	-- The response must be a JSON object with a "status" and "value" property.
	json <- (JSON.decode body :: (JSON.Result (JSON.JSObject JSON.JSValue)))
	statusJson <- JSON.valFromObj "status" json
	status <- case JSON.readJSON statusJson of
		JSON.Ok (JSON.JSRational False e) -> return $ fromEnum e
		JSON.Error msg -> JSON.Error "Error parsing JSON reponse: Invalid status property."
	value <- JSON.valFromObj "value" json
	return (status, value)

-- Commands
-------------------------------------------------------------------------------

-- Browser navigation commands
-------------------------------------------------------------------------------

goTo :: String -> H.HeleniumM ()
goTo url = do
	let body = JSON.toJSObject [("url", JSON.toJSString url)]
	callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/url"
	return ()

getUrl :: H.HeleniumM String
getUrl = do
	value <- callSelenium $ HN.Request True HN.Get "/url"
	case value of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError $ H.Unknown "Error reading url, not a valid JSON response."

getTitle :: H.HeleniumM String
getTitle = do
	value <- callSelenium $ HN.Request True HN.Get "/title"
	case value of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError $ H.Unknown "Error reading title, not a valid JSON response."

refresh :: H.HeleniumM ()
refresh = do
	callSelenium $ HN.Request True (HN.Post "") "/refresh"
	return ()

back :: H.HeleniumM ()
back = do
	callSelenium $ HN.Request True (HN.Post "") "/back"
	return ()

forward :: H.HeleniumM ()
forward = do
	callSelenium $ HN.Request True (HN.Post "") "/forward"
	return ()

-- Returns the screenshot as a base64 encoded PNG.
getScreenshot :: H.HeleniumM String
getScreenshot = do
	value <- callSelenium $ HN.Request True HN.Get "/screenshot"
	case value of
		JSON.JSString jsString -> return (JSON.fromJSString jsString)
		_ -> throwError $ H.Unknown "Error reading screenshot, not a valid JSON response."

-- Frame manipulation
-------------------------------------------------------------------------------

changeFocusToIframeById :: String -> H.HeleniumM ()
changeFocusToIframeById iframeName = do
	let body = JSON.toJSObject [("id", JSON.toJSString iframeName)]
	callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/frame"
	state <- get
	put $ state {H.currentFrame = H.FrameById iframeName}
	return ()

changeFocusToIframeByNumber :: Int -> H.HeleniumM ()
changeFocusToIframeByNumber iframeNumber = do
	let body = JSON.toJSObject [("id", JSON.showJSON iframeNumber)]
	callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/frame"
	state <- get
	put $ state {H.currentFrame = H.FrameByNumber iframeNumber}
	return ()

changeFocusToDefaultIframe :: H.HeleniumM ()
changeFocusToDefaultIframe = do
	let body = JSON.toJSObject [("id", JSON.JSNull)]
	callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/frame"
	state <- get
	put $ state {H.currentFrame = H.DefaultFrame}
	return ()

-- Find elements commands
-------------------------------------------------------------------------------

-- Process the response of a search for multiple elements on the page.
-- Search starts from the document root and element are in the order located in the DOM.
-- The located elements will be returned as a WebElement JSON objects.
processMultipleElementsResponse :: ResponseValue -> H.HeleniumM [String]
processMultipleElementsResponse value = do
	case value of
		(JSON.JSArray js) -> mapM processElementResponse js
		_ -> throwError $ H.Unknown "Error reading multiple elements, not a valid JSON response."

processElementResponse :: ResponseValue -> H.HeleniumM String
processElementResponse value = do
	case value of
		(JSON.JSObject obj) -> case JSON.valFromObj "ELEMENT" obj of
			JSON.Ok (JSON.JSString element) -> return $ JSON.fromJSString element
			_ -> throwError $ H.Unknown "Error reading element, not a valid JSON response."
		_ -> throwError $ H.Unknown "Error reading element, not a valid JSON response."

getActiveElement :: H.HeleniumM String
getActiveElement = do
	ans <- callSelenium $ HN.Request True (HN.Post "") "/element/active"
	processElementResponse ans

-- Search for an element on the page, starting from the document root.
-- Each locator must return the first matching element located in the DOM.
getResponseElementBy :: String -> String -> H.HeleniumM ResponseValue
getResponseElementBy using value = do
	let body = JSON.toJSObject [
 		("using", JSON.toJSString using),
		("value", JSON.toJSString value)]
	-- Return {"ELEMENT":":wdc:1322198176445"}
	callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/element"

getResponseElementById :: String -> H.HeleniumM ResponseValue
getResponseElementById id = getResponseElementBy "id" id

getResponseElementByName :: String -> H.HeleniumM ResponseValue
getResponseElementByName name = getResponseElementBy "name" name

getResponseElementByClassName :: String -> H.HeleniumM ResponseValue
getResponseElementByClassName className = getResponseElementBy "class name" className

getResponseElementByCssSelector :: String -> H.HeleniumM ResponseValue
getResponseElementByCssSelector css = getResponseElementBy "css selector" css

getResponseElementByText :: String -> H.HeleniumM ResponseValue
getResponseElementByText text = getResponseElementBy "link text" text

getResponseElementByPartialText :: String -> H.HeleniumM ResponseValue
getResponseElementByPartialText text = getResponseElementBy "partial link text" text

getResponseElementByXPath :: String -> H.HeleniumM ResponseValue
getResponseElementByXPath x = getResponseElementBy "xpath" x

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

getElementByText :: String -> H.HeleniumM String
getElementByText text = do
	ans <- getResponseElementByText text
	processElementResponse ans

getElementByPartialText :: String -> H.HeleniumM String
getElementByPartialText text = do
	ans <- getResponseElementByPartialText text
	processElementResponse ans

getElementByXPath :: String -> H.HeleniumM String
getElementByXPath x = do
	ans <- getResponseElementByXPath x
	processElementResponse ans

-- Keyboard
-------------------------------------------------------------------------------

sendKeys :: [Char] -> H.HeleniumM ()
sendKeys ks = do
	let body = JSON.toJSObject [
		("value", JSON.JSArray $ map JSON.showJSON ks)]
	callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/keys"
	return ()

sendKeysToElement :: String -> [Char] -> H.HeleniumM ()
sendKeysToElement e ks = do
	let body = JSON.toJSObject [
		("value", JSON.JSArray $ map JSON.showJSON ks)]
	callSelenium $ HN.Request True (HN.Post $ JSON.encode body) $ "/element/" ++ e ++ "/value"
	return ()

-- Elements basic actions
-------------------------------------------------------------------------------

clickElement :: String -> H.HeleniumM ()
clickElement e = do
	callSelenium $ HN.Request True (HN.Post "") $ "/element/" ++ e ++ "/click"
	return ()

clearElement :: String -> H.HeleniumM ()
clearElement e = do
	callSelenium $ HN.Request True (HN.Post "") $ "/element/" ++ e ++ "/clear"
	return ()

submitElement :: String -> H.HeleniumM ()
submitElement e = do
	callSelenium $ HN.Request True (HN.Post "") $ "/element/" ++ e ++ "/submit"
	return ()

-- Elements properties
-------------------------------------------------------------------------------

getElementText :: String -> H.HeleniumM String
getElementText e = do
	value <- callSelenium $ HN.Request True HN.Get ("/element/" ++ e ++ "/text")
	case value of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError $ H.Unknown "Error reading element text, not a valid JSON response."

getElementIsEnabled :: String -> H.HeleniumM Bool
getElementIsEnabled e = do
	value <- callSelenium $ HN.Request True HN.Get $ "/element/" ++ e ++ "/enabled"
	case value of
		JSON.JSBool bool -> return bool
		_ -> throwError $ H.Unknown "Error reading element enabled property, not a valid JSON response."

getElementIsSelected :: String -> H.HeleniumM Bool
getElementIsSelected e = do
	value <- callSelenium $ HN.Request True HN.Get $ "/element/" ++ e ++ "/selected"
	case value of
		JSON.JSBool bool -> return bool
		_ -> throwError $ H.Unknown "Error reading element selected property, not a valid JSON response."

getElementIsDisplayed :: String -> H.HeleniumM Bool
getElementIsDisplayed e = do
	value <- callSelenium $ HN.Request True HN.Get $ "/element/" ++ e ++ "/displayed"
	case value of
		JSON.JSBool bool -> return bool
		_ -> throwError $ H.Unknown "Error reading element displayed property, not a valid JSON response."

-- Cookies
-------------------------------------------------------------------------------

-- TODO: Create a Cookie data type
getCookies :: H.HeleniumM [JSON.JSValue]
getCookies = do
	value <- callSelenium $ HN.Request True HN.Get "/cookie"
	case value of
		JSON.JSArray cookies -> return cookies
		_ -> throwError $ H.Unknown "Error reading cookies, not a valid JSON response."

deleteAllCookies :: H.HeleniumM ()
deleteAllCookies = do
	callSelenium $ HN.Request True HN.Delete "/cookie"
	return ()

deleteCookieByName :: String -> H.HeleniumM ()
deleteCookieByName name = do
	callSelenium $ HN.Request True HN.Delete ("/cookie/" ++ name)
	return ()

-- TODO
-------------------------------------------------------------------------------

-- Set the amount of time, in milliseconds, that asynchronous scripts executed 
-- by /session/:sessionId/execute_async are permitted to run before they are 
-- aborted and a |Timeout| error is returned to the client.
setTimeoutAsyncScript :: Int -> H.HeleniumM ()
setTimeoutAsyncScript ms = do
	let body = JSON.toJSObject [("ms", JSON.showJSON ms)]
	callSelenium $ HN.Request True (HN.Post $ JSON.encode body) "/timeouts/async_script"
	return ()

-- Inject a snippet of JavaScript into the page for execution in the context of the currently selected frame.
execute :: String -> H.HeleniumM ()
execute s = do
	callSelenium $ HN.Request True (HN.Post s) "/execute"
	return ()

-- Inject a snippet of JavaScript into the page for execution in the context of the currently selected frame.
executeAsync :: String -> H.HeleniumM ()
executeAsync s = do
	callSelenium $ HN.Request True (HN.Post s) "/execute_async"
	return ()

