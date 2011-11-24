module Helenium (
	HeleniumM
) where

-------------------------------------------------------------------------------

import Data.Maybe
import Data.Either
import Control.Monad.Error
import Control.Monad.RWS.Strict
import qualified Network.URI as URI
import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Stream as Stream
import qualified Text.JSON as JSON

-------------------------------------------------------------------------------

-- Helenium Monad

type HeleniumM =
	ErrorT HeleniumError (RWST HeleniumReader HeleniumWriter HeleniumState IO)

runHeleniumM ::
	HeleniumM a ->
	HeleniumReader ->
	HeleniumState ->
	IO (Either HeleniumError a, HeleniumState, HeleniumWriter)
runHeleniumM hm r s = runRWST (runErrorT hm) r s

type HeleniumError = String

type HeleniumReader = String

type HeleniumWriter = String

data HeleniumState = 
	HeleniumState {
		serverHost :: String,
		serverPort :: Integer,
		serverPath :: String,
		serverBrowser :: HeleniumBrowser,
		serverCapabilities :: [HeleniumCapability],
		serverSessionId :: Maybe String
	}

data HeleniumBrowser = 
	HeleniumBrowser 
		HeleniumBrowserName 
		HeleniumBrowserVersion 
		HeleniumBrowserPlatform

data HeleniumBrowserName = Chrome | Firefox | HtmlUnit | IE | IPhone

type HeleniumBrowserVersion = String

data HeleniumBrowserPlatform = Windows| XP | Vista | Mac | Linux | Unix

data HeleniumCapability =
	JavascriptEnabled |
	TakesScreenshot |
	HandlesAlerts |
	DatabaseEnabled |
	LocationContextEnabled |
	ApplicationCacheEnabled |
	BrowserConnectionEnabled |
	CssSelectorsEnabled |
	WebStorageEnabled |
	Rotatable |
	AcceptSslCerts |
	NativeEvents

heleniumCapabilityKey :: HeleniumCapability -> String
heleniumCapabilityKey JavascriptEnabled = "javascriptEnabled"
heleniumCapabilityKey TakesScreenshot = "takesScreenshot"
heleniumCapabilityKey HandlesAlerts = "handlesAlerts"
heleniumCapabilityKey DatabaseEnabled = "databaseEnabled"
heleniumCapabilityKey LocationContextEnabled = "locationContextEnabled"
heleniumCapabilityKey ApplicationCacheEnabled = "applicationCacheEnabled"
heleniumCapabilityKey BrowserConnectionEnabled = "browserConnectionEnabled"
heleniumCapabilityKey CssSelectorsEnabled = "cssSelectorsEnabled"
heleniumCapabilityKey WebStorageEnabled = "webStorageEnabled"
heleniumCapabilityKey Rotatable = "rotatable"
heleniumCapabilityKey AcceptSslCerts = "acceptSslCerts"
heleniumCapabilityKey NativeEvents = "nativeEvents"

-- :t runHeleniumT
-- runHeleniumT :: HeleniumT e r w s m a -> ErrorT e (RWST r w s m) a
-- :t runErrorT . runHeleniumT
-- runErrorT . runHeleniumT :: HeleniumT e r w s m a -> RWST r w s m (Either e a)
-- :t runRWST . runErrorT . runHeleniumT
-- runRWST . runErrorT . runHeleniumT :: HeleniumT e r w s m a -> r -> s -> m (Either e a, s, w)

-------------------------------------------------------------------------------

main :: IO ()
main = do
	let state = HeleniumState {
		serverHost = "http://127.0.0.1",
		serverPort = 9515,
		serverPath = "",
		serverBrowser = HeleniumBrowser Chrome "16" Linux,
		serverCapabilities = [JavascriptEnabled],
		serverSessionId = Nothing
	}
	(eitherAns, state', writer) <- runHeleniumM test "" state
	putStrLn (show eitherAns)

test :: HeleniumM ()
test = do
	connect
	goTo "http://www.olx.com"
	return ()

-- Commands
-------------------------------------------------------------------------------

-- Create a new session.
-- TODO: Add desiredCapabilities JSON object
connect :: HeleniumM ()
connect = do
	state <- get
	let browser = serverBrowser state
	let capabilities = serverCapabilities state
	ans <- callSelenium $ Request 
		False 
		(Post "{\"desiredCapabilities\": {\"javascriptEnabled\": true}}") 
		"/session"
	-- Response is: {"status":303,"value":"/session/f3ae93822f855f545dbdab66cc556453"}
	let json = responseValue ans
	case json of
		JSON.JSString jsString -> do
			let sessString = JSON.fromJSString jsString 
			let sessId = drop (length "/session/") sessString
			put $ state {serverSessionId = (Just sessId)}
		_ -> throwError "Not a valid new session answer."
	return () 

-- Delete the session.
disconnect :: HeleniumM ()
disconnect = do
	callSelenium $ Request True Delete "/"
	return ()

-- Set the amount of time, in milliseconds, that asynchronous scripts executed 
-- by /session/:sessionId/execute_async are permitted to run before they are 
-- aborted and a |Timeout| error is returned to the client.
commandTimeoutsSetAsyncScript :: String -> HeleniumM ()
commandTimeoutsSetAsyncScript s = do
	callSelenium $ Request True (Post s) "/timeouts/async_script"
	return ()

-- Set the amount of time the driver should wait when searching for elements.
commandTimeoutsSetImplicitWait :: String -> HeleniumM ()
commandTimeoutsSetImplicitWait s = do
	callSelenium $ Request True (Post s) "/timeouts/implicit_wait"
	return ()

-- Retrieve the current window handle.
commandWindowHandle :: String -> HeleniumM ()
commandWindowHandle _ = do
	callSelenium $ Request True Get "/window_handle"
	return ()

-- Retrieve the list of all window handles available to the session.
commandWindowHandles :: String -> HeleniumM ()
commandWindowHandles _ = do
	callSelenium $ Request True Get "/window_handles"
	return ()

-- Retrieve the URL of the current page.
getUrl :: HeleniumM String
getUrl = do
	ans <- callSelenium $ Request True Get "/url"
	case responseValue ans of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError "Error reading url"

-- Navigate to a new URL.
goTo :: String -> HeleniumM ()
goTo url = do
	let body = JSON.toJSObject [("url", JSON.toJSString url)]
	callSelenium $ Request True (Post $ JSON.encode body) "/url"
	return ()

-- Navigate forwards in the browser history, if possible.
commandForward :: String -> HeleniumM ()
commandForward _ = do
	callSelenium $ Request True (Post "") "/forward"
	return ()

-- Navigate backwards in the browser history, if possible.
commandBack :: String -> HeleniumM ()
commandBack _ = do
	callSelenium $ Request True (Post "") "/back"
	return ()

-- Refresh the current page.
commandRefresh :: String -> HeleniumM ()
commandRefresh _ = do
	callSelenium $ Request True (Post "") "/refresh"
	return ()

-- Inject a snippet of JavaScript into the page for execution in the context of the currently selected frame.
commandExecute :: String -> HeleniumM ()
commandExecute s = do
	callSelenium $ Request True (Post s) "/execute"
	return ()

-- Inject a snippet of JavaScript into the page for execution in the context of the currently selected frame.
commandExecuteAsync :: String -> HeleniumM ()
commandExecuteAsync s = do
	callSelenium $ Request True (Post s) "/execute_async"
	return ()

-- Take a screenshot of the current page.
commandScreenshot :: String -> HeleniumM ()
commandScreenshot _ = do
	callSelenium $ Request True Get "/screenshot"
	return ()

-- Change focus to another frame on the page.
commandFrame :: String -> HeleniumM ()
commandFrame s = do
	callSelenium $ Request True (Post s) "/frame"
	return ()

-- Change focus to another window.
commandWindowFocus :: String -> HeleniumM ()
commandWindowFocus s = do
	callSelenium $ Request True (Post s) "/window"
	return ()

-- Close the current window.
commandWindowClose :: String -> HeleniumM ()
commandWindowClose _ = do
	callSelenium $ Request True Delete "/window"
	return ()

-------------------------------------------------------------------------------

data Request = Request RequestStateful RequestMethod RequestPath

type RequestStateful = Bool

data RequestMethod = Get | Post String | Delete

type RequestPath = String

data Response = Response {
	responseHTTPCode :: ResponseHTTPCode,
	responseHTTPReason :: ResponseHTTPReason,
	responseStatus :: ResponseStatus,
	responseValue :: ResponseValue}

type ResponseHTTPCode = (Int, Int, Int)

type ResponseHTTPReason = String

type ResponseStatus = Int

type ResponseValue = JSON.JSValue

callSelenium :: Request -> HeleniumM Response
callSelenium req = do
	httpReq <- makeRequest req
	httpRes <- sendRequest httpReq
	processResponse httpRes

sendRequest :: HTTP.Request String -> HeleniumM (HTTP.Response String)
sendRequest req = do
	result <- liftIO $ catch 
		(HTTP.simpleHTTP req)
		-- Send the ioError inside the HTTP.Result type
		(\ioErr -> return $ Stream.failMisc (show ioErr)) 
	either whenLeft whenRight result where
		whenLeft connErr = throwError $ show connErr
		whenRight response = return response

processResponse :: HTTP.Response String -> HeleniumM Response
processResponse res = do
	let (x,y,z) = HTTP.rspCode res -- HTTP 200, etc
	let code = x * 100 + y * 10 + z * 1
	let reason = HTTP.rspReason res -- The "Ok", "Found" that comes after the HTTP code
	processResponseCodes (x, y, z) reason
	-- Do something with the haders??
	let headers = HTTP.rspHeaders res
	let body = HTTP.rspBody res -- The body string
	liftIO $ putStr $ show res
	liftIO $ putStrLn body
	(status, value) <- processResponseBody body
	return (Response (x,y,z) reason status value)

processResponseCodes :: (Int, Int, Int) -> String -> HeleniumM String
-- TODO: Do something!!!
processResponseCodes (x, y, z) reason = return ""

processResponseBody :: String -> HeleniumM (ResponseStatus, ResponseValue)
processResponseBody body = do
	let jsonResult = processResponseBodyJson body
	case jsonResult of
		JSON.Error msg -> throwError msg
		JSON.Ok ans -> return ans

processResponseBodyJson :: String -> JSON.Result (ResponseStatus, ResponseValue)
processResponseBodyJson body = do
	json <- JSON.decode body :: JSON.Result (JSON.JSObject JSON.JSValue)
	-- status <- JSON.valFromObj "status" json
	value <- JSON.valFromObj "value" json
	-- TODO: Parse status
	return (0, value)

-- WebDriver command messages should conform to the HTTP/1.1 request specification. 
-- All commands accept a content-type of application/json;charset=UTF-8. 
-- Message bodies for POST and PUT request must use application/json;charset=UTF-8.
makeRequest :: Request -> HeleniumM (HTTP.Request String)
makeRequest (Request rs rm rp) = do
	uri <- makeRequestUri rs rp
	method <- makeRequestMethod rm
	headers <- makeRequestHeaders rm
	body <- makeRequestBody rm
	let req = HTTP.Request {
		HTTP.rqURI = fromJust $ URI.parseURI uri,
		HTTP.rqMethod = method,
		HTTP.rqHeaders = headers,
		HTTP.rqBody = body
	}
	liftIO $ putStr $ show req
	liftIO $ putStrLn body
	return req

makeRequestUri :: RequestStateful -> RequestPath -> HeleniumM String
makeRequestUri rs rp = do
	state <- get
	let sessionId = serverSessionId state
	let baseUri = (serverHost state) ++ ":" ++ (show $ serverPort state)
	uriPath <- if rs == True
		then if isNothing sessionId
			then throwError "Making a stateful call without a session"
			else return ("/session/" ++ (fromJust sessionId) ++ rp)
		else return rp
	return $ baseUri ++ (serverPath state) ++ uriPath

makeRequestMethod :: RequestMethod -> HeleniumM HTTP.RequestMethod
makeRequestMethod Get = do return HTTP.GET
makeRequestMethod Delete = do return HTTP.DELETE
makeRequestMethod (Post _) = do return HTTP.POST

makeRequestHeaders :: RequestMethod -> HeleniumM [HTTP.Header]
makeRequestHeaders (Post s) = do 
	return [
		HTTP.Header HTTP.HdrContentType "application/json;charset=UTF-8",
		HTTP.Header HTTP.HdrContentLength $ show (length s)]
makeRequestHeaders _ = do
	return []

makeRequestBody :: RequestMethod -> HeleniumM String
makeRequestBody (Post body) = do return body
makeRequestBody _ = do return ""

data ErrorCode = 
	Success |
	NoSuchElement |
	NoSuchFrame |
	UnknownCommand |
	StaleElementReference |
	ElementNotVisible |
	InvalidElementState | 
	UnknownError | 
	ElementIsNotSelectable | 
	JavaScriptError | 
	XPathLookupError | 
	Timeout | 
	NoSuchWindow | 
	InvalidCookieDomain | 
	UnableToSetCookie | 
	UnexpectedAlertOpen | 
	NoAlertOpenError | 
	ScriptTimeout | 
	InvalidElementCoordinates | 
	IMENotAvailable | 
	IMEEngineActivationFailed | 
	InvalidSelector

errorNumberToErrorCode :: Integer -> ErrorCode
errorNumberToErrorCode 0 = Success
errorNumberToErrorCode 7 = NoSuchElement
errorNumberToErrorCode 8 = NoSuchFrame
errorNumberToErrorCode 9 = UnknownCommand
errorNumberToErrorCode 10 = StaleElementReference
errorNumberToErrorCode 11 = ElementNotVisible
errorNumberToErrorCode 12 = InvalidElementState
errorNumberToErrorCode 13 = UnknownError
errorNumberToErrorCode 15 = ElementIsNotSelectable
errorNumberToErrorCode 17 = JavaScriptError
errorNumberToErrorCode 19 = XPathLookupError
errorNumberToErrorCode 21 = Timeout
errorNumberToErrorCode 23 = NoSuchWindow
errorNumberToErrorCode 24 = InvalidCookieDomain
errorNumberToErrorCode 25 = UnableToSetCookie
errorNumberToErrorCode 26 = UnexpectedAlertOpen
errorNumberToErrorCode 27 = NoAlertOpenError
errorNumberToErrorCode 28 = ScriptTimeout
errorNumberToErrorCode 29 = InvalidElementCoordinates
errorNumberToErrorCode 30 = IMENotAvailable
errorNumberToErrorCode 31 = IMEEngineActivationFailed
errorNumberToErrorCode 32 = InvalidSelector

errorCodeToErrorMessage :: ErrorCode -> String
errorCodeToErrorMessage Success = "The command executed successfully."
errorCodeToErrorMessage NoSuchElement = "An element could not be located on the page using the given search parameters."
errorCodeToErrorMessage NoSuchFrame = "A request to switch to a frame could not be satisfied because the frame could not be found."
errorCodeToErrorMessage UnknownCommand = "The requested resource could not be found, or a request was received using an HTTP method that is not supported by the mapped resource."
errorCodeToErrorMessage StaleElementReference = "An element command failed because the referenced element is no longer attached to the DOM."
errorCodeToErrorMessage ElementNotVisible = "An element command could not be completed because the element is not visible on the page."
errorCodeToErrorMessage InvalidElementState = "An element command could not be completed because the element is in an invalid state (e.g. attempting to click a disabled element)."
errorCodeToErrorMessage UnknownError = "An unknown server-side error occurred while processing the command."
errorCodeToErrorMessage ElementIsNotSelectable = "An attempt was made to select an element that cannot be selected."
errorCodeToErrorMessage JavaScriptError = "An error occurred while executing user supplied JavaScript."
errorCodeToErrorMessage XPathLookupError = "An error occurred while searching for an element by XPath."
errorCodeToErrorMessage Timeout = "An operation did not complete before its timeout expired."
errorCodeToErrorMessage NoSuchWindow = "A request to switch to a different window could not be satisfied because the window could not be found."
errorCodeToErrorMessage InvalidCookieDomain = "An illegal attempt was made to set a cookie under a different domain than the current page."
errorCodeToErrorMessage UnableToSetCookie = "A request to set a cookie's value could not be satisfied."
errorCodeToErrorMessage UnexpectedAlertOpen = "A modal dialog was open, blocking this operation."
errorCodeToErrorMessage NoAlertOpenError = "An attempt was made to operate on a modal dialog when one was not open."
errorCodeToErrorMessage ScriptTimeout = "A script did not complete before its timeout expired."
errorCodeToErrorMessage InvalidElementCoordinates = "The coordinates provided to an interactions operation are invalid."
errorCodeToErrorMessage IMENotAvailable = "IME was not available."
errorCodeToErrorMessage IMEEngineActivationFailed = "An IME engine could not be started."
errorCodeToErrorMessage InvalidSelector = "Argument was an invalid selector (e.g. XPath/CSS)."

