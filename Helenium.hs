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
		serverPort = 4444,
		serverPath = "/wd/hub",
		serverCapabilities = [],
		serverSessionId = Just "11"
		serverBrowser = HeleniumBrowser Chrome "16" Linux,
	}
	(eitherAns, state', writer) <- runHeleniumM (commandUrlPost "Google") "" state
	putStrLn (show eitherAns)

-- Use session?
type RequestStateful = Bool

data RequestMethod = Get | Post String | Delete

type RequestPath = String

-- Commands
-------------------------------------------------------------------------------

-- Query the server's current status.
commandStatus :: HeleniumM String
commandStatus = callSelenium False Get "/status"

-- Create a new session.
-- TODO: Add desiredCapabilities JSON object
commandSession :: String -> HeleniumM String
commandSession s = callSelenium False (Post s) "/session"

-- Returns a list of the currently active sessions.
_commandSessions :: String -> HeleniumM String
_commandSessions _ = callSelenium False Get "/sessions"

-- Retrieve the capabilities of the specified session.
commandSessionGet :: String -> HeleniumM String
commandSessionGet _ = callSelenium True Get "/"

-- Delete the session.
commandSessionDelete :: String -> HeleniumM String
commandSessionDelete _ = callSelenium True Delete "/"

-- Set the amount of time, in milliseconds, that asynchronous scripts executed 
-- by /session/:sessionId/execute_async are permitted to run before they are 
-- aborted and a |Timeout| error is returned to the client.
commandTimeoutsAsyncScript :: String -> HeleniumM String
commandTimeoutsAsyncScript s = callSelenium True (Post s) "/timeouts/async_script"

-- Set the amount of time the driver should wait when searching for elements.
commandTimeoutsImplicitWait :: String -> HeleniumM String
commandTimeoutsImplicitWait s = callSelenium True (Post s) "/timeouts/implicit_wait"

-- Retrieve the current window handle.
commandWindowHandle :: String -> HeleniumM String
commandWindowHandle _ = callSelenium True Get "/window_handle"

-- Retrieve the list of all window handles available to the session.
commandWindowHandles :: String -> HeleniumM String
commandWindowHandles _ = callSelenium True Get "/window_handles"

-- Retrieve the URL of the current page.
commanUrlGet :: String -> HeleniumM String
commanUrlGet _ = callSelenium True Get "/url"

-- Navigate to a new URL.
commandUrlPost :: String -> HeleniumM String
commandUrlPost s = callSelenium True (Post s) "/url"

-- Navigate forwards in the browser history, if possible.
commandForward :: String -> HeleniumM String
commandForward _ = callSelenium True (Post "") "/forward"

-- Navigate backwards in the browser history, if possible.
commandBack :: String -> HeleniumM String
commandBack _ = callSelenium True (Post "") "/back"

-- Refresh the current page.
commandRefresh :: String -> HeleniumM String
commandRefresh _ = callSelenium True (Post "") "/refresh"

-- Inject a snippet of JavaScript into the page for execution in the context of the currently selected frame.
commandExecute :: String -> HeleniumM String
commandExecute s = callSelenium True (Post s) "/execute"

-- Inject a snippet of JavaScript into the page for execution in the context of the currently selected frame.
commandExecuteAsync :: String -> HeleniumM String
commandExecuteAsync s = callSelenium True (Post s) "/execute_async"

-- Take a screenshot of the current page.
commandScreenshot :: String -> HeleniumM String
commandScreenshot _ = callSelenium True Get "/screenshot"

-- TODO: IME commands

-- Change focus to another frame on the page.
commandFrame :: String -> HeleniumM String
commandFrame s = callSelenium True (Post s) "/frame"

-- Change focus to another window.
commandWindowFocus :: String -> HeleniumM String
commandWindowFocus s = callSelenium True (Post s) "/window"

-- Close the current window.
commandWindowClose :: String -> HeleniumM String
commandWindowClose _ = callSelenium True Delete "/window"

-------------------------------------------------------------------------------

callSelenium :: RequestStateful -> RequestMethod -> RequestPath -> HeleniumM String
callSelenium rs rm rp = do
	req <- makeRequest rs rm rp
	liftIO $ putStrLn $ show req
	sendRequest req

sendRequest :: HTTP.Request String -> HeleniumM String
sendRequest req = do
	result <- liftIO $ HTTP.simpleHTTP req
	body <- liftIO $ HTTP.getResponseBody result
	-- TODO: Parse error
	-- return $ either left right result where
		-- left conError = throwError
		-- right ans = HTTP.rspBody ans
	return body

-- WebDriver command messages should conform to the HTTP/1.1 request specification. 
-- All commands accept a content-type of application/json;charset=UTF-8. 
-- Message bodies for POST and PUT request must use application/json;charset=UTF-8.
makeRequest :: RequestStateful -> RequestMethod -> RequestPath -> HeleniumM (HTTP.Request String)
makeRequest rs rm rp = do
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
	return req

makeRequestUri :: RequestStateful -> RequestPath -> HeleniumM String
makeRequestUri rs rp = do
	state <- get
	let baseUri = (serverHost state) ++ ":" ++ (show $ serverPort state)
	let uriPath =
		if rs == True
		-- TODO: Throw (throwError) a proper error when no session available
		then "/session/" ++ (fromJust $ serverSessionId state) ++ rp
		else rp
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

