module Helenium (
	HeleniumM,
	HeleniumError,
	HeleniumReader (..),
		--debugHttp,
		--debugTime,
		--screenshotPath,
	HeleniumWriter,
	HeleniumState (..),
		--serverHost,
		--serverPort,
		--serverPath,
		--serverBrowser,
		--serverCapabilities,
		--serverSessionId,
	HeleniumBrowser (..),
		--browserName,
		--browserVersion,
		--browserPlatform,
	HeleniumBrowserName (..),
	HeleniumBrowserVersion,
	HeleniumBrowserPlatform (..),
	HeleniumCapability (..),
	runTest,
	sleep,
	goTo,
	getUrl,
	getTitle,
	refresh,
	back,
	forward,
	takeScreenshot,
	getActiveElement,
	getElementById,
	getElementByName,
	getElementByClassName,
	getElementByCssSelector,
	getElementByText,
	getElementByPartialText,
	getElementByXPath,
	clickElement,
	submitElement,
	sendKeysToElement
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
import qualified System.Posix.Unistd as Sys

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

{- 
Hint:
:t runHeleniumT
    runHeleniumT :: HeleniumT e r w s m a -> ErrorT e (RWST r w s m) a
:t runErrorT . runHeleniumT
    runErrorT . runHeleniumT :: HeleniumT e r w s m a -> RWST r w s m (Either e a)
:t runRWST . runErrorT . runHeleniumT
    runRWST . runErrorT . runHeleniumT :: HeleniumT e r w s m a -> r -> s -> m (Either e a, s, w)
-}

type HeleniumError = String

data HeleniumReader = 
	HeleniumReader {
		debugHttp :: Bool,
		debugTime :: Bool,
		screenshotPath :: String
	}

type HeleniumWriter = [String]

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
	HeleniumBrowser {
		browserName :: HeleniumBrowserName,
		browserVersion :: HeleniumBrowserVersion,
		browserPlatform :: HeleniumBrowserPlatform
	}

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

-------------------------------------------------------------------------------

main :: IO ()
main = do
	let reader = HeleniumReader {
		debugHttp = True,
		debugTime = False, -- TODO: Debug how long it takes to run the test.
		screenshotPath = "/home/developer"
	}
	let state = HeleniumState {
		serverHost = "http://127.0.0.1",
		serverPort = 9515,
		serverPath = "",
		serverBrowser = HeleniumBrowser Chrome "16" Linux,
		serverCapabilities = [
			JavascriptEnabled,
			TakesScreenshot,
			ApplicationCacheEnabled,
			BrowserConnectionEnabled,
			HandlesAlerts,
			CssSelectorsEnabled
		],
		serverSessionId = Nothing
	}
	runTest reader state test

test :: HeleniumM ()
test = do
	goTo "http://www.google.com"
	searchInput <- getElementById "lst-ib"
	sendKeysToElement searchInput "google"
	sleep 5
	submitElement searchInput
	--goTo "http://www.olx.com"
	back
	forward
	refresh
	url <- getUrl
	title <- getTitle
	return ()

-- Test runner.
-------------------------------------------------------------------------------

runTest :: HeleniumReader -> HeleniumState -> HeleniumM () -> IO ()
runTest r s t = do
	t' <- wrapTest t
	(eitherAns, s', w) <- runHeleniumM t' r s
	case eitherAns of
		Left err -> putStrLn $ "An error ocurred: " ++ err
		Right _ -> putStrLn "Ok!"

wrapTest :: HeleniumM () -> IO (HeleniumM ())
wrapTest t = do return $ do {connect; t; disconnect} `catchError` (\e -> do {disconnect; throwError e})

-- Commands
-------------------------------------------------------------------------------

-- Create a new session.
connect :: HeleniumM ()
connect = do
	state <- get
	let browser = serverBrowser state
	let capabilities = serverCapabilities state
	let capabilitiesArray = map 
		(\c -> (heleniumCapabilityKey c, JSON.JSBool True)) 
		capabilities
	let capabilitiesJson = JSON.makeObj capabilitiesArray
	let bodyJson = JSON.makeObj [("desiredCapabilities", capabilitiesJson)]
	ans <- callSelenium $ Request False (Post $ JSON.encode bodyJson) "/session"
	-- Response is: {"status":303,"value":"/session/f3ae93822f855f545dbdab66cc556453"}
	let json = responseValue ans
	case json of
		JSON.JSString jsString -> do
			let sessString = JSON.fromJSString jsString 
			let sessId = drop (length "/session/") sessString
			put $ state {serverSessionId = (Just sessId)}
		_ -> throwError "Response has an invalid new session."
	return () 

-- Delete the session.
disconnect :: HeleniumM ()
disconnect = do
	callSelenium $ Request True Delete "/"
	return ()

-- Suspends the current thread for a given number of seconds.
sleep :: Int -> HeleniumM ()
sleep ms = do
	liftIO $ Sys.sleep ms
	return ()

-- Set the amount of time, in milliseconds, that asynchronous scripts executed 
-- by /session/:sessionId/execute_async are permitted to run before they are 
-- aborted and a |Timeout| error is returned to the client.
setTimeoutAsyncScript :: Int -> HeleniumM ()
setTimeoutAsyncScript ms = do
	let body = JSON.toJSObject [("ms", JSON.showJSON ms)]
	callSelenium $ Request True (Post $ JSON.encode body) "/timeouts/async_script"
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

-- Navigate to a new URL.
goTo :: String -> HeleniumM ()
goTo url = do
	let body = JSON.toJSObject [("url", JSON.toJSString url)]
	callSelenium $ Request True (Post $ JSON.encode body) "/url"
	return ()

-- Retrieve the URL of the current page.
getUrl :: HeleniumM String
getUrl = do
	ans <- callSelenium $ Request True Get "/url"
	case responseValue ans of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError "Error reading url"

getTitle :: HeleniumM String
getTitle = do
	ans <- callSelenium $ Request True Get "/title"
	case responseValue ans of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError "Error reading title"

-- Refresh the current page.
refresh :: HeleniumM ()
refresh = do
	callSelenium $ Request True (Post "") "/refresh"
	return ()

-- Navigate backwards in the browser history, if possible.
back :: HeleniumM ()
back = do
	callSelenium $ Request True (Post "") "/back"
	return ()

-- Navigate forwards in the browser history, if possible.
forward :: HeleniumM ()
forward = do
	callSelenium $ Request True (Post "") "/forward"
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
takeScreenshot :: String -> HeleniumM ()
takeScreenshot name = do
	ans <- callSelenium $ Request True Get "/screenshot"
	-- Returns the screenshot as a base64 encoded PNG.
	case responseValue ans of
		JSON.JSString jsString -> saveScreenshot name (JSON.fromJSString jsString)
		_ -> throwError "Error reading screenshot answer."
	return ()

saveScreenshot :: String -> String -> HeleniumM ()
saveScreenshot name png = do
	reader <- ask
	let path = screenshotPath reader
	err <- liftIO $ catch 
		(writeFile (path ++ "/" ++ name ++ ".png") png >> return "")
		(\err -> return $ show err)
	if null err
		then return ()
		else throwError $ "Error saving screenshot: " ++ (show err)

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

-- Get the element on the page that currently has focus.
getActiveElement :: HeleniumM String
getActiveElement = do
	ans <- callSelenium $ Request True (Post "") "/element/active"
	processElementResponse ans

-- Search for an element on the page, starting from the document root.
-- Each locator must return the first matching element located in the DOM.
getElementBy :: String -> String -> HeleniumM String
getElementBy using value = do
	let body = JSON.toJSObject [
		("using", JSON.toJSString using),
		("value", JSON.toJSString value)]
	-- Return {"ELEMENT":":wdc:1322198176445"}
	ans <- callSelenium $ Request True (Post $ JSON.encode body) "/element"
	processElementResponse ans

processElementResponse :: Response -> HeleniumM String
processElementResponse ans = do 
	case responseValue ans of
		(JSON.JSObject obj) -> case JSON.valFromObj "ELEMENT" obj of
			JSON.Ok (JSON.JSString element) -> return $ JSON.fromJSString element
			_ -> throwError "Error reading element response"
		_ -> throwError "Error reading element response"

getElementById :: String -> HeleniumM String
getElementById id = getElementBy "id" id

getElementByName :: String -> HeleniumM String
getElementByName name = getElementBy "name" name

getElementByClassName :: String -> HeleniumM String
getElementByClassName className = getElementBy "class name" className

getElementByCssSelector :: String -> HeleniumM String
getElementByCssSelector css = getElementBy "css selector" css

-- Returns an anchor element whose visible text matches the search value.
getElementByText :: String -> HeleniumM String
getElementByText text = getElementBy "link text" text

-- Returns an anchor element whose visible text partially matches the search value.
getElementByPartialText :: String -> HeleniumM String
getElementByPartialText text = getElementBy "partial link text" text

getElementByXPath :: String -> HeleniumM String
getElementByXPath x = getElementBy "xpath" x

-- Click on an element.
clickElement :: String -> HeleniumM ()
clickElement e = do
	callSelenium $ Request True (Post "") $ "/element/" ++ e ++ "/click"
	return ()

-- Submit a FORM element. The submit command may also be applied to any element 
-- that is a descendant of a FORM element.
submitElement :: String -> HeleniumM ()
submitElement e = do
	callSelenium $ Request True (Post "") $ "/element/" ++ e ++ "/submit"
	return ()

sendKeysToElement :: String -> [Char] -> HeleniumM ()
sendKeysToElement e ks = do
	let body = JSON.toJSObject [
                ("value", JSON.JSArray $ map JSON.showJSON ks)]
	callSelenium $ Request True (Post $ JSON.encode body) $ "/element/" ++ e ++ "/value"
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
	reader <- ask
	when (debugHttp reader) $ 
		liftIO $ 
			putStr (show httpReq) >>
			putStrLn (HTTP.rqBody httpReq)
	httpRes <- sendRequest httpReq
	when (debugHttp reader) $
		liftIO $
			putStr (show httpRes) >>
			putStrLn (HTTP.rspBody httpRes)
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

