module Helenium (
	HeleniumM,
	HeleniumReader (..),
	HeleniumBrowser (..),
	HeleniumBrowserName (..),
	HeleniumBrowserVersion,
	HeleniumBrowserPlatform (..),
	runTest,
	echo,
	getCurrentTime,
	sleep,
	times,
	assertEqual,
	assertLess,
	assertGreater,
	assertLessOrEqual,
	assertGreaterOrEqual,
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
	clickElement,
	getElementText,
	submitElement,
	sendKeys,
	sendKeysToElement,
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

import Data.Maybe
import Data.Either
import qualified Data.Time as Time
import Data.List (find, isPrefixOf, isSuffixOf, isInfixOf)
import Control.Monad.Error
import Control.Monad.RWS.Strict
import qualified Network.URI as URI
import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Stream as Stream
import qualified Text.JSON as JSON
import System (getArgs)
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
		name :: String,
		server :: String,
		browser :: HeleniumBrowser,
		logTime :: Bool,
		debugHttp :: Bool,
		debugTime :: Bool,
		timeoutTest :: Int,
		timeoutElement :: Int,
		screenshotPath :: String
	}

type HeleniumWriter = [(Time.UTCTime, HeleniumWriterLevel, HeleniumWriterMsg)]

-- TODO: This should be only INFO, DEBUG, WARN and ERROR
data HeleniumWriterLevel = Info | Debug | DebugRequest | DebugResponse

-- TODO: Implement different type of messages.
-- Simples with a String, with a Request object, with a screenshot, etc.
type HeleniumWriterMsg = String

data HeleniumState = 
	HeleniumState {
		serverCapabilities :: [HeleniumCapability],
		serverSessionId :: Maybe HeleniumSessionId
	}

type HeleniumSessionId = String

data HeleniumBrowser = 
	HeleniumBrowser {
		browserName :: HeleniumBrowserName,
		browserVersion :: HeleniumBrowserVersion,
		browserPlatform :: HeleniumBrowserPlatform
	}

data HeleniumBrowserName = Chrome | Firefox | HtmlUnit | IE | IPhone

type HeleniumBrowserVersion = String

data HeleniumBrowserPlatform = Windows| XP | Vista | Mac | Linux | Unix | Any

-- TODO: Move to the reader after moving out the config from the tests.
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

-- Test runner.
-------------------------------------------------------------------------------

runTest :: HeleniumM () -> IO ()
runTest t = do
	args <- getArgs
	when (null args) (error $ "No paramaters. Server and browser are needed.")
	when (not $ any (isPrefixOf "--server=") args) (error $ "No --server parameter.")
	when (not $ any (isPrefixOf "--browser=") args) (error $ "No --browser parameter.")
	let config = HeleniumReader {
		name = "Test",
		server = "http://127.0.0.1:9515",
		browser = HeleniumBrowser Chrome "16" Linux,
		logTime = True,
		debugHttp = False,
		debugTime = False,
		timeoutTest = 0,
		timeoutElement = 5000,
		screenshotPath = "/home/developer"
	}
	let config' = foldl updateConfig config args
	when (isNothing $ URI.parseURI $ server config') (error "Not a valid server URL.")
	-- TODO: Check valid browser name! The browser in config is lazy (not evaluated).
	runTest' config' t

browserStringToBrowserName :: String -> HeleniumBrowserName
browserStringToBrowserName "chrome" = Chrome
browserStringToBrowserName "firefox" = Firefox
browserStringToBrowserName "htmlunit" = HtmlUnit
browserStringToBrowserName "internet explorer" = IE
browserStringToBrowserName "iphone" = IPhone
browserStringToBrowserName b = error $ b ++ " is not a valid browser."

updateConfig :: HeleniumReader -> String -> HeleniumReader
updateConfig r c
	| isPrefixOf "--server=" c && length c > 9 = 
		r {server = drop 9 c}
	| isPrefixOf "--browser=" c && length c > 10 = 
		r {browser = HeleniumBrowser (browserStringToBrowserName $ drop 10 c) "16" Linux}
	| c == "--debugHttp" = 
		r {debugHttp = True}
	| otherwise = error $ c ++ " is not a valid parameter."

runTest' :: HeleniumReader -> HeleniumM () -> IO ()
runTest' r t = do
	-- If the URI has a trailing '/', remove it..
	let r' = if (last $ server r) == '/'
		then r {server = (init $ server r)}
		else r
	let s = HeleniumState {
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
	t' <- wrapTest t
	(eitherAns, s', w) <- runHeleniumM t' r' s
	showWriter w
	case eitherAns of
		Left err -> showError err
		Right _ -> showOk

showOk :: IO ()
showOk = putStrLn "Test finished successfully!"

showError :: HeleniumError -> IO ()
showError e = putStrLn $ "An error ocurred: " ++ e

showWriter :: HeleniumWriter -> IO ()
-- TODO: Do something with message type!
showWriter w = mapM_ putStrLn (logGenerator w)

wrapTest :: HeleniumM () -> IO (HeleniumM ())
wrapTest t = do
	return $ do
		reader <- ask
		logMsg Info ("Running test: " ++ (name reader))
		connect
		do {
			setElementTimeout $ timeoutElement reader;
			t
		} `catchError` (\e -> do {disconnect; throwError e})
		disconnect

-- Logging
-------------------------------------------------------------------------------

logMsg :: HeleniumWriterLevel -> HeleniumWriterMsg -> HeleniumM ()
logMsg level msg = do
	t <- liftIO $ Time.getCurrentTime
	tell [(t, level, msg)]

logGenerator :: HeleniumWriter -> [String]
logGenerator logs = map f logs where
	f (t, l, m) = (showLogTime t) ++ " - " ++ (showLogLevel l) ++ " - " ++ m

showLogTime :: Time.UTCTime -> String
showLogTime t = show t
	
showLogLevel Info = "INFO"
showLogLevel Debug = "DEBUG" 
showLogLevel DebugRequest = "REQUEST"
showLogLevel DebugResponse = "RESPONSE"

-- Commands
-------------------------------------------------------------------------------

echo :: String -> HeleniumM ()
echo m = do
	logMsg Info m

getCurrentTime :: HeleniumM String
getCurrentTime = do
	t <- liftIO Time.getCurrentTime
	return $ show t

-- Suspends the current thread for a given number of seconds.
sleep :: Int -> HeleniumM ()
sleep ms = do
	liftIO $ Sys.sleep ms
	return ()

times :: Int -> HeleniumM () -> HeleniumM ()
times n t = replicateM_ n t

assert :: Show x => String -> (x -> x -> Bool) -> x -> x -> HeleniumM ()
assert p f a b =
	if f a b
		then return ()
		else throwError ("Not " ++ p ++ ": " ++ show a ++ " with " ++ show b)

assertEqual :: (Eq x, Show x) => x -> x -> HeleniumM ()
assertEqual a b = assert "equal" (==) a b

assertLess :: (Ord x, Show x) => x -> x -> HeleniumM ()
assertLess a b = assert "less" (<) a b

assertGreater :: (Ord x, Show x) => x -> x -> HeleniumM ()
assertGreater a b = assert "greater" (>) a b

assertLessOrEqual :: (Ord x, Show x) => x -> x -> HeleniumM ()
assertLessOrEqual a b = assert "less" (<) a b

assertGreaterOrEqual :: (Ord x, Show x) => x -> x -> HeleniumM ()
assertGreaterOrEqual a b = assert "greater" (>) a b

assertPrefix :: String -> String -> HeleniumM ()
assertPrefix a b = assert "prefix" isPrefixOf a b

assertSuffix :: String -> String -> HeleniumM ()
assertSuffix a b = assert "suffix" isSuffixOf a b

assertInfix :: String -> String -> HeleniumM ()
assertInfix a b = assert "infix" isInfixOf a b

type ResponseStatus = Int

type ResponseValue = JSON.JSValue

processResponseBody :: String -> HeleniumM (ResponseStatus, ResponseValue)
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
	-- The response must be a JSON object with a "status" and a "value" property.
	json <- (JSON.decode body :: (JSON.Result (JSON.JSObject JSON.JSValue)))
	statusJson <- JSON.valFromObj "status" json
	status <- case JSON.readJSON statusJson of
		JSON.Ok (JSON.JSRational False e) -> return $ fromEnum e
		JSON.Error msg -> JSON.Error "Error parsing JSON reponse: Invalid status property."
	value <- JSON.valFromObj "value" json
	return (status, value)

heleniumBrowserNameKey Chrome = "chrome"
heleniumBrowserNameKey Firefox = "firefox"
heleniumBrowserNameKey HtmlUnit = "htmlunit"
heleniumBrowserNameKey IE = "internet explorer"
heleniumBrowserNameKey IPhone = "iphone"

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

-- Create a new session.
connect :: HeleniumM ()
connect = do
	state <- get
	reader <- ask
	let b = heleniumBrowserNameKey $ browserName $ browser reader
	let capabilities = serverCapabilities state
	let capabilitiesArray = map 
		(\c -> (heleniumCapabilityKey c, JSON.JSBool True)) 
		capabilities
	let capabilitiesArray' = capabilitiesArray ++ [("browserName", JSON.showJSON b)]
	let capabilitiesJson = JSON.makeObj capabilitiesArray'
	let bodyJson = JSON.makeObj [("desiredCapabilities", capabilitiesJson)]
	ans <- callSelenium $ Request False (Post $ JSON.encode bodyJson) "/session"
	-- Return a redirect with header:
	-- Location: /session/bf8deb5adc6e61e87c09913f78e5c82c
	let location = responseHTTPHeaderLocation $ responseHTTPHeaders ans
	case location of
		Just location -> do
			let sessId = reverse $ takeWhile (/= '/') $ reverse location
			put $ state {serverSessionId = (Just sessId)}
		Nothing -> throwError "Response has no Location header with the new session."
	return () 

-- Set the amount of time the driver should wait when searching for elements.
-- When searching for an element, the driver should poll the page until the 
-- element is found or the timeout expires, whichever occurs first.
setElementTimeout :: Int -> HeleniumM ()
setElementTimeout ms = do
	let body = JSON.makeObj [("ms", JSON.showJSON ms)]
	callSelenium $ Request True (Post $ JSON.encode body) "/timeouts/implicit_wait"
	return ()

-- Delete the session.
disconnect :: HeleniumM ()
disconnect = do
	callSelenium $ Request True Delete "/"
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
	(status, value) <- processResponseBody $ responseHTTPBody ans
	case value of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError "Error reading url, not a valid JSON response."

getTitle :: HeleniumM String
getTitle = do
	ans <- callSelenium $ Request True Get "/title"
	(status, value) <- processResponseBody $ responseHTTPBody ans
	case value of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError "Error reading title, not a valid JSON response."

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

-- Take a screenshot of the current page.
-- Returns the screenshot as a base64 encoded PNG.
takeScreenshot :: String -> HeleniumM ()
takeScreenshot name = do
	ans <- callSelenium $ Request True Get "/screenshot"
	(status, value) <- processResponseBody $ responseHTTPBody ans
	case value of
		JSON.JSString jsString -> saveScreenshot name (JSON.fromJSString jsString)
		_ -> throwError "Error reading screenshot, not a valid JSON response."

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

changeFocusToIframeById :: String -> HeleniumM ()
changeFocusToIframeById iframeName = do
	let body = JSON.toJSObject [("id", JSON.toJSString iframeName)]
	ans <- callSelenium $ Request True (Post $ JSON.encode body) "/frame"
	return ()

changeFocusToIframeByNumber :: Int -> HeleniumM ()
changeFocusToIframeByNumber iframeNumber = do
	let body = JSON.toJSObject [("id", JSON.showJSON iframeNumber)]
	ans <- callSelenium $ Request True (Post $ JSON.encode body) "/frame"
	return ()

changeFocusToDefaultIframe :: HeleniumM ()
changeFocusToDefaultIframe = do
	let body = JSON.toJSObject [("id", JSON.JSNull)]
	ans <- callSelenium $ Request True (Post $ JSON.encode body) "/frame"
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
	(status, value) <- processResponseBody $ responseHTTPBody ans 
	case value of
		(JSON.JSObject obj) -> case JSON.valFromObj "ELEMENT" obj of
			JSON.Ok (JSON.JSString element) -> return $ JSON.fromJSString element
			_ -> throwError "Error reading element, not a valid JSON response."
		_ -> throwError "Error reading element, not a valid JSON response."

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

getElementText :: String -> HeleniumM String
getElementText e = do
	ans <- callSelenium $ Request True Get ("/element/" ++ e ++ "/text")
	(status, value) <- processResponseBody $ responseHTTPBody ans
	case value of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError "Error reading element text, not a valid JSON response."

-- Submit a FORM element. The submit command may also be applied to any element 
-- that is a descendant of a FORM element.
submitElement :: String -> HeleniumM ()
submitElement e = do
	callSelenium $ Request True (Post "") $ "/element/" ++ e ++ "/submit"
	return ()

sendKeys :: [Char] -> HeleniumM ()
sendKeys ks = do
	let body = JSON.toJSObject [
		("value", JSON.JSArray $ map JSON.showJSON ks)]
	callSelenium $ Request True (Post $ JSON.encode body) "/keys"
	return ()

sendKeysToElement :: String -> [Char] -> HeleniumM ()
sendKeysToElement e ks = do
	let body = JSON.toJSObject [
                ("value", JSON.JSArray $ map JSON.showJSON ks)]
	callSelenium $ Request True (Post $ JSON.encode body) $ "/element/" ++ e ++ "/value"
	return ()

getCookies :: HeleniumM [JSON.JSValue]
getCookies = do
	ans <- callSelenium $ Request True Get "/cookie"
	(status, value) <- processResponseBody $ responseHTTPBody ans
	case value of
		JSON.JSArray cookies -> return cookies
		_ -> throwError "Error reading cookies, not a valid JSON response."

getCookieByName :: String -> HeleniumM (Maybe JSON.JSValue)
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

getCookieValue :: String -> HeleniumM String
getCookieValue name = do
	cookie <- getCookieByName name
	case cookie of
		Just (JSON.JSObject obj) -> case JSON.valFromObj "value" obj of
			JSON.Ok (JSON.JSString value) -> return $ JSON.fromJSString value
			_ -> throwError $ "Error reading cookie value, not a valid JSON response."
		Nothing -> throwError $ "Cookie does not exists: " ++ name ++ "."
		_ -> throwError $ "Error reading cookie value, not a valid JSON response."

getCookieExpiresEpoch :: String -> HeleniumM Int
getCookieExpiresEpoch name = do
	cookie <- getCookieByName name
	case cookie of
		Just (JSON.JSObject obj) -> case JSON.valFromObj "expiry" obj of
			JSON.Ok (JSON.JSRational False e) -> return $ fromEnum e
			_ -> throwError $ "Error reading cookie expires, not a valid JSON response."
		Nothing -> throwError $ "Cookie does not exists: " ++ name ++ "."
		_ -> throwError $ "Error reading cookie expires, not a vlaid JSON response."

deleteAllCookies :: HeleniumM ()
deleteAllCookies = do
	callSelenium $ Request True Delete "/cookie"
	return ()

deleteCookieByName :: String -> HeleniumM ()
deleteCookieByName name = do
	callSelenium $ Request True Delete ("/cookie/" ++ name)
	return ()

assertCookieDoesNotExists :: String -> HeleniumM ()
assertCookieDoesNotExists name = do
	cookie <- getCookieByName name
	case cookie of
		Nothing -> return ()
		_ -> throwError $ "Cookie exists: " ++ name ++ "."

-- Set the amount of time, in milliseconds, that asynchronous scripts executed 
-- by /session/:sessionId/execute_async are permitted to run before they are 
-- aborted and a |Timeout| error is returned to the client.
setTimeoutAsyncScript :: Int -> HeleniumM ()
setTimeoutAsyncScript ms = do
	let body = JSON.toJSObject [("ms", JSON.showJSON ms)]
	callSelenium $ Request True (Post $ JSON.encode body) "/timeouts/async_script"
	return ()

-- TODO:
-- Inject a snippet of JavaScript into the page for execution in the context of the currently selected frame.
execute :: String -> HeleniumM ()
execute s = do
	callSelenium $ Request True (Post s) "/execute"
	return ()

-- TODO:
-- Inject a snippet of JavaScript into the page for execution in the context of the currently selected frame.
executeAsync :: String -> HeleniumM ()
executeAsync s = do
	callSelenium $ Request True (Post s) "/execute_async"
	return ()

-------------------------------------------------------------------------------

data Request = Request RequestStateful RequestMethod RequestPath

type RequestStateful = Bool

data RequestMethod = Get | Post String | Delete

type RequestPath = String

data Response = Response {
	responseHTTPCode :: ResponseHTTPCode,
	responseHTTPReason :: ResponseHTTPReason,
	responseHTTPHeaders :: ResponseHTTPHeaders,
	responseHTTPBody :: ResponseHTTPBody}

type ResponseHTTPCode = (Int, Int, Int)

type ResponseHTTPReason = String

data ResponseHTTPHeaders = ResponseHTTPHeaders {
		responseHTTPHeaderLocation :: Maybe String
	}
	
type ResponseHTTPBody = String

callSelenium :: Request -> HeleniumM Response
callSelenium req = do
	httpReq <- makeRequest req
	reader <- ask
	when (debugHttp reader) $ 
		logMsg DebugRequest $
			"\n" ++ (show httpReq) ++ "\n" ++ (HTTP.rqBody httpReq)
	httpRes <- sendRequest httpReq
	when (debugHttp reader) $
		logMsg DebugResponse $
			"\n" ++ (show httpRes) ++ "\n" ++ (HTTP.rspBody httpRes)
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
	headers <- processHeaders res
	let body = HTTP.rspBody res -- The body string
	return (Response (x,y,z) reason headers body)

processResponseCodes :: (Int, Int, Int) -> String -> HeleniumM String
-- TODO: Do something!!!
processResponseCodes (x, y, z) reason = return ""

processHeaders :: HTTP.Response String -> HeleniumM ResponseHTTPHeaders
processHeaders httpRes = do
	return $ ResponseHTTPHeaders {
		responseHTTPHeaderLocation = HTTP.findHeader HTTP.HdrLocation httpRes
	}
	

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
		-- TODO: Check valid URL
		HTTP.rqURI = fromJust $ URI.parseURI uri,
		HTTP.rqMethod = method,
		HTTP.rqHeaders = headers,
		HTTP.rqBody = body
	}
	return req

makeRequestUri :: RequestStateful -> RequestPath -> HeleniumM String
makeRequestUri rs rp = do
	state <- get
	reader <- ask
	let sessionId = serverSessionId state
	uriPath <- if rs == True
		then if isNothing sessionId
			then throwError "Making a stateful call without a session."
			else return ("/session/" ++ (fromJust sessionId) ++ rp)
		else return rp
	return $ (server reader) ++ uriPath

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
	InvalidSelector |
	UnknownErrorNumber

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
errorNumberToErrorCode _  = UnknownErrorNumber

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

