module Helenium (
	runTest,
	echo,
	-- TODO: getEpoch as an integer!
	getCurrentTime,
	sleep,
	for,
	times,
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
import Helenium.Network
import Helenium.Log
import Data.Maybe
import Data.Either
import qualified Data.Time as Time
import Data.List (find, isPrefixOf, isSuffixOf, isInfixOf)
import Control.Monad.Error
import Control.Monad.RWS.Strict
import qualified Text.JSON as JSON
import System (getArgs)
import qualified System.Posix.Unistd as Sys

-- Test runner.
-------------------------------------------------------------------------------

runTest :: H.HeleniumM () -> IO ()
runTest t = do
	args <- getArgs
	when (null args) (error $ "No paramaters. Server and browser are needed.")
	when (not $ any (isPrefixOf "--server=") args) (error $ "No --server parameter.")
	when (not $ any (isPrefixOf "--browser=") args) (error $ "No --browser parameter.")
	let config = H.HeleniumReader {
		H.name = "Test",
		H.server = "http://127.0.0.1:9515",
		H.browser = H.HeleniumBrowser H.Chrome "16" H.Linux,
		H.logTime = True,
		H.debugHttp = False,
		H.debugTime = False,
		H.timeoutTest = 0,
		H.timeoutElement = 5000,
		H.screenshotPath = "/home/developer"
	}
	let config' = foldl updateConfig config args
	when (not $ isUriValid $ H.server config') (error "Not a valid server URL.")
	-- TODO: Check valid browser name! The browser in config is lazy (not evaluated).
	runTest' config' t

browserStringToBrowserName :: String -> H.HeleniumBrowserName
browserStringToBrowserName "chrome" = H.Chrome
browserStringToBrowserName "firefox" = H.Firefox
browserStringToBrowserName "htmlunit" = H.HtmlUnit
browserStringToBrowserName "internet explorer" = H.IE
browserStringToBrowserName "iphone" = H.IPhone
browserStringToBrowserName b = error $ b ++ " is not a valid browser."

updateConfig :: H.HeleniumReader -> String -> H.HeleniumReader
updateConfig r c
	| isPrefixOf "--server=" c && length c > 9 = 
		r {H.server = drop 9 c}
	| isPrefixOf "--browser=" c && length c > 10 = 
		r {H.browser = H.HeleniumBrowser (browserStringToBrowserName $ drop 10 c) "16" H.Linux}
	| c == "--debugHttp" = 
		r {H.debugHttp = True}
	| otherwise = error $ c ++ " is not a valid parameter."

runTest' :: H.HeleniumReader -> H.HeleniumM () -> IO ()
runTest' r t = do
	-- If the URI has a trailing '/', remove it..
	let r' = if (last $ H.server r) == '/'
		then r {H.server = (init $ H.server r)}
		else r
	let s = H.HeleniumState {
		H.serverCapabilities = [
			H.JavascriptEnabled,
			H.TakesScreenshot,
			H.ApplicationCacheEnabled,
			H.BrowserConnectionEnabled,
			H.HandlesAlerts,
			H.CssSelectorsEnabled
		],
		H.serverSessionId = Nothing
	}
	t' <- wrapTest t
	(eitherAns, s', w) <- H.runHeleniumM t' r' s
	showWriter w
	case eitherAns of
		Left err -> showError err
		Right _ -> showOk

showOk :: IO ()
showOk = putStrLn "Test finished successfully!"

showError :: H.HeleniumError -> IO ()
showError e = putStrLn $ "An error ocurred: " ++ e

showWriter :: H.HeleniumWriter -> IO ()
-- TODO: Do something with message type!
showWriter w = mapM_ putStrLn (logGenerator w)

wrapTest :: H.HeleniumM () -> IO (H.HeleniumM ())
wrapTest t = do
	return $ do
		reader <- ask
		logMsg H.Info ("Running test: " ++ (H.name reader))
		connect
		do {
			setElementTimeout $ H.timeoutElement reader;
			t
		} `catchError` (\e -> do {disconnect; throwError e})
		disconnect

-- Commands
-------------------------------------------------------------------------------

echo :: String -> H.HeleniumM ()
echo m = do
	logMsg H.Info m

getCurrentTime :: H.HeleniumM String
getCurrentTime = do
	t <- liftIO Time.getCurrentTime
	return $ show t

-- Suspends the current thread for a given number of seconds.
sleep :: Int -> H.HeleniumM ()
sleep ms = do
	liftIO $ Sys.sleep ms
	return ()

for :: [a] -> (a -> H.HeleniumM b) -> H.HeleniumM ()
for as f = forM_ as f

times :: Int -> H.HeleniumM () -> H.HeleniumM ()
times n t = replicateM_ n t

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

type ResponseStatus = Int

type ResponseValue = JSON.JSValue

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
	-- The response must be a JSON object with a "status" and a "value" property.
	json <- (JSON.decode body :: (JSON.Result (JSON.JSObject JSON.JSValue)))
	statusJson <- JSON.valFromObj "status" json
	status <- case JSON.readJSON statusJson of
		JSON.Ok (JSON.JSRational False e) -> return $ fromEnum e
		JSON.Error msg -> JSON.Error "Error parsing JSON reponse: Invalid status property."
	value <- JSON.valFromObj "value" json
	return (status, value)

heleniumBrowserNameKey H.Chrome = "chrome"
heleniumBrowserNameKey H.Firefox = "firefox"
heleniumBrowserNameKey H.HtmlUnit = "htmlunit"
heleniumBrowserNameKey H.IE = "internet explorer"
heleniumBrowserNameKey H.IPhone = "iphone"

heleniumCapabilityKey :: H.HeleniumCapability -> String
heleniumCapabilityKey H.JavascriptEnabled = "javascriptEnabled"
heleniumCapabilityKey H.TakesScreenshot = "takesScreenshot"
heleniumCapabilityKey H.HandlesAlerts = "handlesAlerts"
heleniumCapabilityKey H.DatabaseEnabled = "databaseEnabled"
heleniumCapabilityKey H.LocationContextEnabled = "locationContextEnabled"
heleniumCapabilityKey H.ApplicationCacheEnabled = "applicationCacheEnabled"
heleniumCapabilityKey H.BrowserConnectionEnabled = "browserConnectionEnabled"
heleniumCapabilityKey H.CssSelectorsEnabled = "cssSelectorsEnabled"
heleniumCapabilityKey H.WebStorageEnabled = "webStorageEnabled"
heleniumCapabilityKey H.Rotatable = "rotatable"
heleniumCapabilityKey H.AcceptSslCerts = "acceptSslCerts"
heleniumCapabilityKey H.NativeEvents = "nativeEvents"

-- Create a new session.
connect :: H.HeleniumM ()
connect = do
	state <- get
	reader <- ask
	let b = heleniumBrowserNameKey $ H.browserName $ H.browser reader
	let capabilities = H.serverCapabilities state
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
			put $ state {H.serverSessionId = (Just sessId)}
		Nothing -> throwError "Response has no Location header with the new session."
	return () 

-- Set the amount of time the driver should wait when searching for elements.
-- When searching for an element, the driver should poll the page until the 
-- element is found or the timeout expires, whichever occurs first.
setElementTimeout :: Int -> H.HeleniumM ()
setElementTimeout ms = do
	let body = JSON.makeObj [("ms", JSON.showJSON ms)]
	callSelenium $ Request True (Post $ JSON.encode body) "/timeouts/implicit_wait"
	return ()

-- Delete the session.
disconnect :: H.HeleniumM ()
disconnect = do
	callSelenium $ Request True Delete "/"
	return ()

-- Navigate to a new URL.
goTo :: String -> H.HeleniumM ()
goTo url = do
	let body = JSON.toJSObject [("url", JSON.toJSString url)]
	callSelenium $ Request True (Post $ JSON.encode body) "/url"
	return ()

-- Retrieve the URL of the current page.
getUrl :: H.HeleniumM String
getUrl = do
	ans <- callSelenium $ Request True Get "/url"
	(status, value) <- processResponseBody $ responseHTTPBody ans
	case value of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError "Error reading url, not a valid JSON response."

getTitle :: H.HeleniumM String
getTitle = do
	ans <- callSelenium $ Request True Get "/title"
	(status, value) <- processResponseBody $ responseHTTPBody ans
	case value of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError "Error reading title, not a valid JSON response."

-- Refresh the current page.
refresh :: H.HeleniumM ()
refresh = do
	callSelenium $ Request True (Post "") "/refresh"
	return ()

-- Navigate backwards in the browser history, if possible.
back :: H.HeleniumM ()
back = do
	callSelenium $ Request True (Post "") "/back"
	return ()

-- Navigate forwards in the browser history, if possible.
forward :: H.HeleniumM ()
forward = do
	callSelenium $ Request True (Post "") "/forward"
	return ()

-- Take a screenshot of the current page.
-- Returns the screenshot as a base64 encoded PNG.
takeScreenshot :: String -> H.HeleniumM ()
takeScreenshot name = do
	ans <- callSelenium $ Request True Get "/screenshot"
	(status, value) <- processResponseBody $ responseHTTPBody ans
	case value of
		JSON.JSString jsString -> saveScreenshot name (JSON.fromJSString jsString)
		_ -> throwError "Error reading screenshot, not a valid JSON response."

saveScreenshot :: String -> String -> H.HeleniumM ()
saveScreenshot name png = do
	reader <- ask
	let path = H.screenshotPath reader
	err <- liftIO $ catch 
		(writeFile (path ++ "/" ++ name ++ ".png") png >> return "")
		(\err -> return $ show err)
	if null err
		then return ()
		else throwError $ "Error saving screenshot: " ++ (show err)

changeFocusToIframeById :: String -> H.HeleniumM ()
changeFocusToIframeById iframeName = do
	let body = JSON.toJSObject [("id", JSON.toJSString iframeName)]
	ans <- callSelenium $ Request True (Post $ JSON.encode body) "/frame"
	return ()

changeFocusToIframeByNumber :: Int -> H.HeleniumM ()
changeFocusToIframeByNumber iframeNumber = do
	let body = JSON.toJSObject [("id", JSON.showJSON iframeNumber)]
	ans <- callSelenium $ Request True (Post $ JSON.encode body) "/frame"
	return ()

changeFocusToDefaultIframe :: H.HeleniumM ()
changeFocusToDefaultIframe = do
	let body = JSON.toJSObject [("id", JSON.JSNull)]
	ans <- callSelenium $ Request True (Post $ JSON.encode body) "/frame"
	return ()

-- Get the element on the page that currently has focus.
getActiveElement :: H.HeleniumM String
getActiveElement = do
	ans <- callSelenium $ Request True (Post "") "/element/active"
	processElementResponse ans

-- Search for an element on the page, starting from the document root.
-- Each locator must return the first matching element located in the DOM.
getResponseElementBy :: String -> String -> H.HeleniumM Response
getResponseElementBy using value = do
	let body = JSON.toJSObject [
		("using", JSON.toJSString using),
		("value", JSON.toJSString value)]
	-- Return {"ELEMENT":":wdc:1322198176445"}
	ans <- callSelenium $ Request True (Post $ JSON.encode body) "/element"
	return ans

getResponseElementById :: String -> H.HeleniumM Response
getResponseElementById id = getResponseElementBy "id" id

getResponseElementByName :: String -> H.HeleniumM Response
getResponseElementByName name = getResponseElementBy "name" name

getResponseElementByClassName :: String -> H.HeleniumM Response
getResponseElementByClassName className = getResponseElementBy "class name" className

getResponseElementByCssSelector :: String -> H.HeleniumM Response
getResponseElementByCssSelector css = getResponseElementBy "css selector" css

-- Returns an anchor element whose visible text matches the search value.
getResponseElementByText :: String -> H.HeleniumM Response
getResponseElementByText text = getResponseElementBy "link text" text

-- Returns an anchor element whose visible text partially matches the search value.
getResponseElementByPartialText :: String -> H.HeleniumM Response
getResponseElementByPartialText text = getResponseElementBy "partial link text" text

getResponseElementByXPath :: String -> H.HeleniumM Response
getResponseElementByXPath x = getResponseElementBy "xpath" x

processElementResponse :: Response -> H.HeleniumM String
processElementResponse ans = do
	(status, value) <- processResponseBody $ responseHTTPBody ans 
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

processElementDoesNotExistsResponse :: Response -> H.HeleniumM ()
processElementDoesNotExistsResponse ans = do
	(status, value) <- processResponseBody $ responseHTTPBody ans
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
	callSelenium $ Request True (Post "") $ "/element/" ++ e ++ "/click"
	return ()

getElementText :: String -> H.HeleniumM String
getElementText e = do
	ans <- callSelenium $ Request True Get ("/element/" ++ e ++ "/text")
	(status, value) <- processResponseBody $ responseHTTPBody ans
	case value of
		JSON.JSString jsString -> return $ JSON.fromJSString jsString
		_ -> throwError "Error reading element text, not a valid JSON response."

-- Submit a FORM element. The submit command may also be applied to any element 
-- that is a descendant of a FORM element.
submitElement :: String -> H.HeleniumM ()
submitElement e = do
	callSelenium $ Request True (Post "") $ "/element/" ++ e ++ "/submit"
	return ()

sendKeys :: [Char] -> H.HeleniumM ()
sendKeys ks = do
	let body = JSON.toJSObject [
		("value", JSON.JSArray $ map JSON.showJSON ks)]
	callSelenium $ Request True (Post $ JSON.encode body) "/keys"
	return ()

sendKeysToElement :: String -> [Char] -> H.HeleniumM ()
sendKeysToElement e ks = do
	let body = JSON.toJSObject [
                ("value", JSON.JSArray $ map JSON.showJSON ks)]
	callSelenium $ Request True (Post $ JSON.encode body) $ "/element/" ++ e ++ "/value"
	return ()

getElementIsEnabled :: String -> H.HeleniumM Bool
getElementIsEnabled e = do
	ans <- callSelenium $ Request True Get $ "/element/" ++ e ++ "/enabled"
	(status, value) <- processResponseBody $ responseHTTPBody ans
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
	ans <- callSelenium $ Request True Get $ "/element/" ++ e ++ "/displayed"
	(status, value) <- processResponseBody $ responseHTTPBody ans
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
	ans <- callSelenium $ Request True Get $ "/element/" ++ e ++ "/selected"
	(status, value) <- processResponseBody $ responseHTTPBody ans
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
	ans <- callSelenium $ Request True Get "/cookie"
	(status, value) <- processResponseBody $ responseHTTPBody ans
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
	callSelenium $ Request True Delete "/cookie"
	return ()

deleteCookieByName :: String -> H.HeleniumM ()
deleteCookieByName name = do
	callSelenium $ Request True Delete ("/cookie/" ++ name)
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
	callSelenium $ Request True (Post $ JSON.encode body) "/timeouts/async_script"
	return ()

-- TODO:
-- Inject a snippet of JavaScript into the page for execution in the context of the currently selected frame.
execute :: String -> H.HeleniumM ()
execute s = do
	callSelenium $ Request True (Post s) "/execute"
	return ()

-- TODO:
-- Inject a snippet of JavaScript into the page for execution in the context of the currently selected frame.
executeAsync :: String -> H.HeleniumM ()
executeAsync s = do
	callSelenium $ Request True (Post s) "/execute_async"
	return ()

