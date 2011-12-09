module Helenium.Network (
	isUriValid,
	Request (..),
	RequestStateful,
	RequestMethod (..),
	RequestPath,
	Response (..),
	ResponseHTTPCode,
	ResponseHTTPReason,
	ResponseHTTPHeaders (..),
	callSelenium
) where

import qualified Helenium.Base as H
import qualified Helenium.Log as HL
import qualified Network.URI as URI
import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Stream as Stream
import Data.Maybe
import Control.Monad.Error
import Control.Monad.RWS.Strict

-------------------------------------------------------------------------------

isUriValid :: String -> Bool
isUriValid uri = isJust $ URI.parseURI uri

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

callSelenium :: Request -> H.HeleniumM Response
callSelenium req = do
	httpReq <- makeRequest req
	reader <- ask
	when (H.debugHttp reader) $ 
		HL.logMsg H.DebugRequest $
			"\n" ++ (show httpReq) ++ "\n" ++ (HTTP.rqBody httpReq)
	httpRes <- sendRequest httpReq
	when (H.debugHttp reader) $
		HL.logMsg H.DebugResponse $
			"\n" ++ (show httpRes) ++ "\n" ++ (HTTP.rspBody httpRes)
	processResponse httpRes

sendRequest :: HTTP.Request String -> H.HeleniumM (HTTP.Response String)
sendRequest req = do
	result <- liftIO $ catch 
		(HTTP.simpleHTTP req)
		-- Send the ioError inside the HTTP.Result type
		(\ioErr -> return $ Stream.failMisc (show ioErr)) 
	either whenLeft whenRight result where
		whenLeft connErr = throwError $ show connErr
		whenRight response = return response

processResponse :: HTTP.Response String -> H.HeleniumM Response
processResponse res = do
	let (x,y,z) = HTTP.rspCode res -- HTTP 200, etc
	let code = x * 100 + y * 10 + z * 1
	let reason = HTTP.rspReason res -- The "Ok", "Found" that comes after the HTTP code
	processResponseCodes (x, y, z) reason
	-- Do something with the haders??
	headers <- processHeaders res
	let body = HTTP.rspBody res -- The body string
	return (Response (x,y,z) reason headers body)

processResponseCodes :: (Int, Int, Int) -> String -> H.HeleniumM String
-- TODO: Do something!!!
processResponseCodes (x, y, z) reason = return ""

processHeaders :: HTTP.Response String -> H.HeleniumM ResponseHTTPHeaders
processHeaders httpRes = do
	return $ ResponseHTTPHeaders {
		responseHTTPHeaderLocation = HTTP.findHeader HTTP.HdrLocation httpRes
	}
	

-- WebDriver command messages should conform to the HTTP/1.1 request specification. 
-- All commands accept a content-type of application/json;charset=UTF-8. 
-- Message bodies for POST and PUT request must use application/json;charset=UTF-8.
makeRequest :: Request -> H.HeleniumM (HTTP.Request String)
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

makeRequestUri :: RequestStateful -> RequestPath -> H.HeleniumM String
makeRequestUri rs rp = do
	state <- get
	reader <- ask
	let sessionId = H.serverSessionId state
	uriPath <- if rs == True
		then if isNothing sessionId
			then throwError "Making a stateful call without a session."
			else return ("/session/" ++ (fromJust sessionId) ++ rp)
		else return rp
	return $ (H.server reader) ++ uriPath

makeRequestMethod :: RequestMethod -> H.HeleniumM HTTP.RequestMethod
makeRequestMethod Get = do return HTTP.GET
makeRequestMethod Delete = do return HTTP.DELETE
makeRequestMethod (Post _) = do return HTTP.POST

makeRequestHeaders :: RequestMethod -> H.HeleniumM [HTTP.Header]
makeRequestHeaders (Post s) = do 
	return [
		HTTP.Header HTTP.HdrContentType "application/json;charset=UTF-8",
		HTTP.Header HTTP.HdrContentLength $ show (length s)]
makeRequestHeaders _ = do
	return []

makeRequestBody :: RequestMethod -> H.HeleniumM String
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

