module Helenium (
	HeleniumM
) where

-------------------------------------------------------------------------------

import Data.Maybe
import Data.Either
import Control.Monad.RWS
import Control.Monad.Error
import Network.HTTP

-------------------------------------------------------------------------------

-- Helenium Monad

type HeleniumError = String

type HeleniumReader = String

type HeleniumWriter = String

data HeleniumState = 
	HeleniumState {
		serverHost :: String,
		serverPort :: Integer,
		serverBrowser :: Browser,
		serverSessionID :: Maybe String
	}

data Browser = Chrome | Firefox | InternetExplorer | Opera | Safari

instance Show Browser where
	show Chrome             = "Chrome"
	show Firefox            = "Firefox"
	show InternetExplorer   = "IE"
	show Opera		= "Opera"
	show Safari             = "Safari"

-- The Helenium Monad transformer has:
-- A Reader that allows to have as environment the actual session.
-- A writer to log.
-- A state.
-- And finally, allows to handle errors.
-- All this inside the monad you choose.
type HeleniumM = 
	ErrorT HeleniumError (RWST HeleniumReader HeleniumWriter HeleniumState IO)

runHeleniumM :: HeleniumM a -> HeleniumReader -> HeleniumState -> IO (Either HeleniumError a, HeleniumState, HeleniumWriter)
runHeleniumM hm r s = runRWST (runErrorT hm) r s

-- :t runHeleniumT
-- runHeleniumT :: HeleniumT e r w s m a -> ErrorT e (RWST r w s m) a
-- :t runErrorT . runHeleniumT
-- runErrorT . runHeleniumT :: HeleniumT e r w s m a -> RWST r w s m (Either e a)
-- :t runRWST . runErrorT . runHeleniumT
-- runRWST . runErrorT . runHeleniumT :: HeleniumT e r w s m a -> r -> s -> m (Either e a, s, w)

-------------------------------------------------------------------------------

-- Use session?
type RequestStateful = Bool

data RequestType = GET | POST String | DELETE

type RequestPath = String

data RequestResponse = RequestResponse ErrorCode String

-- WebDriver command messages should conform to the HTTP/1.1 request specification. 
-- All commands accept a content-type of application/json;charset=UTF-8. 
-- Message bodies for POST and PUT request must use application/json;charset=UTF-8.
makeRequest :: RequestStateful -> RequestType -> RequestPath -> HeleniumM String
makeRequest rs rt rp = do
	state <- get
	let baseUri = (serverHost state) ++ ":" ++ (show $ serverPort state)
	let uriPath = 
		if rs == True
		-- Throw (throwError) a proper error when no session available
		then "/session/" ++ (fromJust $ serverSessionID state) ++ rp
		else "/session/" ++ rp
	let uri = baseUri ++ uriPath	
	return uri

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

