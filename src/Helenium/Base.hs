{-# OPTIONS_HADDOCK hide #-}

module Helenium.Base (
	HeleniumM,
	runHeleniumM,
	HeleniumError (..),
	Status,
	HeleniumReader (..),
	HeleniumWriter (..),
	HeleniumState (..),
	Screenshot,
	HeleniumWriterMsg (..),
	HeleniumBrowser (..),
	HeleniumBrowserName (..),
	HeleniumBrowserVersion,
	HeleniumBrowserPlatform (..),
	heleniumBrowserNameKey,
	Frame (..),
	HeleniumCapability (..),
	heleniumCapabilityKey
) where

-------------------------------------------------------------------------------

import Data.Time
import Data.Maybe
import Control.Monad.Error
import Control.Monad.RWS.Strict

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

data HeleniumError = 
	Assert String | 
	Unknown String |
	InvalidRequest String | 
	FailedCommand Status String (Maybe Screenshot)

type Status = Int

instance Error HeleniumError where
	noMsg = Unknown "An unknown error ocurred."
	strMsg s = Unknown s

data HeleniumReader =
	HeleniumReader {
		server :: String,
		browser :: HeleniumBrowser,
		logTime :: Bool,
		debugHttp :: Bool,
		debugTime :: Bool,
		timeoutTest :: Int,
		timeoutElement :: Int,
		screenshotPath :: String
	}

type HeleniumWriter = [(UTCTime, HeleniumWriterMsg)]

data HeleniumState =
	HeleniumState {
		serverCapabilities :: [HeleniumCapability],
		serverSessionId :: Maybe HeleniumSessionId,
		currentFrame :: Frame
	}

data HeleniumWriterMsg = Info String | HttpRequest String | HttpResponse String | ScreenshotMsg Screenshot

-- A Base 64 encoded PNG image.
type Screenshot = String

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

heleniumBrowserNameKey Chrome = "chrome"
heleniumBrowserNameKey Firefox = "firefox"
heleniumBrowserNameKey HtmlUnit = "htmlunit"
heleniumBrowserNameKey IE = "internet explorer"
heleniumBrowserNameKey IPhone = "iphone"

data Frame = DefaultFrame | FrameById String | FrameByNumber Int

--- TODO: Move to the reader after moving out the config from the tests.
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

