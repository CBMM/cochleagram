{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif

module WebAudio where

import qualified JavaScript.Array as JA
import qualified JavaScript.TypedArray as TA
import JavaScript.TypedArray.Internal
import GHCJS.Foreign
import GHCJS.Marshal.Pure (pToJSVal, pFromJSVal)
import GHCJS.DOM.Enums (PToJSVal(..), PFromJSVal(..))
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.DOM.AnalyserNode
import GHCJS.DOM.AudioBuffer hiding (getGain)
import GHCJS.DOM.AudioBufferCallback
import GHCJS.DOM.AudioContext
import GHCJS.DOM.AudioDestinationNode
import GHCJS.DOM.AudioListener
import GHCJS.DOM.AudioNode
import GHCJS.DOM.AudioParam
import GHCJS.DOM.AudioProcessingEvent
import GHCJS.DOM.AudioStreamTrack
import GHCJS.DOM.AudioTrack
import GHCJS.DOM.AudioTrackList
import GHCJS.DOM.GainNode
import GHCJS.DOM.OscillatorNode

import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import GHCJS.DOM
import GHCJS.DOM.Types hiding (Event(..))
import qualified GHCJS.DOM.Types as D
import GHCJS.Types
import Reflex
import Reflex.Dom hiding (setValue)
import Reflex.Dom.Class

data OscillatorNodeConfig t = OscillatorNodeConfig {
    _oscillatorNodeConfig_initialFrequency :: Double
  , _oscillatorNodeConfig_setFrequency :: Event t Double
  }

oscillatorNode :: MonadWidget t m
               => AudioContext
               -> OscillatorNodeConfig t
               -> m OscillatorNode
oscillatorNode ctx cfg = do
  Just osc <- createOscillator ctx
  Just p   <- getFrequency osc
  performEvent_ (ffor (_oscillatorNodeConfig_setFrequency cfg)$ \f ->
                   liftIO (setValue p (realToFrac f)))
  return osc

data GainConfig t = GainConfig {
   _gainConfig_initialGain :: Double
 , _gainConfig_setGain :: Event t Double
 }

gain :: MonadWidget t m
     => AudioContext
     -> GainConfig t
     -> m GainNode
gain ctx cfg = do
  Just g <- createGain ctx
  Just p <- getGain g
  liftIO $ setValue p (realToFrac (_gainConfig_initialGain cfg))
  performEvent_ (ffor (_gainConfig_setGain cfg)$ \f ->
                  liftIO (setValue p (realToFrac f)))
  return g

data AnalyserNodeConfig t = AnalyserNodeConfig {
   _analyserNodeConfig_initial_fftSize               :: Int
 , _analyserNodeConfig_change_fftSize                :: Event t Int
 , _analyserNodeConfig_initial_minDecibels           :: Double
 , _analyserNodeConfig_change_minDecibels            :: Event t Double
 , _analyserNodeConfig_initial_maxDecibels           :: Double
 , _analyserNodeConfig_change_maxDecibels            :: Event t Double
 , _analyserNodeConfig_initial_smoothingTimeConstant :: Double
 , _analyserNodeConfig_change_smoothingTimeConstant  :: Event t Double
 }

instance Reflex t => Default (AnalyserNodeConfig t) where
  def = AnalyserNodeConfig {
      _analyserNodeConfig_initial_fftSize               = 1024
    , _analyserNodeConfig_change_fftSize                = never
    , _analyserNodeConfig_initial_minDecibels           = -100
    , _analyserNodeConfig_change_minDecibels            = never
    , _analyserNodeConfig_initial_maxDecibels           = -30
    , _analyserNodeConfig_change_maxDecibels            = never
    , _analyserNodeConfig_initial_smoothingTimeConstant = 0.8
    , _analyserNodeConfig_change_smoothingTimeConstant  = never
    }

data Analyser t m = Analyser {

    _analyser_node                  :: AnalyserNode
  , _analyser_getFloatFrequencyData :: Event t () -> m (Event t D.Float32Array)
  , _analyser_getByteFrequencyData  :: Event t () -> m (Event t D.Uint8Array)
}

analyserNode :: MonadWidget t m
             => AudioContext
             -> AnalyserNodeConfig t
             -> m (Analyser t m)
analyserNode ctx
  (AnalyserNodeConfig nFFT dnFFT minDB dminDB maxDB dmaxDB tau dtau) = do
    Just a <- liftIO $ createAnalyser ctx

    setFftSize a (fromIntegral nFFT)
    performEvent (ffor dnFFT $ \n -> liftIO (setFftSize a (fromIntegral n)))

    setMinDecibels a minDB
    performEvent (ffor dminDB $ \m -> liftIO (setMinDecibels a m))

    setMaxDecibels a maxDB
    performEvent (ffor dmaxDB $ \m -> liftIO (setMaxDecibels a m))

    setSmoothingTimeConstant a tau
    performEvent (ffor dtau $ \t -> liftIO (setSmoothingTimeConstant a t))

    return $ Analyser a (getFreqFloat a) (getFreqByte a)

getFreqFloat :: MonadWidget t m
             => AnalyserNode
             -> Event t () -> m (Event t D.Float32Array)
getFreqFloat a e = performEvent $ ffor e $ \_ -> liftIO $ do
  nSamp :: Int <- fromIntegral <$> getFrequencyBinCount a
  buffer <- js_createFloat32Array' nSamp
  getFloatFrequencyData a (Just buffer)
  return buffer

getFreqByte :: MonadWidget t m
             => AnalyserNode
             -> Event t () -> m (Event t Uint8Array)
getFreqByte a e = performEvent $ ffor e $ \_ -> liftIO $ do
  nSamp  <- fromIntegral <$> getFrequencyBinCount a
  buffer <- js_createUint8Array' nSamp
  getByteFrequencyData a (Just buffer)
  return buffer

foreign import javascript unsafe "new Float32Array($1)"
  js_createFloat32Array' :: Int -> IO Float32Array

foreign import javascript unsafe "new Uint8Array($1)"
  js_createUint8Array' :: Int -> IO Uint8Array

foreign import javascript unsafe "new Uint8ClampedArray($1)"
  js_createUint8ClampedArray' :: Int -> IO Uint8ClampedArray

foreign import javascript unsafe "($1).length"
  js_lengthUint8ClampedArray' :: Uint8ClampedArray -> IO Int

foreign import javascript unsafe "new Uint8ClampedArray($1)"
  js_clampUint8Array :: Uint8Array -> IO Uint8ClampedArray
