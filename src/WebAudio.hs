{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif

module WebAudio where

import GHCJS.Foreign
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
import GHCJS.DOM
import GHCJS.DOM.Types hiding (Event(..))
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

data Analyser t = Analyser {

    _analyser_node             :: AnalyserNode
  , _analyser_getFrequencyData :: MonadWidget t m
                               => Event t ()
                               -> Event t [Double]

  }

analyserNode :: MonadWidget t m
             => AudioContext
             -> AnalyserNodeConfig t
             -> m AnalyserNode
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

    return $ Analyser a (getFreq a)

getFreq :: MonadWidget t m
        => AnalyserNode
        -> Event t () -> m (Event t [Double])
getFreq a e = ffor e $ \_ -> liftIO $ do
  nSamp <- getFrequencyBinCount

