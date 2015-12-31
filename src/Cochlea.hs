{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cochlea where

import Control.Monad.IO.Class (liftIO)
import Data.Map
import Data.Traversable
import Reflex
import Reflex.Dom
import GHCJS.DOM.Types hiding (Event)
import GHCJS.DOM.AudioNode
import GHCJS.DOM.AudioContext
import GHCJS.DOM.AnalyserNode
import GHCJS.DOM.ConvolverNode
import GHCJS.DOM.AudioBuffer
import GHCJS.Marshal
import qualified JavaScript.Array as JA
import WebAudio


data GammaToneFilter = GammaToneFilter
  { gtfOrder      :: Int
  , gtfCenterFreq :: Double
  , gtfBandwidth  :: Double
  , gtfAmplitude  :: Double
  } deriving (Show)


data Filter = FGammaTone GammaToneFilter    -- ^ Standard gammatone filter
            | FImpulse   (Double -> Double) -- ^ Arbitrary impulse response
            | FFreq      (Double -> Double) -- ^ Arbitrary frequency response

instance Show Filter where
  show (FGammaTone g) = show g

data CochlearFilterConfig t = CochlearFilterConfig {
    _cfcFilter   :: Dynamic t Filter
  , _cfcNSamples :: Dynamic t Int
  }

data CochlearFilter t m = CochlearFilter {
    _cfAudioContext    :: AudioContext
  , _cfConvolverNode   :: ConvolverNode
  , _cfAnalyserNode    :: AnalyserNode
  , _cfGetPower        :: Event t () -> m (Event t Double)
  -- , _cfFreqResponse    :: Dynamic t (Map Double Double)
  -- , _cfImpulseResponse :: Dynamic t (Map Double Double)
  }

impulseResponse :: Double -> Double -> Filter -> [Double]
impulseResponse _ _ (FFreq _) = error "FFreq filter not implemented"
impulseResponse freq buffLen filt = Prelude.map sample sampTimes
  where nSamps    = floor $ buffLen * freq
        sampTimes = Prelude.map ((/ freq) . realToFrac) [0..nSamps - 1]
        sample = case filt of
          FImpulse f   -> f
          FGammaTone f -> gammaTone f

gammaTone :: GammaToneFilter -> Double -> Double
gammaTone (GammaToneFilter n f b a) t =
  a * (t ^^ (n-1)) * exp (-2 * pi * b * t) * cos (2*pi*f*t + 0)
  -- NOTE I'm ignoring the 'carrier phase' parameter. That's ok?
  -- NOTE There's no way to use complex numbers in the web audio convolver
  --      This seems weird. Fortunately gammatone impulse response
  --      is purely real. Right?

setImpulseResponse :: AudioContext -> CochlearFilter t m -> Filter -> Int -> IO ()
setImpulseResponse ctx (CochlearFilter _ conv anyl _) filt nSamps = do
  let freq = 44100 -- TODO: Magic number. Can get this from AudioContext maybe?
      len = fromIntegral nSamps / freq
  let samps = impulseResponse freq len filt
  sampVals <- traverse toJSVal samps
  buf <- js_doublesToBuffer ctx (JA.fromList sampVals)
  setBuffer conv (Just buf)


foreign import javascript unsafe
  "$r = ($1).createBuffer(2,22050,44100); var c = new Float32Array($2); ($r).copyToChannel(c,0,0)"
  js_doublesToBuffer :: AudioContext -> JA.JSArray -> IO AudioBuffer

foreign import javascript unsafe
  "var buf = new Float32Array(($1).fftSize); ($1).getFloatTimeDomainData(buf); var $r = 0; buf.forEach(function(s){ $r = $r + s*s;}); $r = Math.sqrt($r)/buf.length"
  js_getPower :: AnalyserNode -> IO Double

cochlearFilter :: MonadWidget t m
               => AudioContext
               -> AudioNode
               -> CochlearFilterConfig t
               -> m (CochlearFilter t m)
cochlearFilter ctx inputNode (CochlearFilterConfig filt nSamp) = mdo
  Just convNode <- liftIO $ createConvolver ctx
  Just anylNode <- liftIO $ createAnalyser  ctx
  connect inputNode (Just convNode) 0 0
  connect convNode  (Just anylNode) 0 0
  let getPower e = (ffor e $ \_ -> liftIO $ js_getPower anylNode)
  pb         <- getPostBuild
  filtParams <- combineDyn (,) filt nSamp
  let cFilter = CochlearFilter ctx convNode anylNode (\req -> performEvent $ ffor req $ \_ -> liftIO (js_getPower anylNode))
  _ <- performEvent (ffor (leftmost [tagDyn filtParams pb , updated filtParams]) $ \(f,n) -> liftIO $ setImpulseResponse ctx cFilter f n)
  return cFilter

