{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE TemplateHaskell #-}
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
import GHCJS.DOM.AudioBuffer hiding (getSampleRate)
import GHCJS.Marshal
import qualified JavaScript.Array as JA
import WebAudio
import Arithmetic


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
  }


impulseResponse :: Double -> Double -> Filter -> [Double]
impulseResponse freq buffLen filt = Prelude.map sample sampTimes
  where nSamps    = floor $ buffLen * freq
        sampTimes = Prelude.map ((/ freq) . realToFrac) [0..nSamps - 1]
        sample = case filt of
          FImpulse f   -> f
          FGammaTone f -> gammaTone f
          FFreq _ -> error "FFreq filter not implemented"


gammaTone :: GammaToneFilter -> Double -> Double
gammaTone (GammaToneFilter n f b a) t =
  a * (t ^^ (n-1)) * exp (-2 * pi * b * t) * cos (2*pi*f*t + 0)
  -- NOTE I'm ignoring the 'carrier phase' parameter. That's ok?
  -- NOTE There's no way to use complex numbers in the web audio convolver
  --      This seems weird. Fortunately gammatone impulse response
  --      is purely real. Right?


setImpulseResponse :: AudioContext -> CochlearFilter t m -> Filter -> Int -> IO ()
setImpulseResponse ctx (CochlearFilter _ conv anyl _) filt nSamps = do
  freq <- realToFrac <$> GHCJS.DOM.AudioContext.getSampleRate ctx
  let -- freq = 44100 -- TODO: Magic number. Can get this from AudioContext maybe?
      len = fromIntegral nSamps / freq
  let samps = impulseResponse freq len filt
  sampVals <- traverse toJSVal samps
  buf <- js_doublesToBuffer ctx (JA.fromList sampVals) freq
  setBuffer conv (Just buf)


foreign import javascript unsafe
  "$r = ($1).createBuffer(2,($2).length,($3)); var d = ($r).getChannelData(0); for (var i = 0; i < ($2).length; i++) {d[i] = ($2)[i]; };"
  js_doublesToBuffer :: AudioContext -> JA.JSArray -> Double -> IO AudioBuffer


foreign import javascript unsafe
  "var buf = new Uint8Array(($1).fftSize); ($1).getByteTimeDomainData(buf); $r = 0; for (var i = 0; i < ($1).fftSize; i++){ var s = (buf[i] - 127) * 0.003921; $r = $r + s*s;}; $r = Math.sqrt($r)/buf.length"
  js_getPower :: AnalyserNode -> IO Double



cochlearFilter :: MonadWidget t m
               => AudioContext
               -> AudioNode
               -> CochlearFilterConfig t
               -> m (CochlearFilter t m)
cochlearFilter ctx inputNode (CochlearFilterConfig filt nSamp) = mdo
  convNode <- liftIO $ createConvolver ctx
  anylNode <- liftIO $ createAnalyser  ctx
  connect inputNode convNode (Just 0) (Just 0)
  connect convNode  anylNode (Just 0) (Just 0)
  let getPower = do
        p <- js_getPower anylNode
        -- print $ "Power: " ++ show p
        return p
  pb         <- getPostBuild
  let filtParams = (,) <$> filt <*> nSamp
  let cFilter = CochlearFilter ctx convNode anylNode (\reqs -> performEvent $ ffor reqs $ \() -> liftIO getPower)
  _ <- performEvent $ ffor (updated nSamp) (\n -> liftIO (setFftSize (_cfAnalyserNode cFilter) (fromIntegral n)))
  _ <- performEvent (ffor (leftmost [tag (current filtParams) pb , updated filtParams]) $ \(f,n) -> liftIO $ setImpulseResponse ctx cFilter f n)
  return cFilter

data CochleaConfig t = CochleaConfig
  { _cochleaConfig_initial_freqRange  :: (Double,Double)
  , _cochleaConfig_change_freqRange   :: Event t (Double,Double)
  , _cochleaConfig_initial_nFreq      :: Int
  , _cochleaConfig_change_nFreq       :: Event t Int
  , _cochleaConfig_initial_logSpace   :: Bool
  , _cochleaConfig_change_logSpace    :: Event t Bool
  , _cochleaConfig_initial_bwFunction :: UExp
  , _cochleaConfig_change_bwFunction  :: Event t UExp
  }

data CochloaConfig' = CochleaConfig'
  { _cc_freqRange :: (Double, Double)
  , _cc_nFreo     :: Int
  , _cc_logSpace  :: Bool
  , _ccBwFunction :: UExp
  } deriving (Show)

data Cochlea t m = Cochlea
  { _cochlea_getPowerData :: Event t () -> m (Event t (Map Double Double))
  , _cochlea_filters      :: Dynamic t (Map Double (CochlearFilter t m))
  }


freqSpace :: (Double, Double) -> Int -> Bool -> UExp -> Map Double Filter
freqSpace (freq1,freqN) n True bwFunc =
  fromList $ zipWith (\f b -> (f, FGammaTone (GammaToneFilter 2 f b 1))) freqs bws
  where lf1   = log freq1
        lfN   = log freqN
        dFr   = (lfN - lf1) / (fromIntegral n - 1)
        freqs = Prelude.map (exp . (+ lf1) . (* dFr) . fromIntegral) [0..n-1]
        inds  = Prelude.map (\f -> log f / dFr) freqs
        bws   = Prelude.map (flip uevalD bwFunc) freqs


cochlea :: MonadWidget t m => AudioContext -> AudioNode -> CochleaConfig t -> m (Cochlea t m)
cochlea ctx inputNode (CochleaConfig rng drng n dn l dl f df) = do

  frange    <- holdDyn rng drng
  nfilts    <- holdDyn n   dn
  logspace  <- holdDyn l   dl
  bwFunc    <- holdDyn f   df
  let filtspecs = freqSpace <$> frange <*> nfilts <*> logspace <*> bwFunc
  filts     <- listWithKey filtspecs $ \freq filt -> do
    cochlearFilter ctx inputNode
      CochlearFilterConfig { _cfcFilter = filt
                           , _cfcNSamples = constDyn 2048 }

  let getPowers reqs = performEvent $ ffor (tag (current filts) reqs) $ \cFilts ->
        traverse (\cf -> liftIO (js_getPower $ _cfAnalyserNode cf)) cFilts

  return (Cochlea getPowers filts)

-- buildCochlea
--   :: MonadWidget t m
--   => AudioContext
--   -> AudioNode
--   -> CochleaConfig'
--   -> Cochlea'
-- buildCochlea = undefined
