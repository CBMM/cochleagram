{-# LANGUAGE ScopedTypeVariables #-}

module Cochlea where

import Data.Map
import Reflex
import Reflex.Dom
import GHCJS.DOM.Types
import GHCJS.DOM.AnalyserNode
import GHCJS.DOM.ConvolverNode
import GHCJS.DOM.AudioBuffer
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

data CochlearFilterConfig t = CochlearFilterConfig {
    _cfcFilter   :: Dynamic t Filter
  , _cfcNSamples :: Dynamic t Int
  }

data CochlearFilter t = CochlearFilter {
    _cfAudioContext    :: AudioContext
  , _cfConvolverNode   :: ConvolverNode
  , _cfAnalyserNode    :: AnalyserNode
  , _cfFreqResponse    :: Dynamic t (Map Double Double)
  , _cfImpulseResponse :: Dynamic t (Map Double Double)
  }

impulseResponse :: Double -> Double -> Filter -> [Double]
impulseResponse _ _ (FFreq _) = error "FFreq filter not implemented"
impulseResponse freq width filt = Prelude.map sample sampTimes
  where nSamps    = floor $ width / freq
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

setImpulseResponse
  :: Reflex t
  => CochlearFilter t
  -> Filter
  -> Int
  -> IO ()
setImpulseResponse cochlearFilt filt nSamps = do
  len <- fromIntegral nSamps / 44100 -- TODO: Magic number
  setBuffer convolver (Just buffer)


cochlearFilter :: MonadWidget t m
               => CochlearFilterConfig t
               -> m (CochlearFilter t)
cochlearFilter (CochlearFilterConfig filt nSamp) = do
  undefined
