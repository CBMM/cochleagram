{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Time (getCurrentTime)
import Data.Monoid
import GHCJS.Foreign
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.DOM.AudioBuffer
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
import GHCJS.DOM.OscillatorNode
import GHCJS.DOM.HTMLCanvasElement
import GHCJS.DOM.CanvasRenderingContext2D
import GHCJS.DOM.ImageData
import GHCJS.DOM.Types
import GHCJS.DOM.Enums
import qualified JavaScript.Web.Canvas as C

import Control.Monad.IO.Class
import Reflex
import Text.Read (readMaybe)
import Reflex.Dom hiding (setValue, restore)
import Reflex.Dom.Time
import Cochlea
import WebAudio
import Data.Map


main :: IO ()
main = mainWidget main'

main' :: MonadWidget t m => m ()
main' = do
  pb <- getPostBuild
  --freqTxt <- value <$> textInput def
  text "Osc Freq"
  fInput <- textInput $ def & textInputConfig_inputType .~ "range"
                            & attributes .~ constDyn (   "min"  =: "500"
                                                      <> "max"  =: "2000"
                                                      <> "step" =: "0.01")
  el "br" $ return ()
  gInput <- textInput $ def & textInputConfig_inputType .~ "range"
                            & attributes .~ constDyn (   "min"  =: "0"
                                                      <> "max"  =: "1"
                                                      <> "step" =: "0.01"
                                                      <> "value" =: "0.01")
  el "br" $ return ()
  let freqs = fmap ((\(x :: Int) -> (x, show x)) . floor . ((2 :: Double) ^^)) [(5 :: Int) ..11] :: [(Int,String)]
  fftLen <- dropdown 2048 (constDyn (Data.Map.fromList freqs)) def
  el "br" $ return ()
  text "Cochlear filter center freq"
  cInput <- textInput $ def & textInputConfig_inputType .~ "range"
                            & attributes .~ constDyn (   "min"  =: "100"
                                                      <> "max"  =: "3000"
                                                      <> "step" =: "0.01"
                                                      <> "value" =: "500")

  c <- liftIO newAudioContext
  let freq = fmapMaybe readMaybe (updated (value fInput))

  osc <- oscillatorNode c (OscillatorNodeConfig 440 freq)
  g   <- gain c (GainConfig 0.01 (fmapMaybe readMaybe (updated (value gInput))))
  analyser <- analyserNode c def { _analyserNodeConfig_initial_smoothingTimeConstant = 0
                                 , _analyserNodeConfig_change_fftSize = updated (value fftLen)}

  gFreq <- holdDyn 500 (fmapMaybe readMaybe (updated (value cInput)))
  gFilt <- forDyn gFreq $ \f -> FGammaTone (GammaToneFilter 2 f 20 1)
  cFilt <- cochlearFilter c (castToAudioNode g) (CochlearFilterConfig gFilt (value fftLen))
  liftIO $ do
    connect osc (Just g) 0 0
    Just dest <- getDestination c
    --connect g (Just dest) 0 0
    connect (castToAudioNode $ _cfConvolverNode cFilt) (Just dest) 0 0
    connect g (Just (_analyser_node analyser)) 0 0
    start osc 0
  text "Hello"
  canvEl <- fmap (castToHTMLCanvasElement . _el_element . fst) $
            elAttr' "canvas" ("id" =: "canvas") $ return ()
  ctx'' <- liftIO $ GHCJS.DOM.HTMLCanvasElement.getContext
           canvEl ("2d" :: JSString)
  Just ctx' :: Maybe CanvasRenderingContext2D <- liftIO $ fromJSVal ctx''

  el "br" $ return ()
  performEvent (ffor pb $ \() -> liftIO $ do
    draw ctx')

  el "br" $ return ()

  clicks  <- button "squeeze"
  clicks' <- button "shift"
  performEvent (ffor clicks $ \() -> liftIO $ squeezeAppendColumn ctx' canvEl)
  performEvent (ffor clicks' $ \_ -> liftIO $ shiftAppendColumn ctx')
  t0 <- liftIO Data.Time.getCurrentTime
  ticks <- tickLossy 0.015 t0
  spectra <- _analyser_getByteFrequencyData analyser (() <$ ticks)
  performEvent (ffor spectra $ \a -> liftIO $ do
                    a' <- js_clampUint8Array a
                    img <- js_toGrayscale a'
                    l <- js_lengthUint8ClampedArray' img
                    imgData <- newImageData (Just img) 1 (fromIntegral $ l `div` 4)
                    shiftAppendColumn ctx'
                    putImageData ctx' (Just imgData) 290 0)

  powers <- (_cfGetPower cFilt) (() <$ ticks)
  -- performEvent (ffor powers $ \p -> liftIO $ print p)

  el "br" $ return ()
  text "end"

draw :: CanvasRenderingContext2D -> IO ()
draw ctx = do
  setFillStyle ctx (Just $ CanvasStyle $ jsval ("rgba(255,255,255,0.05)" :: JSString))
  fillRect ctx 0 0 300 150
  setFillStyle ctx (Just $ CanvasStyle $ jsval ("blue" :: JSString))
  fillRect ctx 104 105 30 30
  beginPath ctx
  putStrLn "Drawing"
  arc ctx 110 110 20 0 3 True -- ctx
  closePath ctx
  let sty :: JSVal = jsval ("rgba(105,105,0,0.5)" :: JSString)
  setFillStyle ctx (Just (CanvasStyle sty))
  fill ctx CanvasWindingRuleNonzero

shiftAppendColumn :: CanvasRenderingContext2D -> IO ()
shiftAppendColumn ctx = do
  setFillStyle ctx (Just (CanvasStyle (jsval ("rgba(0,255,0,0.5)" :: JSString))))
  fillRect ctx 50 50 10 10
  Just d <- getImageData ctx 1 0 300 150
  putImageData ctx (Just d) 0 0

squeezeAppendColumn :: CanvasRenderingContext2D -> HTMLCanvasElement -> IO ()
squeezeAppendColumn ctx canv = do
  save ctx
  scale ctx (299/300) 1
  drawImageFromCanvas ctx (Just canv) 0 0
  restore ctx

-- toGrayscale :: Uint8ClampedArray -> IO Uint8ClampedArray
-- toGrayscale x = do
--   n <- js_lengthUint8ClampedArray x
--   r <- js_createUint8ClampedArray (n*4)
--   forM [0..n-1] $ \i -> do

foreign import javascript unsafe
 "$r = new Uint8ClampedArray(($1).length * 4); for (var i = 0; i < ($1).length; i++) { var i0 = i*4; ($r)[i0] = ($1)[i]; ($r)[i0+1] = 255 - ($r)[i0]; ($r)[i0+2] = ($r)[i0]; ($r)[i0+3] = 255;}"
  js_toGrayscale :: Uint8ClampedArray -> IO Uint8ClampedArray

foreign import javascript unsafe "console.log(($1).data[0])"
  print' :: ImageData -> IO ()
-- foreign import unsafe "new Uint8ClampedArray()"
-- js_toGrayscale :: Uint8ClampedArray -> Uint8ClampedArray
-- js_toGrayscale
