{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (when)
import Data.Time (getCurrentTime)
import Data.Monoid
import GHCJS.Foreign
import GHCJS.Types
import qualified GHCJS.Types as T
import GHCJS.Marshal
import GHCJS.DOM
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
import GHCJS.DOM.NavigatorUserMediaSuccessCallback
import GHCJS.DOM.NavigatorUserMediaErrorCallback
import GHCJS.DOM.Types
import GHCJS.DOM.Enums
import GHCJS.DOM.Navigator
import GHCJS.DOM.Window
import qualified JavaScript.Web.Canvas as C
import qualified JavaScript.Array as JA
import qualified JavaScript.Object as JO

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

  -- Just win <- liftIO currentWindow
  -- Just nav <- liftIO $ getNavigator win
  -- b <- liftIO $ toJSVal True
  -- myDict <- liftIO $ JO.create >>= \o -> JO.setProp "audio" b o >> return o
  -- liftIO $ print''' $ Dictionary (jsval myDict)
  g   <- gain c (GainConfig 0.01 (fmapMaybe readMaybe (updated (value gInput))))

  -- mediaCallback <- liftIO $ newNavigatorUserMediaSuccessCallback $ \s -> do
  --   case s of
  --     Just stream -> do
  --       Just src <- liftIO $ createMediaStreamSource c (Just stream)
  --       liftIO $ Prelude.putStrLn "SUCCESS"
  --       liftIO $ connect (castToAudioNode src) (Just g) 0 0
  --     Nothing -> liftIO (putStrLn "SUCCESS NOTHING")

  -- failureCallback <- liftIO $ newNavigatorUserMediaErrorCallback $ \e -> do
  --   case e of
  --     Just err -> do
  --       liftIO $ putStrLn "FAILURE"
  --       liftIO $ print'' err
  --     Nothing -> liftIO (putStrLn "FAILURE NOTHING")

  -- webkitGetUserMedia nav (Just $ Dictionary (jsval myDict)) (Just mediaCallback) (Just failureCallback)
  -- liftIO $ putStrLn "Did getUserMedia"
  -- stream2 <- liftIO $ getUserMedia nav (Just (Dictionary (jsval myDict)))
  -- Just src2 <- liftIO $ createMediaStreamSource c (Just stream2)
  -- liftIO $ connect (castToAudioNode src2) (Just g) 0 0

  analyser <- analyserNode c def { _analyserNodeConfig_initial_smoothingTimeConstant = 0
                                 , _analyserNodeConfig_change_fftSize = updated (value fftLen)}

  gFreq <- holdDyn 500 (fmapMaybe readMaybe (updated (value cInput)))
  gFilt <- forDyn gFreq $ \f -> FGammaTone (GammaToneFilter 2 f 20 1)
  cFilt <- cochlearFilter c (castToAudioNode g) (CochlearFilterConfig gFilt (value fftLen))

  fullCochlea <- cochlea c (castToAudioNode g) (CochleaConfig (100,3000) never 60 never True never)

  liftIO $ do
    --connect mic (Just g) 0 0
    connect osc (Just g) 0 0
    Just dest <- getDestination c
    connect g (Just dest) 0 0
    --connect (castToAudioNode $ _cfConvolverNode cFilt) (Just dest) 0 0
    connect g (Just (_analyser_node analyser)) 0 0
    start osc 0
  liftIO $ js_connectMic c (castToAudioNode g)
  text "Hello"
  canvEl <- fmap (castToHTMLCanvasElement . _el_element . fst) $
            elAttr' "canvas" ("id" =: "canvas" <> "width" =: "300" <> "height" =: "60" <> "image-rendering" =: "pixelated") $ return ()
  ctx'' <- liftIO $ GHCJS.DOM.HTMLCanvasElement.getContext
           canvEl ("2d" :: JSString)
  Just ctx' :: Maybe CanvasRenderingContext2D <- liftIO $ fromJSVal ctx''

  el "br" $ return ()
  performEvent (ffor pb $ \() -> liftIO $ do
    draw ctx')

  el "br" $ return ()

  clicks  <- button "squeeze"
  clicks' <- button "shift"
  performEvent (ffor clicks $ \() -> liftIO $ squeezeAppendColumn ctx' canvEl >> js_connectMic c (castToAudioNode g))
  performEvent (ffor clicks' $ \_ -> liftIO $ shiftAppendColumn ctx')
  t0 <- liftIO Data.Time.getCurrentTime
  -- ticks <- tickLossy 0.015 t0
  ticks <- tickLossy 0.060 t0
  spectra <- _analyser_getByteFrequencyData analyser (() <$ ticks)
  cochleaPowers <- _cochlea_getPowerData fullCochlea (() <$ ticks)

  -- performEvent (ffor spectra $ \a -> liftIO $ do
  --                   a' <- js_clampUint8Array a
  --                   img <- js_toGrayscale a'
  --                   l <- js_lengthUint8ClampedArray' img
  --                   imgData <- newImageData (Just img) 1 (fromIntegral $ l `div` 4)
  --                   shiftAppendColumn ctx'
  --                   putImageData ctx' (Just imgData) 290 0)

  powers <- (_cfGetPower cFilt) (() <$ ticks)
  -- performEvent (ffor powers $ \p -> liftIO $ print p)
  performEvent (ffor cochleaPowers $ \ps -> liftIO $ do
                    vs <- traverse toJSVal $ Prelude.map (dblToInt (-90) (-30) . toDb) $ elems ps
                    when (length vs > 0) $ do
                      a' <- js_makeUint8ClampedArray (JA.fromList vs)
                      -- a' <- js_clampUint8Array a
                      img <- js_toGrayscale a'
                      l <- js_lengthUint8ClampedArray' img
                      imgData <- newImageData (Just img) 1 (fromIntegral $ l `div` 4)
                      shiftAppendColumn ctx'
                      putImageData ctx' (Just imgData) 290 0)
  -- performEvent (ffor cochleaPowers $ \ps -> liftIO $ print (maximum (ps <> 0 =: 0)))

  el "br" $ return ()
  text "end"

-- map double to 0-255 range
dblToInt :: Double -> Double -> Double -> Int
dblToInt lo hi x = let x' = min hi (max lo x)
                   in  floor $ 255 * ((x' - lo) / (hi - lo))

toDb :: Double -> Double
toDb x = 20 * logBase 10 x

draw :: CanvasRenderingContext2D -> IO ()
draw ctx = do
  setFillStyle ctx (Just $ CanvasStyle $ jsval ("rgba(255,255,255,0.05)" :: JSString))
  fillRect ctx 0 0 300 150

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

foreign import javascript unsafe "new Uint8ClampedArray($1)"
  js_makeUint8ClampedArray :: JA.JSArray -> IO Uint8ClampedArray

foreign import javascript unsafe
 "$r = new Uint8ClampedArray(($1).length * 4); for (var i = 0; i < ($1).length; i++) { var i0 = i*4; ($r)[i0] = ($1)[i]; ($r)[i0+1] = 255 - ($r)[i0]; ($r)[i0+2] = ($r)[i0]; ($r)[i0+3] = 255;}"
  js_toGrayscale :: Uint8ClampedArray -> IO Uint8ClampedArray

foreign import javascript unsafe "console.log(($1).data[0])"
  print' :: ImageData -> IO ()

foreign import javascript unsafe "console.log($1)"
 print'' :: NavigatorUserMediaError -> IO ()

foreign import javascript unsafe "console.log($1)"
 print''' :: Dictionary -> IO ()

foreign import javascript unsafe "console.log('hi'); try {navigator.webkitGetUserMedia({'audio':true},function(s){mss = ($1).createMediaStreamSource(s); mss.connect($2); console.log('SUCCESS');}, function(e){ console.log('ERROR:' + e);})} catch (e) { alert(e) }"
  js_connectMic :: AudioContext -> AudioNode -> IO ()
