{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative (liftA3)
import Control.Monad (join, when)
import Data.Bool (bool)
import qualified Data.JSString as JS
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
import GHCJS.DOM.Types hiding (Event)
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
import Arithmetic
import Cochlea
import WebAudio
import Data.Map

data NumInputConfig = NumInputConfig {
   numInputConfig_min          :: Double
  ,numInputConfig_max          :: Double
  ,numInputConfig_n            :: Int
  ,numInputConfig_log          :: Bool
  ,numInputConfig_sigfigs      :: Int
  ,numInputConfig_initialValue :: Double
  }

numInput :: (MonadWidget t m)
         => NumInputConfig
         -> m (Dynamic t Double)
numInput (NumInputConfig lo hi n isLog sigfigs x0) =
  elClass "div" "num-input" $ do
    let (rMin,rMax) = bool (lo, hi) (log lo, log hi) isLog
        rStep       = (rMax - rMin) / (fromIntegral n - 1)
        rVal        = bool x0 (log x0) isLog
        mkRound     :: Double -> Double
        mkRound     = (/ 10 ^^ sigfigs)
                      . (fromIntegral :: Int -> Double)
                      . round
                      . (* 10 ^^ sigfigs)
    nInput <- textInput $
      def & textInputConfig_inputType  .~ "range"
          & textInputConfig_attributes .~ constDyn
            ( "min"   =: show rMin
           <> "max"   =: show rMax
           <> "step"  =: show rStep
           <> "value" =: show rVal)
    valNum <- holdDyn x0 $ fmap mkRound
                         $ fmap (bool id exp isLog)
                         $ fmapMaybe readMaybe
                         $ (updated $ value nInput)
    dynText =<< mapDyn show valNum
    return valNum

main :: IO ()
main = js_forceHttps >> mainWidget main'

main' :: forall t m. MonadWidget t m => m ()
main' = do
  pb <- getPostBuild

  let gain0 = 2
  -- numInput (NumInputConfig 1 1000 7 True 0 10)
  el "br" $ return ()
  text "Mic Gain"
  gainCoef <- numInput (NumInputConfig 0.01 10 33 True 2 gain0)
  el "br" $ return ()

  nFilts :: Dynamic t Int <- mapDyn floor =<< numInput (NumInputConfig 1 128 15 True 0 32)
  text "Bandwidth Function"
  bwInput <- textInput $ def (textInputConfig_initialValue .~ "x / 100")
  let defBwFunc = (UPrim2 PDiv UVar (ULit 100.0))
  bwFunc <- holdDyn (UPrim2 PDiv UVar (ULit 100.0)) $
            fmapMaybe (hush . parseUexp) (updated (value bwInput))

  fInput <- textInput $ def & textInputConfig_inputType .~ "range"
                            & attributes .~ constDyn (   "min"  =: "500"
                                                      <> "max"  =: "2000"
                                                      <> "step" =: "0.01")
  el "br" $ return ()
  gInput <- textInput $ def & textInputConfig_inputType .~ "range"
                            & attributes .~ constDyn (   "min"  =: "0.1"
                                                      <> "max"  =: "5"
                                                      <> "step" =: "0.01"
                                                      <> "value" =: "0.01")
  el "br" $ return ()


  let freqs = fmap ((\(x :: Int) -> (x, show x)) . floor . ((2 :: Double) ^^)) [(5 :: Int) ..11] :: [(Int,String)]
  fftLen <- dropdown 2048 (constDyn (Data.Map.fromList freqs)) def
  el "br" $ return ()

  c <- liftIO newAudioContext

  -- osc <- oscillatorNode c (OscillatorNodeConfig 440 freq)

  Just win <- liftIO currentWindow
  Just nav <- liftIO $ getNavigator win
  b <- liftIO $ toJSVal True
  myDict <- liftIO $ JO.create >>= \o -> JO.setProp "audio" b o >> return o
  g   <- gain c (GainConfig gain0 (updated gainCoef))

  mediaCallback <- liftIO $ newNavigatorUserMediaSuccessCallback $ \s -> do
    case s of
      Just stream -> do
        Just src <- liftIO $ createMediaStreamSource c (Just stream)
        liftIO $ connect (castToAudioNode src) (Just g) 0 0
      Nothing -> liftIO (putStrLn "SUCCESS NOTHING")

  failureCallback <- liftIO $ newNavigatorUserMediaErrorCallback $ \e -> do
    case e of
      Just err -> liftIO $ print'' err
      Nothing -> liftIO (putStrLn "FAILURE NOTHING")

  analyser <- analyserNode c def { _analyserNodeConfig_initial_smoothingTimeConstant = 0
                                 , _analyserNodeConfig_change_fftSize = updated (value fftLen)}

  -- TODO
  fullCochlea <- cochlea c (castToAudioNode g)
                         (CochleaConfig (100,5000) never 32 (updated nFilts) True never defBwFunc (updated bwFunc))

  liftIO $ do
    --connect mic (Just g) 0 0
    -- connect osc (Just g) 0 0
    Just dest <- getDestination c
    -- connect g (Just dest) 0 0
    --connect (castToAudioNode $ _cfConvolverNode cFilt) (Just dest) 0 0
    connect g (Just (_analyser_node analyser)) 0 0
    -- start osc 0
    nm <- js_userAgent
    when ("Chrome" `JS.isInfixOf` nm)  $ putStrLn "Chrome" >>
      webkitGetUserMedia nav (Just $ Dictionary (jsval myDict)) (Just mediaCallback) (Just failureCallback)
    when ("Firefox" `JS.isInfixOf` nm) $ putStrLn "FF" >> js_connectMic' c (castToAudioNode g)
    putStrLn "Test2"
  canvEl <- fmap (castToHTMLCanvasElement . _el_element . fst) $
            elAttr' "canvas" ("id" =: "canvas"
                           <> "width" =: "100"
                           <> "height" =: "128"
                           <> "style" =: "height:256px;") $ return ()
  ctx'' <- liftIO $ GHCJS.DOM.HTMLCanvasElement.getContext
           canvEl ("2d" :: JSString)
  Just ctx' :: Maybe CanvasRenderingContext2D <- liftIO $ fromJSVal ctx''

  el "br" $ return ()
  performEvent (ffor pb $ \() -> liftIO $ do
    draw ctx')

  el "br" $ return ()


  t0 <- liftIO Data.Time.getCurrentTime
  ticks <- tickLossy 0.033 t0

  let defEq = UPrim2 PPow (UPrim2 PRange (UPrim1 PToDb UVar) (UPair (ULit (-90)) (ULit (-50)))) (ULit 2)
      defStr = "(x dB -> (-90,-50))^2"

  rInp <- textInput $ def & textInputConfig_initialValue .~ defStr
  rFunc <- holdDyn defEq $ fmapMaybe (hush . parseUexp) (updated (value rInp))
  gInp <- textInput $ def & textInputConfig_initialValue .~ defStr
  gFunc <- holdDyn defEq $ fmapMaybe (hush. parseUexp) (updated (value gInp))
  bInp <- textInput $ def & textInputConfig_initialValue .~ defStr
  bFunc <- holdDyn defEq $ fmapMaybe (hush. parseUexp) (updated (value bInp))

  cFuncs <- $(qDyn [| ( $(unqDyn [|rFunc|]), $(unqDyn [|gFunc|]), $(unqDyn [|bFunc|])) |])

  let applyExpr e xs = Prelude.map (flip uevalD e) xs

  cochleaPowers <- _cochlea_getPowerData fullCochlea (() <$ ticks)
  let cochleaColors = attachWith (\(fR,fG,fB) ps ->
        let cs = elems ps
            cR = applyExpr fR cs
            cG = applyExpr fG cs
            cB = applyExpr fB cs
        in  (cR,cG,cB)) (current cFuncs) cochleaPowers

  performEvent (ffor cochleaColors $ \(rs,gs,bs) -> liftIO $ do
                    when (length rs > 0) $ do
                      let toClamped :: [Double] -> IO Uint8ClampedArray
                          toClamped xs = (js_makeUint8ClampedArray . JA.fromList) =<< traverse (toJSVal . (* 255)) xs
                      r <- toClamped rs
                      g <- toClamped gs
                      b <- toClamped bs
                      img :: Uint8ClampedArray <- js_zip_colors r g b
                      -- l <- js_lengthUint8ClampedArray' img
                      let l = length rs * 4
                      imgData <- newImageData (Just img) 1 (fromIntegral $ l `div` 4)
                      shiftAppendColumn ctx'
                      putImageData ctx' (Just imgData) 99 0)

  el "br" $ return ()


  -- ticks60 <- tickLossy 0.015 t0
  -- lightLevels <- foldDyn
  --                (\l0 l -> unionWith (\a b -> max (a*0.99) b) l0 l)
  --                mempty
  --                cochleaPowers
  -- -- _ <- el "div" $ do
  -- listViewWithKey' lightLevels $ \k v -> do
  --     let boxColor l = "rgba("
  --                   <> show (dblToInt (-90) (-30) (20 * logBase 10 l))
  --                   <> ",0,0,1)"
  --     boxAttrs <- forDyn v $ \light ->
  --       "style" =: ("float:left;height:20px;width:20px;background-color:" <> boxColor light)
  --                                   -- <> "hight" =: "20px"
  --                                   -- <> "width" =: "20px"
  --                                   -- <> "style" =: boxColor light
  --     -- boxLabel <- mapDyn (show . boxColor) v
  --     (thisBox :: El t, _) <- elDynAttr' "div" boxAttrs $
  --       return () -- (dynText boxLabel)
  --     return (domEvent Click thisBox :: Reflex.Dom.Event t ())
  -- return ()

-- map double to 0-255 range
dblToInt :: Double -> Double -> Double -> Int
dblToInt lo hi x = let x' = min hi (max lo x)
                   in  floor $ 255 * ((x' - lo) / (hi - lo))

toDb :: Double -> Double
toDb x = 20 * logBase 10 x

hush :: Either l r -> Maybe r
hush (Left _)  = Nothing
hush (Right r) = Just r

draw :: CanvasRenderingContext2D -> IO ()
draw ctx = do
  setFillStyle ctx (Just $ CanvasStyle $ jsval ("rgba(255,255,255,0.05)" :: JSString))
  fillRect ctx 0 0 300 150

shiftAppendColumn :: CanvasRenderingContext2D -> IO ()
shiftAppendColumn ctx = do
  Just d <- getImageData ctx 1 0 100 128
  putImageData ctx (Just d) 0 0

squeezeAppendColumn :: CanvasRenderingContext2D -> HTMLCanvasElement -> IO ()
squeezeAppendColumn ctx canv = do
  save ctx
  scale ctx (99/100) 1
  drawImageFromCanvas ctx (Just canv) 0 0
  restore ctx

foreign import javascript unsafe "$r = new Uint8ClampedArray($1)"
  js_makeUint8ClampedArray :: JA.JSArray -> IO Uint8ClampedArray

foreign import javascript unsafe
 "$r = new Uint8ClampedArray(($1).length * 4); function logC(x){ return(((Math.sqrt(x/255))*255)|0)}; var l = ($1).length; for (var i = 0; i < l; i++) { var i0 = (l-i-1)*4; ($r)[i0] = ($1)[i]; ($r)[i0+1] = ($1)[i]; ($r)[i0+2] = logC(($1)[i]); ($r)[i0+3] = 255;}"
  js_toGrayscale :: Uint8ClampedArray -> IO Uint8ClampedArray

foreign import javascript unsafe
 "$r = new Uint8ClampedArray(($1).length * 4); var l = ($1).length; for (var i = 0; i < l; i++) { var i0 = (l - i - 1) * 4; ($r)[i0] = ($1)[i]; ($r)[i0+1] = ($2)[i]; ($r)[i0+2] = ($3)[i]; ($r)[i0+3] = 255;}"
   js_zip_colors :: Uint8ClampedArray -> Uint8ClampedArray -> Uint8ClampedArray -> IO Uint8ClampedArray

foreign import javascript unsafe "console.log(($1).data[0])"
  print' :: ImageData -> IO ()

foreign import javascript unsafe "console.log($1)"
 print'' :: NavigatorUserMediaError -> IO ()

foreign import javascript unsafe "console.log($1)"
 print''' :: Dictionary -> IO ()

foreign import javascript unsafe "try {navigator.webkitGetUserMedia({'audio':true},function(s){mss = ($1).createMediaStreamSource(s); mss.connect($2); console.log('SUCCESS');}, function(e){ console.log('ERROR:' + e);})} catch (e) { alert(e) }"
  js_connectMic :: AudioContext -> AudioNode -> IO ()

foreign import javascript unsafe "navigator.userAgent"
  js_userAgent :: IO JSString

foreign import javascript unsafe "navigator.mediaDevices.getUserMedia({'audio':true}).then(function(s){ mss = ($1).createMediaStreamSource(s); mss.connect($2); console.log('SUCCESS2');})"
  js_connectMic' :: AudioContext -> AudioNode -> IO ()

foreign import javascript unsafe "if (window.location.protocol != 'https:') { window.location.href = 'https:' + window.location.href.substring(window.location.protocol.length) }"
  js_forceHttps :: IO ()
