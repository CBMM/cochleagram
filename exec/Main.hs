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
import GHCJS.DOM.Types
import GHCJS.DOM.Enums
import qualified JavaScript.Web.Canvas as C

import Control.Monad.IO.Class
import Reflex
import Text.Read (readMaybe)
import Reflex.Dom hiding (setValue, restore)
import Reflex.Dom.Time
import WebAudio
import Data.Map


main :: IO ()
main = mainWidget main'

main' :: MonadWidget t m => m ()
main' = do
  pb <- getPostBuild
  freqTxt <- value <$> textInput def
  gInput <- textInput $ def & textInputConfig_inputType .~ "range"
                            & attributes .~ constDyn (   "min"  =: "0"
                                                      <> "max"  =: "1"
                                                      <> "step" =: "0.01")
  c <- liftIO newAudioContext
  let freq = fmapMaybe readMaybe (updated freqTxt)
  osc <- oscillatorNode c (OscillatorNodeConfig 440 freq)
  g   <- gain c (GainConfig 1 (fmapMaybe readMaybe (updated (value gInput))))
  liftIO $ do
    connect osc (Just g) 0 0
    Just dest <- getDestination c
    connect g (Just dest) 0 0
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
  performEvent (ffor ticks $ \_ -> liftIO $ shiftAppendColumn ctx')

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
  scale ctx 0.99 1
  drawImageFromCanvas ctx (Just canv) 0 0
  restore ctx
  -- scale ctx 0.9 1
  -- Just d <- getImageData ctx 0 0 350 150
  -- save ctx
  -- putImageData ctx (Just d) 0 0
  -- restore ctx

