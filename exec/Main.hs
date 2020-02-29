{-# LANGUAGE GADTs               #-}
{-# LANGUAGE JavaScriptFFI       #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Applicative                         (liftA3)
import           Control.Monad                               (join, when)
import           Data.Bool                                   (bool)
import qualified Data.JSString                               as JS
import           Data.Maybe                                  (fromMaybe, isJust)
import           Data.Monoid
import qualified Data.Text                                   as Text
import qualified Data.Time                                   as Time
import           GHCJS.DOM
import           GHCJS.DOM.AudioBuffer
import           GHCJS.DOM.AudioBufferCallback
import           GHCJS.DOM.AudioContext
import           GHCJS.DOM.AudioDestinationNode
import           GHCJS.DOM.AudioListener
import           GHCJS.DOM.AudioNode
import           GHCJS.DOM.AudioParam
import           GHCJS.DOM.AudioProcessingEvent
import           GHCJS.DOM.AudioTrack
import           GHCJS.DOM.AudioTrackList
import           GHCJS.DOM.CanvasRenderingContext2D
import           GHCJS.DOM.Enums
import           GHCJS.DOM.HTMLCanvasElement
import           GHCJS.DOM.ImageData
import           GHCJS.DOM.Navigator
import qualified GHCJS.DOM.Navigator                         as Navigator
import           GHCJS.DOM.NavigatorUserMediaErrorCallback
import           GHCJS.DOM.NavigatorUserMediaSuccessCallback
import           GHCJS.DOM.OscillatorNode
import           GHCJS.DOM.Types                             hiding (Event)
import           GHCJS.DOM.Window
import           GHCJS.Foreign
import           GHCJS.Marshal
import           GHCJS.Types
import qualified GHCJS.Types                                 as T
import qualified JavaScript.Array                            as JA
import qualified JavaScript.Object                           as JO
import qualified JavaScript.Web.Canvas                       as C
import qualified Text.PrettyPrint                            as Pretty

import           Arithmetic
import           Cochlea
import           Control.Monad.IO.Class
import qualified Data.Map                                    as Map
import           Reflex
import           Reflex.Dom                                  hiding (restore,
                                                              setValue)
import           Reflex.Dom.Contrib.Widgets.ButtonGroup
import           Reflex.Dom.Contrib.Widgets.Common
import           Reflex.Time
import           Text.Read                                   (readMaybe)
import           WebAudio


main :: IO ()
-- main = js_forceHttps >> mainWidget main'
main = mainWidget main'

main' :: forall t m. (MonadWidget t m) => m ()
main' = do
  pb <- getPostBuild

  (cochleaConfig, gainConfig, cFuncs, nSkip) <- elClass "div" "controls" $ do
    el "br" $ return ()
    text "Mic Boost"
    gainCoef <- justButtonGroup 4
                (constDyn [(1,"1x") ,(2,"2x"),(4,"4x"),(8,"8x")])
                def
    let gainConfig = GainConfig 8 (updated gainCoef)
    el "br" $ return ()

    text "Freq 1"
    filtsLo <- justButtonGroup 100
               (constDyn [(50,"50 Hz"),(100,"100 Hz"),(200,"200 Hz"), (400,"400 Hz")])
               def
    --filtsLo  <- numInput (NumInputConfig 0.1 1000 300 True 2 100)
    el "br" $ return ()

    text "Freq n"
    filtsHi <- justButtonGroup 6400
               (constDyn [(800,"800"), (1600, "1600"), (3200, "3200")
                         , (6400, "6400"), (12800, "12800")])
               def
    --filtsHi <- numInput (NumInputConfig 0.1 10000 300 True 2 5000)
    el "br" $ return ()

    text "N Filts"
    nFilts <- justButtonGroup 32
              (constDyn [(8,"8"), (16,"16"), (32,"32")
                        , (64,"64"), (128,"128")])
              def
    --nFilts  <- mapDyn floor =<< numInput (NumInputConfig 1 128 15 True 0 64)
    el "br" $ return ()

    let filtsRange = (,) <$> filtsLo <*> filtsHi

    text "Bandwidth Function"
    let defBwFunc = (UPrim2 PDiv UVar (ULit 100))
    bwFunc <- funExprInput defBwFunc


    text "Sampling rate"
    nSkip <- justButtonGroup 4
             (constDyn $ [(120, "1") ,(15,"8"),(4,"30")
                         ,(2,"60"),(1,"120")])
             def

    el "br" $ return ()

    cFuncs <- colorMappings

    let cochleaConfig = CochleaConfig (100,5000) (updated filtsRange) 64 (updated nFilts) True never defBwFunc (updated bwFunc)
    return (cochleaConfig, gainConfig, cFuncs, nSkip)


  let
      widgetStartButton = button "run" >>= \b -> return (never, b)
      widgetRunning     = do
        c <- liftIO newAudioContext

        g   <- gain c gainConfig

        fullCochlea <- cochlea c (toAudioNode g) cochleaConfig

        -- osc <- oscillatorNode c (OscillatorNodeConfig 440 freq)

        dynText $ ("N filters4: " <>) . Text.pack . show . Map.size <$> _cochlea_filters fullCochlea

        win :: GHCJS.DOM.Window.Window <- fromMaybe (error "No window") <$> liftIO currentWindow
        nav <- liftIO $ getNavigator win
        b <- liftIO $ toJSVal True
        myDict <- liftIO $ JO.create >>= \o -> JO.setProp "audio" b o >> return o

        mediaCallback :: NavigatorUserMediaSuccessCallback <- liftIO $ newNavigatorUserMediaSuccessCallback $ \stream -> do
          src <- liftIO $ createMediaStreamSource c stream
          liftIO $ connect (toAudioNode src) (g) (Just 0) (Just 0)

        failureCallback :: NavigatorUserMediaErrorCallback <- liftIO $ newNavigatorUserMediaErrorCallback $ \err -> do
          liftIO $ print'' err

        liftIO $ do
          dest <- getDestination c
          nm   <- js_userAgent
          when ("Chrome" `JS.isInfixOf` nm) $ do
            putStrLn "Chrome"

            -- Navigator.getUserMedia nav (Just (Dictionary (jsval myDict)))
            -- userMedia :: MediaStream <- getUserMedia nav (Just $ Dictionary (jsval myDict)) -- (mediaCallback) -- (failureCallback)
            -- connect userMedia (toAudioNode g) (Just 0) (Just 0)
            js_connectMic c (toAudioNode g)
            return ()

          when ("Firefox" `JS.isInfixOf` nm) $ putStrLn "FF" >> js_connectMic' c (toAudioNode g)
          putStrLn "Test2"

        el "br" $ return ()

        let playing' = constDyn True

        ticks' <- tickLossyFromPostBuildTime (1/120) :: m (Event t TickInfo)
        ticks'' <- downsample (current (nSkip)) ticks'
        let ticks = gate (current (traceDyn "playing" playing')) ticks''

        -- performEvent (ffor ticks' $ \_ -> liftIO (putStrLn "Tick'"))


        el "br" $ return ()

        -- dynText $ (("Playing: " <>) . Text.pack . show) <$> playing

        el "br" $ return ()

        cochleaPowers :: Event t (Map.Map Double Double) <- _cochlea_getPowerData fullCochlea (() <$ ticks)

        let applyExpr e xs = Prelude.map (flip uevalD e) xs
        let cochleaColors = attachWith (\(fR,fG,fB) ps ->
                                           let cs = Map.elems ps
                                               cR = applyExpr fR cs
                                               cG = applyExpr fG cs
                                               cB = applyExpr fB cs
                                           in  (cR,cG,cB)) (current cFuncs) cochleaPowers

        return ( cochleaColors :: Event t ([Double],[Double],[Double])
               , never :: Event t ()
               )

  rec
    cochleaTicksAndWidgetswaps :: Dynamic t (Event t ([Double],[Double],[Double]), Event t ()) <- widgetHold widgetStartButton nextWidget
    let nextWidget = leftmost
          [ widgetRunning  <$ switchDyn (fmap snd cochleaTicksAndWidgetswaps)
            -- TODO add more conditions for restarting
          ]
    -- ticks = switchDyn $ fmap (\(_,t,_) -> t) <$> cochleaTicksAndWidgetswaps
  let cochleaColors :: Event t ([Double],[Double],[Double]) = switchDyn $ fmap fst $ cochleaTicksAndWidgetswaps

  elClass "div" "coch-display" $ do
    canvEl :: HTMLCanvasElement <-
      fmap (HTMLCanvasElement . unElement . _element_raw . fst) $
              elAttr' "canvas" ("id" =: "canvas"
                                <> "width" =: "200"
                                <> "height" =: "128"
                                <> "style" =: "height:256px;") $ return ()
    ctx'' <- fmap (fromMaybe (error "getContext error")) $ liftIO $
               GHCJS.DOM.HTMLCanvasElement.getContext
                 canvEl ("2d" :: JSString) ([] :: [Int])

    -- TODO: Is there a better way to cast
    --       RenderingContext to CanvasRenderingContext2D?
    ctx' :: CanvasRenderingContext2D <-
      fmap (fromMaybe (error "2d context error")) $ liftIO $ fromJSVal =<< toJSVal ctx''


    -- cochleaPowers <- fmap _cochlea_getPowerData fullCochlea (() <$ ticks)
      -- cochleaPowers :: Dynamic t (Maybe (Event t () -> m (Event t (Map.Map Double Double)))) = fmap ( _cochlea_getPowerData) <$> fullCochlea

    -- let cochlea' = ffor fullCochlea $ \case
    --       Nothing -> \e  -> return never -- (\e -> fmap (\() -> Map.empty) e)
    --       Just f  -> _cochlea_getPowerData f

    -- -- cochleaPowerData <- forM fullCochlea $


    -- let cochleaColors =
    --       undefined
    --       -- fmap
    --       -- (\case
    --       --     (Nothing,_)  -> (0,0,0)
    --       --     (Just (fR,fG,fB), ps) ->
    --       --       let cs = Map.elems ps
    --       --           cR = applyExpr fR cs
    --       --           cG = applyExpr fG cs
    --       --           cB = applyExpr fB cs
    --       --       in  (cR,cG,cB)
    --       -- )
    --       -- $ attachWith
    --       --   ((,) <$> cochleaPowers <*> cFuncs)

    performEvent (ffor cochleaColors $ \(rs,gs,bs) -> liftIO $ do
                    when (length rs > 0) $ do
                      let toClamped :: [Double] -> IO Uint8ClampedArray
                          toClamped xs = (js_makeUint8ClampedArray . JA.fromList) =<< traverse (toJSVal . (* 255)) xs
                      r <- toClamped rs
                      g <- toClamped gs
                      b <- toClamped bs
                      img <- js_zip_colors_magic_height r g b
                      let l = 128 * 4 :: Int -- length rs * 4
                      imgData <- newImageData img 1 (Just $ fromIntegral $ l `div` 4)
                      shiftAppendColumn ctx'
                      putImageData ctx' imgData 199 0
                      printJSVal =<< toJSVal rs
                 )

    el "br" $ return ()


-- map double to 0-255 range
dblToInt :: Double -> Double -> Double -> Int
dblToInt lo hi x = let x' = min hi (max lo x)
                   in  floor $ 255 * ((x' - lo) / (hi - lo))

-- toDb :: Double -> Double
-- toDb x = 20 * logBase 10 x

shiftAppendColumn :: CanvasRenderingContext2D -> IO ()
shiftAppendColumn ctx = do
  d <- getImageData ctx 1 0 200 128
  putImageData ctx d 0 0

squeezeAppendColumn :: CanvasRenderingContext2D -> HTMLCanvasElement -> IO ()
squeezeAppendColumn ctx canv = do
  save ctx
  scale ctx (199/200) 1
  drawImage ctx canv 0 0
  restore ctx

foreign import javascript unsafe "$r = new Uint8ClampedArray($1)"
  js_makeUint8ClampedArray :: JA.JSArray -> IO Uint8ClampedArray

foreign import javascript unsafe
 "$r = new Uint8ClampedArray(($1).length * 4); function logC(x){ return(((Math.sqrt(x/255))*255)|0)}; var l = ($1).length; for (var i = 0; i < l; i++) { var i0 = (l-i-1)*4; ($r)[i0] = ($1)[i]; ($r)[i0+1] = ($1)[i]; ($r)[i0+2] = logC(($1)[i]); ($r)[i0+3] = 255;}"
  js_toGrayscale :: Uint8ClampedArray -> IO Uint8ClampedArray

foreign import javascript unsafe
 "$r = new Uint8ClampedArray(($1).length * 4); var l = ($1).length; for (var i = 0; i < l; i++) { var i0 = (l - i - 1) * 4; ($r)[i0] = ($1)[i]; ($r)[i0+1] = ($2)[i]; ($r)[i0+2] = ($3)[i]; ($r)[i0+3] = 255;}"
   js_zip_colors :: Uint8ClampedArray -> Uint8ClampedArray -> Uint8ClampedArray -> IO Uint8ClampedArray

foreign import javascript unsafe
 "var out_l = 128; var in_l = ($1).length; $r = new Uint8ClampedArray(out_l * 4);  for (var i = 0; i < out_l; i++) { var in_i = (i * in_l / out_l) | 0; var i0 = (out_l - i - 1) * 4; ($r)[i0] = ($1)[in_i]; ($r)[i0+1] = ($2)[in_i]; ($r)[i0+2] = ($3)[in_i]; ($r)[i0+3] = 255;}"
   js_zip_colors_magic_height :: Uint8ClampedArray -> Uint8ClampedArray -> Uint8ClampedArray -> IO Uint8ClampedArray

foreign import javascript unsafe "console.log($1)"
  printJSVal :: JSVal -> IO ()

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

downsample :: forall t m a. MonadWidget t m => Behavior t Int -> Event t a -> m (Event t a)
downsample nDropPerKeep e = do
  counted :: Event t (Int, a) <- zipListWithEvent (,) [0 :: Int ..] e
  let f :: Int -> (Int, a) -> Maybe a
      f n (ind, ev)
        | ind `mod` n == 0 = Just ev
        | otherwise        = Nothing
      kept    = attachWithMaybe f nDropPerKeep counted
  return kept



justButtonGroup
  :: (MonadWidget t m, Eq a, Show a)
  => a
  -> Dynamic t [(a,Text.Text)]
  -> WidgetConfig t (Maybe a)
  -> m (Dynamic t a)
justButtonGroup vDef btns cfg = do
  bg <- bootstrapButtonGroup btns cfg {_widgetConfig_initialValue = Just vDef}
  holdDyn vDef (fmapMaybe id $ updated (value bg))
