module Canvas where

import GHCJS.DOM.HTMLCanvasElement

import Reflex
import Reflex.DOM

withCanvas :: MonadWidget t m
           => m ()
           -> Dynamic t (Map String String)
           -> (HTMLCanvasElement -> m a)
           -> m a
withCanvas child attrs act = do
  (e, _) <- elDynAttrs' "canvas" attrs child
  act e
