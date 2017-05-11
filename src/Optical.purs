module Optical (StateRenderer, embed, patchRepeatedly) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef)
import Control.Monad.State.Reified (MonadStateV, fromGetPutFunctions, mapState)
import DOM (DOM)
import DOM.Event.Event (Event)
import DOM.Node.Types (Node)
import Data.Lens (view)
import Data.Lens.Types (Lens')
import Text.Smolder.Markup (Markup)
import Text.Smolder.Renderer.IncrementalDom (render)
import Web.IncrementalDOM (patch)


onStateChange ::
  forall m s. Monad m =>
  (s -> m Unit) -> MonadStateV s m -> MonadStateV s m
onStateChange listener { get, put } = fromGetPutFunctions get put'
  where
    put' :: s -> m Unit
    put' s = do
      listener s
      put s

-- | Create a new reified MonadState interface within the `Eff` monad with
-- | given initial state.
makeEffState ::
  forall e s. s -> Eff (ref :: REF | e) (MonadStateV s (Eff (ref :: REF | e)))
makeEffState initialState = do
  stateRef <- newRef initialState
  let
    get = readRef stateRef
    put s = modifyRef stateRef $ const s
  pure $ fromGetPutFunctions get put

-- | A renderer of some state `s` into `Markup`, with stateful actions running
-- | in monad `m`.
type StateRenderer s m = s -> MonadStateV s m -> Markup (Event -> m Unit)

-- | Turn a renderer of a `t` into a renderer of an `s` via a `Lens'` from `s`
-- | to `t`, ignoring all information contained in an `s` that is not present
-- | in `t`.
embed ::
  forall s t m. Monad m =>
  Lens' s t -> StateRenderer t m -> StateRenderer s m
embed l r s msv = r (view l s) (mapState l msv)

-- | `patchRepeatedly node initialState renderer`: Render `initialState` via
-- | `renderer` into `node` and update the interface whenever the state
-- | changes.
patchRepeatedly :: forall s e.
  Node ->
  s ->
  StateRenderer s (Eff (dom :: DOM, ref :: REF | e)) ->
  Eff (dom :: DOM, ref :: REF | e) (MonadStateV s (Eff (dom :: DOM, ref :: REF | e)))
patchRepeatedly node initialState renderer = do
  msv <- makeEffState initialState
  let
    patchNode :: s -> (Eff (dom :: DOM, ref :: REF | e)) Unit
    patchNode s = patch node $ render $ renderer s (onStateChange patchNode msv)
  patchNode initialState
  pure $ onStateChange patchNode msv
