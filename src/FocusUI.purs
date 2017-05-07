module FocusUI (StateRenderer, embed, patchRepeatedly) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, modifyRef, newRef, readRef)
import Control.Monad.State.Reified (MonadStateV, fromGetPutFunctions, mapState)
import DOM (DOM)
import DOM.Event.Event (Event)
import DOM.Node.Types (Node)
import Optic.Core (view)
import Optic.Types (Lens')
import Text.Smolder.Markup (Markup)
import Text.Smolder.Renderer.IncrementalDom (render)
import Web.IncrementalDOM (patch)

-- | `onStateChange listener action statev`: Run the stateful effect `action`
-- | with `statev` and run the action given by `listener` whenever the state
-- | changes. Whenever `action` calls `put`, `modify` or `state`, `listener` is
-- | called with the new value. Using `put`, `modify`, `state` whitin the
-- | listener directly will result into an infinite loop, so `listener` should
-- | only use `get` or `gets`. Only after the `listener` has run is the new
-- | value set as new state.
onStateChange ::
  forall m s a. Monad m =>
  (s -> MonadStateV s m -> m Unit) ->
  (MonadStateV s m -> m a) ->
  MonadStateV s m -> m a
onStateChange listener stateAction { get, put } =
  stateAction $ monadStateVal unit
  where
    put' :: s -> m Unit
    put' s = do
      listener s $ monadStateVal unit
      put s
      -- TODO: also implement modify, state in terms of the given ones instead
      -- of using defaults?

    -- with trivial parameter because of circular dependency
    monadStateVal :: Unit -> MonadStateV s m
    monadStateVal unit = fromGetPutFunctions get put'

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
embed l r s monadStateV = r (view l s :: t) (mapState l monadStateV)

-- | `patchRepeatedly node initialState renderer`: Render `initialState` via
-- | `renderer` into `node` and update the interface whenever the state
-- | changes.
patchRepeatedly :: forall s e.
  Node ->
  s ->
  StateRenderer s (Eff (dom :: DOM, ref :: REF | e)) ->
  Eff (dom :: DOM, ref :: REF | e) Unit
patchRepeatedly node initialState renderer = do
  stateSig <- makeEffState initialState
  onStateChange patchNode (patchNode initialState) stateSig
  where
    patchNode ::
      s ->
      MonadStateV s (Eff (dom :: DOM, ref :: REF | e)) ->
      (Eff (dom :: DOM, ref :: REF | e)) Unit
    patchNode s sig = patch node $ render $ renderer s sig
