module Control.Monad.State.Reified
( MonadStateV
, reifiedStateClass
, fromStateFunction
, fromGetPutFunctions
, mapMonad
, mapState
)
where

import Prelude
import Control.Monad.State (class MonadState)
import Control.Monad.State.Class (get, gets, modify, put, state)
import Control.MonadZero (class MonadZero, empty)
import Data.Lens (_1, _2, over, set, view)
import Data.Lens.Types (Lens')
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- | The datum of a `MonadState`, reified as record.  
-- | `mapMonad` and `mapState` induce the structure of a bifunctor
-- |`MonadStateV :: Monad x Lens' -> Type`.
type MonadStateV s m = 
  { state :: forall a. (s -> Tuple a s) -> m a
  , get :: m s
  , gets :: forall a. (s -> a) -> m a
  , put :: s -> m Unit
  , modify :: (s -> s) -> m Unit
  }

-- | The canonical `MonadStateV s m` given `MonadState s m`.
reifiedStateClass :: forall m s. MonadState s m => MonadStateV s m
reifiedStateClass = { state, get, gets, put, modify }

-- | Create a complete `MonadStateV` given just `state`, using default
-- | implementations for the other functions.
fromStateFunction ::
  forall s m. (forall a. (s -> Tuple a s) -> m a) -> MonadStateV s m
fromStateFunction state = { state, get, gets, put, modify }
  where
    gets :: forall a. (s -> a) -> m a
    gets f = state \s -> Tuple (f s) s
    get :: m s
    get = gets id
    modify :: (s -> s) -> m Unit
    modify f = state \s -> Tuple unit (f s)
    put :: s -> m Unit
    put s = modify $ const s

-- | Create a complete `MonadStateV` given just `get` and `put`, using default
-- | implementations for the other functions.
fromGetPutFunctions ::
  forall s m. Monad m => m s -> (s -> m Unit) -> MonadStateV s m
fromGetPutFunctions get put = { state, get, gets, put, modify }
  where
    state :: forall a. (s -> Tuple a s) -> m a
    state f = do
      Tuple x s <- f <$> get
      put s
      pure x
    gets :: forall a. (s -> a) -> m a
    gets f = f <$> get
    modify :: (s -> s) -> m Unit
    modify f = get >>= f >>> put

-- | Change the underlying monad of a `MonadStateV`, given a morphism of monads.
mapMonad :: forall s m n. (m ~> n) -> MonadStateV s m -> MonadStateV s n
mapMonad nat { state, get, gets, put, modify } =
  { state: state'
  , get: get'
  , gets: gets'
  , put: put'
  , modify: modify'
  }
  where
    state' :: forall a. (s -> Tuple a s) -> n a
    state' f = nat $ state f
    get' :: n s
    get' = nat get
    gets' :: forall a. (s -> a) -> n a
    gets' f = nat $ gets f
    put' :: s -> n Unit
    put' s = nat $ put s
    modify' :: (s -> s) -> n Unit
    modify' f = nat $ modify f

-- | Change the state type of a `MonadStateV`, given a morphism a lens.
mapState :: forall s t m.
  Monad m =>
  Lens' s t -> MonadStateV s m -> MonadStateV t m
mapState l { state, get, gets, put, modify } =
  { state: state'
  , get: get'
  , gets: gets'
  , put: put'
  , modify: modify'
  }
  where
    state' :: forall a. (t -> Tuple a t) -> m a
    state' f = state (\x -> over _2 (\y -> set l y x) $ f (view l x))
    get' :: m t
    get' = gets (view l :: s -> t)
    gets' :: forall a. (t -> a) -> m a
    gets' f = gets ((view l >>> f) :: s -> a)
    put' :: t -> m Unit
    put' t = modify $ set l t
    modify' :: (t -> t) -> m Unit
    modify' f = modify $ over l f

-- | Change the state type of a `MonadStateV`, given a partial lens. *All*
-- | `MonadStateV` actions fail using the `MonadZero` instance if the target of
-- | the lens is `Nothing`, even `put`.
-- | In the future, this should be generalized to affine traversals because a
-- | `Nothing` is never `set` to a `Just`.
mapStatePartial :: forall s t m.
  MonadZero m =>
  Lens' s (Maybe t) -> MonadStateV s m -> MonadStateV t m
mapStatePartial l { state, get, gets, put, modify } =
  { state: state'
  , get: get'
  , gets: gets'
  , put: put'
  , modify: modify'
  }
  where
    fromJust' :: forall a. Maybe a -> m a
    fromJust' = case _ of
      Nothing -> empty
      Just x -> pure x
    state' :: forall a. (t -> Tuple a t) -> m a
    state' f = join $ state f'
      where
        f' :: s -> Tuple (m a) s
        f' x = case view l x of
          Nothing -> Tuple empty x
          (Just y) ->
            over _1 pure $ -- Tuple a _ -> Tuple (m a) _
            over _2 (\y' ->  set l (Just y') x) $ -- Tuple _ t -> Tuple _ s
            f y
    get' :: m t
    get' =
      view l <$> get >>= fromJust'
    gets' :: forall a. (t -> a) -> m a
    gets' f = gets (view l >>> map f) >>= fromJust'
    put' :: t -> m Unit
    put' y = modify' $ const y
    modify' :: (t -> t) -> m Unit
    modify' f = state' $ f >>> Tuple unit
