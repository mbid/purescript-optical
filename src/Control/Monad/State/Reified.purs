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
import Data.Tuple (Tuple(..))
import Optic.Core (over, set, view)
import Optic.Types (Lens')

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
    state' f = state g
      where
        g :: s -> Tuple a s
        g = l f
    get' :: m t
    get' = gets (view l :: s -> t)
    gets' :: forall a. (t -> a) -> m a
    gets' f = gets ((view l >>> f) :: s -> a)
    put' :: t -> m Unit
    put' t = modify $ set l t
    modify' :: (t -> t) -> m Unit
    modify' f = modify $ over l f
