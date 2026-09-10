{-# LANGUAGE RankNTypes #-}

module Data.Array.Accelerate.LLVM.Metal.Execute.Par
  ( Par
  , evalPar
  , liftPar
  , spawnPar
  , block
  ) where

import Control.Concurrent.Async (withAsync, link, wait)
import Control.Monad.IO.Class (MonadIO(..))

import Data.Array.Accelerate.LLVM.State (LLVM, unliftIOLLVM)
import Data.Array.Accelerate.LLVM.Metal.Target (Metal)

newtype Par a = Par
  { runPar :: forall r. (a -> LLVM Metal r) -> LLVM Metal r
  }

instance Functor Par where
  fmap f (Par action) = Par $ \next -> action (next . f)

instance Applicative Par where
  pure value = Par $ \next -> next value

  Par makeFunction <*> Par makeArgument =
    Par $ \next ->
      makeFunction $ \f ->
        makeArgument $ \a ->
          next (f a)

instance Monad Par where
  Par action >>= f =
    Par $ \next ->
      action $ \value ->
        runPar (f value) next

instance MonadIO Par where
  liftIO action = liftPar (liftIO action)

liftPar :: LLVM Metal a -> Par a
liftPar action = Par $ \next -> action >>= next

evalPar :: Par a -> LLVM Metal a
evalPar action = runPar action pure

spawnPar :: Par () -> Par ()
spawnPar child =
  Par $ \next ->
    unliftIOLLVM $ \run ->
      withAsync (run (evalPar child)) $ \worker -> do
        -- child failure interrupts a parent blocked on a signal/ref
        link worker

        -- continue the parent without waiting for the child
        result <- run (next ())

        -- normal completion waits for the child, including its children
        -- exceptional completion cancels it via withAsync
        _ <- wait worker
        pure result

-- current Metal launch calls already wait for GPU completion
-- this is a GPU  sync boundary, not a child-thread join
block :: Par ()
block = pure ()

