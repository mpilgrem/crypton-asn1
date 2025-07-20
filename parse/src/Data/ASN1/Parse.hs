{-# LANGUAGE CPP #-}

{- |
Module      : Data.ASN1.Parse
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

A parser combinator for a stream of ASN.1 items.
-}

module Data.ASN1.Parse
  ( ParseASN1
    -- * Run 'ParseASN1'
  , runParseASN1State
  , runParseASN1
  , throwParseError
    -- * Combinators
  , onNextContainer
  , onNextContainerMaybe
  , getNextContainer
  , getNextContainerMaybe
  , getNext
  , getNextMaybe
  , hasNext
  , getObject
  , getMany
  ) where

import           Control.Applicative ( Alternative (..) )
import           Control.Arrow ( first )
import           Control.Monad ( liftM2 )
import           Data.ASN1.Types
                   ( ASN1 (..), ASN1ConstructionType (..), ASN1Object (..) )
import           Data.ASN1.Stream ( getConstructedEnd )

-- | Type representing a parser combinator for a stream of ASN.1 items.
newtype ParseASN1 a = P { runP :: [ASN1] -> Either String (a, [ASN1]) }

instance Functor ParseASN1 where
  fmap f m = P (fmap (first f) . runP m)

instance Applicative ParseASN1 where
  pure a = P $ \s -> Right (a, s)

  (<*>) mf ma = P $ \s ->
    case runP mf s of
      Left err      -> Left err
      Right (f, s2) ->
        case runP ma s2 of
          Left err      -> Left err
          Right (a, s3) -> Right (f a, s3)

instance Monad ParseASN1 where
  return = pure

  (>>=) m1 m2 = P $ \s ->
    case runP m1 s of
      Left err      -> Left err
      Right (a, s2) -> runP (m2 a) s2

instance Alternative ParseASN1 where
  empty = P $ \_ -> Left "empty Alternative"

  (<|>) m1 m2 = P $ \s ->
    case runP m1 s of
      Left _        -> runP m2 s
      Right (a, s2) -> Right (a, s2)

instance MonadFail ParseASN1 where
  fail = throwParseError

-- | Returns a list of ASN.1 items in the stream.
get :: ParseASN1 [ASN1]
get = P $ \stream -> Right (stream, stream)

-- | Puts the given list of ASN.1 items as the remainder of the stream and
-- returns @()@.
put :: [ASN1] -> ParseASN1 ()
put stream = P $ \_ -> Right ((), stream)

-- | Run the given parse monad over the given list of ASN.1 items. Returns the
-- result and a list of the ASN.1 items remaining in the stream (if successful).
runParseASN1State :: ParseASN1 a -> [ASN1] -> Either String (a, [ASN1])
runParseASN1State = runP

-- | Run the given parse monad over the given list of ASN.1 items and returns
-- the result (if successful).
--
-- If ASN.1 items remain in the stream after doing so, returns an error.
runParseASN1 :: ParseASN1 a -> [ASN1] -> Either String a
runParseASN1 f s =
  case runP f s of
    Left err      -> Left err
    Right (o, []) -> Right o
    Right (_, er) -> Left ("runParseASN1: remaining state " ++ show er)

-- | Throw a parse error.
throwParseError ::
     String
     -- ^ Error message.
  -> ParseASN1 a
throwParseError s = P $ \_ -> Left s

-- | Get the object from the next ASN.1 item in a stream of ASN.1 items.
-- Throws a parse error if the object cannot be obtained from the item.
getObject :: ASN1Object a => ParseASN1 a
getObject = do
  l <- get
  case fromASN1 l of
    Left err     -> throwParseError err
    Right (a, l2) -> put l2 >> return a

-- | Get the next ASN.1 item in a stream of ASN.1 items.
getNext :: ParseASN1 ASN1
getNext = do
  list <- get
  case list of
    []    -> throwParseError "empty"
    (h : l) -> put l >> return h

-- | Get many items from the stream until there are none left.
getMany :: ParseASN1 a -> ParseASN1 [a]
getMany getOne = do
  next <- hasNext
  if next
    then liftM2 (:) getOne (getMany getOne)
    else return []

-- | Applies the given function to the next ASN.1 item in a stream of ASN.1
-- items, if there is one.
getNextMaybe :: (ASN1 -> Maybe a) -> ParseASN1 (Maybe a)
getNextMaybe f = do
  list <- get
  case list of
    []      -> return Nothing
    (h : l) -> let r = f h
               in  do case r of
                        Nothing -> put list
                        Just _  -> put l
                      return r

-- | Get the next container of the specified type and return a list of all its
-- ASN.1 elements. Throws a parse error if there is no next container of the
-- specified type.
getNextContainer :: ASN1ConstructionType -> ParseASN1 [ASN1]
getNextContainer ty = do
  list <- get
  case list of
    []                -> throwParseError "empty"
    (h : l)
      | h == Start ty -> do let (l1, l2) = getConstructedEnd 0 l
                            put l2 >> return l1
      | otherwise     -> throwParseError "not an expected container"

-- | Run the parse monad over the elements of the next container of specified
-- type. Throws an error if there is no next container of the specified type.
onNextContainer :: ASN1ConstructionType -> ParseASN1 a -> ParseASN1 a
onNextContainer ty f =
  getNextContainer ty >>= either throwParseError return . runParseASN1 f

-- | As for 'getNextContainer', except that it does not throw an error if there
-- is no next container of the specified type.
getNextContainerMaybe :: ASN1ConstructionType -> ParseASN1 (Maybe [ASN1])
getNextContainerMaybe ty = do
  list <- get
  case list of
    []                -> return Nothing
    (h : l)
      | h == Start ty -> do let (l1, l2) = getConstructedEnd 0 l
                            put l2 >> return (Just l1)
      | otherwise     -> return Nothing

-- | As for 'onNextContainer', except that it does not throw an error if there
-- is no next container of the specified type.
onNextContainerMaybe ::
     ASN1ConstructionType
  -> ParseASN1 a
  -> ParseASN1 (Maybe a)
onNextContainerMaybe ty f = do
  n <- getNextContainerMaybe ty
  case n of
    Just l  -> either throwParseError (return . Just) $ runParseASN1 f l
    Nothing -> return Nothing

-- | Are there any more ASN.1 items in the stream?
hasNext :: ParseASN1 Bool
hasNext = not . null <$> get
