{-# LANGUAGE DefaultSignatures, ScopedTypeVariables, DataKinds, KindSignatures, UndecidableInstances, TypeOperators, DerivingVia, FlexibleInstances, FlexibleContexts #-}
module AutoInput where

import Control.Monad.Trans.Maybe
import Data.Time
import GHC.Generics
import Control.Monad
import Text.Read (readMaybe)

-- Author: Kurashina Asuka (Eiko)
-- This module provides a way to automatically prompt the user for input for a generic data type.
--
-- Summary on how to use generic programming:
-- 1. Define a class Class that you need (in this case, AskUserInput)
-- 2. Define a generic class GClass (GInput) that works on the representation of the data type
-- 3. Define instances for the generic class GClass for each metadata type (U1, M1, etc.)
-- 4. Define a default implementation for the class Class using the generic class GClass like
--      default someFunction :: (Generic a, GClass (Rep a)) => ...
--      someFunction = genericSomeFunction
-- 5. Define base instances for the class Class for each data type
-- 6. make the canonical instance 
--      Class a => GClass (K1 i a) 
-- 7. Derive Generic for any data type you want to use the generic class with, and write an empty instance for the class
-- Done! Works like magic! owo
-- The compiler will automatically derive the complex functions for you. At the cost of some runtime overhead for converting between the generic representation and the original data type.
--
-- Here is how things work:
--   Providing base case (5) ---6--> Generic base case ---3--> Derived Generic complex case ---4--> Defauting to derived complex case

data InputMode = NoInput | UserInput
newtype Data (input :: InputMode) a = Data {unData :: a} deriving (Show, Read, Eq, Ord, Num, Enum, Bounded) via a
-- this data type is used to mark whether the data should be prompted to let the user to input
-- any type wrapped with Data NoInput will be skipped, replacing with a default value

class DefaultIO a where
  def :: IO a

instance DefaultIO String where def = return ""
instance DefaultIO Int where def = return 0
instance DefaultIO Day where def = utctDay <$> getCurrentTime

instance AskUserInput a => AskUserInput (Data UserInput a) where
  askUserInput = fmap Data <$> askUserInput
instance (DefaultIO a) => AskUserInput (Data NoInput a) where
  askUserInput = Just . Data <$> def

class AskUserInput a where
  askUserInput :: IO (Maybe a) -- this class provides a way to get user input
  default askUserInput :: (Generic a, GInput (Rep a)) => IO (Maybe a)
  askUserInput = genericAskUserInput
  -- use a class to let the compiler derive complex functions for us

class GInput f where
  gAskUserInput :: IO (Maybe (f a))

-- starting with the inner most U1, K1 for the base case
instance GInput U1 where -- U1 represents unit type, it has only one value
  gAskUserInput = return (Just U1)

-- no need to implement for V1, since it can't be constructed, have no constructors

instance (GInput f, Constructor c) => GInput (M1 C c f) where
  gAskUserInput = do
    putStrLn $ "data -> " ++ conName (undefined :: M1 C c f p) -- you can use 'undefined' to pass a type to a function without values, how cool is that?
    fmap M1 <$> gAskUserInput -- mapping over two monads here, IO and Maybe

-- For Datatype metadata
instance (GInput f, Datatype d) => GInput (M1 D d f) where
  gAskUserInput = do
    --putStrLn $ "Datatype -> " ++ datatypeName (undefined :: M1 D d f p)
    fmap M1 <$> gAskUserInput

-- For Selector metadata, you could potentially prompt for each field if selName is not empty.
instance (GInput f, Selector s) => GInput (M1 S s f) where
  gAskUserInput = do
    let selector = selName (undefined :: M1 S s f p)
    unless (null selector) $ putStrLn $ "input " ++ selector ++ ":"
    fmap M1 <$> gAskUserInput

instance AskUserInput String where askUserInput = Just <$> getLine

instance AskUserInput Int where 
  askUserInput = readMaybe <$> getLine

instance AskUserInput Day where 
  askUserInput = do
    putStrLn "(Format: yyyy-mm-dd, input empty to use today.)"
    input <- getLine -- if input nothing, use today
    if null input
      then Just . utctDay <$> getCurrentTime
      else return (readMaybe input :: Maybe Day)

-- product types
instance (GInput f, GInput g) => GInput (f :*: g) where -- if the first one is Nothing, the entire computation should short-circuit
  gAskUserInput = runMaybeT $ liftM2 (:*:) (MaybeT gAskUserInput) (MaybeT gAskUserInput)

genericAskUserInput :: (Generic a, GInput (Rep a)) => IO (Maybe a)
genericAskUserInput = fmap to <$> gAskUserInput

--instance (Generic a, GInput (Rep a)) => AskUserInput a where
--  askUserInput = genericAskUserInput

instance AskUserInput a => GInput (K1 i a) where
  gAskUserInput = fmap K1 <$> askUserInput

-- this function repeatedly prompts the user for input until a valid value is entered
safeInput :: (Read a) => IO a
safeInput = do
  input <- getLine
  case readMaybe input of
    Just x -> return x
    Nothing -> putStrLn "Invalid input, please try again." >> safeInput

safeAskUserInput :: (AskUserInput a) => IO a
safeAskUserInput = do
  input <- askUserInput
  case input of
    Just x -> return x
    Nothing -> putStrLn "Invalid input, please try again." >> safeAskUserInput

