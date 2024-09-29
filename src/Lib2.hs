{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query = Order

-- | The instances are needed basically for tests
instance Eq Query where
  (==) Order Order = True

instance Show Query where
  show :: Query -> String
  show Order = "Order"

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery "Order" = Right Order
parseQuery _ = Left "Some error message"

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = Empty | Busy | Full

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = Empty

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Too many orders"
