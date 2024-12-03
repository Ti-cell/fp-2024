{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use readTVarIO" #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Statements,
    genStatements
    ) where

import Control.Concurrent
import Control.Concurrent.STM
import qualified Lib2
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen
import Lib2 (Query)


data StorageOp = Save String (Chan ()) | Load (Chan String)

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
    op <- readChan chan
    case op of
        Save content ackChan -> do
            writeFile "storage.txt" content
            writeChan ackChan ()
        Load respChan -> do
            content <- readFile "storage.txt"
            writeChan respChan content
    storageOpLoop chan

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses Statements.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input =
  let
    linesInput = lines input
    parsedQueries = traverse Lib2.parseQuery linesInput
  in
    case parsedQueries of
      Left err -> Left err
      Right [singleQuery] -> Right (Single singleQuery, "")
      Right queries -> Right (Batch queries, "") 

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input
  | input == "load" = Right (LoadCommand, "")
  | input == "save" = Right (SaveCommand, "")
  | otherwise = case parseStatements input of
      Left err -> Left err
      Right (statements, rest) -> Right (StatementCommand statements, rest)

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState (Lib2.State active completed removed) = 
    let
        activeQueries = map createAddQuery active
        completedQueries = concatMap createCompleteQuery completed
        removedQueries = concatMap createCancelQuery removed
        allQueries = reverse (activeQueries ++ completedQueries ++ removedQueries)
    in if length allQueries == 1
        then Single (head allQueries)
        else Batch allQueries 

createAddQuery :: Lib2.Order -> Lib2.Query
createAddQuery (Lib2.Order _ name number) = Lib2.Add name number 

createCompleteQuery :: Lib2.Order -> [Lib2.Query]
createCompleteQuery (Lib2.Order _ name number) = [Lib2.Complete 0, Lib2.Add name number]

createCancelQuery :: Lib2.Order -> [Lib2.Query]
createCancelQuery (Lib2.Order _ name number) = [Lib2.Cancel 0, Lib2.Add name number]

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Batch queries) = unlines (map renderQuery queries)
renderStatements (Single query)  = renderQuery query

renderQuery :: Lib2.Query -> String
renderQuery Lib2.List                = "list"
renderQuery (Lib2.Add name quantity) = "add " ++ name ++ " " ++ show quantity
renderQuery (Lib2.Cancel taskIndex)         = "cancel " ++ show taskIndex
renderQuery (Lib2.Complete taskIndex)       = "complete " ++ show taskIndex


genStatements :: Gen Statements
genStatements = oneof
    [ Single <$> genQuery
    , Batch <$> suchThat (listOf genQuery) (\lst -> length lst >= 2)
    ]

genQuery :: Gen Lib2.Query
genQuery = oneof
    [ return Lib2.List
    , genAdd
    , genComplete
    , genCancel
    ]
 
genAdd :: Gen Lib2.Query
genAdd = Lib2.Add <$> genName <*> genNumber

genComplete :: Gen Lib2.Query
genComplete = Lib2.Complete <$> genNumber

genCancel :: Gen Lib2.Query
genCancel = Lib2.Cancel <$> genNumber

genName :: Gen String
genName = oneof
    [ return "Latte"
    , return "Expresso"
    , return "Milk"
    , return "Cookie"
    , return "Bagel"
    , return "Croissant"
    ]

genNumber :: Gen Int
genNumber = arbitrary `suchThat` (> -1)

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String))
stateTransition stateVar command ioChan = case command of
    StatementCommand statements -> 
        atomically $ do
            currentState <- readTVar stateVar
            case applyStatements currentState statements of
                Left err -> return $ Left err
                Right (output, newState) -> do
                    writeTVar stateVar newState
                    return $ Right output

    SaveCommand -> do
        ackChan <- newChan
        currentState <- atomically $ readTVar stateVar
        let serializedState = renderStatements (marshallState currentState)
        writeChan ioChan (Save serializedState ackChan)
        readChan ackChan
        return $ Right (Just "State saved.")

    LoadCommand -> do
        respChan <- newChan
        writeChan ioChan (Load respChan)
        serializedState <- readChan respChan 
        case parseStatements serializedState of
            Left err -> return $ Left err
            Right (statements, _) ->
                atomically $ do
                    writeTVar stateVar Lib2.emptyState
                    currentState <- readTVar stateVar
                    case applyStatements currentState statements of
                        Left err -> return $ Left err
                        Right (_, newState) -> do
                            writeTVar stateVar newState
                            return $ Right (Just "State loaded.")


applyStatements :: Lib2.State -> Statements -> Either String (Maybe String, Lib2.State)
applyStatements state (Single query) =
    case Lib2.stateTransition state query of
        Left err -> Left err
        Right (output, newState) -> Right (output, newState)
applyStatements state (Batch queries) =
    foldl applyQuery (Right (Nothing, state)) queries
  where
    applyQuery :: Either String (Maybe String, Lib2.State) -> Lib2.Query -> Either String (Maybe String, Lib2.State)
    applyQuery (Left err) _ = Left err
    applyQuery (Right (_, currentState)) query =
        case Lib2.stateTransition currentState query of
            Left err -> Left err
            Right (output, newState) -> Right (output, newState)