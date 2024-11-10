{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

import qualified Data.Char

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.

--this part i currently have add string, but i need it to become add order or add string int
data Query = List | Add String Int | Cancel Int | Complete Int

-- | The instances are needed basically for tests
instance Eq Query where
  (==) q1 q2 = show q1 == show q2

instance Show Query where
    show List = "List"
    show (Add taskContent orderNum) = "Add " ++ taskContent ++ " " ++ show orderNum
    show (Cancel taskIndex) = "Cancel" ++ show taskIndex
    show (Complete taskIndex) = "Complete " ++ show taskIndex

-- | Parses user's input.
-- The function must have tests.
type Parser a = String -> Either String (a, String)


andParser :: Parser a -> Parser b -> Parser (a, b)
andParser firstParser secondParser content =
    case firstParser content of
        Left err1 -> Left err1
        Right (result1, rest1) ->
            case secondParser rest1 of
                Left err2 -> Left err2
                Right (result2, rest2) -> Right ((result1, result2), rest2)

ifThenParser :: Parser a -> Parser b -> Parser b
ifThenParser firstParser secondParser content =
    case andParser firstParser secondParser content of
        Left err -> Left err
        Right ((_, value), rest) -> Right (value, rest)

prepareFor :: Parser a -> Parser b -> Parser a
prepareFor mainParser preparationParser content =
    case preparationParser content of
        Left _ -> mainParser content
        Right (_, preparedContent) -> mainParser preparedContent

listParser :: [Parser a] -> Parser a
listParser [] _ = Left "all parsers failed"
listParser (headParser : parsersLeft) content =
    case headParser content of
        Right successfulParseResult -> Right successfulParseResult
        Left _ -> listParser parsersLeft content


charParser :: (Char -> Bool) -> Parser Char
charParser _ "" = Left "empty string given"
charParser validationFn (h:t)
    | validationFn h = Right (h, t)
    | otherwise = Left "invalid char"

stringParser :: Parser Char -> Parser Char -> Parser String
stringParser _ _ "" = Left "empty string given"
stringParser validationParser endParser content =
    case validationParser content of
        Left e1 -> Left e1
        Right (c, "") -> Right ([c], "")
        Right (c, rest) ->
            case stringParser validationParser endParser rest of
                Left e2 ->
                    case endParser rest of
                        Left _ -> Left e2
                        Right (_, _) -> Right ([c], rest)
                Right (parsingsSoFar, restFinal) -> Right (c:parsingsSoFar, restFinal)


parseWhitespaceChar :: Parser Char
parseWhitespaceChar = charParser Data.Char.isSpace

parseNotWhitespaceChar :: Parser Char
parseNotWhitespaceChar = charParser (not . Data.Char.isSpace)

parseDigit :: Parser Char
parseDigit = charParser Data.Char.isDigit


parseWhitespace :: Parser String
parseWhitespace = stringParser parseWhitespaceChar parseNotWhitespaceChar

parseWord :: Parser String
parseWord = stringParser parseNotWhitespaceChar parseWhitespaceChar

parseInt :: Parser Int
parseInt content =
    case stringParser parseDigit parseWhitespaceChar content of
        Left stringParserError -> Left stringParserError
        Right (intString, rest) -> Right (strToInt intString 0, rest)
            where
                strToInt "" integer = integer
                strToInt (h:t) integer = strToInt t (10 * integer + Data.Char.digitToInt h)

parseAddArgs :: Parser Query
parseAddArgs content = 
    case commandWordParser "add" content of
        Left err -> Left err
        Right (_, rest) ->
            case commandArgParser parseWord rest of  
                Left err -> Left err
                Right (orderString, rest1) ->
                    case commandArgParser parseInt rest1 of 
                        Left err -> Left err
                        Right (orderInt, rest2) -> Right (Add orderString orderInt, rest2)


commandWordParser :: String -> Parser String
commandWordParser commandName content = 
    case prepareFor parseWord parseWhitespace content of
        Left _ -> Left "failed to parse command name"
        Right (nameEntered, rest) -> if nameEntered == commandName then Right (commandName, rest) else Left ("unknown command: " ++ nameEntered)

commandArgParser :: Parser a -> Parser a
commandArgParser parseArgs = prepareFor parseArgs parseWhitespace

commandParser :: String -> Parser Query -> Parser Query
commandParser commandName parseArgs = ifThenParser (commandWordParser commandName) (commandArgParser parseArgs)

parseIntArgs :: (Int -> Query) -> Parser Query
parseIntArgs convertIntToQuery content =
    case parseInt content of
        Left _ -> Left "failed to parse an int arg"
        Right (intValue, rest) -> Right (convertIntToQuery intValue, rest)

commandParsers :: [Parser Query]
commandParsers = [ 
        commandParser "list" (\leftover -> Right (List, leftover)),
        parseAddArgs,
        commandParser "cancel" (parseIntArgs Cancel),
        commandParser "complete" (parseIntArgs Complete)
    ]


parseQuery :: String -> Either String Query
parseQuery queryContent =
    case listParser commandParsers queryContent of
        Left err -> Left err
        Right (query, rest) ->
            if all Data.Char.isSpace rest then
                Right query
            else
                Left "query contains too many args"
                

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data OrderStatus = Active | Removed | Completed deriving Show
data Order = Order OrderStatus String Int

instance Show Order where
    show (Order _ content orderNum) = content ++ " #" ++ show orderNum

type ActiveOrders = [Order]
type CompletedOrders = [Order]
type RemovedOrders = [Order]
data State = State ActiveOrders CompletedOrders RemovedOrders

numberOfOrders :: [Order] -> Int
numberOfOrders [] = 0
numberOfOrders (_:t) = 1 + numberOfOrders t

stateList :: State -> String
stateList (State activeOrders completedOrders removedOrders) =
    "[ACTIVE]:\n" ++
        listOrders activeOrders (0 :: Int) ++ "\n" ++
    "[COMPLETED]:\n" ++
        listOrders completedOrders (numberOfOrders activeOrders) ++ "\n" ++
    "[REMOVED]:\n" ++
        listOrders removedOrders (numberOfOrders activeOrders + numberOfOrders completedOrders)
    where
        listOrders [] _ = ""
        listOrders (task:t) index = "    " ++ show index ++ ") " ++ show task ++ "\n" ++ listOrders t (index + 1)

stateAdd :: State -> String -> Int -> Either String (Maybe String, State)
stateAdd (State activeOrders completedOrders removedOrders) newOrderContent orderNum = 
    Right (Just "Added order to the list", State (Order Active newOrderContent orderNum : activeOrders) completedOrders removedOrders)

removeNth :: [a] -> Int -> Maybe (a, [a])
removeNth [] _ = Nothing
removeNth (h:t) n
    | n > 0 = case removeNth t (n - 1) of
        Nothing -> Nothing
        Just (value, listAfter) -> Just (value, h:listAfter)
    | n == 0 = Just (h, t)
    | otherwise = Nothing

stateComplete :: State -> Int -> Either String (Maybe String, State)
stateComplete (State activeOrders completedOrders removedOrders) completeIndex =
    case removeNth activeOrders completeIndex of
        Nothing -> Left "Failed to add new task: index out of range"
        Just (completedOrder, activeAfter) -> Right (Just "The order has been completed", State activeAfter (completedOrder : completedOrders) removedOrders)

stateRemove :: State -> Int -> Either String (Maybe String, State)
stateRemove (State activeOrders completedOrders removedOrders) removeIndex =
    case removeNth activeOrders removeIndex of
        Nothing -> Left "Failed to remove order: index out of range"
        Just (removedOrder, activeAfter) -> Right (Just "The order has been canceled", State activeAfter completedOrders (removedOrder : removedOrders))


-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State [] [] []

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition currentState List = Right (Just (stateList currentState), currentState)
stateTransition currentState (Add orderContent orderNum) = stateAdd currentState orderContent orderNum
stateTransition currentState (Complete orderIndex) = stateComplete currentState orderIndex
stateTransition currentState (Cancel orderIndex) = stateRemove currentState orderIndex