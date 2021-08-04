{-# OPTIONS_GHC -Wall #-}

module Warehouse where

import Data.Char (toUpper)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- | An Item in the Warehouse
data Item = Item
  { -- | The name of the Item
    itemName :: String,
    -- | The type of the Item
    itemType :: String
  }
  deriving (Show, Eq, Ord)

-- | A Transaction in the system.
data Transaction
  = -- | A Transaction is either an 'import' with some 'importer', a number of 'item's and an 'item'
    ImportTransaction String Int Item
  | -- | Or a 'withdraw' with some 'exporter', a number of 'item's and an 'item'
    WithdrawTransaction String Int Item
  | -- | Or a 'wastage', with a number of 'item's and an 'item'
    WastageTransaction Int Item
  deriving (Show, Eq)

transactionItem :: Transaction -> Item
transactionItem (ImportTransaction _ _ item) = item
transactionItem (WithdrawTransaction _ _ item) = item
transactionItem (WastageTransaction _ item) = item

-- | The Database of the system.
newtype Database = Database [Transaction]
  deriving (Show)

-- | A Command in the system, allowing the user to ask for some behaviour
data Command
  = -- | Register an 'import' with the appropiate fields (importer, number of items and an item - with a name and a type)
    ImportCommand Int String String String
  | -- | Register a 'withdraw' with the appropiate fields (...)
    WithdrawCommand Int String String String
  | -- | Register a 'wastage' with the appropiate fields (...)
    WastageCommand Int String String
  | -- | Query the total stock
    ShowStockCommand
  | -- | Search for all transactions with some 'item' (by name)
    SearchCommand String
  | -- | List all transactions with some optional filter
    ListCommand (Maybe (Transaction -> Bool))
  | -- | Ask for help
    HelpCommand
  | -- | Exit the application
    ExitCommand

-- | An empty Database with no 'Transaction's recorded.
emptyDatabase :: Database
emptyDatabase = Database []

-- | Record a 'Transaction' in a 'Database'
recordTransaction :: Transaction -> Database -> Database
recordTransaction t (Database ts) = Database (t : ts)

-- | Get all 'Transaction's that match a given predicate
queryTransactions :: (Transaction -> Bool) -> Database -> [Transaction]
queryTransactions predicate (Database ts) = filter predicate ts

-- | Shows a list of transactions in a human redeable format
prettyTransactions :: [Transaction] -> String
prettyTransactions ts = unlines $ map prettyfy ts
  where
    prettyfy (ImportTransaction importer count item) = "[IMPORT] '" ++ itemName item ++ "' x " ++ show count ++ " - '" ++ itemType item ++ "' - '" ++ importer ++ "'"
    prettyfy (WithdrawTransaction exporter count item) = "[WITHDRAW] '" ++ itemName item ++ "' x " ++ show count ++ " - '" ++ itemType item ++ "' - '" ++ exporter ++ "'"
    prettyfy (WastageTransaction count item) = "[WASTAGE] '" ++ itemName item ++ "' x " ++ show count ++ " - '" ++ itemType item ++ "'"

-- | The Stock is a Map of 'Item's to 'Int's
type Stock = Map Item Int

-- | Calculate the stock in the Warehouse.
--
-- The result is a Map containing Items as keys, and the number of those items as values.
getStock :: Database -> Stock
-- The idea is to traverse all transactions and build a Map, updating the stock of each item
-- according to the transaction.
-- For example, an 'import' will increase the ammount of items, where a 'withdraw' will decrease it.
getStock (Database transactions) = foldr mergeStock Map.empty transactions
  where
    mergeStock (ImportTransaction _ count item) = Map.insertWith (+) item count
    -- If an item is present, some operations, like 'withdraw' will decrease the ammount of items. If the item is not present, we want to store the ammount as a negative number, so we end up 'adding the negation' of the ammount
    mergeStock (WithdrawTransaction _ count item) = Map.insertWith (+) item (negate count)
    mergeStock (WastageTransaction count item) = Map.insertWith (+) item (negate count)

-- | Format a 'Stock' into a 'String'
prettyStock :: Stock -> String
prettyStock stock
  | Map.null stock = "No stock.\n" -- An empty stock
  | otherwise = unlines $ map prettyLine (Map.toList stock) -- At least one Item in stock
  where
    prettyLine (item, count) = prettyItem item ++ " -> " ++ show count
    prettyItem (Item name type') = "[" ++ type' ++ "] " ++ name

-- | Parse a 'Command'.
--
-- Fails with 'Nothing' if the input string is not a valid 'Command'.
parseCommand :: String -> Maybe Command
parseCommand input = do
  tokens <- tokenize input -- We split the input into tokens
  case tokens of
    -- Import command
    ["IMPORT", count, name, type', importer] -> do
      count' <- readMaybe count -- Fail with 'Nothing' if 'count' is not an 'Int'
      return $ ImportCommand count' name type' importer
    -- Withdraw command
    ["WITHDRAW", count, name, type', exporter] -> do
      count' <- readMaybe count -- Idem 'Import'
      return $ WithdrawCommand count' name type' exporter
    -- Wastage command
    ["WASTAGE", count, name, type'] -> do
      count' <- readMaybe count -- Idem 'Import'
      return $ WastageCommand count' name type'
    -- Search command
    ["SEARCH", query] -> return $ SearchCommand query
    -- List command
    ["LIST"] -> return $ ListCommand Nothing -- No optional filter
    ["LIST", "FILTER", query] -> do
      filter' <- parseFilterQuery query
      return $ ListCommand (Just filter')
    -- Stock command
    ["STOCK"] -> return ShowStockCommand
    -- Help command
    ["HELP"] -> return HelpCommand
    -- Exit command
    ["EXIT"] -> return ExitCommand
    _ -> Nothing -- Any other input is an invalid command

-- | Parse a 'ListCommand' optional filter, returning a predicate on a 'Transaction'
--
-- Fails with 'Nothing' if the filter query can't be parsed
--
-- Valid queries are as follows:
-- "<operation-type>[+<item-type>]""
--   where
--    <operation-type> = "import" | "withdraw" | "wastage"
--    <item-type>      = any String
-- Values inside '[]' are optional
parseFilterQuery :: String -> Maybe (Transaction -> Bool)
parseFilterQuery query = do
  arg1 : arg2 <- pure $ splitOn '+' query
  transactionTypeFilter <- parseTransactionTypeFilter arg1
  itemTypeFilter <- parseItemTypeFilter arg2
  return $ \t -> transactionTypeFilter t && itemTypeFilter t
  where
    -- We create predicates that will suceed when the type of the transaction matches
    -- For example, if we provide "import", we create a function that will return 'True' on Imports only
    parseTransactionTypeFilter "IMPORT" =
      return $ \t -> case t of ImportTransaction {} -> True; _ -> False
    parseTransactionTypeFilter "WITHDRAW" =
      return $ \t -> case t of WithdrawTransaction {} -> True; _ -> False
    parseTransactionTypeFilter "WASTAGE" =
      return $ \t -> case t of WastageTransaction {} -> True; _ -> False
    parseTransactionTypeFilter _ = Nothing

    -- We try to parse the second argument as an optional 'item type' filter
    parseItemTypeFilter [] = return $ const True -- Without a filter, we create a predicate that will always suceed, no matter the type of the item.
    parseItemTypeFilter [itemType'] = return $ \t -> itemType (transactionItem t) == itemType' -- If we have a filter, we create a predicate that given a transaction will return True only if the type of the item involved matches the filter.
    parseItemTypeFilter _ = Nothing -- All other formats are invalid, and we fail with 'Nothing'

-- | Evaluate a 'Command', execute any side-effect required and return a new 'Database'
evalCommand :: Database -> Command -> IO Database
evalCommand db command = case command of
  ImportCommand count name type' importer -> do
    let item = Item name type'
    let transaction = ImportTransaction importer count item
    -- We 'create' a new Database - we don't modify the previous one!
    let newDB = recordTransaction transaction db
    return newDB
  WithdrawCommand count name type' exporter -> do
    let item = Item name type'
    let transaction = WithdrawTransaction exporter count item
    let newDB = recordTransaction transaction db
    return newDB
  WastageCommand count name type' -> do
    let item = Item name type'
    let transaction = WastageTransaction count item
    let newDB = recordTransaction transaction db
    return newDB
  SearchCommand query -> do
    -- Here we'll need to perform side-effects in order to display the results of the search
    let transactions = queryTransactions (\t -> itemName (transactionItem t) == query) db
    putStr $ prettyTransactions transactions
    -- We return the old Database since it was not modified
    return db
  ListCommand filter' -> do
    let filter'' = case filter' of
          Just p -> p -- If there's a filter, use it
          Nothing -> const True -- Otherwise, use as filter a predicate that always returns 'True'
    let transactions = queryTransactions filter'' db
    putStr $ prettyTransactions transactions
    return db
  ShowStockCommand -> do
    let stock = getStock db
    putStr $ prettyStock stock
    return db
  HelpCommand -> do
    putStr helpMessage
    return db
  ExitCommand -> do
    putStrLn "Goodbye!"
    -- 'exitSuccess' will quit the application with 'status 0'
    exitSuccess

main :: IO ()
main = do
  putStr startMessage
  loop emptyDatabase
  where
    loop db = do
      -- All inputs are converted to upper-case
      input <- map toUpper <$> prompt "$> "
      case parseCommand input of
        Nothing -> do
          putStrLn "[ERROR] Invalid command. See valid commands by typing 'help'"
          loop db
        Just command -> do
          newDB <- evalCommand db command
          loop newDB

-- *** Info messages

helpMessage :: String
helpMessage =
  unlines
    [ "Available commands:",
      "  Record an import",
      "    $> import <count> <item-name> <item-type> <importer>",
      "  Record a withdraw",
      "    $> withdraw <count> <item-name> <item-type> <exporter>",
      " Record a wastage",
      "    $> wastage <count> <item-name> <item-type>",
      "  Search all transactions with some item",
      "    $> search <item-name>",
      "  List transactions with an optional filter",
      "    $> list [filter <filter-query>]",
      "      where",
      "    <filter-query> = <operation>[+<item-type>]",
      "  Exit the application",
      "    $> exit",
      "Examples: ",
      "  $> import 100 \"iPhone\" \"electronics\" \"Apple, Inc.\"",
      "  for recording the incoming delivery of 100 iPhones from Apple",
      "",
      "  $> withdraw 50 \"iPhone\" \"Supersale Electronics Ltd.\"",
      "  for logging the outgoing delivery of 50 iPhones to Supersale Electronics Ltd.",
      "",
      "  $> search \"iPhone\"",
      "  for listing all transactions of iPhones",
      "",
      "  $> list",
      "  to see all transactions",
      "",
      "  $> list filter \"wastage+electronics\"",
      "  for listing all wastage of electronics items."
    ]

startMessage :: String
startMessage =
  unlines
    [ "WAREHOUSE SYSTEM",
      "Type 'help' to see available commands",
      ""
    ]

-- *** Utility functions

-- Read a line from 'stdin' showing some prompt at the start of the line
-- Deals with console buffering, preventing incorrect behaviour
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

-- | Split a string into tokens.
--
-- Tokens are separated by at least 1 whitespace.
-- If needed, whitespace can be escaped by surrounding a token in double quotes (@"@)
--
-- Fails with 'Nothing' if a douuble quote is not matched
--
-- __Note__: Currently, double quotes can't be escaped!
--
-- __Examples__
-- >>> tokenize "hello world"
-- Just ["hello","world"]
-- >>> tokenize "search iPhone"
-- Just ["search","iPhone"]
-- >>> tokenize "list   filter \"wastage+electronics\" "
-- Just ["list","filter","wastage+electronics"]
-- >>> tokenize "will fail; \"dobule quotes not matched"
-- Nothing
tokenize :: String -> Maybe [String]
tokenize s = case dropWhile isSpace s of
  "" -> return []
  '\"' : s' -> do
    (token, '\"' : s'') <- pure $ break isQuote s'
    tokens <- tokenize s''
    return $ token : tokens
  s' -> do
    (token, s'') <- pure $ break isSpace s'
    tokens <- tokenize s''
    return $ token : tokens
  where
    isSpace = (== ' ')
    isQuote = (== '\"')

-- | Split a list on items that match a predicate
--
-- __Examples__
-- >>> split (== '+') "hello+world"
-- ["hello","world"]
-- >>> split even [1..10]
-- [[1],[3],[5],[7],[9]]
split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case dropWhile p xs of
  [] -> []
  s' -> let (x, s'') = break p s' in x : split p s''

-- | Utility for splitting on equality of particular item
--
-- Defined as: @splitOn c == split (== c)@
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c = split (== c)
