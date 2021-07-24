{-# OPTIONS_GHC -Wall #-}

module Warehouse where

{-
1. Identifiers should have reasonable names hinting and their purpose or function.
2. Code should be reasonably commented:
  i. Module files should have a brief outline summarising their contents.
  ii. Any non-trivial function should have its purpose explained and arguments listed, including their semantics.
  iii. you can use white space (tabulators, empty lines, etc.) that should be conducive to reading the code.

3. All codes in the solutions must be your own and not from libraries. Unless otherwise specified, you can use the Full standard prelude, but not any external modules, libraries, packages, or similar. Unless otherwise. mentioned, all modules imported in my code must be your own.

4. You can develop the code in whatever environment you like. Therefore, it is a significant requirement that your code executes correctly on a clean WinGHCi

5. Some parts of the criteria stipulate strict code structure (module elements, function arguments, class members, etc.) and identifier values (e.g., names for files, folders, functions, classes, class members, etc.). Meeting these requirements to the letterforms is a significant part of the completion of the app/code.

This is the Warehousing program brief:

Write a text-based transaction program for recording stock levels in a warehouse. A transaction is either an import, and export, or wastage (e.g., when a stock falls off a shelf and breaks) of items, and consists of the item name, item type, number of items, and, if applicable, the Importer/exporter name. Item type can be freely chosen by whoever uses the application.

The program should have the following features:
- Users will record ingoing and outgoing deliveries (imports and exports) as well as wastage.
- Users will further be able to list all transactions and all transaction data, optionally filtered by transaction-type (import, export, wastage) or item type, and/or sorted by item numbers, or importer/exporter name.
- Users will also be able to search transactions by importer/exporter name, item type, or item name.
- Last but not least, users will be able to query the total current stock.

The program should have explanatory text as necessary and handle input errors etc. gracefully. Implement application features in a command processing style. For example, incoming and outgoing deliveries might be recorded via commands such as:

`import 100 "iPhone" "electronics" "Apple, Inc."`
for recording the incoming delivery of 100 iPhones from Apple,

`withdraw 50 "iPhone" "Supersale Electronics Ltd."`
for logging the outgoing delivery of 50 iPhones to Supersale Electronics Ltd.,

search "iPhone"
for listing all transactions of iPhones,

list
to see all transactions, or

`list filter "wastage+electronics"`
for listing all wastage of electronics items.

-}

import Data.Char (toUpper)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

data Item = Item
  { itemName :: String,
    itemType :: String
  }
  deriving (Show, Eq, Ord)

data Transaction
  = ImportTransaction String Int Item
  | ExportTransaction String Int Item
  | WastageTransaction Int Item
  deriving (Show, Eq)

transactionItem :: Transaction -> Item
transactionItem (ImportTransaction _ _ item) = item
transactionItem (ExportTransaction _ _ item) = item
transactionItem (WastageTransaction _ item) = item

newtype Database = Database [Transaction]
  deriving (Show)

data Command
  = ImportCommand Int String String String
  | ExportCommand Int String String String
  | WastageCommand Int String String
  | ShowStockCommand
  | SearchCommand String
  | ListCommand (Transaction -> Bool)
  | HelpCommand
  | ExitCommand

emptyDatabase :: Database
emptyDatabase = Database []

recordTransaction :: Transaction -> Database -> Database
recordTransaction t (Database db) = Database $ t : db

queryTransactions :: (Transaction -> Bool) -> Database -> [Transaction]
queryTransactions p (Database db) = filter p db

prettyTransactions :: [Transaction] -> String
prettyTransactions ts = unlines $ map pretty ts
  where
    pretty (ImportTransaction importer count item) = "[IMPORT] '" ++ itemName item ++ "' x " ++ show count ++ " - '" ++ itemType item ++ "' - '" ++ importer ++ "'"
    pretty (ExportTransaction exporter count item) = "[EXPORT] '" ++ itemName item ++ "' x " ++ show count ++ " - '" ++ itemType item ++ "' - '" ++ exporter ++ "'"
    pretty (WastageTransaction count item) = "[WASTAGE] '" ++ itemName item ++ "' x " ++ show count ++ " - '" ++ itemType item ++ "'"

getStock :: Database -> Map Item Int
getStock (Database db) = foldr mergeStock Map.empty db
  where
    mergeStock (ImportTransaction _ count item) = Map.insertWith (+) item count
    mergeStock (ExportTransaction _ count item) = Map.insertWith (+) item (negate count)
    mergeStock (WastageTransaction count item) = Map.insertWith (+) item (negate count)

prettyStock :: Map Item Int -> String
prettyStock stock
  | Map.null stock = "No stock.\n"
  | otherwise = unlines $ map prettyLine (Map.toList stock)
  where
    prettyLine (item, count) = prettyItem item ++ " -> " ++ show count
    prettyItem (Item name type') = "[" ++ type' ++ "] " ++ name

parseCommand :: String -> Maybe Command
parseCommand input = do
  tokens <- tokenize input
  case tokens of
    -- Import command
    ["import", count, name, type', importer] -> do
      count' <- readMaybe count
      return $ ImportCommand count' name type' importer
    -- Export command
    ["export", count, name, type', exporter] -> do
      count' <- readMaybe count
      return $ ExportCommand count' name type' exporter
    -- Wastage command
    ["wastage", count, name, type'] -> do
      count' <- readMaybe count
      return $ WastageCommand count' name type'
    -- Search command
    ["search", query] -> return $ SearchCommand query
    -- List command
    ["list"] -> return $ ListCommand (const True)
    ["list", "filter", query] -> do
      filter' <- parseFilterQuery query
      return $ ListCommand filter'
    -- Stock command
    ["stock"] -> return ShowStockCommand
    -- Help command
    ["help"] -> return HelpCommand
    -- Exit command
    ["exit"] -> return ExitCommand
    _ -> Nothing

parseFilterQuery :: String -> Maybe (Transaction -> Bool)
parseFilterQuery query = do
  arg1 : arg2 <- pure $ splitOn '+' query
  transactionTypeFilter <- parseTransactionTypeFilter arg1
  itemTypeFilter <- parseItemTypeFilter arg2
  return $ \t -> transactionTypeFilter t && itemTypeFilter t
  where
    parseTransactionTypeFilter "import" = return $ \t -> case t of ImportTransaction {} -> True; _ -> False
    parseTransactionTypeFilter "export" = return $ \t -> case t of ExportTransaction {} -> True; _ -> False
    parseTransactionTypeFilter "wastage" = return $ \t -> case t of WastageTransaction {} -> True; _ -> False
    parseTransactionTypeFilter _ = Nothing

    parseItemTypeFilter [] = return $ const True
    parseItemTypeFilter [itemType'] = return $ \t -> itemType (transactionItem t) == itemType'
    parseItemTypeFilter _ = Nothing

evalCommand :: Database -> Command -> IO Database
evalCommand db command = case command of
  ImportCommand count name type' importer' -> do
    let item = Item (map toUpper name) (map toUpper type')
    let transaction = ImportTransaction (map toUpper importer') count item
    let newDB = recordTransaction transaction db
    return newDB
  ExportCommand count name type' exporter' -> do
    let item = Item (map toUpper name) (map toUpper type')
    let transaction = ExportTransaction (map toUpper exporter') count item
    let newDB = recordTransaction transaction db
    return newDB
  WastageCommand count name type' -> do
    let item = Item (map toUpper name) (map toUpper type')
    let transaction = WastageTransaction count item
    let newDB = recordTransaction transaction db
    return newDB
  SearchCommand query -> do
    let query' = map toUpper query
    let transactions = queryTransactions (\t -> itemName (transactionItem t) == query') db
    putStr $ prettyTransactions transactions
    return db
  ListCommand filter' -> do
    let transactions = queryTransactions filter' db
    putStr $ prettyTransactions transactions
    return db
  ShowStockCommand -> do
    let stock = getStock db
    putStr $ prettyStock stock
    return db
  HelpCommand -> do
    putStrLn helpMessage
    return db
  ExitCommand -> do
    putStrLn "Goodbye."
    exitSuccess

helpMessage :: String
helpMessage =
  unlines
    [ "Available commands:",
      "  Record an import",
      "    $> import <count> <item-name> <item-type> <importer>",
      "  Record an export",
      "    $> export <count> <item-name> <item-type> <exporter>",
      " Record a wastage",
      "    $> wastage <count> <item-name> <item-type>",
      "  Search all transactions with some item",
      "    $> search <item-name>",
      "  List transactions with an optional filter",
      "    $> list [filter <filter-query>]",
      "      where",
      "    <filter-query> = <operation>+[<item-type>]",
      "  Exit the application",
      "    $> exit",
      "Examples: ",
      "  $> import 100 \"iPhone\" \"electronics\" \"Apple, Inc.\"",
      "  for recording the incoming delivery of 100 iPhones from Apple",
      "",
      "  $> export 50 \"iPhone\" \"Supersale Electronics Ltd.\"",
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

main :: IO ()
main = do
  putStr startMessage
  loop emptyDatabase
  where
    loop db = do
      input <- prompt "$ "
      case parseCommand input of
        Nothing -> do
          putStrLn "[ERROR] Invalid command"
          loop db
        Just command -> do
          newDB <- evalCommand db command
          loop newDB

-- *** Utility functions

space :: Int -> String -> String
space num content = (replicate num ' ') ++ content

-- Read a line from 'stdin' showing some prompt at the start of the line
-- Deals with console buffering, preventing incorrect behaviour
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

-- Split a string into tokens
-- Tokens are separated by at least 1 whitespace
-- If needed, whitespace can be 'escaped' by surrounding a token in double quotes
-- Fails with 'Nothing' if a douuble quote is not matched
--
-- NOTE: Currently, double quotes can't be escaped!
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

-- Split a list on items that match a predicate
-- __Examples__
-- >>> split (== '+') "hello+world"
-- ["hello","world"]
-- >>> split even [1..10]
-- [[1],[3],[5],[7],[9]]
split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case dropWhile p xs of
  [] -> []
  s' -> let (x, s'') = break p s' in x : split p s''

-- Utility for splitting on equality of particular item
-- Defined as: 'splitOn c == split (== c)'
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn c = split (== c)
