{-# LANGUAGE LambdaCase #-}

module Document where

import Data.List

data Element
  = Header Int Element
  | Link String Element
  | Text String
  | Newline

example :: [Element]
example =
  [ Header 1 $ Link "https://example.com" $ Text "Example"
  , Header 2 $ Text "Subsection"
  , Text "This is a paragraph."
  ]

html :: Element -> String
html = \case
  Header n e -> "<h" ++ show n ++ ">" ++ html e ++ "</h" ++ show n ++ ">"
  Link url e -> "<a href=\"" ++ url ++ "\">" ++ html e ++ "</a>"
  Text s -> s
  Newline -> "<br>"

markdown :: Element -> String
markdown = \case
  Header n e -> replicate n '#' ++ " " ++ markdown e
  Link url e -> "[" ++ markdown e ++ "](" ++ url ++ ")"
  Text s -> s
  Newline -> "\n"

render :: (Element -> String) -> [Element] -> String
render renderer = concatMap renderer . intersperse Newline

-- >>> render markdown $ example
-- "# [Example](https://example.com)\n## Subsection\nThis is a paragraph."
