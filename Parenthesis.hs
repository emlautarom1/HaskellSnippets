module Parenthesis (isBalanced) where

-- | Checks if a parenthesis expresion is balanced
-- The input must contain only '(', ')' and '*'.
-- '*' can act as '(', ')' or a "ghost character" (no meaning).
-- >>> isBalanced "(())"
-- True
-- >>> isBalanced "()("
-- False
-- >>> isBalanced "((()))()(())(*()()())**(())()()()()((*()*))((*()*)"
-- True
isBalanced :: [Char] -> Bool
isBalanced str = go str []
  where
    go (c : cs) stack = case c of
      '(' -> go cs (c : stack) -- Push a '(' into the Stack
      ')' -> case stack of
        '(' : stack' -> go cs stack' -- Keep going only if we can pop a '(' from the Stack
        _ -> False
      '*' ->
        -- Special case for the "polymorphic" character
        go ('(' : cs) stack -- Assume is a '(' and keep going
          || go (')' : cs) stack -- Assume is a ')' and keep going
          || go cs stack -- Assume is a "ghost token" - ignore it - and keep going
      _ -> False -- Invalid char
    go [] stack = null stack -- If no characters left, check that the Stack is empty
