module Records where

data Person = Person
  { name :: String,
    sons :: [Person]
  }
  deriving (Show)

father :: Person
father = Person "Mariano" [son, daugther]

son :: Person
son = Person "Lautaro" [Person "Nieto 1" [], Person "Nieto 2" []]

daugther :: Person
daugther = Person "Lucia" [Person "Nieto 3" [], Person "Nieto 4" []]

grandchild :: Person -> [[Person]]
grandchild = (sons <$>) . sons

main :: IO ()
main = do
  let example = maybe [] grandchild (Just father)
  print example
