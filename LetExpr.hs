myf :: Bool -> Int
myf bool =
  let var1
        | bool = 5
        | otherwise = 6
      var2
        | bool = 10
        | otherwise = 11
   in var1 + (let var2' = 1 + var2 in var2')