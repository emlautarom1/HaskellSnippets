{-# OPTIONS_GHC -Wall #-}

-- | Property-based (possible infinite) sets, heavily inspired by 'https://www.youtube.com/watch?v=Y5rPHZaUakg'
-- I tried to mimic the interface of 'Data.Set' as closely as possible, but some operations
-- are impossible, given the nature of the sets (ex. 'elems' can't be defined).
module PropertySet
  ( -- * Set type
    Set,
    Predicate,

    -- * Query
    member,
    notMember,

    -- * Construction
    empty,
    insert,
    insertAll,
    delete,
    deleteAll,
    singleton,
    fromList,
    fromPredicate,

    -- * Combine
    union,
    difference,
    intersection,

    -- * Operators
    (\\),
  )
where

-- | Alias for @a -> Bool@.
type Predicate a = a -> Bool

data Operation a
  = -- Insert all elements that satisfy a predicate
    Insert (Predicate a)
  | -- Delete all elements that satisfy a predicate
    Delete (Predicate a)

-- | A set of values @a@.
data Set a
  = -- A set is either Empty
    Empty
  | -- Or another set with an operation applied over
    (Set a) `WithOperation` (Operation a)

instance Semigroup (Set a) where
  (<>) = union

instance Monoid (Set a) where
  mempty = empty

-- | The empty set.
empty :: Set a
empty = Empty

-- | Is a given element in the set?
member :: a -> Set a -> Bool
member _ Empty = False
member a (set `WithOperation` op) = case op of
  Insert predicate -> a `member` set || predicate a
  Delete predicate -> a `member` set && (not . predicate) a

-- | Is a given element __not__ in the set?
notMember :: a -> Set a -> Bool
notMember a s = not $ member a s

-- | Insert a single element in a set.
-- Insertion of an element already on the set is a no-op.
insert :: Eq a => a -> Set a -> Set a
insert a s
  | notMember a s = s `WithOperation` Insert (== a)
  | otherwise = s

-- | Insert all the elements that satisfy a @Predicate@ in a set.
insertAll :: Predicate a -> Set a -> Set a
insertAll p s = s `WithOperation` Insert p

-- | Delete a single element from a set.
-- Deletion of an element not in the set is a no-op.
delete :: Eq a => a -> Set a -> Set a
delete a s
  | member a s = s `WithOperation` Delete (== a)
  | otherwise = s

-- | Delete all the elements that satisfy a given @Predicate@ in a set.
deleteAll :: Predicate a -> Set a -> Set a
deleteAll p s = s `WithOperation` Delete p

-- | Create a set with a single element.
singleton :: Eq a => a -> Set a
singleton a = insertAll (== a) empty

-- | Create a set with all the elements from a list.
fromList :: Eq a => [a] -> Set a
fromList l = insertAll (`elem` l) empty

-- | Create a set with all the elements that satisfy a given @Predicate@.
fromPredicate :: Predicate a -> Set a
fromPredicate p = insertAll p empty

-- | Union of two sets.
union :: Set a -> Set a -> Set a
union s1 s2 = s1 `WithOperation` Insert (`member` s2)

-- | Difference of two sets.
difference :: Set a -> Set a -> Set a
difference s1 s2 = s1 `WithOperation` Insert (`notMember` s2)

-- | Intersection of two sets.
intersection :: Set a -> Set a -> Set a
intersection s1 s2 = empty `WithOperation` Insert (\x -> x `member` s1 && x `member` s2)

-- | See 'difference'
(\\) :: Set a -> Set a -> Set a
(\\) = difference

-- | Examples
-- __Simple examples__
-- >>> member 5 $ empty
-- False

-- >>> member 5 $ insert 5 $ empty
-- True

-- >>> member 5 $ delete 5 $ insert 5 $ empty
-- False

-- __Examples with properties__
-- >>> member 10 $ fromPredicate (\x -> x `mod` 5 == 0)
-- True

-- >>> member 10 $ deleteAll (\x -> x `mod` 5 == 0) $ fromPredicate even
-- False
