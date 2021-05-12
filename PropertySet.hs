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
  = Noop
  | Insert (Predicate a) (Set a)
  | Delete (Predicate a) (Set a)

-- | A set of values @a@.
data Set a = MkSet (Operation a)

instance Semigroup (Set a) where
  (<>) = union

instance Monoid (Set a) where
  mempty = empty

evalOp :: Operation a -> Predicate a
evalOp Noop = const False
evalOp (Insert predicate s) = \x -> x `member` s || predicate x
evalOp (Delete predicate s) = \x -> x `member` s && (not $ predicate x)

-- | The empty set.
empty :: Set a
empty = MkSet Noop

-- | Is a given element in the set?
member :: a -> Set a -> Bool
member a (MkSet op) = evalOp op a

-- | Is a given element __not__ in the set?
notMember :: a -> Set a -> Bool
notMember a s = not $ member a s

-- | Insert a single element in a set.
-- Insertion of an element already on the set is a no-op.
insert :: Eq a => a -> Set a -> Set a
insert a s
  | notMember a s = MkSet $ Insert (== a) s
  | otherwise = s

-- | Insert all the elements that satisfy a @Predicate@ in a set.
insertAll :: (Predicate a) -> Set a -> Set a
insertAll p s = MkSet $ Insert p s

-- | Delete a single element from a set.
-- Deletion of an element not in the set is a no-op.
delete :: Eq a => a -> Set a -> Set a
delete a s
  | member a s = MkSet $ Delete (== a) s
  | otherwise = s

-- | Delete all the elements that satisfy a given @Predicate@ in a set.
deleteAll :: (Predicate a) -> Set a -> Set a
deleteAll p s = MkSet $ Delete p s

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
union s1 s2 = MkSet $ Insert (`member` s1) s2

-- | Difference of two sets.
difference :: Set a -> Set a -> Set a
difference s1 s2 = MkSet $ Insert (`notMember` s2) s1

-- | Intersection of two sets.
intersection :: Set a -> Set a -> Set a
intersection s1 s2 = MkSet $ Insert (\x -> x `member` s1 && x `member` s2) empty

-- | See 'difference'
(\\) :: Set a -> Set a -> Set a
(\\) = difference