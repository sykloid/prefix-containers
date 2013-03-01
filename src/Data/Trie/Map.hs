-- | An associative map implemented as a prefix-tree, supports efficient lookup by prefix.
module Data.Trie.Map (
    TrieMap(..),

    -- * Primitive Tries
    empty,

    -- * Trie Modification
    insert,
    insertWith,
) where

import Data.Maybe

-- | The basic Trie data structure. Each node contains the value associated with the current prefix,
-- and a set of sub-tries based on the symbols that can appear subsequently.
data TrieMap k v
    = TrieMap {
        -- | The value associated with the current prefix.
        label :: (Maybe v),

        -- | The symbols that can follow the current prefix, and their corresponding maps.
        submaps :: [(k, TrieMap k v)]
    }
  deriving (Eq, Read, Show)

-- | The empty Trie contains no sequence, which is different from the Trie which contains the empty
-- sequence
empty :: TrieMap k v
empty = TrieMap Nothing []

-- | Insert a sequence and a value into the Trie. If the sequence is already present in the Trie,
-- apply the function to the new and old values, and insert the result into the Trie.
insertWith :: Eq k => (v -> v -> v) -> [k] -> v -> TrieMap k v -> TrieMap k v
insertWith f [] v t = t { label = Just $ maybe v (f v) (label t) }
insertWith f ks v t = t { submaps = submapInsertWith f ks v (submaps t) }
  where
    submapInsertWith :: Eq k => (v -> v -> v) -> [k] -> v -> [(k, TrieMap k v)] -> [(k, TrieMap k v)]
    submapInsertWith f (x:xs) v [] = [(x, insertWith f xs v empty)]
    submapInsertWith f (x:xs) v ((c, m):ms)
        | x == c = (c, insertWith f xs v m) : ms
        | otherwise = (c, m) : submapInsertWith f (x:xs) v ms

-- | Insert a sequence and a value into the Trie, overriding any value which might have been there
-- already for that sequence.
insert :: Eq k => [k] -> v -> TrieMap k v -> TrieMap k v
insert = insertWith const
