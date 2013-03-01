-- | An associative map implemented as a prefix-tree, supports efficient lookup by prefix.
module Data.Trie.Map (
    TrieMap(..),

    -- * Primitive Tries
    empty,

    -- * Trie Modification
    insert,
    insertWith,
    delete,

    -- * Trie Queries
    lookup,
    hasPrefix,
) where

import Prelude hiding (lookup)
import Data.Maybe

import qualified Prelude as P

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

-- | Delete a sequence from a Trie. Do nothing if the sequence does not exist in the Trie.
delete :: Eq k => [k] -> TrieMap k v -> TrieMap k v
delete ks t = fromMaybe empty $ deleteMaybe ks t
  where
    deleteMaybe :: Eq k => [k] -> TrieMap k v -> Maybe (TrieMap k v)
    deleteMaybe [] t
        | null (submaps t) = Nothing
        | otherwise = Just $ t { label = Nothing }
    deleteMaybe ks t
        | null ds = Nothing
        | otherwise = Just $ t { submaps = ds }
      where
        ds = deleteSubmaps ks (submaps t)

    deleteSubmaps :: Eq k => [k] -> [(k, TrieMap k v)] -> [(k, TrieMap k v)]
    deleteSubmaps (x:xs) [] = []
    deleteSubmaps (x:xs) ((c, m):ms)
        | x == c && isJust m' = (c, fromJust m') : ms
        | x == c = ms
        | otherwise = (c, m) : deleteSubmaps (x:xs) ms
      where
        m' = deleteMaybe xs m

-- | Apply the given function to the value stored with the given key. If the key is not in the Trie,
-- do nothing.
adjust :: Eq k => [k] -> (v -> v) -> TrieMap k v -> TrieMap k v
adjust ks f t = maybe t (\v -> insert ks (f v) t) (lookup ks t)

-- | Look up a sequence from a Trie. Return @Nothing@ if the sequence is not contained in the Trie.
-- This is different from the case where some sequence with the given prefix exists in the Trie.
lookup :: Eq k => [k] -> TrieMap k v -> Maybe v
lookup [] t = label t
lookup (k:ks) t = (P.lookup k $ submaps t) >>= lookup ks

-- | Determine if the Trie contains some sequence with the given prefix. Note that the empty
-- sequence is a prefix of every sequence, but @hasPrefix [] t@ will only return true if @t@ is
-- non-empty.
hasPrefix :: Eq k => [k] -> TrieMap k v -> Bool
hasPrefix [] t = (not . null $ submaps t) || (isJust $ label t)
hasPrefix (k:ks) t = maybe False (const True) (P.lookup k $ submaps t)
