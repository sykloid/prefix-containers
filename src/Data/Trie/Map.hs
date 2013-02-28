-- | An associative map implemented as a prefix-tree, supports efficient lookup by prefix.
module Data.Trie.Map (
    TrieMap(..),
) where

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
