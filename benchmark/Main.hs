
import qualified List
import qualified Trie
import qualified Data.Set as Set
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Criterion.Main (defaultMain, bgroup, bench, nf)

main :: IO ()
main = defaultMain [
   bgroup "small"
   [ bench "list" $ nf (List.parse glossSet 3) test
   , bench "trie" $ nf (Trie.parse glossTrie) $ encodeUtf8 $ pack test
   ]
 ]
 where
  gloss = ["lake", "awesome crater lake", "crater lake"]
  glossSet = Set.fromList gloss
  glossTrie = Trie.mkGlossary $ pack <$> gloss
  test = "The awesome crater lake is an awesome lake"
