#!/usr/bin/env stack
-- stack --resolver lts-7.14 exec --package criterion -- ghc -O2
-- Bencmarking String
-- lenient here means that if there is a character encoding error, 
-- it will be replaced with the Unicode replacement character
-- The following functions for working with files:
-- Files are inherently binary data, we shouldn't hide that. 
-- We should also make it convenient for people to do the very common task of reading
-- and writing textual data with UTF-8 character encoding.

import           Criterion.Main
import qualified Data.ByteString          as S
import qualified Data.ByteString.Lazy     as L
import qualified Data.Text.Encoding       as T
import           Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO             as TIO
import qualified Data.Text.Lazy.Encoding  as TL
import qualified Data.Text.Lazy.IO        as TLIO

-- readFile :: Monad m => FilePath -> m ByteString -- strict byte string, no lazy I/O!
-- readFileUtf8 :: Monad m => FilePath -> m Text
-- readFileUtf8 = fmap (decodeUtf8With lenientDecode) . readFile
-- maybe include a non-lenient variant that throws exceptions or
-- returns an Either value on bad character encoding
-- writeFile :: Monad m => FilePath -> ByteString -> m ()
-- writeFileUtf8 :: Monad m => FilePath -> Text -> m ()
-- writeFileUtf8 fp = writeFile fp . encodeUtf8
-- conduit, pipes, streaming, etc, can handle the too-large-for-memory
-- case




-- Downloaded from: http://www.gutenberg.org/cache/epub/345/pg345.txt

fp :: FilePath
fp = "pg345.txt"

main :: IO ()
main = defaultMain
   [ bench "String" $ nfIO $ readFile fp
    , bench "Data.Text.IO" $ nfIO $ TIO.readFile fp
    , bench "Data.Text.Lazy.IO" $ nfIO $ TLIO.readFile fp
    , bench "Data.ByteString.readFile" $ nfIO $ S.readFile fp
    , bench "Data.ByteString.Lazy.readFile" $ nfIO $ L.readFile fp
    , bench "strict decodeUtf8" $ nfIO $ fmap T.decodeUtf8 $ S.readFile fp
    , bench "strict decodeUtf8With lenientDecode"
        $ nfIO $ fmap (T.decodeUtf8With lenientDecode) $ S.readFile fp
    , bench "lazy decodeUtf8" $ nfIO $ fmap TL.decodeUtf8 $ L.readFile fp
    , bench "lazy decodeUtf8With lenientDecode"
        $ nfIO $ fmap (TL.decodeUtf8With lenientDecode) $ L.readFile fp
    ]

--  Run ./bench.hs && ./bench --output bench.html
-- String I/O is the slowest,
-- and ByteString I/O is the fastest (since no character encoding overhead is involved)
-- Stick with Data.ByteString.readFile for known-small data,
--  use a streaming package (e.g, conduit) if your choice for large data, 
--  and handle the character encoding yourself.
-- And apply this to writeFile and other file-related functions as well.
--
--
-- benchmarking Data.ByteString.readFile
-- time                 218.1 μs   (215.1 μs .. 221.6 μs)
--                      0.992 R²   (0.987 R² .. 0.996 R²)
-- mean                 229.2 μs   (221.0 μs .. 242.5 μs)
-- std dev              34.36 μs   (24.13 μs .. 48.99 μs)












