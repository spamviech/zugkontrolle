-- | Useful utility functions
module Zug.Util (isRaspi, writeFileUtf8, readFileUtf8) where

import Data.Text (Text)
import qualified Data.Text.IO as Text
import Debug.Trace (trace)
import System.IO (withFile, IOMode(WriteMode, ReadMode), hSetEncoding, utf8, hSetNewlineMode
                , noNewlineTranslation)
import System.Info (os, arch)

-- | Decide based on 'os' and 'arch' value wether compilation happens on the rhaspberry pi.
--
-- Every comination not linux os with arm architecture is concidered non-raspi.
isRaspi :: Bool
isRaspi = trace (show os ++ ", " ++ show arch) $ (os == "linux") && (arch == "arm")

{------------------------------------------------------------------------
Inspired by this blog post: https://www.snoyman.com/blog/2020/10/haskell-bad-parts-1
Normal Read/WriteFile uses a local dependent file encoding.
Originally copied from RIO, adjusted to leave newlines unchanged as well.
------------------------------------------------------------------------}
-- | Write a file in UTF8 encoding.
--
-- No changes are made to newline characters.
writeFileUtf8 :: FilePath -> Text -> IO ()
writeFileUtf8 fp text = withFile fp WriteMode $ \h -> do
    hSetEncoding h utf8
    hSetNewlineMode h noNewlineTranslation
    Text.hPutStr h text

-- | Read a file in UTF8 encoding, throwing an exception on invalid character encoding.
--
-- No changes are made to newline characters.
readFileUtf8 :: FilePath -> IO Text
readFileUtf8 fp = withFile fp ReadMode $ \h -> do
    hSetEncoding h utf8
    hSetNewlineMode h noNewlineTranslation
    Text.hGetContents h
