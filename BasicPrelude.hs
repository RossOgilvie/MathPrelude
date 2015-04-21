{-# LANGUAGE NoImplicitPrelude #-}

-- | BasicPrelude mostly re-exports
-- several key libraries in their entirety.
-- The exception is Data.List,
-- where various functions are replaced
-- by similar versions that are either
-- generalized, operate on Text,
-- or are implemented strictly.
module BasicPrelude
  ( -- * Module exports
    module CorePrelude
  , module Data.List
  , module Control.Monad

    -- * Enhanced exports
    -- ** Simpler name for a typeclassed operation
  , map
  , empty
  , (++)
  , concat
  , intercalate

    -- ** Text for Read and Show operations
  , show
  , show'
  , read
  , read'
  , readIO
    -- ** FilePath for file operations
  , readFile
  , writeFile
  , appendFile

    -- * Text exports
    -- ** Text operations (Pure)
  , Text.lines
  , Text.words
  , Text.unlines
  , Text.unwords
  , Text.pack
  , Text.unpack
  , textToString
  , ltextToString
  , encodeUtf8
  , decodeUtf8
    -- ** Text operations (IO)
  , Text.putStr
  , Text.getLine
  , LText.getContents
  , LText.interact

    -- * Miscellaneous prelude re-exports
    -- ** Math
  --, Prelude.gcd
  --, Prelude.lcm
    -- ** Show and Read
  , Prelude.ShowS
  , Prelude.showsPrec
  , Prelude.showList
  , Prelude.shows
  , Prelude.showChar
  , Prelude.showString
  , Prelude.showParen
  , Prelude.ReadS
  , Prelude.readsPrec
  , Prelude.readList
  , Prelude.reads
  , Prelude.readParen
  , Prelude.lex
  , readMay
    -- ** IO operations
  , Prelude.putChar
  , Prelude.getChar
  , Prelude.readLn
  ) where

import CorePrelude

import Data.List hiding
  ( -- prefer monoid versions instead
    (++)
  , concat
  , intercalate
    -- prefer Text versions instead
  , lines
  , words
  , unlines
  , unwords
    -- prefer map = fmap instead
  , map
    -- prefer strict versions
  , sum
  , product
  , and
  , or
  )

-- Import *all of the things* from Control.Monad,
-- specifically, the list-based things that
-- CorePrelude doesn't export
import Control.Monad


import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Filesystem.Path.CurrentOS as FilePath
import qualified Prelude
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Safe

-- | > map = fmap
map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap

-- | > empty = mempty
empty :: Monoid w => w
empty = mempty

infixr 5 ++

-- | > (++) = mappend
(++) :: Monoid w => w -> w -> w
(++) = mappend

-- | > concat = mconcat
concat :: Monoid w => [w] -> w
concat = mconcat

-- | > intercalate = mconcat .: intersperse
intercalate :: Monoid w => w -> [w] -> w
intercalate xs xss = mconcat (Data.List.intersperse xs xss)


-- | Convert a value to readable Text
show :: Show a => a -> Text
show = Text.pack . Prelude.show

-- | The original prelude version
show' :: Show a => a -> String
show' = Prelude.show

-- | Parse Text to a value
read :: Read a => Text -> a
read = Prelude.read . Text.unpack

-- | The original prelude version
read' :: Read a => String -> a
read' = Prelude.read

-- | The readIO function is similar to read
-- except that it signals parse failure to the IO monad
-- instead of terminating the program.
readIO :: Read a => Text -> IO a
readIO = Prelude.readIO . Text.unpack


-- | Read a file and return the contents of the file as Text.
-- The entire file is read strictly.
readFile :: FilePath -> IO Text
readFile = Text.readFile . FilePath.encodeString

-- | Write Text to a file.
-- The file is truncated to zero length before writing begins.
writeFile :: FilePath -> Text -> IO ()
writeFile = Text.writeFile . FilePath.encodeString

-- | Write Text to the end of a file.
appendFile :: FilePath -> Text -> IO ()
appendFile = Text.appendFile . FilePath.encodeString

textToString :: Text -> Prelude.String
textToString = Text.unpack

ltextToString :: LText -> Prelude.String
ltextToString = LText.unpack

-- | Note that this is /not/ the standard @Data.Text.Encoding.decodeUtf8@. That
-- function will throw impure exceptions on any decoding errors. This function
-- instead uses @decodeLenient@.
decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode

readMay :: Read a => Text -> Maybe a
readMay = Safe.readMay . Text.unpack
