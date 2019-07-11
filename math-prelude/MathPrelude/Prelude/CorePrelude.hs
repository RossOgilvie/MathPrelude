{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP #-}

module MathPrelude.Prelude.CorePrelude
( 
--------------------------------------------------
-- MATH STUFF NOT EXPORTED
--------------------------------------------------
  --, Prelude.Num (..)
  --, Prelude.Real (..)
  --, Prelude.Integral (..)
  --, Prelude.Fractional (..)
  --, Prelude.Floating (..)
  --, Prelude.RealFrac (..)
  --, Prelude.RealFloat(..)
  -- , Prelude.Rational

  -- , (Prelude.&&)
  -- , (Prelude.||)
  -- , Prelude.not
  -- , Prelude.otherwise
  --, (Prelude.^)
  --, (Prelude.^^)
  --, Prelude.subtract
  -- , Prelude.odd
  -- , Prelude.even
  --, Prelude.gcd
--, Prelude.lcm
  --, Prelude.fromIntegral
  --, Prelude.realToFrac
--------------------------------------------------
-- NORMAL PRELUDE STUFF
--------------------------------------------------
  -- ** Operators
    (Prelude..)
    , (Prelude.$)
    , (Prelude.$!)
    , (Control.Monad.=<<)
    -- ** Functions
    , Prelude.fst
    , Prelude.snd
    , Prelude.maybe
    , Prelude.either
    , Prelude.flip
    , Prelude.const
    , Prelude.id
    , Prelude.error
    , Prelude.uncurry
    , Prelude.curry
    , Data.Tuple.swap
    , Prelude.until
    , Prelude.asTypeOf
    , Prelude.undefined
    , Prelude.seq
    , Prelude.print
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
    , Prelude.putChar
    , Prelude.getChar
    , Prelude.readLn
    
    -- ** Type classes
    , Prelude.ShowS
    , Prelude.Ord (..)
    , Prelude.Eq (..)
    , Prelude.Bounded (..)
    , Prelude.Enum (..)
    , Prelude.Show
    , Prelude.Read
    , Prelude.Functor (..)
    , Prelude.Monad (..)
    , Data.String.IsString (..)
    -- ** Data types
    , Prelude.Bool (..)
    , Prelude.Char
    , Prelude.Either (..)
    , Prelude.IO
    , Prelude.Maybe (..)
    , Prelude.Ordering (..)
    , Prelude.String
      -- ** Numbers
    , Prelude.Int
    , Prelude.Integer
    , Prelude.Word
    , Prelude.Float
    , Prelude.Double
    -- , Prelude.Int32
    -- , Prelude.Int64
    -- , Prelude.Word8
    -- , Prelude.Word32
    -- , Prelude.Word64

-----------------------------------------------
-- BASE LIBRARY EXPORTS
-----------------------------------------------
  -- ** Monoids
    , Semigroup (..)
    , Monoid (..)
    -- ** List
    , module Data.List
    -- ** Maybe
    , Data.Maybe.mapMaybe
    , Data.Maybe.catMaybes
    , Data.Maybe.fromMaybe
    , Data.Maybe.isJust
    , Data.Maybe.isNothing
    -- ** Either
    , Data.Either.partitionEithers
    , Data.Either.lefts
    , Data.Either.rights
    -- ** Ord
    , Data.Function.on
    , Data.Ord.comparing
    , equating
    -- ** Applicative
    , Control.Applicative.Applicative (..)
    , (Control.Applicative.<$>)
    , (Control.Applicative.<|>)
    -- ** Monad
    , module Control.Monad
    , (Control.Monad.>=>)

-----------------------------------------------
-- TEXT AND FILEPATH STUFF
-----------------------------------------------
    , Text
    , LText    
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
    -- ** Text operations (IO)
  , Text.putStr
  , Text.putStrLn
  , Text.getLine
  , LText.getContents
  , LText.interact
      -- ** Files
    , F.FilePath
    , (F.</>)
    , (F.<.>)
    , F.hasExtension
    , F.basename
    , F.filename
    , F.directory


-----------------------------------------------
-- MODIFIED BASE FUNCTIONS
-----------------------------------------------
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
  , readMay
    -- ** FilePath for file operations
  , readFile
  , writeFile
  , appendFile

-----------------------------------------------
-- MISC
-----------------------------------------------
  , module MathPrelude.Prelude.Logic
  ) where

import qualified Prelude
import Prelude (Char, (.), Eq, Bool, Show(), Read(), String, IO, Maybe)

import Data.Semigroup (Semigroup (..))
import Data.Monoid (Monoid (..))
import Control.Applicative
import Control.Monad

import qualified Data.Maybe
import qualified Data.Either
import qualified Data.Ord
import qualified Data.Function
import qualified Data.Tuple
import qualified Data.String

import Data.List hiding ( 
  -- prefer monoid versions instead
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

import qualified Data.Text                     as Text
import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO                  as Text
import qualified Data.Text.Lazy                as LText
import qualified Data.Text.Lazy.IO             as LText

import qualified Filesystem.Path.CurrentOS     as F

import qualified Safe

import MathPrelude.Prelude.Logic



equating :: Eq a => (b -> a) -> b -> b -> Bool
equating = Data.Function.on (Prelude.==)

------------------------------------------------------
-- Generalised Functions
------------------------------------------------------

-- | > map = fmap
map :: (Functor f) => (a -> b) -> f a -> f b
map = fmap

infixr 5 ++

-- | > (++) = mappend
(++) :: Semigroup w => w -> w -> w
(++) = (<>)

-- | > concat = mconcat
concat :: Monoid w => [w] -> w
concat = mconcat

-- | > intercalate = mconcat .: intersperse
intercalate :: Monoid w => w -> [w] -> w
intercalate xs xss = mconcat (Data.List.intersperse xs xss)


------------------------------------------------------
-- Text Stuff
------------------------------------------------------

type LText = LText.Text

-- | Convert a value to readable Text
show :: Show a => a -> Text
show = pack . Prelude.show

-- | The original prelude version
show' :: Show a => a -> String
show' = Prelude.show

-- | Parse Text to a value
read :: Read a => Text -> a
read = Prelude.read . unpack

-- | The original prelude version
read' :: Read a => String -> a
read' = Prelude.read

-- | The readIO function is similar to read
-- except that it signals parse failure to the IO monad
-- instead of terminating the program.
readIO :: Read a => Text -> IO a
readIO = Prelude.readIO . unpack

readMay :: Read a => Text -> Maybe a
readMay = Safe.readMay . unpack

textToString :: Text -> Prelude.String
textToString = unpack

ltextToString :: LText -> Prelude.String
ltextToString = LText.unpack


------------------------------------------------------
-- Filepath stuff
------------------------------------------------------

-- | Read a file and return the contents of the file as Text.
-- The entire file is read strictly.
readFile :: F.FilePath -> IO Text
readFile = Text.readFile . F.encodeString

-- | Write Text to a file.
-- The file is truncated to zero length before writing begins.
writeFile :: F.FilePath -> Text -> IO ()
writeFile = Text.writeFile . F.encodeString

-- | Write Text to the end of a file.
appendFile :: F.FilePath -> Text -> IO ()
appendFile = Text.appendFile . F.encodeString


