{-# LANGUAGE OverloadedStrings #-}
module Imports where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.Except
import           Control.Monad.Fix
import           Control.Monad.Identity
import           Control.Monad.RWS
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Array
import           Data.Bits
import           Data.Bool
import           Data.Char
import           Data.Complex
import           Data.Dynamic
import           Data.Either
import           Data.Eq
import           Data.Fixed
import           Data.Function
import           Data.Graph
import           Data.Int
import           Data.Ix
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Ratio
import           Data.Tree
import           Data.Tuple
import           Data.Typeable
import           Data.Word
import           Debug.SimpleReflect
import           Prelude hiding (IO,putStr,putStrLn,getLine,readLn,print,readIO,readFile,writeFile,appendFile)
import           PureIO as IO
--import           ShowFun
import           System.Random
import           Test.QuickCheck
import           Text.JSON
import           Text.PrettyPrint.HughesPJ
import           Text.Printf
import           Crossword
import           Anagram
import           StandardDictionaries
import           Dictionary
import           Text.Regex.TDFA
import           HuntTools
import           Lucid
import           Lucid.Bootstrap
import qualified Cipher
squiggle = "SQUIGGLE"

--anagram dict = doAnagram $ anaDict dict
--crossword dict = doCrossword $ crossDict dict

-- | Run the given command and then show the output. This constraint
-- is to aid communication between mueval and tryhaskell.
runTryHaskellIO :: (Read b,Show b)
                => ([String],[(FilePath,String)])
                -> IO b
                -> Either (Interrupt,([String],[(FilePath,String)]))
                          (String,([String],[(FilePath,String)]))
runTryHaskellIO (is,fs) m =
  case runIO (Input is (M.fromList fs)) m of
    (Left i,out) -> Left (i,convert out)
    (Right r,out) -> Right (show r,convert out)
  where convert (Output os fs) = (os,M.toList fs)


class FancyResult a where
  doFancy :: a -> IO ()


data SimpleFancy = SimpleFancy

instance (FancyResult SimpleFancy) where
  doFancy SimpleFancy = putStr "I. Am. Fancy."


