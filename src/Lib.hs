{-# LANGUAGE DeriveGeneric #-}
module Lib where

import Control.Monad
import Control.Monad.Loops

import Data.Text as T (unpack)
import Data.ByteString.Char8 as BS (unpack)
import Data.List
import Data.Yaml

import GHC.Generics

import System.Directory
import System.Process

import Text.Printf

-- path :: FilePath
-- path = "/Users/simon/Documents/Haskell/Literate Unit-B/GitHub/reactive-banana-transformers/stack-lts-8.yaml"

getGitRefs :: FilePath -> String -> IO (Maybe String)
getGitRefs url key = do
    let mkSwapPair xs = (xs!!1,xs!!0)
    lns <- map (mkSwapPair . words) . lines 
        <$> readProcess "git" ["ls-remote",url] ""
    return $ lookup key lns

data Loc = Loc
    { git :: String
    , commit :: String }
    deriving (Eq,Show,Generic)
data GitDep = GitDep
    { location :: Loc }
    deriving (Eq,Show,Generic)

data Dep = S String | Git GitDep
    deriving (Eq,Show,Generic)

data StackProject = 
    StackProject { packages :: [Dep] }
    deriving (Eq,Show,Generic)

foo :: Either ParseException StackProject -> ()
foo _ = ()

instance ToJSON StackProject where
instance FromJSON StackProject where
instance ToJSON Dep where
    toJSON (S x) = toJSON x
    toJSON (Git x) = toJSON x
instance FromJSON Dep where
    parseJSON (String x) = return (S $ T.unpack x)
    parseJSON (Object v) = Git <$> parseJSON (Object v)
    parseJSON _ = fail "expecting String or Object"
instance ToJSON GitDep where
instance FromJSON GitDep where
instance ToJSON Loc where
instance FromJSON Loc where

updateDep :: Dep -> IO Dep
updateDep (S x) = return $ S x
updateDep (Git x) = do
    h <- maybe (fail "HEAD revision not found") return =<< getGitRefs (git $ location x) "HEAD"
    return $ Git x 
       { location = (location x) 
          { commit = h } }

updateProject :: StackProject -> IO StackProject
updateProject (StackProject xs) = StackProject <$> traverse updateDep xs

selectPath :: IO FilePath
selectPath = do
    xs <- getDirectoryContents "."
    let p fp = "stack" `isPrefixOf` fp && ".yaml" `isSuffixOf` fp
        xs'  = filter p xs
    case xs' of
        [fp] -> return fp
        []  -> fail "no 'state*.yaml' file found"
        _ -> do
            putStrLn "Choose one:"
            zipWithM_ (\i x -> printf "(%d) %s\n" (i :: Int) x) [0..] xs'
            untilJust $ do
                i <- readLn
                if 0 <= i && i < length xs' 
                    then return $ Just $ xs' !! i
                    else return Nothing

lib :: IO ()
lib = do
    path <- selectPath
    Right x <- decodeFileEither path
    x' <- updateProject x
    if x' == x 
        then putStrLn "no changes"
        else putStrLn $ BS.unpack . encode $ x'
