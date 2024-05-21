{-# LANGUAGE TypeApplications #-}
module HCat where

import qualified Control.Exception as Exception
import qualified System.Environment as Env
import qualified System.IO.Error as IOError

import qualified Data.ByteString as BS

import qualified Data.Text as T
import qualified Data.Text.IO as TIO


runHCat :: IO ()
runHCat =
    handleIOError $
        handleArgs
        >>= eitherToErr
        >>= TIO.readFile
        >>= TIO.putStrLn
    where
        handleIOError :: IO () -> IO ()
        handleIOError ioAction = 
            Exception.catch ioAction $
            \e -> putStrLn "I ran into an error:" >> print @IOError e

handleArgs :: IO (Either String FilePath)
handleArgs =
    parseArgs <$> Env.getArgs
    where
        parseArgs argList =
            case argList of
                [arg] -> Right arg
                [] -> Left "no filename provided"
                _ -> Left "multiple files not supported"

eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left e) =
    Exception.throwIO . IOError.userError $ show e

groupsOf :: Int -> [a] -> [[a]]
groupsOf n [] = []
groupsOf 0 _ = []
groupsOf n xs =
    let (hd, tl) = splitAt n xs
    in hd : groupsOf n tl