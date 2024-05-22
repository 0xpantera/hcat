{-# LANGUAGE TypeApplications #-}
module HCat where

import qualified Control.Exception as Exception
import qualified System.Environment as Env
import qualified System.IO.Error as IOError
import qualified System.Info as SystemInfo
import           System.Process (readProcess)

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

wordWrap :: Int -> T.Text -> [T.Text]
wordWrap lineLen lineTxt
    | T.length lineTxt <= lineLen = [lineTxt]
    | otherwise =
        let 
            (candidate, nextLines) = T.splitAt lineLen lineTxt
            (fstLine, overflow) = softWrap candidate (T.length candidate - 1)
        in fstLine : wordWrap lineLen (overflow <> nextLines)
    where
        softWrap hardWrappedTxt txtIx
            | txtIx <= 0 = (hardWrappedTxt, T.empty)
            | T.index hardWrappedTxt txtIx == ' ' =
                let (wrappedLine, rest) = T.splitAt txtIx hardWrappedTxt
                in (wrappedLine, T.tail rest)
            | otherwise = softWrap hardWrappedTxt (txtIx - 1)

paginate :: ScreenDimensions -> T.Text -> [T.Text]
paginate (ScreenDimensions rows cols) txt =
    let
        unwrappedLines = T.lines txt
        wrappedLines = concatMap (wordWrap cols) unwrappedLines
        pageLines = groupsOf rows wrappedLines
    in map T.unlines pageLines

data ScreenDimensions = ScreenDimensions
    { screenRows :: Int
    , screenCols :: Int
    } deriving Show

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
    case SystemInfo.os of
        "darwin" -> tputScreenDimensions
        "linux" -> tputScreenDimensions
        _other -> pure $ ScreenDimensions 25 80
    where
        tputScreenDimensions :: IO ScreenDimensions
        tputScreenDimensions =
            readProcess "tput" ["lines"] ""
            >>= \lines ->
                readProcess "tput" ["cols"] ""
                >>= \cols ->
                    let lines' = read $ init lines
                        cols' = read $ init cols
                    in return $ ScreenDimensions lines' cols'