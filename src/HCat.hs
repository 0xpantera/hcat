{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module HCat where

import qualified Data.ByteString as BS
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import qualified Data.Time.Clock.POSIX as PosixClock
import qualified System.Directory as Dir

import qualified Control.Exception as Exception
import qualified System.Environment as Env
import qualified System.IO.Error as IOError
import qualified System.Info as SystemInfo
import           System.Process (readProcess)
import           System.IO

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf
import Data.List


runHCat :: IO ()
runHCat =
    withErrorHandling $ do
        filePath <- eitherToErr =<< handleArgs
        contents <- TIO.hGetContents =<< openFile filePath ReadMode
        termSize <- getTerminalSize
        hSetBuffering stdout NoBuffering
        finfo <- fileInfo filePath
        let pages = paginate termSize finfo contents
        showPages pages
    where
        withErrorHandling :: IO () -> IO ()
        withErrorHandling ioAction = 
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

paginate :: ScreenDimensions -> FileInfo -> T.Text -> [T.Text]
paginate (ScreenDimensions rows cols) finfo txt =
    let
        rows' = rows - 1
        wrappedLines = concatMap (wordWrap cols) (T.lines txt)
        pages = map (T.unlines . padTo rows') $ groupsOf rows' wrappedLines
        pageCount = length pages
        statusLines = map (fmtFileInfo finfo cols pageCount) [1..pageCount]
    in zipWith (<>) pages statusLines
    where
        padTo :: Int -> [T.Text] -> [T.Text]
        padTo lineCount rowsToPad =
            take lineCount $ rowsToPad <> repeat ""

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
        tputScreenDimensions = do
            lines <- readProcess "tput" ["lines"] ""
            cols <- readProcess "tput" ["cols"] ""
            let lines' = read $ init lines
                cols' = read $ init cols
            return $ ScreenDimensions lines' cols'

data ContinueCancel = Continue | Cancel
    deriving (Eq, Show)

getContinue :: IO ContinueCancel
getContinue = do
    hSetBuffering stdin NoBuffering
    input <- hGetChar stdin
    case input of
        ' ' -> return Continue
        'q' -> return Cancel
        _   -> getContinue

showPages :: [T.Text] -> IO ()
showPages [] = return ()
showPages (page:pages) = do
    clearScreen
    TIO.putStrLn page
    input <- getContinue
    case input of
        Continue -> showPages pages
        Cancel -> return ()

clearScreen :: IO ()
clearScreen =
    BS.putStr "\^[[1J\^[[1;1H"

data FileInfo = FileInfo
    { filePath :: FilePath
    , fileSize :: Int
    , fileMTime :: Clock.UTCTime
    , fileReadable :: Bool
    , fileWriteable :: Bool
    , fileExecutable :: Bool
    } deriving Show

fileInfo :: FilePath -> IO FileInfo
fileInfo filePath = do
    perms <- Dir.getPermissions filePath
    mtime <- Dir.getModificationTime filePath
    size <- BS.length <$> BS.readFile filePath
    return FileInfo
        { filePath = filePath
        , fileSize = size
        , fileMTime = mtime
        , fileReadable = Dir.readable perms
        , fileWriteable = Dir.writable perms
        , fileExecutable = Dir.executable perms
        }

fmtFileInfo :: FileInfo -> Int -> Int -> Int -> T.Text
fmtFileInfo FileInfo{..} maxWidth totalPages currentPage =
    let
        timestamp =
            TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
        permissionString =
            [ if fileReadable then 'r' else '-'
            , if fileWriteable then 'w' else '-'
            , if fileExecutable then 'x' else '-' ]
        statusLine = T.pack $
            printf
            "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d"
            filePath
            permissionString
            fileSize
            timestamp
            currentPage
            totalPages
    in invertText (truncateStatus statusLine)
    where
        invertText inputStr =
            let
                reverseVideo = "\^[[7m"
                resetVideo = "\^[[0m"
            in reverseVideo <> inputStr <> resetVideo
        truncateStatus statusLine
            | maxWidth <= 3 = ""
            | T.length statusLine > maxWidth =
              T.take (maxWidth - 3) statusLine <> "..."
            | otherwise = statusLine


interleaveLines :: String -> String -> String
interleaveLines a b =
    unlines . concat . Data.List.transpose $ [lines a, lines b]

interLeaveFiles :: FilePath -> FilePath -> FilePath -> IO String
interLeaveFiles f1 f2 outFile =
    readFile f1
    >>= \c1 ->
    readFile f2
    >>= \c2 -> putStrLn "I've read two files"
    >>= \_ -> 
        let c3 = interleaveLines c1 c2
        in writeFile outFile c3
        >> return c3