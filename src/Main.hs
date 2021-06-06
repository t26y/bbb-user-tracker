{-# LANGUAGE TupleSections #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Control.Monad.Zip (mzip)
import Data.Bifunctor (Bifunctor (first))
import Data.ByteString.Char8 (pack, unpack)
import Data.List (intersect, isPrefixOf, isSuffixOf, nub, (\\))
import Data.List.Extra (trim)
import Debug.Trace
import Distribution.Simple.Utils (safeLast)
import System.Directory (doesFileExist, makeAbsolute, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.INotify
  ( Event (Created),
    EventVariety (Create),
    addWatch,
    initINotify,
  )
import qualified System.IO.Strict as SIO

type State = (String, [String], [(String, String)]) -- timestamp, current users, users who left: (name, last seen)

sleeptime = 1 -- time to wait for file to be written in seconds

statePath = "user-tracker-state.txt"

csvPath = "user-tracker-events.csv"

main :: IO ()
main = do
  args <- getArgs
  binname <- getProgName
  if length args /= 1
    then putStrLn ("Usage: " ++ binname ++ " path\n where path is the directory the files will be downloaded to")
    else do
      inotify <- initINotify
      let path = head args
      setCurrentDirectory path
      saveexists <- doesFileExist statePath
      if saveexists
        then putStrLn "using existing state file"
        else writeStateFile statePath ("1.1.1970:00:00:00", [], [])
      csvexists <- doesFileExist csvPath
      if csvexists
        then putStrLn "using existing csv file"
        else writeFile csvPath "timestamp,event,name\n"
      wd <- addWatch inotify [Create] (pack path) handler
      quitloop
  where
    quitloop = do
      putStrLn "Type 'quit' to terminate"
      input <- getLine
      unless (not (null input) && input `isPrefixOf` "quit") quitloop

handler :: Event -> IO ()
handler (Created False path) = do
  let pathStr = unpack path
  if not ("save-users-list-" `isPrefixOf` pathStr && ".txt" `isSuffixOf` pathStr)
    then putStrLn $ "Ignoring file " ++ pathStr
    else do
      putStrLn $ "detected userlist file. waiting " ++ show sleeptime ++ "s for download"
      threadDelay $ sleeptime * 10 ^ 6
      sf <- SIO.readFile statePath
      -- putStrLn "Parsing state"
      let s = parseStateFile sf
      case s of
        Nothing -> putStrLn "corrupt state file"
        Just old@(oldTimestamp, oldcur, oldgone) -> do
          -- putStrLn "state:"
          -- print s
          file <- readFile pathStr
          let p = parseUserList file
          case p of
            Nothing -> putStrLn $ "failed to parse users-list file " ++ pathStr
            Just new@(timestamp, cur) -> do
              if oldTimestamp > timestamp
                then putStrLn $ "Ignoring new file with timestamp " ++ timestamp ++ " (older than " ++ oldTimestamp ++ ")"
                else do
                  putStrLn $ "Processed userlist from " ++ timestamp
                  -- print p
                  let (joined, rejoined, newgone) = process old new
                  let oldgoneWithoutReconnects = filter (\(n, _) -> n `notElem` cur) oldgone
                  printChanges joined rejoined newgone oldgoneWithoutReconnects
                  let joinedcsv = unlines $ map ((timestamp ++ ",joined,") ++) joined
                      rejoinedcsv = unlines $ map ((timestamp ++ ",rejoined,") ++) rejoined
                      newgonecsv = unlines $ map ((timestamp ++ ",disconnected,") ++) newgone
                  appendFile csvPath (joinedcsv ++ rejoinedcsv ++ newgonecsv)
                  let newgoneTimestamped = map (,oldTimestamp) newgone
                  writeStateFile statePath (timestamp, cur, newgoneTimestamped ++ oldgoneWithoutReconnects)
handler e = do
  putStr "Ignoring event "
  print e

printChanges :: [String] -> [String] -> [String] -> [(String, String)] -> IO ()
printChanges j r g o = do
  unless (null j) (putStrLn $ "These users joined:\n" ++ unlines j)
  unless (null r) (putStrLn $ "These users reconnected:\n" ++ unlines r)
  unless (null g) (putStrLn $ "These users left:\n" ++ unlines g)
  unless (null o) (putStrLn $ "These users left previously:\n" ++ unlines (map (\(n, t) -> n ++ ", last seen " ++ t) o))

writeStateFile :: FilePath -> State -> IO ()
writeStateFile p (ts, cur, gone) = do writeFile p (unlines ((ts : "" : c) ++ [""] ++ g))
  where
    gonelines = map (\(u, t) -> u ++ "\n" ++ t) gone
    c = if null cur then [""] else cur
    g = if null gonelines then [""] else gonelines

process :: State -> (String, [String]) -> ([String], [String], [String])
process (oldts, oldcur, oldgone) (newts, newcur) = (joined, rejoined, newgone)
  where
    joined = newcur \\ (oldcur ++ rejoined)
    rejoined = map fst oldgone `intersect` newcur
    newgone = oldcur \\ newcur

parseUserList :: String -> Maybe (String, [String])
parseUserList str = mzip (ts (lines str)) (nub <$> cur (map trim (drop 3 (lines str))))
  where
    ts :: [String] -> Maybe String
    ts (l : _) = safeLast (words l)
    ts _ = trace "userlist error b" Nothing
    cur :: [String] -> Maybe [String]
    cur [] = trace "userlist error a" Nothing -- no user list in file
    cur ([] : _) = Just []
    cur (n : ns) = (n :) <$> cur ns

parseStateFile :: String -> Maybe State
parseStateFile str = ts $ lines str
  where
    ts :: [String] -> Maybe State
    ts [] = trace "state error d" Nothing
    ts (t : str) = (\(cur, gone) -> (t, cur, gone)) <$> c (tail str)
    c :: [String] -> Maybe ([String], [(String, String)])
    c [] = trace "state error c" Nothing
    c ([] : ns) = ([],) <$> g ns
    c (n : ns) = Data.Bifunctor.first (n :) <$> c ns -- was: (\(cur, gone) -> (n : cur, gone)) <$> c ns
    g :: [String] -> Maybe [(String, String)]
    g [] = Just []
    g ([] : ns) = g ns
    g (n : t : ns) = ((n, t) :) <$> g ns
    g (_ : ns) = g ns
