{-

Copyright 2015 Rafał Nowak

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

-}

module Main where

import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Time
import System.IO

import RaytracaH

decodeFromJsonFile :: JSON.FromJSON a => String -> IO (Either String a)
decodeFromJsonFile fileName = do
    file <- openFile fileName ReadMode
    contents <- BS.hGetContents file
    return (JSON.eitherDecode contents :: JSON.FromJSON a => Either String a)

loadOptions :: String -> IO (Either String RayTracerOptions)
loadOptions = decodeFromJsonFile

loadSceneWithCamera :: String -> IO (Either String SceneWithCamera)
loadSceneWithCamera = decodeFromJsonFile

-- TODO: remove nested case ofs
-- TODO: make file names readable from program arguments
main :: IO ()
main = do
    putStrLn "RaytracaH\n"
    startTime <- getCurrentTime
    optionsDecoded <- loadOptions "conf/config.json"
    case optionsDecoded of
        Left err ->
            putStrLn ("Error while reading config file: " ++ err)
        Right options -> do
            putStrLn ("Loaded options: " ++ show options ++ "\n")
            sceneWithCameraDecoded <- loadSceneWithCamera "conf/input.json"
            case sceneWithCameraDecoded of
                Left err ->
                    putStrLn ("Error while reading input file: " ++ err)
                Right sceneWithCamera -> do
                    putStrLn "Loaded scene with camera"
                    writeAsciiPPMFile (outputFileName options) (fileWithRender options sceneWithCamera)
                    endTime <- getCurrentTime
                    putStr ("\nWork finished, results saved to " ++ "test" ++ ", total time: ")
                    print $ diffUTCTime endTime startTime
