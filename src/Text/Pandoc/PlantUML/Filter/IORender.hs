
-- | Module: Text.Pandoc.PlantUML.Filter.Render
-- Defines the actual rendering done with PlantUML
module Text.Pandoc.PlantUML.Filter.IORender() where

import System.IO (hClose, hPutStr, IOMode(..), withBinaryFile, Handle)
import Data.ByteString.Lazy (hGetContents, hPut)
import System.Process
import System.Directory

import System.Environment (getExecutablePath)
import System.FilePath 
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Trans (liftIO)

import Text.Pandoc.PlantUML.Filter.Types

instance ImageIO IO where
  renderImage imageFileName (DiagramSource source) = do

    execPath <- getExecutablePath
    let plantPath = joinPath [ takeDirectory execPath, plantName ]

    (Just hIn, Just hOut, _, _) <- createProcess $ plantUmlProcess plantPath imageFileName
    hPutStr hIn source
    hClose hIn
    withImageFile $ pipe hOut
    hClose hOut
      where withImageFile = withBinaryFile (show imageFileName) WriteMode
  doesImageExist imageFileName = doesFileExist $ show imageFileName

-- getPlantumlPath : 
--   returns the the absolute file path to "plantuml.jar" 
--   with the directory if this executable

plantName = "plantuml.jar"
plantUmlProcess :: FilePath -> ImageFileName -> CreateProcess
plantUmlProcess plantPath (ImageFileName _ fileType) = ( proc "java" ["-jar", plantPath, "-pipe", "-t" ++ fileType])
  { std_in = CreatePipe, std_out = CreatePipe }

pipe :: Handle -> Handle -> IO ()
pipe hIn hOut = do
  input <- hGetContents hIn
  hPut hOut input

