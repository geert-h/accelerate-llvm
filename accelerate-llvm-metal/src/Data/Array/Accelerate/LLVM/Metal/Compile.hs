module Data.Array.Accelerate.LLVM.Metal.Compile
  (withCompiledModule
  ) where

import Control.Monad (unless)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Data.Array.Accelerate.Error (internalError)
import Formatting (string)

-- Temporary compiled module runner
withCompiledModule :: String -> (FilePath -> IO a) -> IO a
withCompiledModule source action =
  withSystemTempDirectory "accelerate-metal" $ \dir -> do
    let llvmFile = dir </> "kernel.ll"
        airFile  = dir </> "kernel.air"
        libFile  = dir </> "kernel.metallib"
    writeFile llvmFile source
    execXcrun [ "-sdk", "macosx", "metal", "-c", llvmFile, "-o", airFile]
    execXcrun [ "-sdk", "macosx", "metallib", airFile, "-o", libFile]
    action libFile

-- this function runs xcrun with the given arguments above
-- for now this is enough because we only pass on simple generate file
execXcrun :: [String]  -> IO ()
execXcrun args = do
  (status, output, errors) <- readProcessWithExitCode "xcrun" args ""
  unless (status == ExitSuccess) $
    internalError string $ 
      "accelerate-llvm-metal: compilation failed"
      ++ "\nran xcrun " ++ unwords args
      ++ "\n" ++ output
      ++ "\n" ++ errors
