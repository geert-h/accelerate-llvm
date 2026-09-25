module Data.Array.Accelerate.LLVM.Metal.Compile
  (withCompiledModule
  , compile
  , ObjectR(..)
  ) where

import Control.Monad (unless)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory, withTempDirectory)
import System.Process (readProcessWithExitCode)
import Data.Array.Accelerate.Error (internalError)
import Formatting (string)
import Data.Array.Accelerate.LLVM.Metal.Compile.Cache (UID, cacheOfUID)
import Data.ByteString.Short.Char8 (ShortByteString)
import LLVM.AST.Type.Module (Module)
import Data.Array.Accelerate.LLVM.Metal.Target (Metal)
import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty.AST as LLVMPretty
import Data.Array.Accelerate.LLVM.State
import LLVM.AST.Type.Downcast (downcast)
import qualified Data.Array.Accelerate.LLVM.Internal.LLVMPretty.PP  as P
import qualified Text.PrettyPrint                                   as P ( render )
import Control.Monad.IO.Class (liftIO)
import System.Directory (renameFile)

data ObjectR f = ObjectR
  { objId   :: !UID
  , objSym  :: !ShortByteString
  , objPath :: FilePath         -- identifies the .metallib location
  }

compile :: UID -> ShortByteString -> Module f -> LLVM Metal (ObjectR f)
compile uid name module' = do
  libraryPath <- cacheOfUID uid
  llvmVer <- getLLVMVer

  let ast :: LLVMPretty.Module
      ast = downcast module'
      source = P.render (P.ppLLVM llvmVer (P.ppModule ast))

  liftIO $ 
    withTempDirectory (takeDirectory libraryPath) "metal-build-" $ \dir -> do
      let llvmFile = dir </> "kernel.ll"
          airFile  = dir </> "kernel.air"
          libFile  = dir </> "kernel.metallib"
      writeFile llvmFile source
      execXcrun [ "-sdk", "macosx", "metal", "-c", llvmFile, "-o", airFile, "-Xclang", "-opaque-pointers"]
      execXcrun [ "-sdk", "macosx", "metallib", airFile, "-o", libFile]
      renameFile libFile libraryPath

  pure ObjectR 
   { objId = uid
   , objSym = name
   , objPath = libraryPath
   }

-- Temporary compiled module runner
withCompiledModule :: String -> (FilePath -> IO a) -> IO a
withCompiledModule source action =
  withSystemTempDirectory "accelerate-metal" $ \dir -> do
    let llvmFile = dir </> "kernel.ll"
        airFile  = dir </> "kernel.air"
        libFile  = dir </> "kernel.metallib"
    writeFile llvmFile source
    execXcrun [ "-sdk", "macosx", "metal", "-c", llvmFile, "-o", airFile, "-Xclang", "-opaque-pointers"]
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
