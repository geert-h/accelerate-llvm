{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings   #-}
module Data.Array.Accelerate.LLVM.Metal.CodeGen.Permute
  ( atomically
  ) where

import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.Metal.Target (Metal)
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.AST.Operation
import Data.Array.Accelerate.LLVM.CodeGen.Array (firstArrayPtr)
import qualified Data.Array.Accelerate.LLVM.CodeGen.Arithmetic as A
import Data.Array.Accelerate.Type
import Data.Array.Accelerate.LLVM.CodeGen.Arithmetic (hash)
import LLVM.AST.Type.Operand (Operand(LocalReference))
import LLVM.AST.Type.Representation
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.GetElementPtr
import Data.Array.Accelerate.LLVM.CodeGen.Sugar
import LLVM.AST.Type.Instruction.Atomic
import LLVM.AST.Type.Instruction.RMW
import LLVM.AST.Type.Instruction.Volatile

-- Number of locks in the array of locks
lockArraySize :: Int
lockArraySize = 16 * 1024 * 8

atomically :: Envs env idxEnv
  -> Arg env (Mut sh e)
  -> Operands Int
  -> CodeGen Metal ()
  -> CodeGen Metal ()
atomically envs mutArray i action = do
  seed <- firstArrayPtr envs mutArray
  i'   <- A.add numType seed i
  i''  <- A.fromIntegral TypeInt (IntegralNumType TypeWord64) i'
  h    <- hash i''
  idx  <- A.rem TypeWord64 h $ A.liftWord64 $ Prelude.fromIntegral lockArraySize

  spin <- newBlock "spinlock.entry"
  crit <- newBlock "spinlock.critical-section"
  exit <- newBlock "spinlock.exit"

  byteIdx <- A.quot TypeWord64 idx $ A.liftWord64 8
  bitIdx  <- A.rem  TypeWord64 idx $ A.liftWord64 8
  bitIdx' <- A.fromIntegral TypeWord64 (IntegralNumType TypeInt) bitIdx

  let bufPtr = LocalReference (PrimType (PtrPrimType (primType @Word8) (AddrSpace 1))) "locks_array"

  addr <- instr' $ GetElementPtr $ GEP1 bufPtr $ op integralType byteIdx
  _    <- br spin

  setBlock spin
  OP_Word64 mask' <- A.shiftL TypeWord64 (A.liftWord64 1) bitIdx'
  mask  <- instr' $ Trunc (IntegralBoundedType TypeWord64) (IntegralBoundedType TypeWord8) mask'
  old   <- instrMD (AtomicRMW numType NonVolatile Or addr mask (CrossThread, Acquire)) (bufferMetadata' Nothing)
  check <- A.band TypeWord8 old (OP_Word8 mask)
  ok    <- A.eq singleType check $ A.liftWord8 0
  _     <- cbr ok crit spin

  setBlock crit
  r <- action
  OP_Word8 maskComplement <- A.complement TypeWord8 (OP_Word8 mask)
  _ <- instrMD (AtomicRMW numType NonVolatile And addr maskComplement (CrossThread, Release)) (bufferMetadata' Nothing)
  _ <- br exit

  setBlock exit
  return r

