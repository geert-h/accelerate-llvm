{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings   #-}
module Data.Array.Accelerate.LLVM.Metal.CodeGen.Permute
  ( atomically
  ) where

import Data.Array.Accelerate.LLVM.CodeGen.Environment
import Data.Array.Accelerate.LLVM.CodeGen.IR
import Data.Array.Accelerate.LLVM.Metal.Target (Metal)
import Data.Array.Accelerate.LLVM.CodeGen.Monad
import Data.Array.Accelerate.AST.Operation hiding (Body, Lam)
import Data.Array.Accelerate.LLVM.CodeGen.Array (firstArrayPtr)
import qualified Data.Array.Accelerate.LLVM.CodeGen.Arithmetic as A
import Data.Array.Accelerate.Type
import LLVM.AST.Type.Operand (Operand(LocalReference))
import LLVM.AST.Type.Representation
import LLVM.AST.Type.Instruction
import LLVM.AST.Type.GetElementPtr
import LLVM.AST.Type.Function (Function(..), Arguments(..), FunctionAttribute(..))
import Data.Array.Accelerate.LLVM.CodeGen.Base (call)
import Data.Array.Accelerate.LLVM.CodeGen.Constant (boolean)
import LLVM.AST.Type.Instruction.Atomic
import LLVM.AST.Type.Instruction.RMW
import LLVM.AST.Type.Instruction.Volatile

-- Number of locks in the array of locks
lockArraySize :: Int
lockArraySize = 16 * 1024 * 8

-- This atomically implementation works a bit different than both the PTX and Native implementations
-- in pseudocode it roughly does the following:
-- done = false
-- repeat:
--   if not done:
--     try to acquire lock
--     if acquired:
--       perform action
--       release lock
--       done = true
-- 
-- This is repeated until all threads in the SIMD group are done
atomically :: Envs env idxEnv
  -> Arg env (Mut sh e)
  -> Operands Int
  -> CodeGen Metal ()
  -> CodeGen Metal ()
atomically envs mutArray i action = do
  seed <- firstArrayPtr envs mutArray
  i'   <- A.add numType seed i
  i''  <- A.fromIntegral TypeInt (IntegralNumType TypeWord64) i'
  h    <- A.hash i''
  idx  <- A.rem TypeWord64 h $ A.liftWord64 $ Prelude.fromIntegral lockArraySize

  loop    <- newBlock "spinlock.loop"
  attempt <- newBlock "spinlock.attempt"
  crit    <- newBlock "spinlock.critical"
  check   <- newBlock "spinlock.check"
  exit    <- newBlock "spinlock.exit"

  -- extract the lock bit index from the unsinged int
  wordIdx <- A.quot TypeWord64 idx $ A.liftWord64 32
  bitIdx  <- A.rem  TypeWord64 idx $ A.liftWord64 32
  bitIdx' <- A.fromIntegral TypeWord64 (IntegralNumType TypeInt) bitIdx

  -- Get address of the word32 where the lock is located in from the locks array
  let bufPtr = LocalReference (PrimType (PtrPrimType (primType @Word32) (AddrSpace 1))) "locks_array"
  addr <- instr' $ GetElementPtr $ GEP1 bufPtr $ op integralType wordIdx

  -- Make a done pointer and initialise it to false
  -- This will be used at the end of each loop to check whether there are sill SIMD lanes working on the 
  done <- hoistAlloca BoolPrimType
  _    <- instr' $ Store NonVolatile done (boolean False) Nothing

  -- Constructs mask and not(mask) for the bit index
  OP_Word64 mask' <- A.shiftL TypeWord64 (A.liftWord64 1) bitIdx'
  mask  <- instr' $ Trunc (IntegralBoundedType TypeWord64) (IntegralBoundedType TypeWord32) mask'
  OP_Word32 maskComplement <- A.complement TypeWord32 (OP_Word32 mask)
  
  _    <- br loop
  setBlock loop
  d <- instr $ Load NonVolatile done Nothing
  _    <- cbr d check attempt

  setBlock attempt
  old <- instr $ AtomicRMW numType NonVolatile Or addr mask (CrossThread, Acquire)
  c   <- A.band TypeWord32 old (OP_Word32 mask)
  ok  <- A.eq singleType c $ A.liftWord32 0
  _   <- cbr ok crit check

  -- The lock is succesfully acquired, the critical action is executed
  setBlock crit
  action
  _ <- instr' $ AtomicRMW numType NonVolatile And addr maskComplement (CrossThread, Release)
  _ <- instr' $ Store NonVolatile done (boolean True) Nothing
  _ <- br check

  -- This block checks whether all threads are done.
  -- When this is not the case, we re-enter the loop
  setBlock check
  completed  <- instr $ Load NonVolatile done Nothing
  pending    <- A.lnot completed
  anyPending <- simdAny pending
  _          <- cbr anyPending loop exit 

  setBlock exit
  return ()

-- This function returns true if at least one active thread evaluates expr to true.
-- In this case, expr = !done
-- The language specs defines simd_any in Table 6.14.
simdAny :: Operands Bool -> CodeGen Metal (Operands Bool)
simdAny (OP_Bool p) = call (Lam BoolPrimType "expr" $ Body (PrimType BoolPrimType) Nothing "air.simd_any")
  (ArgumentsCons p [] ArgumentsNil)
  [NoUnwind, Convergent]
