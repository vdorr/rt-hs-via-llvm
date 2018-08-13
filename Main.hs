{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds -fwarn-unused-imports -fno-warn-tabs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")

module Main where

import Control.Concurrent
import Control.Monad
import Foreign.Ptr

import LLVM.Linking

--------------------------------------------------------------------------------

-- based on https://github.com/llvm-hs/llvm-hs-examples/blob/master/irbuilder/Main.hs

import qualified Data.Text.Lazy.IO as T

import LLVM.AST hiding (function)
import LLVM.AST.Type as AST
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as P

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction

import qualified JIT

--------------------------------------------------------------------------------

foreign import ccall "cbits_hello" cbits_hello :: Int -> IO Int

foreign import ccall "rtrt_newQueue" rtrt_newQueue :: Int -> IO (Ptr ())
foreign import ccall "rtrt_deleteQueue" rtrt_deleteQueue :: Ptr () -> IO ()
foreign import ccall "rtrt_queueReadAvailable" rtrt_queueReadAvailable :: Ptr () -> IO Int
foreign import ccall "rtrt_queueWriteAvailable" rtrt_queueWriteAvailable :: Ptr () -> IO Int
foreign import ccall "rtrt_queuePush" rtrt_queuePush :: Ptr () -> Int -> IO ()
foreign import ccall "rtrt_queuePop" rtrt_queuePop :: Ptr () -> IO Int

--------------------------------------------------------------------------------

simple :: Integer -> Module
simple q = buildModule "exampleModule" $ mdo

	f <- extern "rtrt_queuePush" [AST.ptr AST.void, AST.i64] AST.void

	function "main" [] AST.double $ \[] -> do
		_entry <- block `named` "entry3"
		call f
			[ (ConstantOperand (C.IntToPtr (C.Int 64 q) (AST.ptr AST.void)), [])
			, (ConstantOperand (C.Int 64 8), [])
			]
		ret (ConstantOperand (C.Float (F.Double 8)))

--------------------------------------------------------------------------------

loop x f = f x >>= flip loop f

main :: IO ()
main = do
	cbits_hello 10 >>= print
	putStrLn "Hello, Haskell!"

--	setNumCapabilities 2

	q <- rtrt_newQueue 128
#if 0
	rtrt_queueReadAvailable q >>= print
	rtrt_queueWriteAvailable q >>= print
	rtrt_queuePush q 123456789
	rtrt_queueReadAvailable q >>= print
	rtrt_queueWriteAvailable q >>= print
	rtrt_queuePop q >>= print
	rtrt_queueReadAvailable q >>= print
	rtrt_queueWriteAvailable q >>= print
#endif

#if 1
	forkOS $ loop 0 $ \i -> do
--		print (here, 1)
		threadDelay 500000
		rtrt_queuePush q i
		return (i + 2)

	forever $ do
		rtrt_queueReadAvailable q >>= \case
			0 -> threadDelay 1000
			n -> replicateM n (rtrt_queuePop q) >>= print
--		print (here, 2)
		return ()
#endif
	rtrt_deleteQueue q

	print here

