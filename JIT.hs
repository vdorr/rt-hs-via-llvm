{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
#define here (__FILE__ ++ ":" ++ show (__LINE__ :: Integer) ++ " ")
--based on https://github.com/cocreature/llvm-hs-jit-external-lib/blob/master/src/Static.hs

module JIT where

import           Control.Monad
import           Data.Int
import           Foreign.Ptr

import           LLVM.Context
import           LLVM.Linking (loadLibraryPermanently, getSymbolAddressInProcess)
import           LLVM.Module
import           LLVM.OrcJIT
import           LLVM.Target hiding (withHostTargetMachine)

import qualified LLVM.CodeGenOpt as CodeGenOpt
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.Relocation as Reloc

import LLVM.OrcJIT.CompileLayer
import qualified Data.ByteString.Char8 as ByteString

import qualified LLVM.AST

foreign import ccall "dynamic"
  mkFun :: FunPtr (IO Int) -> IO Int

resolver :: IRCompileLayer l -> SymbolResolver
resolver compileLayer =
  SymbolResolver
    (\s -> findSymbol compileLayer s True)
    (\s ->
       fmap
         (\a -> Right $ JITSymbol a (JITSymbolFlags False False False True))
         (getSymbolAddressInProcess s))

withHostTargetMachine :: (TargetMachine -> IO a) -> IO a
withHostTargetMachine f = do
  initializeAllTargets
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  features <- getHostCPUFeatures
  (target, _) <- lookupTarget Nothing triple
  withTargetOptions $ \options ->
    withTargetMachine target triple cpu features options Reloc.PIC CodeModel.Default CodeGenOpt.Default f

runJIT :: LLVM.AST.Module -> IO ()
runJIT m = do

  b <- loadLibraryPermanently Nothing
  unless (not b) (error "Couldn’t load library")
  withContext $ \ctx ->
--    withModuleFromLLVMAssembly ctx (File "module.ll") $ \mod' ->
    withModuleFromAST ctx m $ \mod' -> do
      optmod <- moduleAST mod'
      s <- moduleLLVMAssembly mod'
      ByteString.putStrLn s

      withHostTargetMachine $ \tm ->
        withObjectLinkingLayer $ \objectLayer ->
          withIRCompileLayer objectLayer tm $ \compileLayer -> do
            withModule
              compileLayer
              mod'
              (resolver compileLayer) $
              \_ -> do

                mainSymbol <- mangleSymbol compileLayer "main"
                Right (JITSymbol mainFn _) <- findSymbol compileLayer mainSymbol True
                unless (mainFn /= fromIntegral 0) (error "Couldn’t find JIT symbol")
                result <- mkFun (castPtrToFunPtr (wordPtrToPtr mainFn))
                print (here, result)

