module Main where

foreign import ccall "cbits_hello" cbits_hello :: Int -> IO Int

main :: IO ()
main = do
	cbits_hello 10 >>= print
	putStrLn "Hello, Haskell!"

