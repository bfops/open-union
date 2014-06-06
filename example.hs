{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.OpenUnion

type MyUnion = Union (Char :| Int :| [()] :| Void)

showMyUnion :: MyUnion -> String
showMyUnion
    =  (\(c :: Char) -> "char: " ++ show c)
    @> (\(i :: Int) -> "int: " ++ show i)
    @> (\(l :: [()]) -> "list length: " ++ show (length l))
    @> (\(s :: String) -> "string: " ++ s) -- MyUnion doesn't contain String. That's fine.
    @> typesExhausted

main :: IO ()
main = do
    putStrLn $ showMyUnion $ liftUnion (4 :: Int)
    putStrLn $ showMyUnion $ liftUnion 'a'
    putStrLn $ showMyUnion $ liftUnion [(), ()]
