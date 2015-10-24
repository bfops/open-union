{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Data.OpenUnion

type MyUnion = Union '[Char, Int, [()]]

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
    let u1 = liftUnion (4  :: Int) :: MyUnion
        u2 = liftUnion (10 :: Int) :: MyUnion
        u3 = liftUnion 'a'         :: MyUnion
    putStrLn $ show u1
    putStrLn $ show u3
    putStrLn $ show $ u1 == u1
    putStrLn $ show $ u1 == u2
    putStrLn $ show $ u1 == u3
