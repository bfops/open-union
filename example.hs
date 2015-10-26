{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception
import Data.OpenUnion
import Data.Typeable

data Exc1 = Exc1
            deriving (Eq, Ord, Show, Typeable)
instance Exception Exc1

data Exc2 = Exc2
            deriving (Eq, Ord, Show, Typeable)
instance Exception Exc2

type ExcUnion = Union '[Exc1, Exc2]

showException :: ExcUnion -> IO String
showException = return . show

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

    print =<< catch (throwIO Exc1) showException
    print =<< catch (throwIO Exc2) showException
