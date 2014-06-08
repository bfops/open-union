## Intro

open-union adds type-safe extensible unions to Haskell, which can be used a la:

    {-# LANGUAGE TypeOperators #-}
    {-# LANGUAGE ScopedTypeVariables #-}
    import Data.OpenUnion

    type MyUnion = Union (Char :| Int :| [()])

    showMyUnion :: MyUnion -> String
    showMyUnion
        =  (\(c :: Char) -> "char: " ++ show c)
        @> (\(i :: Int) -> "int: " ++ show i)
        @> (\(l :: [()]) -> "list length: " ++ show (length l))
        @> (\(s :: String) -> "string: " ++ s)
        @> typesExhausted

    main :: IO ()
    main = do
        putStrLn $ showMyUnion $ liftUnion (4 :: Int)
        putStrLn $ showMyUnion $ liftUnion 'a'
        putStrLn $ showMyUnion $ liftUnion [(), ()]

which prints:

    int: 4
    char: 'a'
    list length: 2

## N.B.
Casting to an unrelated type does not cause errors;
In the above example,`showMyUnion` contains a `String` case despite `MyUnion` not containing
`String` - superfluous cases are ignored, for the time being.

`typesExhausted` is NOT a catchall. It is a `Void` case, and using it as a catchall
(or forgetting to provide a certain case, for instance) will result in an error like:

    example.hs:12:8:
        Couldn't match type ‘[()] :| (Void :\ [Char])’ with ‘Void’
        Expected type: Union (([()] :| Void) :\ String) -> String
          Actual type: Union Void -> String
        In the second argument of ‘(@>)’, namely ‘typesExhausted’
        In the second argument of ‘(@>)’, namely
          ‘(\ (s :: String) -> "string: " ++ s) @> typesExhausted’

Types to the right of `:\` have been handled.

Trying to lift an incorrect type to a `Union` will cause an error resembling:

    example.hs:20:30:
        No instance for (Data.OpenUnion.Internal.LiftToUnion Void [Char])
          arising from a use of ‘liftUnion’
        In the second argument of ‘($)’, namely ‘liftUnion "asdf"’
        In the second argument of ‘($)’, namely
          ‘showMyUnion $ liftUnion "asdf"’
        In a stmt of a 'do' block:
          putStrLn $ showMyUnion $ liftUnion "asdf"
