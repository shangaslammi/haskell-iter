# Data.Iter

`Data.Iter` has an API similar to using lazy IO, but final evaluation is done explicitly in the IO monad and there is support for deterministic finalizers (e.g. for closing file handles).

Example:

    main = do
        let i = itake 10 $ iterFile "foo.txt"
        toList i >>= putStrLn

Unlike lazy I/O, `toList` evaluates its contents fully, and doesn't "leak" interleaved IO actions into pure code, and even though we only read the first 10 characters of the file, the file handle is closed immediately after `toList` is executed.

