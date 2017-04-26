import Data.Vect

-- Exercise 1
readToBlank : IO (List String)
readToBlank = do line <- getLine
                 case line of
                   "" => pure []
                   _ => do lines <- readToBlank
                           pure $ line :: lines

-- Exercise 2
readAndSave : IO ()
readAndSave = do lines <- readToBlank
                 putStrLn "Filename: "
                 fname <- getLine
                 Right () <- writeFile fname (unlines lines)
                   | Left err => putStrLn (show err)
                 pure ()


-- Exercise 3
readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right h <- openFile filename Read
                               | Left err => pure (_ ** [])
                           Right contents <- readContents h
                               | Left err => pure (_ ** [])
                           closeFile h
                           pure contents
    where readContents : File -> IO (Either FileError (n ** Vect n String))
          readContents h = do eof <- fEOF h
                              if eof then pure (Right (_ ** [])) else do
                                 Right str <- fGetLine h
                                    | Left err => pure (Left err)
                                 Right (_ ** rest) <- readContents h
                                    | Left err => pure (Left err)
                                 pure (Right (_ ** str :: rest))
