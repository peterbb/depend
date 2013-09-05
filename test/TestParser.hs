import Parser



main = do
    content <- getContents
    putStrLn "Input ===================="
    putStr content
    putStrLn "=======================EOF"
    case parse "(stdin)" content of
        Left err -> putStrLn $ "Error: " ++ (show err)
        Right prog -> putStrLn $ "Success: " ++ (show prog)
    putStrLn "Good bye."

