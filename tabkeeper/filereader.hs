import System.IO 
import Network.URI

pickAndSplit []    = ("null", [])
pickAndSplit (a:d) = (a,unlines d)

geturl Nothing    = "null"
geturl (Just uri) = 
    case uriAuthority uri of 
        Nothing   -> "null"
        Just auth -> uriRegName auth 

main = do 
    contents <- readFile "input.txt"
    let links  = lines contents
        urls   = map (geturl . parseURI) links
    (link,others) <- return $ pickAndSplit urls
    putStrLn $ "the link   look like: " ++ link  
    putStrLn $ "the others look like: " ++ others
    writeFile  "input2.txt" others
    appendFile "seen.txt"   (link ++ "\n")
    putStrLn "done"
