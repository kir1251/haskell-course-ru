import System.Directory
import Data.List
import Prelude (Show, Read, error, not, (&&), String, Bool, IO(..), return)
 
checkDE :: String -> String -> Bool
checkDE dir1 dir2 = do
    b1 <- doesDirectoryExist dir1
    b2 <- doesDirectoryExist dir2
    (not (b1 && b2))
    

compareDirs :: String -> String -> IO [String]
compareDirs dir1 dir2 = do
    let b = checkDE dir1 dir2
    ans <- if (b) then do
       return []
    else do 
        l1 <- getDirectoryContents dir1
        l2 <- getDirectoryContents dir2        
        let l = [] ++ (l1 \\ l2)
        let f = nub (l1 ++ l2)
        let l = l ++ concat [ do; a <- compareDirs (dir1 ++ "/" ++ s) (dir2 ++ "/" ++ s); return a | s <- f ] 
        return l 
    return ans
