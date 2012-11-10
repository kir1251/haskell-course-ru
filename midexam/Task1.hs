import System.Directory
import Data.List
import Prelude (Show, Read, error, not, (&&), (||), String, Bool(..), IO(..), return, (==), print, concat, sequence)
 
--checkDE :: String -> String -> IO Bool
--checkDE dir1 dir2 = do
--    let d1 = dir1
--    let d2 = dir2
--    b1 <- doesDirectoryExist dir1
--    b2 <- doesDirectoryExist dir2 
--    return (not (b1 && b2))

rGetDirectoryContents :: String -> IO [String]
rGetDirectoryContents dir = do
    b1 <- doesDirectoryExist dir
    l <- if (not b1) then do
       return []
    else do
       l2 <- getDirectoryContents dir       
       let l1 = map ((dir ++ "/") ++) (delete ".." (delete "." l2))
       t1 <- sequence [ rGetDirectoryContents (x) | 
                         x <- l1, not (x == ".") && not (x == "..")]
--       t <- concat t1
       return (l1 ++ (concat t1))
    return l

    
--concatIO :: [IO [String]] -> IO [String]
--concatIO [] = do return []
--concatIO  (x:list1) = do
--    a1 <- x
--    a <- concatIO list1
--    return (a1 ++ a)
    
--appendIO :: [String] -> IO [String] -> IO [String]
--appendIO xs1 xs2 = do
--    a2 <- xs2
--    return (xs1 ++ a2)


compareDirs :: String -> String -> IO [String]
compareDirs dir1 dir2 = do
    l1 <- rGetDirectoryContents dir1
    l2 <- rGetDirectoryContents dir2
    --print l1
    --print l2
    return (map (dir1 ++ ) ((map (drop (length dir1)) l1) \\ (map (drop (length dir2)) l2)))
    --b <- checkDE dir1 dir2     
    --ans <- if (b) then do
    --   return []
    --else do 
    --    l1 <- getDirectoryContents dir1  
    --    l2 <- getDirectoryContents dir2        
    --    let l = (l1 \\ l2)
    --    let f = nub (l1 ++ l2)
    --    l <- appendIO l (concatIO [ compareDirs (dir1 ++ "/" ++ s) (dir2 ++ "/" ++ s)
    --                               | s <- f, not (s == ".." || s == ".") ])
    --    return l 
    --return ans
