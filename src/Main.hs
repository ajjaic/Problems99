import Probs1to10

main :: IO ()
main = do
    putStrLn $ show $ mlast "Ajjai"
    {-putStrLn $ show $ lastbutone "Ajjai"-}
    {-putStrLn $ show $ kth "Ajjai" 0-}
    {-putStrLn $ show $ mlen "Ajjai"-}
    {-putStrLn $ show $ rev "Ajjai"-}
    {-putStrLn $ show $ frev "Ajjai"-}
    {-putStrLn $ show $ ispalin "Ajjai"-}
    {-putStrLn $ show $ flatlist ((List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) :: NestedList Int)-}
    {-putStrLn $ show $ flatten ((List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) :: NestedList Int)-}
    putStrLn $ show $ remdups "aaaabccaadeeee"
    putStrLn $ show $ remdups' "aaaabccaadeeee"
    putStrLn $ show $ pack "aaaabccaadeeee"
    putStrLn $ show $ encode $ pack "aaaabccaadeeee"
