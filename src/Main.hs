import Probs1to10
import Probs11to20

main :: IO ()
main = do
    putStrLn $ show $ mlast "RandPer"
    putStrLn $ show $ lastbutone "RandPer"
    putStrLn $ show $ kth "RandPer" 0
    putStrLn $ show $ mlen "RandPer"
    putStrLn $ show $ rev "RandPer"
    putStrLn $ show $ frev "RandPer"
    putStrLn $ show $ ispalin "RandPer"
    putStrLn $ show $ flatlist ((List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) :: NestedList Int)
    putStrLn $ show $ flatten ((List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) :: NestedList Int)
    putStrLn $ show $ remdups "aaaabccaadeeee"
    putStrLn $ show $ remdups' "aaaabccaadeeee"
    putStrLn $ show $ pack "aaaabccaadeeee"
    putStrLn $ show $ encode $ pack "aaaabccaadeeee"

    putStrLn $ show $ encodemodified "aaaabccaadeeee"
    putStrLn $ show $ decodemodified [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
