import Data.List (sort)
import Data.Word (Word32)
import Network.Address
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_ifindex :: Property
prop_ifindex = monadicIO $ do
    ifindexes <- run getIfindexes
    interfaces <- run getNetworkInterfaces
    let ifindexes2 = map ifindex interfaces
    assert $ sort ifindexes == sort ifindexes2
    assert $ length ifindexes == length ifindexes2

prop_random_ifindex :: Word32 -> Property
prop_random_ifindex n = monadicIO $ do
    ifindexes <- run getIfindexes
    interfaces <- run getNetworkInterfaces
    let ifindexes2 = map ifindex interfaces
    case n `elem` ifindexes of
        True  -> assert $ n `elem` ifindexes2
        False -> assert $ not $ n `elem` ifindexes2

prop_all_ipv4_address :: Property
prop_all_ipv4_address = monadicIO $ do
    allipv4s <- fmap (map fst) (run getAllIPv4Address)
    ipv4s <- fmap (concat . map ipv4) (run getNetworkInterfaces)
    mapM (\a -> assert $ a `elem` ipv4s) allipv4s
    mapM (\b -> assert $ b `elem` allipv4s) ipv4s
    assert $ length allipv4s == length ipv4s

prop_all_ipv6_address :: Property
prop_all_ipv6_address = monadicIO $ do
    allipv6s <- fmap (map fst) (run getAllIPv6Address)
    ipv6s <- fmap (concat . map ipv6) (run getNetworkInterfaces)
    mapM (\a -> assert $ a `elem` ipv6s) allipv6s
    mapM (\b -> assert $ b `elem` allipv6s) ipv6s
    assert $ length allipv6s == length ipv6s

main = do
    quickCheck prop_ifindex
    quickCheck prop_random_ifindex
    quickCheck prop_all_ipv4_address
    quickCheck prop_all_ipv6_address
