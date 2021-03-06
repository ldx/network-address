{-# LANGUAGE ForeignFunctionInterface #-}

#include "network_info.h"

module Network.Address (
    getIPv4Address,
    getAllIPv4Address,
    getIPv6Address,
    getAllIPv6Address,
    getIfindexes,
    getNetworkInterfaces,
    NetworkInterface (..),
    NetworkInterfaceIndex (..),
    NetworkInterfaceName (..),
) where

import Control.Monad (mapM)
import Data.Bits (shiftR)
import Data.List (intersperse, zipWith4)
import Foreign (Word8, Word16, Word32, alignment, alloca, allocaBytes,
                peek, peekByteOff, sizeOf)
import Foreign.Ptr (nullPtr, Ptr)
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (Storable)
import Numeric (showHex)

---------
-- FFI --
---------
foreign import ccall unsafe "get_if_addrs4" get_if_addrs4 :: CString -> Ptr CInt -> IO (Ptr NetworkInfo4)
foreign import ccall unsafe "free" if_addrs4_free :: Ptr NetworkInfo4 -> IO ()
foreign import ccall unsafe "get_if_addrs6" get_if_addrs6 :: CString -> Ptr CInt -> IO (Ptr NetworkInfo6)
foreign import ccall unsafe "free" if_addrs6_free :: Ptr NetworkInfo6 -> IO ()
foreign import ccall unsafe "get_ifindexes" get_ifindexes :: Ptr CInt -> IO (Ptr NetworkInterfaceIndex)
foreign import ccall unsafe "free" ifindexes_free :: Ptr NetworkInterfaceIndex -> IO ()
foreign import ccall unsafe "ifindex2name" ifindex2name :: Word32 -> CString -> Word32 -> IO (CString)
foreign import ccall unsafe "ntohl" ntohl :: Word32 -> Word32

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-----------------------
-- IPv4 address type --
-----------------------
data IPv4Address = IPv4Address Word32

instance Show IPv4Address where
    show (IPv4Address addr) = concat . intersperse "." $ map show $ octets addr

instance Eq IPv4Address where
    (==) (IPv4Address a) (IPv4Address b) = a == b

octets :: Word32 -> [Word8]
octets q =
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]
    where w = ntohl q

data NetworkInfo4 = NetworkInfo4
    { ipv4ifindex   :: Word32
    , ipv4family    :: Word32
    , ipv4prefixlen :: Word32
    , ipv4address   :: IPv4Address
    } deriving (Show)

-----------------------
-- IPv6 address type --
-----------------------
data IPv6Address = IPv6Address Word32 Word32 Word32 Word32

instance Show IPv6Address where
    show (IPv6Address a0 a1 a2 a3) = concat . intersperse ":" $
        map (\x -> showHex x "") $ concat $ map doubleoctets [a0, a1, a2, a3]

instance Eq IPv6Address where
    (==) (IPv6Address a0 a1 a2 a3) (IPv6Address b0 b1 b2 b3) =
        a0 == b0 && a1 == b1 && a2 == b2 && a3 == b3

doubleoctets :: Word32 -> [Word16]
doubleoctets q =
    [ fromIntegral (w `shiftR` 16)
    , fromIntegral w
    ]
    where w = ntohl q

data NetworkInfo6 = NetworkInfo6
    { ipv6ifindex   :: Word32
    , ipv6family    :: Word32
    , ipv6prefixlen :: Word32
    , ipv6address   :: IPv6Address
    } deriving (Show)

------------------------
-- Network interfaces --
------------------------
type NetworkInterfaceIndex = Word32

type NetworkInterfaceName = String

data NetworkInterface = NetworkInterface
    { ifindex   :: NetworkInterfaceIndex
    , ifname    :: NetworkInterfaceName
    , ipv4      :: [IPv4Address]
    , ipv6      :: [IPv6Address]
    -- , mac       :: [MACAddress]
    } deriving (Show)

ifnamsiz = 16 -- In net/if.h

ifindex2Name :: NetworkInterfaceIndex -> IO NetworkInterfaceName
ifindex2Name idx = allocaBytes ifnamsiz $ \ptr -> do
    name <- ifindex2name idx ptr $ fromIntegral ifnamsiz
    if name == nullPtr
        then return ""
        else peekCString name

getIfindexes :: IO [NetworkInterfaceIndex]
getIfindexes = alloca $ \nptr -> do
    ptr <- get_ifindexes nptr
    n <- peek nptr
    ifindexes <- peekArray (fromIntegral n) ptr
    ifindexes_free ptr
    return ifindexes

getNetworkInterfaces :: IO [NetworkInterface]
getNetworkInterfaces = do
    ifindexes   <- getIfindexes
    ifnames     <- mapM ifindex2Name ifindexes
    ipv4s       <- mapM (fmap (map fst) . getIPv4Address) ifnames
    ipv6s       <- mapM (fmap (map fst) . getIPv6Address) ifnames
    return $ zipWith4 NetworkInterface ifindexes ifnames ipv4s ipv6s

-----------------------------
-- Retrieve IPv4 addresses --
-----------------------------

instance Storable NetworkInfo4 where
    alignment _ = #alignment struct network_info4
    sizeOf _    = #size struct network_info4
    peek ptr    = do
        ipv4ifindex     <- (#peek struct network_info4, ifindex) ptr
        ipv4family      <- (#peek struct network_info4, family) ptr
        ipv4prefixlen   <- (#peek struct network_info4, prefixlen) ptr
        address         <- (#peek struct network_info4, address) ptr
        let ipv4address = IPv4Address address
        return $ NetworkInfo4 ipv4ifindex ipv4family ipv4prefixlen ipv4address

getIPv4Info :: String -> IO [NetworkInfo4]
getIPv4Info interface = alloca $ \nptr -> do
    ifname <- newCString interface
    ptr <- get_if_addrs4 ifname nptr
    n <- peek nptr
    info <- peekArray (fromIntegral n) ptr
    if_addrs4_free ptr
    return info

getIPv4Address :: String -> IO [(IPv4Address, Word32)]
getIPv4Address interface = do
    info <- getIPv4Info interface
    return $ map (\x -> (ipv4address x, ipv4prefixlen x)) info

getAllIPv4Address :: IO [(IPv4Address, Word32)]
getAllIPv4Address = getIPv4Address ""

-----------------------------
-- Retrieve IPv6 addresses --
-----------------------------
instance Storable NetworkInfo6 where
    alignment _ = #alignment struct network_info6
    sizeOf _    = #size struct network_info6
    peek ptr    = do
        ipv6ifindex     <- (#peek struct network_info6, ifindex) ptr
        ipv6family      <- (#peek struct network_info6, family) ptr
        ipv6prefixlen   <- (#peek struct network_info6, prefixlen) ptr
        addr0           <- (#peek struct network_info6, address[0]) ptr
        addr1           <- (#peek struct network_info6, address[1]) ptr
        addr2           <- (#peek struct network_info6, address[2]) ptr
        addr3           <- (#peek struct network_info6, address[3]) ptr
        let ipv6address = IPv6Address addr0 addr1 addr2 addr3
        return $ NetworkInfo6 ipv6ifindex ipv6family ipv6prefixlen ipv6address

getIPv6Info :: String -> IO [NetworkInfo6]
getIPv6Info interface = alloca $ \nptr -> do
    ifname <- newCString interface
    ptr <- get_if_addrs6 ifname nptr
    n <- peek nptr
    info <- peekArray (fromIntegral n) ptr
    if_addrs6_free ptr
    return info

getIPv6Address :: String -> IO [(IPv6Address, Word32)]
getIPv6Address interface = do
    info <- getIPv6Info interface
    return $ map (\x -> (ipv6address x, ipv6prefixlen x)) info

getAllIPv6Address :: IO [(IPv6Address, Word32)]
getAllIPv6Address = getIPv6Address ""
