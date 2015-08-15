{-# LANGUAGE ForeignFunctionInterface #-}

#include "network_info.h"

module Network.Address (
    getIPv4Address,
    getAllIPv4Address,
    getIPv6Address,
    getAllIPv6Address,
    NetworkInfo (..),
    NetworkInterface (..),
) where

import Data.Bits (shiftR)
import Data.List (intersperse)
import Foreign (Word8, Word16, Word32, alignment, alloca, peek, peekByteOff,
                sizeOf)
import Foreign.Ptr (Ptr)
import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (Storable)
import Numeric (showHex)

foreign import ccall unsafe "get_if_addrs4" get_if_addrs4 :: CString -> Ptr CInt -> IO (Ptr NetworkInfo4)
foreign import ccall unsafe "get_if_addrs6" get_if_addrs6 :: CString -> Ptr CInt -> IO (Ptr NetworkInfo6)
foreign import ccall unsafe "ntohl" ntohl :: Word32 -> Word32

data IPv4Address = IPv4Address Word32

instance Show IPv4Address where
    show (IPv4Address addr) = concat . intersperse "." $ map show $ octets addr

octets :: Word32 -> [Word8]
octets q =
    [ fromIntegral (w `shiftR` 24)
    , fromIntegral (w `shiftR` 16)
    , fromIntegral (w `shiftR` 8)
    , fromIntegral w
    ]
    where w = ntohl q

data IPv6Address = IPv6Address Word32 Word32 Word32 Word32

instance Show IPv6Address where
    show (IPv6Address a0 a1 a2 a3) = concat . intersperse ":" $
        map (\x -> showHex x "") $ concat $ map doubleoctets [a0, a1, a2, a3]

doubleoctets :: Word32 -> [Word16]
doubleoctets q =
    [ fromIntegral (w `shiftR` 16)
    , fromIntegral w
    ]
    where w = ntohl q

data NetworkInfo4 = NetworkInfo4
    { ipv4ifindex   :: Word32
    , ipv4family    :: Word32
    , ipv4prefixlen :: Word32
    , ipv4address   :: IPv4Address
    }

data NetworkInfo6 = NetworkInfo6
    { ipv6ifindex   :: Word32
    , ipv6family    :: Word32
    , ipv6prefixlen :: Word32
    , ipv6address   :: IPv6Address
    }

type NetworkInterface = String

data NetworkInfo = NetworkInfo
    { ifindex   :: Word32
    , ifname    :: NetworkInterface
    , ipv4      :: [IPv4Address]
    , ipv6      :: [IPv6Address]
    }

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

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
    peekArray (fromIntegral n) ptr

getIPv4Address :: String -> IO [(IPv4Address, Word32)]
getIPv4Address interface = do
    info <- getIPv4Info interface
    return $ map (\x -> (ipv4address x, ipv4prefixlen x)) info

getAllIPv4Address :: IO [(IPv4Address, Word32)]
getAllIPv4Address = getIPv4Address ""

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
    peekArray (fromIntegral n) ptr

getIPv6Address :: String -> IO [(IPv6Address, Word32)]
getIPv6Address interface = do
    info <- getIPv6Info interface
    return $ map (\x -> (ipv6address x, ipv6prefixlen x)) info

getAllIPv6Address :: IO [(IPv6Address, Word32)]
getAllIPv6Address = getIPv6Address ""
