{-# LANGUAGE ForeignFunctionInterface #-}

#include "network_info.h"

module Network.Address (
    getIPv4Addresses,
    getAllIPv4Address,
    NetworkInfo (..),
    NetworkInterface (..),
) where

import Data.Bits (shiftR)
import Data.List (intersperse)
import Foreign (Int32, Word8, Word32, alignment, alloca, peek, peekByteOff, sizeOf)
import Foreign.Ptr (Ptr)
import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (Storable)

foreign import ccall "get_if_addrs4" get_if_addrs4 :: CString -> Ptr CInt -> IO (Ptr NetworkInfo4)
-- foreign import ccall "get_if_addrs6" get_if_addrs6 :: CString -> Ptr CInt -> IO (Ptr NetworkInfo6)
foreign import ccall "ntohl" ntohl :: Word32 -> Word32

data IPv4Address = IPv4Address Word32

data IPv6Address = IPv6Address Word32 Word32 Word32 Word32

data NetworkInfo4 = NetworkInfo4
    { ipv4ifindex   :: Int32
    , ipv4family    :: Int32
    , ipv4prefixlen :: Int32
    , ipv4address   :: IPv4Address
    }

-- data NetworkInfo6 = NetworkInfo6
--     { ipv6ifindex   :: Int32
--     , ipv6family    :: Int32
--     , ipv6prefixlen :: Int32
--     , ipv6address   :: IPv6Address
--     }

type NetworkInterface = String

data NetworkInfo = NetworkInfo
    { ifindex   :: Int32
    , ifname    :: NetworkInterface
    , ipv4      :: [IPv4Address]
    -- , ipv6      :: [IPv6Address]
    }

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

getIPv4Addresses :: String -> IO [(IPv4Address, Int32)]
getIPv4Addresses interface = do
    info <- getIPv4Info interface
    return $ map (\x -> (ipv4address x, ipv4prefixlen x)) info

getAllIPv4Address :: IO [(IPv4Address, Int32)]
getAllIPv4Address = getIPv4Addresses ""
