module Graphics.Wayland.Wire.Msg
    ( sendMsg
    , recvMsg
    , CMsg (..)
    )
where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Foreign hiding (void)
import Foreign.C
import Network.Socket
import Network.Socket.Internal

#include <sys/types.h>
#include <sys/socket.h>
#include <cmsg.h>

{#pointer *msghdr as MsgHdrPtr -> MsgHdr#}
{#pointer *iovec as IoVecPtr -> IoVec#}

{#fun pure cmsg_firsthdr as ^
    { `MsgHdrPtr'
    } -> `Ptr CMsg' castPtr
#}

{#fun pure cmsg_nxthdr as ^
    {         `MsgHdrPtr'
    , castPtr `Ptr CMsg'
    } -> `Ptr CMsg' castPtr
#}

{#fun pure cmsg_space as ^
    { `Int'
    } -> `Int'
#}

{#fun pure cmsg_len as ^
    { `Int'
    } -> `Int'
#}

{#fun pure cmsg_data as cmsgPtr
    { castPtr `Ptr CMsg'
    } -> `Ptr a' castPtr
#}

{#fun sendmsg as c_sendmsg
    { fdSocket    `Socket'
    ,             `MsgHdrPtr'
    ,             `CInt'
    } -> `CLong'
#}

{#fun recvmsg as c_recvmsg
    { fdSocket      `Socket'
    ,               `MsgHdrPtr'
    ,               `CInt'
    } -> `CLong'
#}

data MsgHdr =
    MsgHdr { msgName       :: Ptr ()
           , msgNameLen    :: Int
           , msgIov        :: Ptr IoVec
           , msgIovLen     :: Int
           , msgControl    :: Ptr CMsg
           , msgControlLen :: Int
           , msgFlags      :: Int
           }

instance Storable MsgHdr where
    sizeOf _ = {#sizeof msghdr#}
    alignment _ = {#alignof msghdr#}

    peek mPtr =
        MsgHdr
        <$> {#get msghdr->msg_name#} mPtr
        <*> (fromIntegral <$> {#get msghdr->msg_namelen#} mPtr)
        <*> {#get msghdr->msg_iov#} mPtr
        <*> (fromIntegral <$> {#get msghdr->msg_iovlen#} mPtr)
        <*> (castPtr <$> {#get msghdr->msg_control#} mPtr)
        <*> (fromIntegral <$> {#get msghdr->msg_controllen#} mPtr)
        <*> (fromIntegral <$> {#get msghdr->msg_flags#} mPtr)

    poke mPtr hdr = do
        {#set msghdr->msg_name#} mPtr (msgName hdr)
        {#set msghdr->msg_namelen#} mPtr (fromIntegral $ msgNameLen hdr)
        {#set msghdr->msg_iov#} mPtr (msgIov hdr)
        {#set msghdr->msg_iovlen#} mPtr (fromIntegral $ msgIovLen hdr)
        {#set msghdr->msg_control#} mPtr (castPtr $ msgControl hdr)
        {#set msghdr->msg_controllen#} mPtr (fromIntegral $ msgControlLen hdr)
        {#set msghdr->msg_flags#} mPtr (fromIntegral $ msgFlags hdr)

data IoVec =
    IoVec { iovBase :: Ptr CChar
          , iovLen  :: Int
          }

instance Storable IoVec where
    sizeOf _ = {#sizeof iovec#}
    alignment _ = {#alignof iovec#}

    peek iPtr =
        IoVec
        <$> (castPtr <$> {#get iovec->iov_base#} iPtr)
        <*> (fromIntegral <$> {#get iovec->iov_len#} iPtr)

    poke iPtr vec = do
        {#set iovec->iov_base#} iPtr (castPtr $ iovBase vec)
        {#set iovec->iov_len#} iPtr (fromIntegral $ iovLen vec)

data CMsg =
    CMsg { cmsgLevel :: Int
         , cmsgType  :: Int
         , cmsgData  :: BS.ByteString
         }

peekCMsgs :: Ptr MsgHdr -> Ptr CMsg -> IO [CMsg]
peekCMsgs mPtr cPtr
  | cPtr == nullPtr = return []
  | otherwise       = do
    cmsg <- CMsg
        <$> (fromIntegral <$> {#get cmsghdr->cmsg_level#} cPtr)
        <*> (fromIntegral <$> {#get cmsghdr->cmsg_type #} cPtr)
        <*> peekByteString cPtr
    (cmsg :) <$> peekCMsgs mPtr (cmsgNxthdr mPtr cPtr)
    where
        peekByteString bsPtr = do
            len  <- fromIntegral <$> {#get cmsghdr->cmsg_len#} bsPtr
            BS.packCStringLen (cmsgPtr bsPtr, len)

pokeCMsgs :: Ptr MsgHdr -> [CMsg] -> Ptr CMsg -> IO ()
pokeCMsgs _ [] _ = return ()
pokeCMsgs mPtr (c:cs) cptr = do
    {#set cmsghdr->cmsg_len#}   cptr (fromIntegral . cmsgLen . BS.length $ cmsgData c)
    {#set cmsghdr->cmsg_level#} cptr (fromIntegral $ cmsgLevel c)
    {#set cmsghdr->cmsg_type#}  cptr (fromIntegral $ cmsgType c)
    BS.unsafeUseAsCStringLen (cmsgData c) (\(cstr, len) -> copyBytes (cmsgPtr cptr) cstr len)
    pokeCMsgs mPtr cs (cmsgNxthdr mPtr cptr)

allocaMsg :: Int -> (Ptr MsgHdr -> CStringLen -> IO a) -> IO a
allocaMsg bufSize f =
    allocaBytes bufSize $ \buf ->
    allocaBytes ctrlLen $ \ctrl ->
    with (IoVec buf bufSize) $ \ioVec ->
    with (MsgHdr nullPtr 0 ioVec 1 ctrl ctrlLen 0) $ \hdr ->
        f hdr (buf, bufSize)
    where
        ctrlLen = 1024

withMsg :: BS.ByteString -> [CMsg] -> (Ptr MsgHdr -> IO a) -> IO a
withMsg bs cmsgs f =
    BS.unsafeUseAsCStringLen bs $ \(buf, bufLen) ->
    allocaBytes (ctrlLen + ctrlPad) $ \ctrl ->
    with (IoVec buf bufLen) $ \ioVec ->
    with (MsgHdr nullPtr 0 ioVec 1 ctrl ctrlLen 0) $ \hdr ->
        pokeCMsgs hdr cmsgs ctrl >> f hdr
    where
        ctrlPad = 16 -- {#sizeof cmsghdr#} does not work, we have to use a hard-coded constant
        ctrlLen = sum (map (cmsgSpace . BS.length . cmsgData) cmsgs)

sendMsg :: Socket -> BS.ByteString -> [CMsg] -> IO ()
sendMsg s bs cmsgs = withMsg bs cmsgs $ \msg ->
    void . throwSocketErrorWaitWrite s "sendMsg" $ c_sendmsg s msg 0

recvMsg :: Socket -> Int -> IO (BS.ByteString, [CMsg])
recvMsg s bufSize = allocaMsg bufSize $ \ptr cstr -> do
    _              <- throwSocketErrorWaitRead s "recvMsg" $ c_recvmsg s ptr 0
    cmsgs          <- peekCMsgs ptr (cmsgFirsthdr ptr)
    bs             <- BS.packCStringLen cstr
    return (bs, cmsgs)
