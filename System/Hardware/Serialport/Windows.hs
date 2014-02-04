{-# LANGUAGE DeriveDataTypeable, ForeignFunctionInterface #-}
{-# OPTIONS_HADDOCK hide #-}
module System.Hardware.Serialport.Windows where

import Data.Bits
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Unsafe as BU
import qualified System.Win32.Comm as Comm
import System.Win32.Types
import System.Win32.File
import Foreign.Marshal.Alloc
import System.Hardware.Serialport.Types
import Control.Applicative
import Control.Monad
import Control.Exception (onException)
import Data.IORef
import GHC.IO.Handle
import GHC.IO.Device
import GHC.IO.BufferedIO
import Data.Typeable
import GHC.IO.Buffer
import Foreign.Ptr
import Foreign.Storable

data SerialPort = SerialPort {
                      handle :: HANDLE,
                      portSettings :: IORef SerialPortSettings
                  }
                  deriving (Typeable)


instance RawIO SerialPort where
  read (SerialPort h _) ptr n = return . fromIntegral =<< safe_ReadFile h ptr (fromIntegral n) Nothing
  readNonBlocking _ _ _ = error "readNonBlocking not implemented"
  write (SerialPort h _) ptr n = void (safe_WriteFile h ptr (fromIntegral n) Nothing)
  writeNonBlocking _ _ _ = error "writenonblocking not implemented"


instance IODevice SerialPort where
  ready _ _ _ = return True
  close = closeSerial
  isTerminal _ = return False
  isSeekable _ = return False
  seek _ _ _ = return ()
  tell _ = return 0
  getSize _ = return 0
  setSize _ _ = return ()
  setEcho _ _ = return ()
  getEcho _ = return False
  setRaw _ _ = return ()
  devType _ = return Stream


instance BufferedIO SerialPort where
  newBuffer _ = newByteBuffer 100
  fillReadBuffer = readBuf
  fillReadBuffer0 = readBufNonBlocking
  flushWriteBuffer = writeBuf
  flushWriteBuffer0 = writeBufNonBlocking


-- |Open and configure a serial port returning a standard Handle.
hOpenSerial :: String
           -> SerialPortSettings
           -> IO Handle
hOpenSerial dev settings = do
  ser <- openSerial dev settings
  h <- mkDuplexHandle ser dev Nothing noNewlineTranslation
  hSetBuffering h NoBuffering
  return h


-- | Open and configure a serial port
openSerial :: String      -- ^ Serial port, such as @COM5@ or @CNCA0@
           -> SerialPortSettings
           -> IO SerialPort
openSerial dev settings = do
    h <- createFile ("\\\\.\\" ++ dev) access_mode share_mode security_attr create_mode file_attr template_file
    serial_port <- SerialPort h <$> newIORef defaultSerialSettings
    do
        Comm.setCommTimeouts h $ Comm.COMMTIMEOUTS {
                    Comm.readIntervalTimeout = maxBound,
                    Comm.readTotalTimeoutMultiplier = maxBound,
                    Comm.readTotalTimeoutConstant = 200,
                    Comm.writeTotalTimeoutMultiplier = 0,
                    Comm.writeTotalTimeoutConstant = 0 }
        setSerialSettings serial_port settings
      `onException` closeHandle h
    return serial_port
  where
    access_mode = gENERIC_READ .|. gENERIC_WRITE
    share_mode = fILE_SHARE_NONE
    security_attr = Nothing
    create_mode = oPEN_EXISTING
    file_attr = 0
    template_file = Nothing


safe_ReadFile :: HANDLE -> Ptr a -> DWORD -> Maybe LPOVERLAPPED -> IO DWORD
safe_ReadFile h buf n mb_over =
  alloca $ \ p_n -> do
  failIfFalse_ "ReadFile" $ c_safe_ReadFile h buf n p_n (maybePtr mb_over)
  peek p_n
foreign import stdcall safe "windows.h ReadFile"
  c_safe_ReadFile :: HANDLE -> Ptr a -> DWORD -> Ptr DWORD -> LPOVERLAPPED -> IO Bool

safe_WriteFile :: HANDLE -> Ptr a -> DWORD -> Maybe LPOVERLAPPED -> IO DWORD
safe_WriteFile h buf n mb_over =
  alloca $ \ p_n -> do
  failIfFalse_ "WriteFile" $ c_safe_WriteFile h buf n p_n (maybePtr mb_over)
  peek p_n
foreign import stdcall safe "windows.h WriteFile"
  c_safe_WriteFile :: HANDLE -> Ptr a -> DWORD -> Ptr DWORD -> LPOVERLAPPED -> IO Bool


-- |Receive bytes, given the maximum number
recv :: SerialPort -> Int -> IO B.ByteString
recv port@(SerialPort h _) n = do
    mOut <- allocaBytes n $ \p -> do
        recv_cnt <- safe_ReadFile h p count overlapped
        if recv_cnt == 0
            then pure Nothing
            else Just <$> B.packCStringLen (p, fromIntegral recv_cnt)
    case mOut of
        Just out -> pure out
        Nothing  -> recv port n
  where
    count = fromIntegral n
    overlapped = Nothing


-- |Send bytes
send :: SerialPort
        -> B.ByteString
        -> IO Int          -- ^ Number of bytes actually sent
send (SerialPort h _) msg =
  BU.unsafeUseAsCString msg $ \p ->
    fromIntegral `fmap` safe_WriteFile h p count overlapped
  where
    count = fromIntegral $ B.length msg
    overlapped = Nothing


-- |Flush buffers
flush :: SerialPort -> IO ()
flush (SerialPort h _) =
  flushFileBuffers h
  {- TO DO: Fix
  >> consumeIncomingChars
  where
    consumeIncomingChars = do
      ch <- recv s 1
      unless (ch == B.empty) consumeIncomingChars
  -}

-- |Close the serial port
closeSerial :: SerialPort -> IO ()
closeSerial = closeHandle . handle


-- |Set the Data Terminal Ready level
setDTR :: SerialPort -> Bool -> IO ()
setDTR (SerialPort h _ ) True =  Comm.escapeCommFunction h Comm.setDTR
setDTR (SerialPort h _ ) False =  Comm.escapeCommFunction h Comm.clrDTR


-- |Set the Ready to send level
setRTS :: SerialPort -> Bool -> IO ()
setRTS (SerialPort h _ ) True = Comm.escapeCommFunction h Comm.setRTS
setRTS (SerialPort h _ ) False = Comm.escapeCommFunction h Comm.clrRTS


-- |Configure the serial port
setSerialSettings :: SerialPort           -- ^ The currently opened serial port
                  -> SerialPortSettings   -- ^ The new settings
                  -> IO ()                -- ^ New serial port
setSerialSettings (SerialPort h _) new_settings =
  Comm.setCommState h new_settings


-- |Get configuration from serial port
getSerialSettings :: SerialPort -> IO SerialPortSettings
getSerialSettings = readIORef . portSettings
