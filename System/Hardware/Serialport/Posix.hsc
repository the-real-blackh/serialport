{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable  #-}
{-# OPTIONS_HADDOCK hide #-}
module System.Hardware.Serialport.Posix where

import Control.Applicative
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Unsafe as BU
import System.Posix.Types
import System.Posix.Terminal
import System.Hardware.Serialport.Types
import Foreign (Ptr, castPtr, alloca, peek, with)
import Foreign.C
import GHC.IO.Handle
import GHC.IO.Device
import GHC.IO.BufferedIO
import Data.IORef
import Data.Typeable
import GHC.IO.FD
import GHC.IO.IOMode
import Data.Bits
import Prelude hiding (read)


data SerialPort = SerialPort {
                         fd :: FD,
                         portSettings :: IORef SerialPortSettings
                     }
                     deriving (Typeable)

instance RawIO SerialPort where
  read s             = read (fd s)
  readNonBlocking s  = readNonBlocking (fd s)
  write s            = write (fd s)
  writeNonBlocking s = writeNonBlocking (fd s)

instance IODevice SerialPort where
  ready s        = ready (fd s)
  close s        = close (fd s)
  isTerminal s   = isTerminal (fd s)
  isSeekable s   = isSeekable (fd s)
  seek s         = seek (fd s)
  tell s         = tell (fd s)
  getSize s      = getSize (fd s)
  setSize s      = setSize (fd s)
  setEcho s      = setEcho (fd s)
  getEcho s      = getEcho (fd s)
  setRaw s       = setRaw (fd s)
  devType s      = devType (fd s)
  dup (SerialPort fd1 set)  = SerialPort <$> dup fd1 <*> pure set
  dup2 (SerialPort fd1 set) (SerialPort fd2 _) = SerialPort <$> dup2 fd1 fd2 <*> pure set

instance BufferedIO SerialPort where
  newBuffer s         = newBuffer (fd s)
  fillReadBuffer    s = fillReadBuffer (fd s)
  fillReadBuffer0   s = fillReadBuffer0 (fd s)
  flushWriteBuffer  s = flushWriteBuffer (fd s)
  flushWriteBuffer0 s = flushWriteBuffer0 (fd s)


-- |Open and configure a serial port returning a standard Handle
hOpenSerial :: FilePath
           -> SerialPortSettings
           -> IO Handle
hOpenSerial dev settings = do
  ser <- openSerial dev settings
  h <- mkDuplexHandle ser dev Nothing noNewlineTranslation
  hSetBuffering h NoBuffering
  return h


-- |Open and configure a serial port
openSerial :: FilePath            -- ^ Serial port, such as @\/dev\/ttyS0@ or @\/dev\/ttyUSB0@
           -> SerialPortSettings
           -> IO SerialPort
openSerial dev settings = do
  (fd', _) <- openFile dev ReadWriteMode False
  serial_port <- SerialPort fd' <$> newIORef defaultSerialSettings
  setSerialSettings serial_port settings
  return serial_port


foreign import ccall safe "read"
   c_safe_read :: CInt -> Ptr CChar -> CSize -> IO CSsize

-- |Receive bytes, given the maximum number
recv :: SerialPort -> Int -> IO B.ByteString
recv (SerialPort fd' _) n = do
  retRef <- newIORef undefined
  txt <- BI.create n $ \p -> do
      ret <- readRawBufferPtr "Serialport.recv" fd' p 0 (fromIntegral n)
      writeIORef retRef ret
  ret <- readIORef retRef
  return $ B.take (fromIntegral ret) txt


-- |Send bytes
send :: SerialPort
        -> B.ByteString
        -> IO Int          -- ^ Number of bytes actually sent
send (SerialPort fd' _) msg =
  fromIntegral <$> BU.unsafeUseAsCStringLen msg
     (\(p, len) -> writeRawBufferPtr "Serialport.send" fd' (castPtr p) 0 (fromIntegral len))


-- |Flush buffers
flush :: SerialPort -> IO ()
flush (SerialPort fd' _) =
  discardData (Fd (fdFD fd')) BothQueues


-- |Close the serial port
closeSerial :: SerialPort -> IO ()
closeSerial = close . fd


#include <sys/ioctl.h>

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

cIoctl' :: Fd -> Int -> Ptr d -> IO ()
cIoctl' f req =
  throwErrnoIfMinus1_ "ioctl" .
     c_ioctl (fromIntegral f) (fromIntegral req) . castPtr


getTIOCM :: Fd -> IO Int
getTIOCM fd' =
  alloca $ \p -> cIoctl' fd' #{const TIOCMGET} p >> peek p


setTIOCM :: Fd -> Int -> IO ()
setTIOCM fd' val =
  with val $ cIoctl' fd' #{const TIOCMSET}


-- |Set the Data Terminal Ready level
setDTR :: SerialPort -> Bool -> IO ()
setDTR (SerialPort fd' _) set = do
  current <- getTIOCM (Fd (fdFD fd'))
  setTIOCM (Fd (fdFD fd')) $ if set
                   then current .|. #{const TIOCM_DTR}
                   else current .&. complement #{const TIOCM_DTR}


-- |Set the Ready to send level
setRTS :: SerialPort -> Bool -> IO ()
setRTS (SerialPort fd' _) set = do
  current <- getTIOCM (Fd (fdFD fd'))
  setTIOCM (Fd (fdFD fd')) $ if set
                   then current .|. #{const TIOCM_RTS}
                   else current .&. complement #{const TIOCM_RTS}


-- |Configure the serial port
setSerialSettings :: SerialPort           -- ^ The currently opened serial port
                  -> SerialPortSettings   -- ^ The new settings
                  -> IO ()                -- ^ New serial port
setSerialSettings (SerialPort fd' settingsRef) new_settings = do
  termOpts <- getTerminalAttributes (Fd (fdFD fd'))
  let termOpts' = configureSettings termOpts new_settings
  setTerminalAttributes (Fd (fdFD fd')) termOpts' Immediately
  writeIORef settingsRef new_settings


-- |Get configuration from serial port
getSerialSettings :: SerialPort -> IO SerialPortSettings
getSerialSettings = readIORef . portSettings


withParity :: TerminalAttributes -> Parity -> TerminalAttributes
withParity termOpts Even =
    termOpts `withMode` EnableParity
             `withoutMode` OddParity
withParity termOpts Odd =
    termOpts `withMode` EnableParity
             `withMode` OddParity
withParity termOpts NoParity =
    termOpts `withoutMode` EnableParity


withFlowControl :: TerminalAttributes -> FlowControl -> TerminalAttributes
withFlowControl termOpts NoFlowControl =
    termOpts `withoutMode` StartStopInput
             `withoutMode` StartStopOutput
withFlowControl termOpts Software =
    termOpts `withMode` StartStopInput
             `withMode` StartStopOutput


withStopBits :: TerminalAttributes -> StopBits -> TerminalAttributes
withStopBits termOpts One =
    termOpts `withoutMode` TwoStopBits
withStopBits termOpts Two =
    termOpts `withMode` TwoStopBits


configureSettings :: TerminalAttributes -> SerialPortSettings -> TerminalAttributes
configureSettings termOpts settings =
    termOpts `withInputSpeed` commSpeedToBaudRate (commSpeed settings)
             `withOutputSpeed` commSpeedToBaudRate (commSpeed settings)
             `withBits` fromIntegral (bitsPerWord settings)
             `withStopBits` stopb settings
             `withParity` parity settings
             `withFlowControl` flowControl settings
             `withoutMode` EnableEcho
             `withoutMode` EchoErase
             `withoutMode` EchoKill
             `withoutMode` ProcessOutput
             `withoutMode` MapCRtoLF
             `withoutMode` EchoLF
             `withoutMode` HangupOnClose
             `withoutMode` KeyboardInterrupts
             `withoutMode` ExtendedFunctions
             `withMode` LocalMode
             `withMode` ReadEnable
             `withMode` ProcessInput


commSpeedToBaudRate :: CommSpeed -> BaudRate
commSpeedToBaudRate speed =
    case speed of
      CS110 -> B110
      CS300 -> B300
      CS600 -> B600
      CS1200 -> B1200
      CS2400 -> B2400
      CS4800 -> B4800
      CS9600 -> B9600
      CS19200 -> B19200
      CS38400 -> B38400
      CS57600 -> B57600
      CS115200 -> B115200


