{-# OPTIONS_HADDOCK hide #-}
module System.Hardware.Serialport.Types where

import Data.Word


-- | Supported baudrates
data CommSpeed
  = CS110
  | CS300
  | CS600
  | CS1200
  | CS2400
  | CS4800
  | CS9600
  | CS19200
  | CS38400
  | CS57600
  | CS115200
  deriving (Show, Eq)


data StopBits = One | Two deriving (Show, Eq)
data Parity = Even | Odd | NoParity deriving (Show, Eq)
data FlowControl = Software | NoFlowControl deriving (Show, Eq)

data SerialPortSettings = SerialPortSettings {
                      commSpeed    :: CommSpeed,   -- ^ baudrate
                      bitsPerWord  :: Word8,       -- ^ Number of bits in a word
                      stopb        :: StopBits,    -- ^ Number of stop bits
                      parity       :: Parity,      -- ^ Type of parity
                      flowControl  :: FlowControl  -- ^ Type of flowcontrol
                  }


-- | Most commonly used configuration
--
--  - 9600 baud
--
--  - 8 data bits
--
--  - 1 stop bit
--
--  - no parity
--
--  - no flow control
--
defaultSerialSettings :: SerialPortSettings
defaultSerialSettings =
  SerialPortSettings CS9600 8 One NoParity NoFlowControl

