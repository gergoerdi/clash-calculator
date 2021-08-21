{-# LANGUAGE CPP #-}
module Calculator where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.SevenSegment
import RetroClash.Keypad
import RetroClash.SerialRx
import RetroClash.SerialTx
import Control.Monad.State

import Calculator.ALU
import Calculator.State
import Calculator.Serial
import Calculator.Keypad
import qualified Data.List as L

topEntity
    :: "CLK" ::: Clock System
    -> "RX" ::: Signal System Bit
    -> "ROWS" ::: Signal System (Vec 4 (Active Low))
    -> ( "TX" ::: Signal System Bit
       , "SS" ::: Signal System (SevenSegment 4 Low Low Low)
       , "COLS" ::: Signal System (Vec 4 (Active Low))
       )
topEntity = withResetEnableGen board
  where
    board rx rows = (tx, display digits, cols)
      where
        digits = logic cmd
        cmd = mplus <$> cmdKey <*> cmdSerial

        (tx, ack) = serialTx (SNat @9600) (fmap bitCoerce <$> serialDisplay ack digits)
        cmdSerial = (byteToCmd . bitCoerce =<<) <$> (serialRx (SNat @9600) rx)

        input = inputKeypad keymap
        (cols, key) = input rows
        cmdKey = (keyToCmd =<<) <$> key

        display = driveSS (\x -> (encodeHexSS . bitCoerce $ x, False))

logic
    :: forall n dom. (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom (Maybe Cmd)
    -> Signal dom (Vec n (Maybe Digit))
logic = moore (flip $ maybe id update) displayedDigits initSt

testInput :: Signal dom (Maybe Hex)
testInput = fromList . (<> L.repeat Nothing) . stretch $
    [ 9, 3, 0xa
    , 1, 4, 6, 0xb, 5, 0xd
    , 5, 3, 0xe
    ]
  where
    stretch = L.concatMap $ \x -> [Nothing, Just x, Nothing]

makeTopEntity 'topEntity
