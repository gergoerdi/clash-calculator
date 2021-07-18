{-# LANGUAGE PartialTypeSignatures, NumericUnderscores, ApplicativeDo, RecordWildCards #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns, TemplateHaskell #-}
module Calculator where

import Clash.Prelude
import Clash.Annotations.TH
import RetroClash.Utils
import RetroClash.SevenSegment
import RetroClash.Keypad
import RetroClash.Clock
import RetroClash.SerialRx
import RetroClash.SerialTx
import Control.Monad
import Data.Maybe
import Data.Char
import Control.Monad.State

import Calculator.ALU
import Calculator.State
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

pattern ByteChar c <- (chr . fromIntegral -> c) where
  ByteChar = fromIntegral . ord

byteToCmd :: Unsigned 8 -> Maybe Cmd
byteToCmd b@(ByteChar c) | '0' <= c && c <= '9' = Just . Digit $ fromIntegral $ b - ByteChar '0'
byteToCmd (ByteChar '+') = Just $ Op Add
byteToCmd (ByteChar '-') = Just $ Op Subtract
byteToCmd (ByteChar '=') = Just Equals
byteToCmd (ByteChar '\r') = Just Equals
byteToCmd (ByteChar '\DEL') = Just Clear
byteToCmd (ByteChar '\b') = Just Backspace
byteToCmd _ = Nothing

serialDisplay
    :: forall n dom. (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Vec n (Maybe Digit))
    -> Signal dom (Maybe (Unsigned 8))
serialDisplay ack digits = mealyStateB step (Nothing @(Index (7 + n)), repeat 0) (ack, digits)
  where
    step (next, digits) = do
        (i, bs) <- get
        case i of
            Nothing -> do
                let bs' = clear ++ map fromDigit digits
                when (bs /= bs') $ put (Just 0, bs')
                return Nothing
            Just i -> do
                when next $ put (succIdx i, rotateLeftS bs (SNat @1))
                return . Just $ head bs

    fromDigit :: Maybe Digit -> Unsigned 8
    fromDigit = maybe (ByteChar ' ') $ \n -> ByteChar '0' + fromIntegral n

    clear :: Vec 7 (Unsigned 8)
    clear =
        0x1b :> ByteChar '[' :> ByteChar '2' :> ByteChar 'J' :> -- clear screen
        0x1b :> ByteChar '[' :> ByteChar 'H' :>                 -- cursor to home position
        Nil

logic
    :: forall n dom. (KnownNat n, HiddenClockResetEnable dom)
    => Signal dom (Maybe Cmd)
    -> Signal dom (Vec n (Maybe Digit))
logic = moore (flip $ maybe id update) displayedDigits initSt

type Hex = Unsigned 4

keyToCmd :: Hex -> Maybe Cmd
keyToCmd n | n <= 9 = Just $ Digit $ bitCoerce n
keyToCmd 0xa = Just $ Op Add
keyToCmd 0xb = Just Backspace
keyToCmd 0xc = Just Clear
keyToCmd 0xd = Just $ Op Subtract
keyToCmd 0xe = Just Equals
keyToCmd _ = Nothing

keymap :: Matrix 4 4 Hex
keymap =
    (0x1 :> 0x2 :> 0x3 :> 0xa :> Nil) :>
    (0x4 :> 0x5 :> 0x6 :> 0xb :> Nil) :>
    (0x7 :> 0x8 :> 0x9 :> 0xc :> Nil) :>
    (0x0 :> 0xf :> 0xe :> 0xd :> Nil) :>
    Nil

testInput :: Signal dom (Maybe Hex)
testInput = fromList . (<> L.repeat Nothing) . stretch $
    [ 9, 3, 0xa
    , 1, 4, 6, 0xb, 5, 0xd
    , 5, 3, 0xe
    ]
  where
    stretch = L.concatMap $ \x -> [Nothing, Just x, Nothing]

makeTopEntity 'topEntity
