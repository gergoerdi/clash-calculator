{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Calculator.Serial where

import Clash.Prelude
import RetroClash.Utils
import Data.Char
import Control.Monad.State

import Calculator.ALU
import Calculator.State

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
