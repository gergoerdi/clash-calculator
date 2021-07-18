{-# LANGUAGE RecordWildCards #-}
module Calculator.State where

import Clash.Prelude
import Calculator.ALU
import Data.Maybe
import Data.Char (intToDigit)
import qualified Data.List as L

data Op
    = Add
    | Subtract
    deriving (Show, Generic, NFDataX)

data St n = MkSt
    { value :: BCD n
    , opBuf :: Op
    , inputBuf :: Maybe (BCD n)
    }
    deriving (Show, Generic, NFDataX)

initSt :: (KnownNat n) => St n
initSt = MkSt{ value = repeat 0, opBuf = Add, inputBuf = Nothing }

data Cmd
    = Digit Digit
    | Op Op
    | Backspace
    | Clear
    | Equals
    deriving (Show, Generic, NFDataX)

update :: (KnownNat n) => Cmd -> St n -> St n
update Clear _ = initSt
update (Digit d) s@MkSt{..} = s{ inputBuf = Just $ fromMaybe (repeat 0) inputBuf <<+ d }
update Backspace s@MkSt{..} = s{ inputBuf = Just $ 0 +>> fromMaybe (repeat 0) inputBuf }
update Equals s@MkSt{..} = compute s
update (Op op) s = (compute s){ opBuf = op }

compute :: (KnownNat n) => St n -> St n
compute s@MkSt{..} = s{ value = newValue, inputBuf = Nothing }
  where
    newValue = case opBuf of
        Add -> maybe value (addBCD value) inputBuf
        Subtract -> maybe value (subBCD value) inputBuf

removeLeadingZeroes :: (KnownNat n) => Vec n Digit -> Vec n (Maybe Digit)
removeLeadingZeroes digits = case mapAccumL step False digits of
    (False, _) -> repeat Nothing <<+ Just 0
    (True, digits') -> digits'
  where
    step False 0 = (False, Nothing)
    step _ d = (True, Just d)

displayedDigits :: (KnownNat n) => St n -> Vec n (Maybe Digit)
displayedDigits MkSt{..} = removeLeadingZeroes $ fromMaybe value inputBuf

prettyDigits :: Vec n (Maybe Digit) -> String
prettyDigits = L.map (maybe ' ' (intToDigit . fromIntegral)) . toList

-- L.foldl update (initS @4) [Digit 1, Digit 2, Digit 3, Digit 4, Backspace, Op Add, Digit 3, Equals]

