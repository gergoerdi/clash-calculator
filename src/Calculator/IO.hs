module Calculator.IO where

import Prelude
import Calculator.State
import System.Terminal
import Control.Monad ((<=<))
import Control.Monad.Extra (loopM)
import Data.Char (digitToInt)
import Calculator (Hex, keyToCmd)

eventToCmd :: Event -> Maybe Cmd
eventToCmd (KeyEvent key mod) | mod == mempty = case key of
    CharKey c | '0' <= c && c <= '9' -> Just $ Digit . fromIntegral . digitToInt $ c
    CharKey '+' -> Just $ Op Add
    CharKey '-' -> Just $ Op Subtract
    CharKey '=' -> Just Equals
    EnterKey -> Just Equals
    BackspaceKey -> Just Backspace
    DeleteKey -> Just Clear
    _ -> Nothing
eventToCmd _ = Nothing

eventToCmd' :: Event -> Maybe Cmd
eventToCmd' = keyToCmd <=< eventToKey

eventToKey :: Event -> Maybe Hex
eventToKey (KeyEvent key mod) | mod == mempty = case key of
    CharKey c | c `elem` ['0'..'9'] -> Just $ fromIntegral . digitToInt $ c
    CharKey c | c `elem` ['a'..'f'] -> Just $ fromIntegral . digitToInt $ c
    _ -> Nothing
eventToKey _ = Nothing

main :: IO ()
main = withTerminal $ runTerminalT $ do
    putStringLn "Welcome to Calculator."
    hideCursor
    runCalculator
    showCursor
  where
    runCalculator = flip loopM (initSt @8) $ \st -> do
        display st
        ev <- awaitEvent
        return $ case ev of
            Left Interrupt -> Right ()
            Right ev -> Left $ processEvent ev st

    display st = do
        setCursorColumn 0
        putString $ prettyDigits . displayedDigits $ st
        flush

    processEvent ev = maybe id update (eventToCmd ev)
