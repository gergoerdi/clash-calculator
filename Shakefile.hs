{-# LANGUAGE RecordWildCards, NumericUnderscores #-}
import Clash.Shake
import Clash.Shake.Xilinx as Xilinx
import qualified Clash.Shake.SymbiFlow as SymbiFlow

import Development.Shake
import Development.Shake.FilePath
import System.Console.GetOpt
import Data.Foldable (forM_)

outDir :: FilePath
outDir = "_build"

data Flags = UseSymbiFlow deriving Eq

flags = [Option "" ["symbiflow"] (NoArg $ Right UseSymbiFlow) "Use SymbiFlow instead of vendor toolchain"]

main :: IO ()
main = shakeArgsWith shakeOptions{ shakeFiles = outDir } flags $ \flags targets -> pure $ Just $ do
    useConfig "build.mk"

    let useSymbiFlow = UseSymbiFlow `elem` flags
        xilinx7
          | useSymbiFlow = SymbiFlow.xilinx7
          | otherwise = Xilinx.vivado

    let boards =
            [ ("nexys-a7-50t", xilinx7 nexysA750T,    100_000_000)
            , ("papilio-pro",  Xilinx.ise papilioPro,  32_000_000)
            , ("papilio-one",  Xilinx.ise papilioOne,  32_000_000)
            ]

    let rules = do
        phony "clean" $ do
            putNormal $ "Cleaning files in " <> outDir
            removeFilesAfter outDir [ "//*" ]

        forM_ boards $ \(name, synth, clock) -> do
            let targetDir = outDir </> name

            kit@ClashKit{..} <- clashRules (targetDir </> "clash") Verilog
                [ "src" ]
                "Calculator"
                [ "-Wno-partial-type-signatures"
                , "-Wunused-imports"
                , "-D__NATIVE_CLOCK__=" <> show clock
                ] $
                return ()
            SynthKit{..} <- synth kit (targetDir </> "synth") ("target" </> name) "Top"

            mapM_ (uncurry $ nestedPhony name) $
              ("clashi", clash ["--interactive", "src/Calculator.hs"]) :
              ("bitfile", need [bitfile]):
              phonies

    if null targets then rules else want targets >> withoutActions rules
