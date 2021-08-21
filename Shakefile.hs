{-# LANGUAGE RecordWildCards, NumericUnderscores #-}
import Clash.Shake
import Clash.Shake.Xilinx

import Development.Shake
import Development.Shake.FilePath
import Data.Foldable (forM_)

targets =
    [ ("nexys-a7-50t", xilinxVivado nexysA750T, 100_000_000)
    , ("papilio-pro",  xilinxISE papilioPro,     32_000_000)
    , ("papilio-one",  xilinxISE papilioOne,     32_000_000)
    ]

outDir :: FilePath
outDir = "_build"

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir } $ do
    useConfig "build.mk"

    phony "clean" $ do
        putNormal $ "Cleaning files in " <> outDir
        removeFilesAfter outDir [ "//*" ]

    forM_ targets $ \(name, synth, clock) -> do
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
