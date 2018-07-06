{-# LANGUAGE CPP #-}

#include "MachDeps.h"
#define HDLSYN Other

import Clash.Driver
import Clash.Driver.Types
import Clash.GHC.Evaluator
import Clash.GHC.GenerateBindings
import Clash.GHC.NetlistTypes
import Clash.Backend
import Clash.Backend.SystemVerilog
import Clash.Backend.VHDL
import Clash.Backend.Verilog
import Clash.Netlist.BlackBox.Types

import Control.DeepSeq
import qualified Data.Time.Clock as Clock

import Data.Text.Lazy
import Data.IntMap
import Data.HashMap.Lazy
import Clash.Core.TyCon
import Clash.Core.Term
import Clash.Core.Type
import           Clash.Annotations.TopEntity (TopEntity)
import           Clash.Primitives.Types  (PrimMap)

genSystemVerilog :: String
                 -> IO ()
genSystemVerilog = doHDL (initBackend WORD_SIZE_IN_BITS HDLSYN :: SystemVerilogState)

genVHDL :: String
        -> IO ()
genVHDL = doHDL (initBackend WORD_SIZE_IN_BITS HDLSYN :: VHDLState)

genVerilog :: String
           -> IO ()
genVerilog = doHDL (initBackend WORD_SIZE_IN_BITS HDLSYN :: VerilogState)

doHDL :: Backend s
       => s
       -> String
       -> IO ()
doHDL b src = do
  startTime <- Clock.getCurrentTime
  pd      <- primDirs b
  (bindingsMap,tcm,tupTcm,topEntities,primMap) <- generateBindings pd ["."] (hdlKind b) src Nothing
  prepTime <- startTime `deepseq` bindingsMap `deepseq` tcm `deepseq` Clock.getCurrentTime
  let prepStartDiff = Clock.diffUTCTime prepTime startTime
  putStrLn $ "Loading dependencies took " ++ show prepStartDiff
  generateHDL bindingsMap (Just b) primMap tcm tupTcm (ghcTypeToHWType WORD_SIZE_IN_BITS True) reduceConstant topEntities
    (ClashOpts 20 20 15 0 DebugApplied False True WORD_SIZE_IN_BITS Nothing HDLSYN True True False ["."]) (startTime,prepTime)
    -- (ClashOpts 20 20 15 0 DebugAll False True WORD_SIZE_IN_BITS Nothing HDLSYN True True False ["."]) (startTime,prepTime)

doBinding ::
  String
  -> IO (BindingMap,HashMap TyConOccName TyCon,IntMap TyConName
        ,[( TmName          -- topEntity bndr
          , Type            -- type of the topEntity bndr
          , Maybe TopEntity -- (maybe) TopEntity annotation
          , Maybe TmName)]  -- (maybe) associated testbench
        ,PrimMap Text)      -- The primitives found in '.' and 'primDir'
doBinding src = do
  let b = (initBackend WORD_SIZE_IN_BITS HDLSYN :: VHDLState)
  pd      <- primDirs b
  (bindingsMap,tcm,tupTcm,topEntities,primMap) <- generateBindings pd ["."] (hdlKind b) src Nothing
  return (bindingsMap,tcm,tupTcm,topEntities,primMap)


main :: IO ()
main = genVHDL "./examples/FIR.hs"
