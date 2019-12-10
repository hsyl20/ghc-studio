module Profiles where

import GHC
import DynFlags
import Data.List (foldl')
import qualified Data.Map  as Map

data CompilationProfile = CompilationProfile
   { profileName  :: String
   , profileDesc  :: String
   , profileFlags :: DynFlags -> DynFlags
   }

defaultProfiles :: [CompilationProfile]
defaultProfiles =
   [ CompilationProfile
      { profileName  = "Timings - O0"
      , profileDesc  = "Use this pass to detect the most expensive phases"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 0
         $ dflags
            { verbosity = 2 -- -dshow-passes is -v2 in fact
            }
      }
   , CompilationProfile
      { profileName  = "Timings - O1"
      , profileDesc  = "Use this pass to detect the most expensive phases"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 1
         $ dflags
            { verbosity = 2 -- -dshow-passes is -v2 in fact
            }
      }
   , CompilationProfile
      { profileName  = "Timings - O2"
      , profileDesc  = "Use this pass to detect the most expensive phases"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 2
         $ dflags
            { verbosity = 2 -- -dshow-passes is -v2 in fact
            }
      }
   , CompilationProfile
      { profileName  = "Timings - O2 LLVM"
      , profileDesc  = "Use this pass to detect the most expensive phases"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 2
         $ dflags
            { verbosity = 2 -- -dshow-passes is -v2 in fact
            , hscTarget = HscLlvm
            }
      }
   , CompilationProfile
      { profileName  = "Debug core - O2"
      , profileDesc  = "Use this pass for core-passes debugging"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 2
         $ dflags
            { verbosity = 2
            } `gopt_set` Opt_DoCoreLinting
              `dopt_set` Opt_D_verbose_core2core
      }
   , CompilationProfile
      { profileName  = "Debug type-checker - O0"
      , profileDesc  = "Use this pass for type-checker debugging"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 0
         $ dflags
            { verbosity = 2
            } `dopt_set` Opt_D_dump_tc
              `dopt_set` Opt_D_dump_tc_trace
      }
   , CompilationProfile
      { profileName  = "Debug type-checker - O1"
      , profileDesc  = "Use this pass for type-checker debugging"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 1
         $ dflags
            { verbosity = 2
            } `dopt_set` Opt_D_dump_tc
              `dopt_set` Opt_D_dump_tc_trace
      }
   , CompilationProfile
      { profileName  = "Debug type-checker - O2"
      , profileDesc  = "Use this pass for type-checker debugging"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 2
         $ dflags
            { verbosity = 2
            } `dopt_set` Opt_D_dump_tc
              `dopt_set` Opt_D_dump_tc_trace
      }
   , CompilationProfile
      { profileName  = "Debug type-checker - O2 DEBUG"
      , profileDesc  = "Use this pass for type-checker debugging"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 2
         $ dflags
            { verbosity = 2
            } `dopt_set` Opt_D_dump_tc
              `dopt_set` Opt_D_dump_tc_trace
              `dopt_set` Opt_D_ppr_debug
      }
   , CompilationProfile
      { profileName  = "Debug simplifier - O0"
      , profileDesc  = "Use this pass for simplifier debugging"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 0
         $ dflags
            { verbosity = 2
            } `dopt_set` Opt_D_dump_simpl
              `dopt_set` Opt_D_dump_simpl_iterations
              `dopt_set` Opt_D_dump_simpl_stats
              `dopt_set` Opt_D_dump_simpl_trace
              `dopt_set` Opt_D_verbose_core2core
      }
   , CompilationProfile
      { profileName  = "Debug simplifier - O1"
      , profileDesc  = "Use this pass for simplifier debugging"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 1
         $ dflags
            { verbosity = 2
            } `dopt_set` Opt_D_dump_simpl
              `dopt_set` Opt_D_dump_simpl_iterations
              `dopt_set` Opt_D_dump_simpl_stats
              `dopt_set` Opt_D_dump_simpl_trace
              `dopt_set` Opt_D_verbose_core2core
      }
   , CompilationProfile
      { profileName  = "Debug simplifier - O2"
      , profileDesc  = "Use this pass for simplifier debugging"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 2
         $ dflags
            { verbosity = 2
            } `dopt_set` Opt_D_dump_simpl
              `dopt_set` Opt_D_dump_simpl_iterations
              `dopt_set` Opt_D_dump_simpl_stats
              `dopt_set` Opt_D_dump_simpl_trace
              `dopt_set` Opt_D_verbose_core2core
      }
   , CompilationProfile
      { profileName  = "Dump almost everything (no trace) - O0"
      , profileDesc  = "Use this pass to dump most logs"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 0
         $ dflags
            { verbosity = 5
            } `dopt_set` Opt_D_dump_tc
              `dopt_set` Opt_D_dump_splices
              `dopt_set` Opt_D_dump_rn
              `dopt_set` Opt_D_dump_rule_firings
              `dopt_set` Opt_D_dump_rule_rewrites
              `dopt_set` Opt_D_dump_inlinings
              `dopt_set` Opt_D_dump_core_stats
              `dopt_set` Opt_D_dump_asm_stats
              `dopt_set` Opt_D_dump_rn_stats
              `dopt_set` Opt_D_dump_simpl_iterations
              `dopt_set` Opt_D_dump_view_pattern_commoning
      }
   , CompilationProfile
      { profileName  = "Dump almost everything (no trace) - O1"
      , profileDesc  = "Use this pass to dump most logs"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 1
         $ dflags
            { verbosity = 5
            } `dopt_set` Opt_D_dump_tc
              `dopt_set` Opt_D_dump_splices
              `dopt_set` Opt_D_dump_rn
              `dopt_set` Opt_D_dump_rule_firings
              `dopt_set` Opt_D_dump_rule_rewrites
              `dopt_set` Opt_D_dump_inlinings
              `dopt_set` Opt_D_dump_core_stats
              `dopt_set` Opt_D_dump_asm_stats
              `dopt_set` Opt_D_dump_rn_stats
              `dopt_set` Opt_D_dump_simpl_iterations
              `dopt_set` Opt_D_dump_view_pattern_commoning
      }
   , CompilationProfile
      { profileName  = "Dump almost everything (no trace) - O2"
      , profileDesc  = "Use this pass to dump most logs"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 2
         $ dflags
            { verbosity = 5
            } `dopt_set` Opt_D_dump_tc
              `dopt_set` Opt_D_dump_splices
              `dopt_set` Opt_D_dump_rn
              `dopt_set` Opt_D_dump_rule_firings
              `dopt_set` Opt_D_dump_rule_rewrites
              `dopt_set` Opt_D_dump_inlinings
              `dopt_set` Opt_D_dump_core_stats
              `dopt_set` Opt_D_dump_asm_stats
              `dopt_set` Opt_D_dump_rn_stats
              `dopt_set` Opt_D_dump_simpl_iterations
              `dopt_set` Opt_D_dump_view_pattern_commoning
      }
   , CompilationProfile
      { profileName  = "Dump almost everything (no trace) - O2 LLVM"
      , profileDesc  = "Use this pass to dump most logs with LLVM"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 2
         $ dflags
            { verbosity = 5
            , hscTarget = HscLlvm
            } `dopt_set` Opt_D_dump_tc
              `dopt_set` Opt_D_dump_splices
              `dopt_set` Opt_D_dump_rn
              `dopt_set` Opt_D_dump_rule_firings
              `dopt_set` Opt_D_dump_rule_rewrites
              `dopt_set` Opt_D_dump_inlinings
              `dopt_set` Opt_D_dump_core_stats
              `dopt_set` Opt_D_dump_asm_stats
              `dopt_set` Opt_D_dump_rn_stats
              `dopt_set` Opt_D_dump_simpl_iterations
              `dopt_set` Opt_D_dump_view_pattern_commoning
      }
   ]

enableWarningGroup :: String -> DynFlags -> DynFlags
enableWarningGroup groupName dflags = case Map.lookup groupName groups of
      Nothing   -> error $ "Invalid warning flag group: " ++ show groupName
                     ++ ". Expecting one of: " ++ show (Map.keys groups)
      Just flgs -> foldl' wopt_set dflags flgs
   where
      groups = Map.fromList warningGroups
