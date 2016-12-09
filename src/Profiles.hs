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
      { profileName  = "Show passes - No optimisation"
      , profileDesc  = "Use this pass to detect the most expensive phases"
      , profileFlags = \dflags ->
         enableWarningGroup "all"
         $ updOptLevel 0
         $ dflags
            { verbosity = 2 -- -dshow-passes is -v2 in fact
            }
      }
   , CompilationProfile
      { profileName  = "Show passes - Some optimisations"
      , profileDesc  = "Use this pass to detect the most expensive phases"
      , profileFlags = \dflags -> 
         enableWarningGroup "all" 
         $ updOptLevel 1
         $ dflags
            { verbosity = 2 -- -dshow-passes is -v2 in fact
            }
      }
   , CompilationProfile
      { profileName  = "Show passes - Most optimisations"
      , profileDesc  = "Use this pass to detect the most expensive phases"
      , profileFlags = \dflags -> 
         enableWarningGroup "all" 
         $ updOptLevel 2
         $ dflags
            { verbosity = 2 -- -dshow-passes is -v2 in fact
            }
      }
   , CompilationProfile
      { profileName  = "Debug type-checker - No optimisation"
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
      { profileName  = "Debug type-checker - Some optimisations"
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
      { profileName  = "Debug type-checker - Most optimisations"
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
      { profileName  = "Debug simplifier - No optimisation"
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
      }
   , CompilationProfile
      { profileName  = "Debug simplifier - Some optimisations"
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
      }
   , CompilationProfile
      { profileName  = "Debug simplifier - Most optimisations"
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
      }
   , CompilationProfile
      { profileName  = "Dump almost everything (no trace) - No optimisation"
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
      { profileName  = "Dump almost everything (no trace) - Some optimisations"
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
      { profileName  = "Dump almost everything (no trace) - Most optimisations"
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
   ]

enableWarningGroup :: String -> DynFlags -> DynFlags
enableWarningGroup groupName dflags = case Map.lookup groupName groups of
      Nothing   -> error $ "Invalid warning flag group: " ++ show groupName
                     ++ ". Expecting one of: " ++ show (Map.keys groups)
      Just flgs -> foldl' wopt_set dflags flgs
   where
      groups = Map.fromList warningGroups


----------------------------------------------
-- TODO: remove these once GHC exports them


warningGroups :: [(String, [WarningFlag])]
warningGroups =
    [ ("compat",       minusWcompatOpts)
    , ("unused-binds", unusedBindsFlags)
    , ("default",      standardWarnings)
    , ("extra",        minusWOpts)
    , ("all",          minusWallOpts)
    , ("everything",   minusWeverythingOpts)
    ]


-- | Warnings enabled unless specified otherwise
standardWarnings :: [WarningFlag]
standardWarnings -- see Note [Documenting warning flags]
    = [ Opt_WarnOverlappingPatterns,
        Opt_WarnWarningsDeprecations,
        Opt_WarnDeprecatedFlags,
        Opt_WarnDeferredTypeErrors,
        Opt_WarnTypedHoles,
        Opt_WarnPartialTypeSignatures,
        Opt_WarnUnrecognisedPragmas,
        Opt_WarnDuplicateExports,
        Opt_WarnOverflowedLiterals,
        Opt_WarnEmptyEnumerations,
        Opt_WarnMissingFields,
        Opt_WarnMissingMethods,
        Opt_WarnWrongDoBind,
        Opt_WarnUnsupportedCallingConventions,
        Opt_WarnDodgyForeignImports,
        Opt_WarnInlineRuleShadowing,
        Opt_WarnAlternativeLayoutRuleTransitional,
        Opt_WarnUnsupportedLlvmVersion,
        Opt_WarnTabs,
        Opt_WarnUnrecognisedWarningFlags
      ]

-- | Things you get with -W
minusWOpts :: [WarningFlag]
minusWOpts
    = standardWarnings ++
      [ Opt_WarnUnusedTopBinds,
        Opt_WarnUnusedLocalBinds,
        Opt_WarnUnusedPatternBinds,
        Opt_WarnUnusedMatches,
        Opt_WarnUnusedForalls,
        Opt_WarnUnusedImports,
        Opt_WarnIncompletePatterns,
        Opt_WarnDodgyExports,
        Opt_WarnDodgyImports
      ]

-- | Things you get with -Wall
minusWallOpts :: [WarningFlag]
minusWallOpts
    = minusWOpts ++
      [ Opt_WarnTypeDefaults,
        Opt_WarnNameShadowing,
        Opt_WarnMissingSignatures,
        Opt_WarnHiShadows,
        Opt_WarnOrphans,
        Opt_WarnUnusedDoBind,
        Opt_WarnTrustworthySafe,
        Opt_WarnUntickedPromotedConstructors,
        Opt_WarnMissingPatternSynonymSignatures
      ]

-- | Things you get with -Weverything, i.e. *all* known warnings flags
minusWeverythingOpts :: [WarningFlag]
minusWeverythingOpts = [ toEnum 0 .. ]

-- | Things you get with -Wcompat.
--
-- This is intended to group together warnings that will be enabled by default
-- at some point in the future, so that library authors eager to make their
-- code future compatible to fix issues before they even generate warnings.
minusWcompatOpts :: [WarningFlag]
minusWcompatOpts
    = [ Opt_WarnMissingMonadFailInstances
      , Opt_WarnSemigroup
      , Opt_WarnNonCanonicalMonoidInstances
      ]

-- Things you get with -Wunused-binds
unusedBindsFlags :: [WarningFlag]
unusedBindsFlags = [ Opt_WarnUnusedTopBinds
                   , Opt_WarnUnusedLocalBinds
                   , Opt_WarnUnusedPatternBinds
                   ]
