* Add warmup for GHC
* Fix trace generation: merge them in a single "dump"
* Asm: don't dump liveness/registers/synthetic instructions for data (e.g.,
  closures)
* Report rewrite rules
* Report vectorisation
* Better tick count report
   * Add scrutinee constant folding tick
* Inlining reports
* Split renamer/typechecker in sub-phases?
* Split codegen in sub-phases
* Show transformation locations in IR codes
* Report module dependency graph
* Make finder use in-memory source files (+fforce-recomp)
* Bench compiled program perf
* Integrate cost centers
* Asm analysis (à la Maqao)
* Integrate perf counters
* Integrate ptrace
