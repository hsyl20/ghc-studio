* Use Text instead of String
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
* Interactive mode:
   * allow GHC pause/stop
   * filter logs on the fly until something is found (breakpoint)
   * start dumping logs at a given phase
* Automatic analysis
   * e.g., detect spikes in core size
* Support log interruption (phase closed with eof during parsing)

* Replace some text messages with an ADT to avoid parsing
   * E.g., phase stop could be: PhaseStop Float Float

* Improve "Compiling..." page
   * e.g.., Show the current module/phase/etc.

# TODO in GHC

* Convert static flags into dynamic flags
   So that we can use them with GHC API
* Clearly separate GHC program and GHC API
   * E.g. separate command-line flags and compiler options/state (currently DynFlags)
   * Add hooks so that dump IO is performed by the program, not the API
   * Make "verbosity" a program option only, not an API option
* Enhance GHC log generation
   * Typed logs (ADT for logs)
* Make GHC purer
   * better separation between pure and IO code
* Integrated tracing mechanism
* Customizable Finder (à la Java's Class loader)
   * load from in-memory sources
