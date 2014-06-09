`llvm-general-util` is a higher-level wrapper around Benjamain
Scarlet's [`llvm-general`](https://github.com/bscarlet/llvm-general)
Haskell bindings to [LLVM](http://llvm.org). Its principal
contributions are:

  * Algebraic data types representing targets, CPUs, CPU features, and
    triples (i.e., arguments to `-march=` / `-mcpu=` / `-mattr=` /
    `-mtriple=`).

  * Monadic encapsulation of bracketed resources like `Context` and
    `TargetMachine`, so that you aren't writing `withFoo` all the
    time.
    
  * A uniform error-handling mechanism.
 
From the user's point of view, `llvm-general-util` is intended to
serve as a *replacement* for `llvm-general`: although some parts of
`llvm-general` are re-exported, it should never be necessary import
anything from `llvm-general` directly. If you need functionality from
`llvm-general` which you believe `llvm-general-util` does not expose,
please file a bug ticket explaining why. (These remarks pertain only
to `llvm-general` proper, and not to `llvm-general-pure`).
