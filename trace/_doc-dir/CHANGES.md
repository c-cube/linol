
# 0.7

- feat: add levels to `Trace_core`. Levels are similar to `logs` levels, to help control verbosity.
- add hmap as a depopt (#28)

- fix: truncate large strings in fuchsia

# 0.6

- add `ppx_trace` for easier instrumentation.
  * `let%trace span = "foo" in …` will enter a scope `span` named "foo"
  * `let%trace () = "foo" in …` will enter a scope named "foo" with a hidden name
- add `trace-fuchsia` backend, which produces traces in the binary format
    of [fuchsia](https://fuchsia.dev/fuchsia-src/reference/tracing/trace-format).
    These traces are reasonably efficient to produce (~60ns per span on my machines)
    and reasonably compact on disk, at least compared to the TEF backend.

# 0.5

- perf: reduce overhead in trace-tef
- perf: add Mpsc_queue, adapted from picos, to trace-tef

# 0.4

- add `?data` to `counter_int` and `counter_float`
- add `float` to user data
- add `add_data_to_current_span` and `add_data_to_manual_span`
- make `explicit_span.meta` mutable
- trace-tef: write to `trace.json` if env variable `TRACE` is either 1 or true
- trace-tef: emit function name, if provided, as a metadata key/value pair
- re-export trace.core in trace

- perf: in trace-tef, use broadcast instead of signal in the job queue

# 0.3

- add explicit spans, for more precise tracing
- rename repo to ocaml-trace
- trace-tef: add a ticker thread to ensure we flush the file regularly

# 0.2

- trace-tef: additional argument to `with_setup`; env for "stdout"/"stderr"
- refactor: avoid conflicting with stdlib `Trace` module by adding sublibrary `trace.core`.
    Programs that use `compiler-libs.toplevel` should use `trace.core`
    directly, because using `trace` will cause linking errors.
- perf(trace-tef): improve behavior of collector under contention by
    pulling all events at once in the worker

# 0.1

initial release
