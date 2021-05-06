## fennel-conditions v0.0.8 (2021-05-06)

- Fix nested `handler-bind` behavior when raising condition inside the handler

## fennel-conditions v0.0.7 (2021-05-05)

- Fix nested `handler-bind` behavior when declining restarts

## fennel-conditions v0.0.6 (2021-05-04)

- Handle Lua errors as conditions of types `fennel-conditions/error` and `fennel-conditions/condition`.
- Add `unwind-protect` and change how `handler-bind` works to match `unwind-protect` semantics.

## fennel-conditions v0.0.5 (2021-05-04)

- Better multiple-values handling.
- New `ignore-errors` macro.
- Default inheritance model for all conditions:
  - Conditions raised with `error` or `cerror` derive from `fennel-conditions/error` and `fennel-conditions/condition`.
  - Conditions raised with `warn` derive from `fennel-conditions/warning`  and `fennel-conditions/condition`.
  - Conditions raised with `signal` derive from `fennel-conditions/condition`.

## fennel-conditions v0.0.4 (2021-05-02)

- Restarts return itself as a handler functions to clean up dynamic scope before invocation.
- Put everything back to single module for easier `--require-as-include` support.

## fennel-conditions v0.0.3 (2021-05-02)

- Rewritten handler and restart implementations.
- Multi-level interactive debugger.
- Added tests.
- Separated code to different modules.

## fennel-conditions v0.0.2 (2021-04-27)

- Feature interactive debugger (quite limited)

## fennel-conditions v0.0.1 (2021-04-24)

Initial release of fennel-conditions library.
