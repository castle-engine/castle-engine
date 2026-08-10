# Test time measurements

Test various time measurement routines from `CastleTimeUtils`:

- `Timer`
- `ProcessTimer`
- `CastleGetTickCount64` (just a wrapper around standard `GetTickCount64`, except some special platforms)

Can be run on any platform.

- Including standard Linux, Windows, macOS.

- Including [web](https://castle-engine.io/web). Runs in both `wasmtime` and in the real WWW browser.

    Note: `Sleep` in a real web browser doesn't work, likely related to the error in console `Unimplemented: TPas2JSWASIEnvironment.poll_oneoff`. So the first measurements will show that `Sleep(2500)` takes zero time. While we could possibly fix it, making WASM code wait for JS to report that 2500 has passed... but for now you can just ignore it. We don't really care whether the standard `Sleep` works (we don't need it in actual games), the important fact is that our time measurement routines work. The second measurement will show some non-zero time for a sample _"some intensive operation"_.