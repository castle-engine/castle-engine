# Register custom URL protocol handler

Demo of registering custom URL protocol handler.

1. We register `my-packed-data:/` protocol to read data from a ZIP file. The demo code shows 2 approaches to register the handler:

    - When `USE_ZIP_URL_HANDLER` symbol is not defined: Use a generic way to register URL handler using `CastleDownload.RegisterUrlProtocol`.

    - When `USE_ZIP_URL_HANDLER` symbol is defined: Use `TCastleZip` and `TCastleZip.RegisterUrlProtocol` to register the handler. This is simpler and more efficient, but it serves a more specific use case: it allows to read contents from a ZIP file by accessing `my-packed-data:/...` URLs.

2. Furthermore, we use `ApplicationDataOverride` to load game data from this protocol. This way `castle-data:/my_image.png` will actually get data from `my-packed-data:/my_image.png`.

    This is done in the same way, regardless of whether `USE_ZIP_URL_HANDLER` symbol was defined or not, so regardless of how the `my-packed-data:/...` protocol was registered.

In effect, we read game data from a ZIP file, but you use `castle-data:/` as usual.

## TODO

Migrate to use UI designed by CGE editor and use TCastleView.
