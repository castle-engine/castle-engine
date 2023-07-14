Demo of using CastleDownload.RegisterUrlProtocol to handle custom protocol.
In this example, we register `my-packed-data:/` protocol to read data from a ZIP file.

Furthermore, you can use ApplicationDataOverride to load game data from
this protocol. This way `castle-data:/my_image.png` will actually get
data from `my-packed-data:/my_image.png`.
