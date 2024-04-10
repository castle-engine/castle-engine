# Convert any file to data URI scheme

This command-line application reads the data from file (given as command-line parameter) and outputs "data URI" for it.

## What is "data URI"?

The "data URI" is a string you can use in [Castle Game Engine](https://castle-engine.io/), [X3D and VRML](https://castle-engine.io/vrml_x3d.php), HTML and CSS whenever they expect a URI.

For example, usually in X3D you specify a texture like this:

```
texture ImageTexture { url "my_texture.png" } # relative URL
```

or like this:

```
texture ImageTexture { url "https://example.org/my_texture.png" } # absolute URL
```

But thanks to _data URI scheme_, you can also specify the texture directly in the X3D file:

```
texture ImageTexture { url "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAQA..." }
```

This application allows you to convert any file to such a data URI, visible in the last example as `data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAQA...`

It can be used to encode images, sounds, 3D models, whatever as data URI.

See http://en.wikipedia.org/wiki/Data_URI_scheme for description of data URIs.

## Usage

Pass input file as a command-line parameter. The output is written to standard output. Example usage:

```
to-data-uri my_texture.png > my_texture_data_uri.txt
```

Command-line parameter is used with our [Download](https://castle-engine.io/apidoc/html/CastleDownload.html#Download-String-TStreamOptions-) routine. See [Networking in Castle Game Engine](https://castle-engine.io/manual_network.php) for more information about supported URLs.

So the given parameter may be:
- just a filename
- or an URL: a file URL, http URL (will be automatically downloaded) or even another data URI.

The _MIME type_ (necessary to output nice data URI) is also detected by our `Download` routine. For http, it may be returned by http server. For file, it's guessed based on file extension.