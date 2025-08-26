# Vampyre Imaging Library (bundled with Castle Game Engine)

This is a local copy of [Vampyre Imaging Library](https://github.com/galfar/imaginglib).

## Why Vampyre Imaging Library?

_Castle Game Engine_ is using [Vampyre Imaging Library](https://github.com/galfar/imaginglib) to read and write many image formats, because:

- It makes our life easier :) We can delegate maintenance of image format support to Vampyre maintainers.

- It's a great library. The API is obvious. It supports lots of image formats. It supports loading and writing also float and 16-bit data, which is [useful for some purposes, like terrain heights](https://castle-engine.io/wp/2024/07/12/float-based-images-e-g-for-precise-terrains-full-support-4-float-based-image-classes-passed-to-gpu-and-shaders-loading-16-bit-png-loading-float-based-ktx-loading-16-32-bit-tiffs-castle-image/).

- It's cross-platform and works with both FPC and Delphi, just like our engine.

- It's relatively fast. E.g. loading PNG is 2x faster than using `FpImage` (which is important for some cases, when game loading "bottleneck" is at PNG loading). It's still not as fast as using C LibPng, but it's a good tradeoff.

See the [examples/images_videos/image_display](https://github.com/castle-engine/castle-engine/tree/master/examples/images_videos/image_display) for some image loading demo and speed measuments. See news about [initial integration with Vampyre (2021)](https://castle-engine.io/wp/2021/12/12/big-merge-delphi-compatibility-integration-with-vampyre-imaging-library-many-new-upgraded-examples-inspector-improvements-more/) and [more news](https://castle-engine.io/wp/2021/12/18/integration-with-vampyre-imaging-library-new-example-image_display-to-test-image-loading-speed-and-format-support/).

Note that some image formats are not enabled by default, this includes TIFF and JPEG2000. See [doc/miscellaneous_notes/additional_image_formats.md](https://github.com/castle-engine/castle-engine/blob/master/doc/miscellaneous_notes/additional_image_formats.md) for reasons and how to enable them.

## CGE image loading

All image saving and loading done through _Castle Game Engine_ `CastleImages` unit with image types like `TCastleImage`. Our routines like `LoadImage` and `SaveImage` just use _Vampyre Imaging Library_ internally.

## Cases when we don't use Vampyre

We do not use Vampyre for 100% of image loading and saving in CGE.

- We still prefer (if detected at runtime, or -- on some platforms --- statically linked) `LibPng` for loading PNG images. It is faster than Vampyre PNG loader.

- We have own code for 3D/game-specific formats like [KTX](https://castle-engine.io/ktx) and [DDS](https://castle-engine.io/dds).

## About this Vampyre code

We provide the Vampyre code inside CGE repository (and without using any GIT submodules!) to make it easy to just build your applications using CGE.

It is our intention to _not_ maintain here a fork of Vampyre -- you can always just take [Vampyre Imaging Library](https://github.com/galfar/imaginglib) from upstream and use it instead. (Though we may have some modifications to help with some compilers/platforms temporarily, so we recommend using this copy for now.)

You can always run

```
./sync.sh
```

to get here latest Vampyre source code here.

## Original Vampyre Lazarus packages (LPK) are not used by CGE

The original Vampyre LPK packages (`VampyreImagingPackage.lpk`, `VampyreImagingPackageExt.lpk`) are not used by _Castle Game Engine_. These packages are in [src/vampyre_imaginglib/src/Packages/](https://github.com/galfar/imaginglib/tree/master/Packages) but we remove them from this local copy, during `./sync.sh`.

This keeps things simple for people who [install CGE from sources](https://castle-engine.io/compiling_from_source.php). The CGE package `castle_engine_base.lpk` just uses Vampyre units directly.

See [this news](https://castle-engine.io/wp/2023/04/22/simpler-castle_base-lazarus-package-no-longer-depends-on-vampyre-lpk/) for more info.