# Command-line tool to compare 2 images

Command-line tool to compare sizes and colors of two images. Designed to be a command-line tool usable from scripts, like (bash script):

```
if image_compare a.png b.png; then
  echo 'Yes'
else
  echo 'No'
fi
```

Returns exit code 0 (and no output) if succcess (images are the same), or exit code 1 (and desciption of difference on error output) if images differ.

The images are considered different if they have different size, or if any component of any pixel differs > `Tolerance` (constant in this program).

Images with different sizes are always detected as "different", we do not bother to check if maybe one image is a shifted, or subset, version of another. It also doesn't bother showing visual differences between images. Try ImageMagick's "compare" tool if you want these features (http://www.imagemagick.org/script/compare.php).

Using [Castle Game Engine](https://castle-engine.io/).

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `x3d_call_pascal_code_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).
