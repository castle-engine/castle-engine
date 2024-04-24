# Test behavior of files with invalid indexes

The `castle.bin` is **invalid file**. It contains invalid indexes.

In CGE, `TShapeNode.GenerateTangents` will encounter these invalid indexes. It should not crash, instead show a proper warning/error.

## History

This testcase comes from old mistake:

https://github.com/castle-engine/demo-models/blob/master/castle/castle.gltf?raw=true

gives you proper glTF, but

https://github.com/castle-engine/demo-models/blob/master/castle/castle.bin

gives you HTML (not raw data of `castle.bin`).

The right solution is in `demo-models/x3d/needs_download_network_resources.x3dv`, one should use

https://raw.githubusercontent.com/castle-engine/demo-models/master/castle/castle.gltf

and then relative URL is correct bin file:

https://raw.githubusercontent.com/castle-engine/demo-models/master/castle/castle.bin

