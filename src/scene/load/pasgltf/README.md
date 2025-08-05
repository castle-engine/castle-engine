## PasGLTF from BeRo

This directory contains `PasGLTF` unit, to read/write glTF files.

Along with dependencies (`PasJSON` and `PasDblStrUtils`).

By Benjamin 'BeRo' Rosseaux.

Original sources with examples:


- https://github.com/BeRo1985/pasgltf
- https://github.com/BeRo1985/pasjson
- https://github.com/BeRo1985/pasdblstrutils

We host the units here, inside CGE repo, because:

- We want maximum convenience for people using CGE. This way you can compile Castle Game Engine with glTF 2.0 support out-of-the-box after cloning CGE repo.

    We considered but rejected use of GIT submodules, as many people are not familiar with them and wouldn't do `git submodule update --init --recursive`.

- Later: We also had to rename them, to avoid conflicts with GLScene that also includes these unis, see https://forum.castle-engine.io/t/installation-in-delphi-12-1/1228 .

We still do not want to modify or fork them here. All the necessary modifications should be submitted back to BeRo repositories, so that everyone benefits.

License: open-source ZLib license: https://opensource.org/licenses/zlib
