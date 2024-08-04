#!/bin/bash
set -euo pipefail

rm -f *.pas
wget https://raw.githubusercontent.com/BeRo1985/pasgltf/master/src/PasGLTF.pas --output-document=CastlePasGLTF.pas
wget https://raw.githubusercontent.com/BeRo1985/pasjson/master/src/PasJSON.pas --output-document=CastlePasJSON.pas
wget https://raw.githubusercontent.com/BeRo1985/pasdblstrutils/master/src/PasDblStrUtils.pas --output-document=CastlePasDblStrUtils.pas

# Fix "unit xxx"
sed -i -e 's|unit PasGLTF|unit CastlePasGLTF|g' CastlePasGLTF.pas
sed -i -e 's|unit PasJSON|unit CastlePasJSON|g' CastlePasJSON.pas
sed -i -e 's|unit PasDblStrUtils|unit CastlePasDblStrUtils|g' CastlePasDblStrUtils.pas

# Fix "uses xxx"
sed -i -e 's|uses PasDblStrUtils|uses CastlePasDblStrUtils|g' CastlePasJSON.pas

# See https://castle-engine.io/coding_conventions#no_longint_longword
sed -i -e 's|longint|Integer|g' CastlePasJSON.pas
sed -i -e 's|longword|Cardinal|g' CastlePasJSON.pas
