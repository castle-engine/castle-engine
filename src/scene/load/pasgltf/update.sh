#!/bin/bash
set -euo pipefail

rm -f *.pas
wget https://raw.githubusercontent.com/BeRo1985/pasgltf/master/src/PasGLTF.pas --output-document=CastlePasGLTF.pas
wget https://raw.githubusercontent.com/BeRo1985/pasjson/master/src/PasJSON.pas --output-document=CastlePasJSON.pas
wget https://raw.githubusercontent.com/BeRo1985/pasdblstrutils/master/src/PasDblStrUtils.pas --output-document=CastlePasDblStrUtils.pas

sed -i -e 's|unit PasGLTF|unit CastlePasGLTF|g' CastlePasGLTF.pas
sed -i -e 's|unit PasJSON|unit CastlePasJSON|g' CastlePasJSON.pas
sed -i -e 's|unit PasDblStrUtils|unit CastlePasDblStrUtils|g' CastlePasDblStrUtils.pas
