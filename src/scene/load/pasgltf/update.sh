#!/bin/bash
set -euo pipefail

rm -f *.pas
wget https://raw.githubusercontent.com/BeRo1985/pasgltf/master/src/PasGLTF.pas
wget https://raw.githubusercontent.com/BeRo1985/pasjson/master/src/PasJSON.pas
wget https://raw.githubusercontent.com/BeRo1985/pasdblstrutils/master/src/PasDblStrUtils.pas
