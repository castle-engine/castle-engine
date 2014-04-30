#!/bin/bash
set -eu

SF_USERNAME="$1"
SF_PATH="$2"

ssh "$SF_USERNAME",castle-engine@shell.sourceforge.net create

ssh "$SF_USERNAME",castle-engine@shell.sourceforge.net <<EOF
cd "$SF_PATH"
rm -Rf old/ new/ html/
tar xzvf html.tar.gz
chmod -R a+rX html/
EOF
