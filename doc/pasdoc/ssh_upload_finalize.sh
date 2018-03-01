#!/bin/bash
set -eu

SSH_USERNAME="$1"
SSH_HOST="$2"
SSH_PATH="$3"

ssh "$SSH_USERNAME"@"$SSH_HOST" <<EOF
cd "$SSH_PATH"
rm -Rf old/ new/ html/
tar xzf html.tar.gz
chmod -R a+rX html/
EOF
