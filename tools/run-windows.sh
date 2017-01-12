#!/bin/sh

TERM=xterm-256color
export LANG=en_US.UTF-8
export ROGUETV_BUNDLE_INFO="$(cat VERSION)"
cd lib
exec python3.4m.exe -c 'import hy, roguetv_run' "$@"
