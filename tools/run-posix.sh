#!/bin/sh

if [ "$TERM" = screen ] ; then
    TERM=screen-256color
else
    TERM=xterm-256color
fi

export ROGUETV_BUNDLE_INFO="$(cat VERSION)"
cd lib
exec python3 run.pyc "$@"
