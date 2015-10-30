#!/bin/sh

rm -f "$TEMP"/rply-*.json
  # There's an rply bug that causes it to be unable to read its
  # temporary files. Editing rply to work around this doesn't
  # seem to offer much of a speedup.

TERM=xterm-256color
PYTHON_CMD=python2.7
export LANG=en_US.UTF-8
export ROGUETV_BUNDLE_INFO="$(cat VERSION)"
cd lib
exec "$PYTHON_CMD" run.pyc "$@"
