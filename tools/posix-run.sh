#!/bin/sh

if [ "$TERM" = screen ] ; then
    TERM=screen-256color
else
    TERM=xterm-256color
fi

PYTHON_CMD=python
V=$("$PYTHON_CMD" -c 'import sys; print(sys.version_info[0])')
if [ "$V" = 3 ] ; then
    # "python" is Python 3. Better hope that Python 2 is
    # available at "python2".
    PYTHON_CMD=python2
fi

export ROGUETV_BUNDLE_INFO="$(cat VERSION)"
cd lib
"$PYTHON_CMD" run.pyc "$@"
