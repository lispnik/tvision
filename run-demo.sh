#!/bin/sh
# Launch the Turbo Vision demo in this terminal.
exec sbcl --script "$(dirname "$0")/run.lisp"
