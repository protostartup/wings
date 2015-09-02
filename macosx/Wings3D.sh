#!/bin/sh
ROOTDIR=$(dirname "$0")
ROOTDIR=$(dirname "$ROOTDIR")/Resources
BINDIR=$ROOTDIR/bin
EMU=beam
PROGNAME=$(basename $0)
export EMU
export ROOTDIR
export BINDIR
export PROGNAME
# Get rid of environment variables starting with ERL_ which may
# cause Wings to misbehave (ERL_LIBS in particular).
for e in $(env); do
  case $e in
    ERL_*) unset ${e%%=*} ;;
  esac
done
exec "$BINDIR/erlexec" -smp -noshell -run wings_start start_halt ${1+"$@"}
