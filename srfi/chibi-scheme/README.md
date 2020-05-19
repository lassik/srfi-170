# Chibi Scheme example implementation of SRFI 170

Please see copyright notice in 170/COPYING.

Please note this is an example implementation of SRFI 170 for Linux
and OpenBSD and is not of production quality (Chibi Scheme already has
a robust set of POSIX libraries, from which this SRFI implementation
freely cribbed).

The Makefile does not assume an installed Chibi Scheme is in any
particular location to build from, so you must set the environment
variable CHIBI_LOCATION_PATH to the desired location.  E.g., in bash:

export CHIBI_LOCATION_PATH=/usr/local/src/chibi-scheme

Or without modifying your environment, with gmake being Gnu Make or
some sort of alias to it, you could execute something like this using
bash to make the example SRFI implementation:

CHIBI_LOCATION_PATH=/home/src/chibi-scheme gmake

To run with all paths set correctly, from the top repo directory,
execute somethin like this:

LD_LIBRARY_PATH="/usr/local/src/chibi-scheme:." DYLD_LIBRARY_PATH="/usr/local/src/chibi-scheme:." CHIBI_IGNORE_SYSTEM_PATH=1 CHIBI_MODULE_PATH="/usr/local/src/chibi-scheme/lib:./lib" /usr/local/src/chibi-scheme/chibi-scheme -m "(srfi 170)"

To run the tests, either do a "make test", or:

LD_LIBRARY_PATH="/usr/local/src/chibi-scheme:." DYLD_LIBRARY_PATH="/usr/local/src/chibi-scheme:." CHIBI_IGNORE_SYSTEM_PATH=1 CHIBI_MODULE_PATH="/usr/local/src/chibi-scheme/lib:./lib" /usr/local/src/chibi-scheme/chibi-scheme -m "(srfi 170 test)" -e "(run-tests)"

Which run successfully on x86-64 Ubuntu 18.04 Linux kernel 4.15.0, gcc
v7.4.1, and x86-64 OpenBSD 6.5, clang v7.0.1.

The test suite 170/test.sld can be run as root, and needs to be to
fully test some features, see the comments at its top.

The test suite is pretty generic and should be adaptable to your
implementation, note aux.c for some unexported magic like (errno) and
(set-errno), and common.scm for Scheme code shared by both.

This includes a very minimal implementation of the pre-release
timespec SRFI as a disjoint type in 174.sld.
