# Chibi Scheme example implementation of SRFI 170

Please see copyright notice in lib/srfi/170/COPYING.

Please note this is an example implementation of SRFI 170 for Linux
and OpenBSD, and is not of production quality.

The Makefile does not assume an installed Chibi Scheme is in any
particular location to build from, or that your Chibi Scheme has SRFI
198 installed in it, so you must set the environment variables
CHIBI_LOCATION_PATH and SRFI_198_LOCATION_PATH to the necessary
locations.  E.g., in bash:

export CHIBI_LOCATION_PATH=/usr/local/src/chibi-scheme
export SRFI_198_LOCATION_PATH=/home/src/srfi/198-foreign-errors/srfi/chibi-scheme

Or without modifying your environment, with gmake being Gnu Make or
some sort of alias to it, you could execute something like this using
bash to make the example SRFI implementation:

CHIBI_LOCATION_PATH=/usr/local/src/chibi-scheme SRFI_198_LOCATION_PATH=/home/src/srfi/198-foreign-errors/srfi/chibi-scheme gmake

To run with all paths set correctly, from srfi/chibi-scheme off the
top repo directory, either do a "gmake repl", or execute something
like this:

LD_LIBRARY_PATH=".:/home/src/srfi/198-foreign-errors/srfi/chibi-scheme:/usr/local/src/chibi-scheme" DYLD_LIBRARY_PATH=".:/home/src/srfi/198-foreign-errors/srfi/chibi-scheme:/usr/local/src/chibi-scheme" CHIBI_IGNORE_SYSTEM_PATH=1 CHIBI_MODULE_PATH="./lib:/home/src/srfi/198-foreign-errors/srfi/chibi-scheme/lib:/usr/local/src/chibi-scheme/lib" /usr/local/src/chibi-scheme/chibi-scheme -m "(srfi 170)"

To run the tests, either do a "gmake test" with the above mentioned
CHIBI_LOCATION_PATH environment variable set, or something like:

LD_LIBRARY_PATH=".:/home/src/srfi/198-foreign-errors/srfi/chibi-scheme:/usr/local/src/chibi-scheme" DYLD_LIBRARY_PATH=".:/home/src/srfi/198-foreign-errors/srfi/chibi-scheme:/usr/local/src/chibi-scheme" CHIBI_IGNORE_SYSTEM_PATH=1 CHIBI_MODULE_PATH="./lib:/home/src/srfi/198-foreign-errors/srfi/chibi-scheme/lib:/usr/local/src/chibi-scheme/lib" /usr/local/src/chibi-scheme/chibi-scheme -m "(srfi 170 test)" -e "(run-tests)"

Which run successfully on x86-64 Ubuntu 18.04 Linux kernel 4.15.0-111,
gcc v7.5.0, and x86-64 OpenBSD 6.7, clang v8.0.1.

The test suite 170/test.sld can be run as root, and needs to be to
run as root to fully test some features, see the comments at its top.

The test suite is pretty generic and should be adaptable to your
implementation, note aux.c for some unexported magic like (errno) and
(set-errno), and common.scm for Scheme code shared by both.

This includes a very minimal implementation of the pre-release at the
time timespec SRFI 174 as a disjoint type in 174.sld.
