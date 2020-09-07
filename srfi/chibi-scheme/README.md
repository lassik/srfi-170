# Chibi Scheme example implementation of SRFI 170

Please see copyright notice in lib/srfi/170/COPYING.

Please note this is a not of production quality example implementation
of SRFI 170 for Linux and OpenBSD, is primarily for pedagogical
purposes as a guide to proper implementation.  Thus it does not for
example handle errno or EINTR at the C level.


In addition to the required error procedures posix-error?,
posix-error-name, and posix-error-message, it also provides:

posix-error-error-set: will be 'errno for a regular POSIX error,
  'sanity-check for error checks prior to calling POSIX.

posix-error-number: errno as a fixnum

posix-error-scheme-procedure: in which the error was raised

posix-error-posix-interface: if a POSIX call failed, which one

posix-error-data: additional data as a alist, such a the arguments
  handed to the Scheme procedure.


The Makefile does not assume an installed Chibi Scheme is in any
particular location to build from, so you must set the environment
variable CHIBI_LOCATION_PATH to the necessary location.  For example,
in bash:

export CHIBI_LOCATION_PATH=/usr/local/src/chibi-scheme

Or without modifying your environment, with gmake being Gnu Make or
some sort of alias to it, you could execute something like this using
bash to make the example SRFI implementation:

CHIBI_LOCATION_PATH=/usr/local/src/chibi-scheme gmake

To run with all paths set correctly, from srfi/chibi-scheme off the
top repo directory, either do a "gmake repl", or execute something
like this:

LD_LIBRARY_PATH=".:/usr/local/src/chibi-scheme" DYLD_LIBRARY_PATH=".:/usr/local/src/chibi-scheme" CHIBI_IGNORE_SYSTEM_PATH=1 CHIBI_MODULE_PATH="./lib:/usr/local/src/chibi-scheme/lib" /usr/local/src/chibi-scheme/chibi-scheme -m "(srfi 170)"

To run the tests, either do a "gmake test" with the above mentioned
CHIBI_LOCATION_PATH environment variable set, or something like:

LD_LIBRARY_PATH=".:/usr/local/src/chibi-scheme" DYLD_LIBRARY_PATH=".:/usr/local/src/chibi-scheme" CHIBI_IGNORE_SYSTEM_PATH=1 CHIBI_MODULE_PATH="./lib:/usr/local/src/chibi-scheme/lib" /usr/local/src/chibi-scheme/chibi-scheme -m "(srfi 170 test)" -e "(run-tests)"

Which run successfully on x86-64 Ubuntu 18.04 Linux kernel 4.15.0-112,
gcc v7.5.0, and x86-64 OpenBSD 6.7, clang v8.0.1.

The test suite 170/test.sld can be run as root, and needs to be to
run as root to fully test some features, see the comments at its top.

The test suite is pretty generic and should be adaptable to your
implementation, note aux.c for some unexported magic like (errno) and
(set-errno), and common.scm for Scheme code shared by both.

This release includes the minimum part of SRFI 19: Time Data Types and
Procedures required by the SRFI 170 API.
