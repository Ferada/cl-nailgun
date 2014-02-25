-*- mode: markdown; coding: utf-8-unix; -*-

CL-NAILGUN - Remotely hosted command line programs for Common Lisp.

Copyright (C) 2014 Olof-Joachim Frahm

Release under a Simplified BSD license.

# WHY

The main application of this library is to run scripts without the high
overhead of running a complete Lisp image, or in the original case, a
JVM.  Consequently the protocol is limited, as is the very bare bones
client, basically just shoveling IO streams around and taking care of
command line arguments and the current working directory.  The
server/daemon is then responsible to arrange itself according to these
parameters.

Related options would be to implement the same approach over SWANK, or
to use another IPC mechanism and custom protocol.  SWANK would require a
Lisp running as the client, rather limiting it to an interpreted one if
startup time is still a concern.  Other IPC mechanisms are of course
feasible and can possibly be made into a library as well, however this
one has the benefit of at least an existing protocol for communicating
stream IO and the basics of process information which could be a concern
to scripts.  Of course depending on the use case this is not enough,
which leaves protocol extensions, or going the custom route.

# USAGE

You have to have the nailgun client installed.

# TODO

- If you're willing to have special code to leave out the multithreaded
  stream implementation feel free to submit code.
- Not all programs IO, so a simplified model could be used in that case,
  leaving out the stream handling entirely.
