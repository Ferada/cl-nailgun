-*- mode: markdown; coding: utf-8-unix; -*-

CL-NAILGUN - Remotely hosted command line programs for Common Lisp.

Copyright (C) 2014 Olof-Joachim Frahm

Release under a Simplified BSD license.

# DEPENDENCIES

- a nailgun client, e.g. from [nailgun]
- bordeaux-threads, trivial-gray-streams, babel, alexandria, flexi-streams

Should be reasonably portable.

# USAGE

You have to have the nailgun client installed and the library loaded:

    > (asdf:load-system '#:cl-nailgun)

Then the server has to be run:

    > (cl-nailgun:run-server
        (lambda (command arguments directory
                 environment output error input)
          (format T "called as ~A ~{~A~^ ~} in ~A~%"
                  command arguments directory)
          (format output "Hello, World!~%")
          (format error "Errors go here!~%")
          (loop
            (format output "~S~%" (or (read-line input NIL) (return))))))

The single argument to `RUN-SERVER` is a handler function.  The handler
is run in a separate thread and may read and write from the streams
provided in the last three arguments.  (At the moment each of them is
wrapped with `flexi-streams` to provide character IO.

Next, calls from the nailgun client will be answered by the server:

    $ ln -s /usr/bin/ng foo && chmod a+x foo
    $ echo HELLO WORLD | ./foo 1 2 3
    Hello, World!
    Errors go here!
    "HELLO WORLD"

The REPL will also show the log message for each connection:

    called as foo 1 2 3 in /opt/cl-nailgun

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

# TODO

- If you're willing to have special code to leave out the multithreaded
  stream implementation feel free to submit code.
- Not all programs IO, so a simplified model could be used in that case,
  leaving out the stream handling via threading entirely.  Could be useful for
  single-threaded Lisps?
- The nailgun client has a few more features, eclim also has a modified version
  with a keep-alive flag.
- Working directory should be a `PATHNAME`.
- Thread pool instead of spawning them all over the place.
- External encoding should be configurable.

[nailgun]: <https://github.com/martylamb/nailgun> "nailgun"
