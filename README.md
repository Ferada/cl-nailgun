<!-- -*- mode: markdown; coding: utf-8-unix; -*- -->

CL-NAILGUN - Remotely hosted command line programs for Common Lisp.

Copyright (C) 2014-2020 Olof-Joachim Frahm <olof@macrolet.net>

Release under a Simplified BSD license.

# DEPENDENCIES

- a nailgun client, e.g. from [nailgun], [facebook], or
  `apt-get install nailgun`
- `bordeaux-threads`, `trivial-gray-streams`, `babel`, `alexandria`,
  `flexi-streams`, `arnesi`

Should be reasonably portable (tested on SBCL and CCL so far).

# USAGE

You have to have the nailgun client installed and the library loaded:

```lisp
(asdf:load-system '#:cl-nailgun)
```

Then the server has to be run:

```lisp
(cl-nailgun:run-server
  (lambda (command arguments directory
           environment streams)
    (declare (ignore environment))
    (let ((output (funcall streams :stdout))
          (error (funcall streams :stderr))
          (input (funcall streams :stdin)))
      (format T "called as ~A ~{~A~^ ~} in ~A~%"
              command arguments directory)
      (format output "Hello, World!~%")
      (format error "Errors go here!~%")
      (loop
        (format output "~S~%" (or (read-line input NIL) (return))))
      (cl-nailgun:exit 2))))
```

The single argument to `RUN-SERVER` is a handler function.  The handler
is run in a separate thread and may read and write from the streams
provided in the last three arguments.  (At the moment each of them is
wrapped with `flexi-streams` to provide character IO.)

After execution is finished, the exit code is 0 by default, or 1 if
execution was aborted, e.g. if an error occured.  The function `EXIT`
can be used to terminate early with an optional status code argument
(again, the default is 0).

Next, calls from the nailgun client will be answered by the server:

```bash
$ ln -s /usr/bin/ng foo
$ echo HELLO WORLD | ./foo 1 2 3
Hello, World!
Errors go here!
"HELLO WORLD"
$ echo $?
2
```

(Note that `ng` might also be called differently, e.g. `ng-nailgun`, depending
on your system.)

The REPL will also show the log message for each connection:

```
called as foo 1 2 3 in /opt/cl-nailgun
```

# SCRIPTING

An example of how to use this library for real is contained in the
`CL-NAILGUN-SCRIPT` package, which implements a very simple `EVAL` based
server.  (Note that this package additionally uses `unix-options` for command
line argument parsing.)

Load it, then call `(START)` (port is optional and defaults to `2323`) and
access it like above via `ng` or a symlink to it:

```bash
$ ln -s /usr/bin/ng foo
$ NAILGUN_PORT=2323 ./foo --help
A very short program.

Usage:
  -v, --version  An option
  -h, --help     Prints this summary
$ NAILGUN_PORT=2323 ./foo
foo NIL (_=./foo DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus ...)
1
1
(+ 1 2 3)
6
```

Each line read will be evaluated and the first result printed back to the
caller.

This is purely intended as an example and should **NOT** be used in a
production setting like this.

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

[nailgun]: <https://github.com/martylamb/nailgun> "martylamb/nailgun"
[facebook]: <https://github.com/facebook/nailgun> "facebook/nailgun"
