#!/bin/sh

PORT=2323
PIDFILE=/home/ferada/.cl-nailgun-script.pid
CL=/home/ferada/src/opt/bin/sbcl
NG=/home/ferada/src/nailgun/ng

PID=
if [ -f $PIDFILE ]
then
  PID=$(cat $PIDFILE)
fi

# TODO: file lock so only one server is started

SPAWN=

[ -z "$PID" ] && SPAWN=YES
/bin/kill -s 0 $PID || SPAWN=yes

if [ -n "$SPAWN" ]
then
  nohup $CL \
    --eval "(asdf:load-system '#:cffi)" \
    --eval "(push \"/home/ferada/src/opt/lib/\" cffi:*foreign-library-directories*)" \
    --eval "(asdf:load-system '#:iolib/grovel)" \
    --eval "(push \"-I/home/ferada/src/opt/include/\" iolib-grovel::*cc-flags*)" \
    --eval "(asdf:load-system '#:cl-nailgun-script)" \
    --eval "(cl-nailgun-script:start $PORT)" > ~/.cl-nailgun-script.log 2>&1 &
  echo $! > $PIDFILE
  while ! nc -q 0 localhost $PORT < /dev/null > /dev/null 2>&1; do sleep 1; done
fi

$NG --nailgun-port $PORT $0 $*
