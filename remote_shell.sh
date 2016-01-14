#!/bin/bash

SSH_TUNNEL_COMMAND="ssh -f -N -L 4369:localhost:4369 -L 49000:localhost:49000 -L 49001:localhost:49001 -L 49002:localhost:49002 -L 49003:localhost:49003 -L 49004:localhost:49004 apotheos.is"

eval $SSH_TUNNEL_COMMAND
set -e
function cleanup {
  kill `pgrep -f "$SSH_TUNNEL_COMMAND"`
}
trap cleanup EXIT

iex --cookie $APATHY_DRIVE_COOKIE --name $USER@localhost -e ":observer.start" --remsh $APATHY_DRIVE_NODE_NAME@127.0.0.1 --hidden
