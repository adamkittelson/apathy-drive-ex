#!/bin/bash

iex --cookie $APATHY_DRIVE_COOKIE --name $USER@localhost -e ":observer.start" --remsh $APATHY_DRIVE_NODE --hidden

