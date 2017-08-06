#!/bin/bash

SESSIONNAME="Main"
tmux has-session -t $SESSIONNAME &> /dev/null

if [ $? != 0 ];
   then
      echo created $SESSIONNAME
      tmux new-session -d -n tab0 -s $SESSIONNAME
      sleep 2
      tmux send-keys -t $SESSIONNAME "clear && screenfetch" ENTER
      sleep 3
      tmux clear-history -t $SESSIONNAME
      #tmux split-window -h -t $SESSIONNAME
fi

tmux attach -t $SESSIONNAME
