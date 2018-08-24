#!/bin/bash

dbus-monitor --session "type='signal',path='/org/xmonad/Log',interface='org.xmonad.Log',member='Update'" | sed -uEn -e '2d' -e '4d' -e '/^ *string/!d' -e 's/^ *string "//; s/"$// p'

