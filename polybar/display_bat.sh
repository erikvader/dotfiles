#!/bin/bash

notify-send "$(acpi -bi | sed -E 's/.*?: (.*)$/\1/')"
