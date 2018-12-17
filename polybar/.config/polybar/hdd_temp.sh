#!/bin/bash

hddtemp /dev/sda | cut -d' ' -f3
