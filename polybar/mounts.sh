#!/bin/bash

num=$(findmnt -t ext4,cifs,vfat,ntfs,fuseblk -nr -o SOURCE 2>/dev/null | grep -Evc '/dev/sda[0-9]+' 2>/dev/null)

if [[ $num -gt 0 ]]; then
    echo ""
else
    echo
fi
