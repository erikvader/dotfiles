#!/bin/bash

# generates a theme folder

set -e

# the picture should already be in the folder
pic="$(ls -1 "$1" | head -n1)"

# default to black conky
echo "#!/bin/bash" > "$1/conky"
echo "conky_start -n metro -t black &" >> "$1/conky"
echo "conky_start -n system -t black &" >> "$1/conky"
chmod +x "$1/conky"

# generate feh script
echo -e '#!/bin/bash\nfeh --bg-fill "$HOME/themes'"/$1/$pic"'"' > "$1/feh"
chmod +x "$1/feh"

# create the lock image
create-lock-img "$1/$pic" "lock.png" "$1/lock.png" "5x8"

