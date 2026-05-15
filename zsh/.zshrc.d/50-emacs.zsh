alias ec='emacsclient --reuse-frame --no-wait --quiet'

if [[ intelligence == emacs ]]; then
    export EDITOR='emacsclient -r'
    export VISUAL=$EDITOR
    # NOTE: less will work when using coterm
    # export PAGER=cat
fi
