intelligence=smart

if [[ -n $INSIDE_EMACS ]]; then
    intelligence=emacs
elif [[ -z $TERM ]] || [[ $TERM == dumb ]]; then
    intelligence=dumb
fi
