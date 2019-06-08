define(`er_echo', `e`'cho $1')dnl
dnl
define(`er_LQ', `changequote([,])`dnl'
changequote`'')dnl
define(`er_RQ',`changequote([,])dnl`
'changequote`'')dnl
dnl
define(`er_map', `ifelse(`$#', 1, , `$#', 2, `$1($2)', `$1($2), er_map(`$1', shift(shift($@)))')')dnl
dnl
define(`er_max2', `ifelse(eval(`$2 > $1'), 1, `$2', `$1')')dnl
define(`er_max', `ifelse(`$#', 1, `$1', `$#', 2, `er_max2(`$1', `$2')', `er_max(er_max2(`$1', `$2'), shift(shift($@)))')')dnl
dnl
define(`er_maxlen', `er_max(er_map(`len', $@))')dnl
