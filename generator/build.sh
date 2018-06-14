#!/bin/sh
cd "$(dirname "$(readlink -f "$0")")"

_ck () {
	_x="$?"
	_ckf="\033[01;%sm %s \033[00m\n"
	_cks="OK"
	_ckc="32"
	if [ x"$_x" != x"0" ]; then
		_cks="Fail"
		_ckc="31"
	fi
	printf "$_ckf" "$_ckc" "$_cks"
	if [ x"$_x" != x"0" ]; then
		exit "$_x"
	fi
}

printf "%s ... " "Initialising environment"
stack build && mkdir -p ../build/
_ck

_bdir="$(dirname "$PWD")/build"
_f="%-10s \033[00;36m'$_bdir/mekfont-logotype.%-5s\033[0m ... "
_fp () {
	printf "$_f" "$1" "$2'"
}

_fp Building svg
stack ghc ../generator/Template.hs -- -e 'buildTpl "../mekfont-logotype-template.svg" "../build/mekfont-logotype.svg"' 2>/dev/null
_ck

cd ../build/

_c () {
	_fp Generating "$1"
	fontforge -lang=ff -c 'Open($1); Generate($2)' mekfont-logotype.svg mekfont-logotype."$1" 2>/dev/null
	_ck
}

_c woff
_c ttf
_c otf
_c eot
