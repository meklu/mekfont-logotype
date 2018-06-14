#!/bin/sh
cd "$(dirname "$(readlink -f "$0")")"

_f="%s \033[00;36m'%s/mekfont-logotype.%s'\033[0m... "
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
stack build
_ck
printf "$_f" Building "$PWD" svg
stack ghc Template.hs -- -e 'buildTpl "../mekfont-logotype-template.svg" "../build/mekfont-logotype.svg"' 2>/dev/null
_ck

cd ../build/

_c () {
	printf "$_f" Generating "$PWD" "$1"
	fontforge -lang=ff -c 'Open($1); Generate($2)' mekfont-logotype.svg mekfont-logotype."$1" 2>/dev/null
	_ck
}

_c woff
_c ttf
_c otf
