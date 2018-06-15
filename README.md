# mekfont-logotype

Hiya there! This repository contains my logotype font that I happen to use for
things and stuff, including my avatar and fancy shmancy keyboard. The
individual glyphs reside under the `glyphs/` directory in a reasonably named
fashion.

## Building the font/doing anything at all what-so-ever

You'll need both FontForge and stack (the Haskell utility). Once you have
those, you can just run `generator/build.sh`.

## What's done

- [ ] Glyphs
	- [x] Alphanumeric
		- [x] Standard Latin
		- [x] åäö
		- [x] Numerals
	- [x] Symbols
	- [ ] Auxiliary keyboard imagery

- [x] Font
	- [x] Font template
	- [x] Glyph metadata? Think baseline/spacing/etc.
		- This is nastily guessti-parsed at the moment but it works
	- [x] Font generator utility/script
