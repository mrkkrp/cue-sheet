# CUE sheet

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/cue-sheet.svg?style=flat)](https://hackage.haskell.org/package/cue-sheet)
[![Stackage Nightly](http://stackage.org/package/cue-sheet/badge/nightly)](http://stackage.org/nightly/package/cue-sheet)
[![Stackage LTS](http://stackage.org/package/cue-sheet/badge/lts)](http://stackage.org/lts/package/cue-sheet)
[![Build Status](https://travis-ci.org/mrkkrp/cue-sheet.svg?branch=master)](https://travis-ci.org/mrkkrp/cue-sheet)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/cue-sheet/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/cue-sheet?branch=master)

The library allows to construct, render, and parse CUE sheets.

## What is CUE sheet?

> A cue sheet, or cue file, is a metadata file which describes how the
> tracks of a CD or DVD are laid out. Cue sheets are stored as plain text
> files and commonly have a “.cue” filename extension. CDRWIN first
> introduced cue sheets, which are now supported by many optical disc
> authoring applications and media players.

[Read more on Wikipedia](https://en.wikipedia.org/wiki/Cue_sheet_(computing)).
The description of the format can be found
[here](https://wayback.archive.org/web/20070614044112/http://www.goldenhawk.com/download/cdrwin.pdf),
scroll to the appendix A (it's closest we get to a “specification”).

## Quick start

[Read the Haddocks](https://hackage.haskell.org/package/cue-sheet). In
short, you parse a `CueSheet` with `parseCueSheet` function and render a
`CueSheet` with `renderCueSheet` function—pretty straightforward, eh? Of
course, you still can construct a `CueSheet` manually. The data types are
defined in such a way that incorrect CUE sheets are impossible to represent.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/cue-sheet/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2016–2017 Mark Karpov

Distributed under BSD 3 clause license.
