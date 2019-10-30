# Changelog

## 0.5.1 (2014-02-20)

  * Made the type of the default 'rexView' fully polymorphic.

## 0.5: (2014-02-20)

  * Changed the configuration datatype to allow custom preprocessing of expr /
    pat antiquotes.  The default match processing is now "rexView", an
    identifier that can be shadowed locally or hidden on import.

  * Removed 'maybeRead'.

## 0.4.3: (2013-12-21)

  * Patch from [aavogt](https://github.com/aavogt)! to use haskell-src-exts to
    parse view patterns.

  * Miscellaneous code cleanups.

## 0.4.2: (2013-02-04)

  * Precompilation bugs fixed by [takano-akio](https://github.com/takano-akio)!

## 0.4: (2012-10-11)

  * Made configuration into a datatype.

## 0.3: (2011-09-25)

  * Fixed a capture indexing bug, where capture fields which aren't bound would
    cause subsequent bound captures to be incorrect.

  * Above bug fix actually neatened up code.

  * Added configuration of default mapping pattern.

## 0.2: (2011-09-24)

  * Added custom configuration of PCRE options.

  * Added non-precompiling quasiquoters.

  * Fixed a bug where patterns with no captures would fail.

  * Decided to remove the defaulting to 'read' - too much magic.

## 0.1: (2011-07-25)

  * initial release
