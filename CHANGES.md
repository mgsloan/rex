0.1: Jul 25 2011
----------------
  * initial release

0.2: Sep 24 2011
----------------
  * Added custom configuration of PCRE options.
  * Added non-precompiling quasiquoters.
  * Fixed a bug where patterns with no captures would fail.
  * Decided to remove the defaulting to 'read' - too much magic.

0.3: Sep 25 2011
  * Fixed a capture indexing bug, where capture fields which aren't bound would
    cause subsequent bound captures to be incorrect.
  * Above bug fix actually neatened up code.
  * Added configuration of default mapping pattern.

0.4: Oct 11 2012
----------------
  * Made configuration into a datatype.

0.4.2: Feb 4 2013
-----------------
  * Precompilation bugs fixed by [takano-akio](https://github.com/takano-akio)!

0.4.3: Dec 21 2013
------------------
  * Patch from [aavogt](https://github.com/aavogt)! to use haskell-src-exts to
    parse view patterns.
  * Miscellaneous code cleanups.

0.5: Feb 20 2014
----------------
  * Changed the configuration datatype to allow custom preprocessing of expr /
    pat antiquotes.  The default match processing is now "rexView", an
    identifier that can be shadowed locally or hidden on import.
  * Removed 'maybeRead'.
