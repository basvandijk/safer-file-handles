This package adds three safety features on top of the regular
[System.IO] file handles and operations:

* Regional file handles. Files must be opened in a *region*. When the
  region terminates all opened files are automatically closed. The
  main advantage of regions is that the handles to the opened files
  can not be returned from the region which ensures no I/O with closed
  files is possible.

* Explicit IOModes. The regional file handles are parameterized by the
  IOMode in which they were opened. All operations on handles
  explicitly specify the needed IOMode. This way it is impossible to
  read from a write-only handle or write to a read-only handle for
  example.

* Type-safe filepath creation and manipulation using the [pathtype]
  package.

The primary technique used in this package is called "Lightweight
monadic regions" which was [invented][1] by Oleg Kiselyov and
Chung-chieh Shan.

This technique is implemented in the [regions] package which is
re-exported from `safer-file-handles`.

See the [safer-file-handles-examples] package for examples how
to use this package:

    git clone git://github.com/basvandijk/safer-file-handles-examples.git

See the [safer-file-handles-bytestring] and [safer-file-handles-text]
packages for `ByteString` / `Text` operations on regional file
handles.

[System.IO]: http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO.html

[1]: http://okmij.org/ftp/Haskell/regions.html#light-weight

[pathtype]: 			 http://hackage.haskell.org/package/pathtype
[regions]:  			 http://hackage.haskell.org/package/regions
[safer-file-handles-bytestring]: http://hackage.haskell.org/package/safer-file-handles-bytestring
[safer-file-handles-text]:       http://hackage.haskell.org/package/safer-file-handles-text

[safer-file-handles-examples]: https://github.com/basvandijk/safer-file-handles-examples
