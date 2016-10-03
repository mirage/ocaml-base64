2.1.1 (2016-10-03):
* Switch build to `topkg` and obey the `odig` conventions
  for installing metadata files.
* Add a test suite based on RFC4648 test vectors.
* Improve Travis CI tests to be multidistro.

2.0.0 (2014-12-24):
* Switch the top-level `Base64` module to `B64` to avoid
  clashing with various other similarly named modules in
  `extlib` and some other libraries.  This is obviously
  backwards compatibility breaking with all current users
  of this library. (#3).

1.1.0 (2014-12-16):
* Allow specifying a different alphabet during encoding or
  decoding, and supply a URI-safe alphabet along with the
  default Base64 standard.
* Add OCaml 4.02 `safe-string` compatibility.
* Optionally support encoding without padding.

1.0.0 (2014-08-03):
* Initial public release.
