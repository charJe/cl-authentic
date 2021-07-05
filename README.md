`authentic` -- password management for Common Lisp (web) applications
=============================================================================

`authentic` provides a light-weight and extendible solution to
user/password management:

* safe password storage:
    + cleartext-free, using your choice of hash algorithm through
      [ironclad](http://method-combination.net/lisp/ironclad/),
    + storage in an SQL database through
      [clsql](http://clsql.b9.com/), in a database your application
      already uses anyway, or in a separate one, and using any backend
      supported by clsql,
* password reset mechanism with one-time tokens (suitable for mailing
  to users for confirmation),
* user creation optionally with confirmation tokens (suitable for
  mailing to users),
* (obviously) user authentication.

Users can be identified by strings or by subclassing
`user-token-mixin`.

For documentation check out the
[API documentation](http://u-u-h.github.io/password-store/password-store-package/index.html), or look at the
[examples](examples.lisp). There is also a
[5am](http://common-lisp.net/project/fiveam/) test suite in the file [tests.lisp](tests.lisp).

The code has been written to be portable, and tested on
[Allegro Common Lisp](http://www.franz.com/products/allegro-common-lisp/)
and [SBCL](http://www.sbcl.org/). All dependency libraries are readily
available through [quicklisp](http://www.quicklisp.org/).

(c) 2013 Utz-Uwe Haus <lisp@uuhaus.de>, licensed under [LLGPL](LICENSE.LGPL)
