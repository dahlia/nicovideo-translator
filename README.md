Nico Nico Douga (ニコニコ動画) Comment Translator
=================================================

[![Build Status][0]][1]

[0]: https://travis-ci.org/dahlia/nicovideo-translator.svg
[1]: https://travis-ci.org/dahlia/nicovideo-translator


Installation
------------

You can install it using `cabal`:

    $ cabal install nicovideo-translator


Aliasing `nmsg.nicovideo.jp` to localhost
-----------------------------------------

To make the translator to intercept comments from Nico Nico comment server,
you have to alias Nico Nico comment server domain (`nmsg.nicovideo.jp`) to
your localhost (`127.0.0.1`).  Open your [hosts file][2] using text editor
(you probably need administrator permission), and then add the following line:

    127.0.0.1    nmsg.nicovideo.jp

[2]: http://en.wikipedia.org/wiki/Hosts_%28file%29


Proxy server
------------

The translator behaves as a proxy server, so it has to be running while
you watch Nico Nico videos.  You can invoke the proxy server using CLI
(you probably need administrator permission to listen 80 port):

    $ nicovideo-translator EiP3NSgLid81OjSwpOMkgV0rzD9SLHRqwqUwx2r
    Running on http://0.0.0.0:80/ (Press ^C to quit)
    Upstream: nmsg.nicovideo.jp (202.248.110.173)

Note that it takes a [Google Translate API][3] key as its first argument.

You can terminate the server by pressing Ctrl-C.

It can optionally take the target language which is a two-letter
e.g. `en`, `ko` through `-l`/`--lang`/`--language` option:

    $ nicovideo-translator -l ko

[3]: https://cloud.google.com/translate/


Open source
-----------

It's written by [Hong Minhee][4], and distributed under [AGPLv3][].
You can find the source code from the [Git repository][5]:

    $ git clone git://github.com/dahlia/nicovideo-translator

Please report bugs to the [issue tracker][6] if you find.
Pull requests welcome!

[4]: https://hongminhee.org/
[5]: https://github.com/dahlia/nicovideo-translator
[6]: https://github.com/dahlia/nicovideo-translator/issues
[AGPLv3]: http://www.gnu.org/licenses/agpl-3.0.html
