Nico Nico Douga (ニコニコ動画) Comment Translator
=================================================

Installation
------------

You can install it using `cabal`:

    $ cabal install nicovideo-translator


Aliasing `msg.nicovideo.jp` to localhost
----------------------------------------

To make the translator to intercept comments from Nico Nico comment server,
you have to alias Nico Nico comment server domain (`msg.nicovideo.jp`) to
your localhost (`127.0.0.1`).  Open your [hosts file][1] using text editor
(you probably need administrator permission), and then add the following line:

    127.0.0.1    msg.nicovideo.jp

[1]: http://en.wikipedia.org/wiki/Hosts_%28file%29


Proxy server
------------

The translator behaves as a proxy server, so it has to be running while
you watch Nico Nico videos.  You can invoke the proxy server using CLI
(you probably need administrator permission to listen 80 port):

    $ nicovideo-translator

You can terminate the server by pressing Ctrl-C.


Open source
-----------

It's written by [Hong Minhee][2], and distributed under [AGPLv3[].
You can find the source code from the [Git repository][3]:

    $ git clone git://github.com/dahlia/nicovideo-translator

Please report bugs to the [issue tracker][4] if you find.
Pull requests welcome!

[2]: http://hongminhee.org/
[3]: https://github.com/dahlia/nicovideo-translator
[4]: https://github.com/dahlia/nicovideo-translator/issues
[AGPLv3]: http://www.gnu.org/licenses/agpl-3.0.html
