Haskey
======

A webservice for browser keywords.

Description
-----------

Haskey is a web service that redirects your browser to other search engines
based on the search query that you send to it. It is supposed to be used as the
default search engine in your web browser.

* Works with any browser that supports a default search engine.
* Configuration is stored on the server, hence no synchronisation is necessary
  when you use more than one computer.

Usage
-----

Currently, you have to run your own instance of Haskey in order to use it.

1. Have a recent version of
   [haskell-platform](http://hackage.haskell.org/platform/) installed
1. run Haskey like this:

        $ git clone https://github.com/mklinik/haskey.git
        $ cd haskey
        $ runhaskell Main.hs

1. Configure your browser to use Haskey as the default search engine:

    * In firefox, open about:config and set the option keyword.URL to
      "http://localhost:8080/?q=" (without the quotes)
    * In chrome, add a new search engine, set its url to
      "http://localhost:8080/?q=%s" (without the quotes) and make it the
      default search engine.
    * In [your-browser] consult [your-browser's-documentation]

1. Type a keyword search in the address bar of your web browser, for example "g
   foobar"
1. Haskey redirects you to the corresponding search engine, in this case
   http://www.google.com/search?q=foobar

Configuration
-------------

Configuration is done by editing Haskell source code. How else?!

Apologia
--------

I wanted to have browser keywords on my Nokia N900, but didn't want to
replicate them from my desktop computer. The software I've been using,
[TouchSearch](http://www.touchsearch.org/), is pretty cool but tedious to set
up (you have to type "{query}" on the N900 keyboard). So I figured it would be
easier to write a haskell program.
