# Media Player Tracker

## About

Media Player Tracker is a simple tool to fetch the title of the currently playing song on a
supported media player.

This has currently only been tested using SBCL on Windows 10.

## Usage

Simply load the library and start the tracker from REPL.

``` lisp
(ql:quickload :media-tracker)
(media-tracker:start-tracker :spotify #P"current-song.txt" NIL)
```

* The first argument is the media player. Currently only `:vlc` and `:spotify` are supported.
* The second argument is a pathname to the file that the artist and song title is written into.
* The third argument rules whether or not the tracker should be run asynchronously.
  * **Note:** Only one instance of the tracker may be running at a time.
* Further key arguments depend on the media player but there are a two common ones:
  * `:interval` for the frequency of the queries in seconds. This defaults to `1.0`.
  * `:timeout` for waiting on the update requests in seconds. This defaults to `5.0`.

To stop a synchronous tracker simply press Enter.

To stop an asynchronous tracker you may simply run the following function.
``` lisp
(media-tracker:stop-tracker)
```

To get the current song as a string call the following function.
``` lisp
(media-tracker:current-song :spotify)
```

* Similarly to `start-tracker` you may pass `:vlc` or `:spotify`.
* And also similarly the additional arguments may be passed depending on the media player.

### Spotify

The tool will simply find the title of the window that Spotify is running on and uses it, if
one is currently being played.

### VLC

Assuming the [HTTP interface](https://wiki.videolan.org/Documentation:Modules/http_intf/)
is enabled this tool will be able to send periodic requests to it.

There is a number of additional arguments that may be passed:
* `:login` and `:password` for Basic authentication.
  * By default only the password is needed. It is set in VLC's Lua interface options.
* `:protocol` for the server protocol. This defaults to `:http`.
* `:hostname` for the server hostname. This defaults to `"127.0.0.1"`.
* `:port` for the server port. This defaults to `9090`.
* `:root-path` for the requests to be sent to. This defaults to `NIL`.
* `:album` as a boolean value to include the album name on the written output.
* `:track-number` as a boolean value to include the track number on the written output.

## TODO

* Linux support for Spotify song tracker.

## Dependencies

All of these should be avaible from SBCL and Quicklisp:
* UIOP
* Alexandria
* Plump
* lquery
* Drakma
* CL-PPCRE
* Verbose
* Bordeaux-Threads
