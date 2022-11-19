Convenience Layer on Top of Nexus
=====

[Achaea MUD on Nexus](https://play.achaea.com)

In order to add some convenience features to Nexus I decided to create a
local web server to capture all the MUD data from Achaea and process it
on a local webserver.

To capture the data I'm using JavaScript to forward the websocket telnet
data to a local Erlang webserver

To process the data I have an Erlang Cowboy web server that does a few things:
- listens for data from the Nexus web client on a websocket (/achaea)
- listens for data subscribers on a different websocket (/mudspy)
- listens for subscriptions to data types (/mudspy)
- publishes data to subscribers (/mudspy)

I'm building this for Achaea only. I haven't tried the other MUDs in
Nexus.

Yes, I could use another MUD client, or triggers, or whatever, but this
is fun.

Build
-----

    $ rebar3 compile

Run Server
-----

    $ rebar3 shell

Run Subscriber Page
-----

Navigate to mudspy.html in a browser. This will automatically open a
websocket to localhost:8080/mudspy and register for "raw" data.

Tap into Nexus
-----

Run the achaea_spy_snippet.js file as a snippet in the browser
before logging into your character at [play.achaea.com](https://play.achaea.com.)
