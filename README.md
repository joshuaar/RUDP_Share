RUDP_Share
=======

P2P file transfer across NATs using UDP.

## Description
This is a work in progress. The goal is to enable a Dropbox-like service where files are stored and retreived on a user's 
"always on" home computer. These files can be accessed anywhere, even if both client and server are behind NATs.

Undergoing new thread model ovhaul, much simpler than before. Will have:

-Signal thread: listens to requests and passes them off to...
-Worker thread: receives signals and processes them. This is where program state will be kept.

##Features
Can establish a connection and transfer files.

## More Info
http://en.wikipedia.org/wiki/UDP_hole_punching

