![vlcj](https://github.com/caprica/vlcj/raw/master/etc/vlcj-logo.png "vlcj")

vlcj
====

The vlcj project provides a Java framework to allow an instance of a native
[vlc](http://www.videolan.org/vlc "vlc") media player to be embedded in a Java 
AWT Window or Swing JFrame. 

You get more than just simple bindings, you also get a higher level framework
that hides a lot of the complexities of working with libvlc.

vlcj is primarily developed and therefore extensively tested on Linux - it does
also work just fine on Windows and should work on Mac - but for Mac you likely
need to use release 2.0.3, 2.0.7 or later, *but not any version inbetween*.

At least JDK 1.6 is required, and it works without changes on JDK 1.7.

This is the open source vlcj project page, see also the 'official' 
[home page](http://www.capricasoftware.co.uk/vlcj "Official vlcj home page at Caprica Software")
where you can find more information as well as some new simple tutorials.

Maven Dependency
----------------

Add the following Maven dependency to your own project pom.xml:

```
<dependency>
    <groupId>uk.co.caprica</groupId>
    <artifactId>vlcj</artifactId>
    <version>2.3.1</version>
</dependency>
```

Note that some features of vlcj 2.2.0 and later (access to the native log) require vlc 2.1.0 which has *not* yet been released.

News
----

22/05/2013 Made new release 2.3.1 at [Maven Central](http://search.maven.org/#search|ga|1|vlcj), changed to fix execution of tests on Windows (you only need to uppdate to this version if you want to run the tests on Windows).

10/05/2013 Made new release 2.3.0 at [Maven Central](http://search.maven.org/#search|ga|1|vlcj), minor API incompatibility with previous version caused by renaming SpuTrackInfo class to TextTrackInfo.

24/01/2013 Made new release 2.2.0 at [Maven Central](http://search.maven.org/#search|ga|1|vlcj).

Note: vlcj now *requires* JNA 3.5.0 or later.

02/01/2013 Happy New Year! Current release artifacts are now available on [Maven Central](http://search.maven.org/#search|ga|1|vlcj).

12/12/2012 github deprecated the downloads section, go to [googlecode](http://code.google.com/p/vlcj/downloads/list) for distribution archives.

20/05/2012 Released version 2.1.0, for use with libvlc 2.0.

28/01/2012 Moved to github.

You can also follow @capricasoftware on Twitter for more vlcj news.

There is also a new vlcj-users discussion group at [vlcj-users](https://groups.google.com/forum/#!forum/vlcj-users "vlcj-users google group").

Documentation
-------------

The vlcj project page is at [github](http://caprica.github.com/vlcj "vlcj at github").

Online Javadoc is available:

* [2.3.0 (current)](http://caprica.github.com/vlcj/javadoc/2.3.0/index.html "2.3.0 Javadoc")
* [2.2.0](http://caprica.github.com/vlcj/javadoc/2.2.0/index.html "2.2.0 Javadoc")
* [2.1.0](http://caprica.github.com/vlcj/javadoc/2.1.0/index.html "2.1.0 Javadoc")
* [2.0.0](http://caprica.github.com/vlcj/javadoc/2.0.0/index.html "2.0.0 Javadoc")

Documentation is being made available at [Caprica Software](http://www.capricasoftware.co.uk/projects/vlcj/index.html "Caprica Software"). 

Support
-------

Free support for Open Source and non-commercial projects is generally provided - you
can use [github issues](https://github.com/caprica/vlcj/issues "vlcj github issues") or the 
[vlcj google group](https://groups.google.com/forum/#!forum/vlcj-users "vlcj users google group")
for this purpose.

Support for commercial projects is provided exclusively on commercial terms -
send an email to the following address for more information:

> mark [dot] lee [at] capricasoftware [dot] co [dot] uk

License
-------

The vlcj framework is provided under the GPL, version 3 or later.

If you want to consider a commercial license for vlcj that allows you to use and 
redistribute vlcj without complying with the GPL then send your proposal to:

> mark [dot] lee [at] capricasoftware [dot] co [dot] uk
