---
layout: default
---
## How to Build the Tuplex Compiler

The current working state of the Tuplex compiler is availabile on GitHub:

<a href="https://github.com/TuplexLanguage/tuplex" target="_blank">https://github.com/TuplexLanguage/tuplex</a>

I have not yet tested to build it on other systems than my development system, but if you have a ubuntu 14/16 system or are an experienced developer it should be straight-forward to get it to build.

These are the prerequisite tools, and the version I currently build with:

* cmake 3.5.1
* flex 2.6.0
* bison 3.0.4
* llvm 3.9.1
* c++11 compilation tool chain (I'm mostly using gcc but have also run with Clang)
* python, if you want to run the test suite

As of LLVM version 3.5, I've also needed to install additional libraries in order to get the project to link. Other installations may or may not need to do this, anyhow these are the apt commands that I ran:

    sudo apt-get install zlib1g-dev
    sudo apt-get install libedit-dev

If you have the above installed, simply clone the git project, cd to its base directory and run these commands to build the compiler:

    cmake .
    make

(If any tool, library or include isn't found, see if the paths in `proto/src/CMakeLists.txt` need to be adapted for your setup.)


After build these might be the first files you'd like to check out:

`proto/bin/txc`

The compiler. You may want to add this to your path. Run with -h to print the command line usage. See also Paths section below.

`proto/autotest/lib/helloworld.tx`

An example program that prints "Hello, world!".

There are also a bunch of other test programs in `proto/autotest/`, this is the suite of test source files used in the automated tests of the compiler (i.e. unit tests). These have plenty of syntax examples covering the implemented capabilities of the language.

To run the auto test suite manually, use the following command:

    python autotest/test.py


### Paths

The txc compiler has two special paths, used to locate source modules.

* The foundation library code: The `tx` module and some of its submodules. These are expected to be found in a directory named `tx` in the current directory, or in a location specified by the `-tx <path>` option. If you run the compiler with `proto/` as the current directory you can skip this option since it is located there.

* The user source code path(s). These are searched to find the source code for imported modules. If not set then the current directory is default. Set using the `-sp <pathlist>` or `-sourcepath <pathlist>` options.


### Scripts

There are some ready-made scripts in the `proto/scripts/` directory. If you add it to your path you can run them directly.

* `txb`
Build script that runs the compiler, the LLVM optimizer and linker to produce a stand-alone executable. Command line args are forwarded to txc.

* `txts`
Runs the test suite.