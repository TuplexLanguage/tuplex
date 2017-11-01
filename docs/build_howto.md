---
layout: default
---
## How to Build the Tuplex Compiler

The current working state of the Tuplex compiler is availabile on GitHub:

<a href="https://github.com/TuplexLanguage/tuplex" target="_blank">https://github.com/TuplexLanguage/tuplex</a>

So far the build has only been tested on Linux/Ubuntu, but if you have a Ubuntu 14/16 system or are an experienced developer it should be straight-forward to get it to build.

These are the prerequisite tools, and the version it currently builds with:

* cmake 3.5.1
* make 4.1
* flex 2.6.0
* bison 3.0.4
* c++11 compilation tool chain (I've tested with gcc (5.4.0) and clang)
* python, if you want to run the test suite
* llvm 5.0.1

These can easily be installed using apt-get, except for llvm 5 which requires a number of commands, see below.

If you have the above installed, simply clone the git project, cd to its "proto" subdirectory and run these commands to build the compiler:

    cmake .
    make

(If any tool, library or include isn't found, see if the paths in `proto/src/CMakeLists.txt` need to be adapted for your setup.)

### Install and build from scratch

This is an install-from-scratch command sequence I've tested on a completely fresh Ubuntu 16.x installation.

First, cd to the directory where you want to put the "tuplex" repository directory.
Then either copy-paste it into your console, or run as a bash script.

You may want to change the PATH settings.

> Check [http://apt.llvm.org/](http://apt.llvm.org/) for Linux flavors other than Ubuntu Xenial (16.04) and edit the corresponding line in the script.

```
#/bin/bash
# First, cd to the directory you want to put the "tuplex" repository directory.

###################
## Install the packages with the build tools needed to build the tuplex compiler:
sudo apt-get update
sudo apt-get install git cmake make gcc g++ flex bison


###################
## Install LLVM 5.0, also needed to build Tuplex:

# Required for add-apt-repository (needed for LLVM installation):
sudo apt-get install software-properties-common

# Install LLVM 5.0:
# !!!!! Note: Check http://apt.llvm.org/ for Linux flavors other than Ubuntu Xenial (16.04)
sudo add-apt-repository 'deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-5.0 main'
wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
sudo apt-get update
sudo apt-get install llvm-5.0 llvm-5.0-dev

# Add LLVM 5.0 to the command path:
# !!!!! To make this permament for all bash sessions, it can for example be appended to your ~/.bashrc file.
export PATH=$PATH:/usr/lib/llvm-5.0/bin


###################
## Get and build Tuplex:

# Clone the tuplex repository to the current directory:
git clone https://github.com/TuplexLanguage/tuplex.git

# Build the Tuplex compiler:
cd tuplex/proto
cmake .
make
```

To add tuplex commands (including the txc compiler) and scripts to the path:
(You may want to do this with absolute paths in your ~/.bashrc)

```
export PATH=$PATH:./bin:./scripts
```

### Trying it out

Suggestions for starting testing the compiler:

Run the test suite:

    txts

Compile the Hello, world! program from the test suite and run it directly in JIT mode:

    txc autotest/lib/helloworld.tx -nobc -jit

Run the Tuplex program build script (produces an executable):

    txb autotest/lib/helloworld.tx

---

These might be some files you'd like to check out:

`proto/bin/txc` The compiler. You may want to add this to your path. *Run with -h to print the command line usage.* See also the Paths section below.

`proto/autotest/lib/helloworld.tx` An example program that prints "Hello, world!".

There are also a bunch of other test programs in `proto/autotest/`, this is the suite of test source files used in the automated tests of the compiler (i.e. unit tests). These have plenty of syntax examples covering the implemented capabilities of the language.

To run the auto test suite manually, use the following command: `python autotest/test.py`

`proto/tx` The Tuplex foundation library source code (under the reserved namespace `tx`). This contains quite advanced code and ties together the built-in types with interfaces and implementations for collections, iteration, I/O, and more.


### Paths

The txc compiler has two special paths that are controlled by command options and used to locate source modules.

* The foundation library code: The `tx` module and some of its submodules. These are expected to be found in a directory named `tx` in the current directory, or in a location specified by the `-tx <path>` option. If you run the compiler with `proto/` as the current directory you can skip this option since it is located there.

* The user source code path(s). These are searched to find the source code for imported modules. If not set then the current directory is default. Set using the `-sp <pathlist>` or `-sourcepath <pathlist>` options.


### Scripts

There are some ready-made scripts in the `proto/scripts/` directory. If you add it to your path you can run them directly.

* `txb`
Build script that runs the compiler, the LLVM optimizer and linker to produce a stand-alone executable. Command line args are forwarded to txc.

* `txts`
Runs the test suite.