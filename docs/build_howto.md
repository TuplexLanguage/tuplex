---
layout: default
---
## How to Build the Tuplex Compiler

The current working state of the Tuplex compiler is availabile on GitHub:

<a href="https://github.com/TuplexLanguage/tuplex" target="_blank">https://github.com/TuplexLanguage/tuplex</a>

So far the build has only been tested on Linux/Ubuntu. If you have a Ubuntu 14/16 system or are an experienced developer it should be straight-forward to get it to build.

These are the prerequisite tools, and the version it currently builds with. There is a script below (also included in the repo) to automatically install them for you if you are on an Ubuntu system. The script uses apt-get for most of them; the llvm 5 installation is a bit more involved but is also handled by the script.

* cmake 3.5.1
* make 4.1
* flex 2.6.0
* bison 3.0.4
* c++11 compilation tool chain (it's been tested with gcc and clang)
* python, if you want to run the test suite
* llvm 5.0.1

### Install and build

This is an install-from-scratch command sequence that has been tested on a completely fresh Ubuntu 16.x installation.

The PATH settings should work as-is, but you may want to make the paths absolute and put them into your .bashrc or similar.

**Step 0:** If you don't have git installed you need to do that first:

```
sudo apt-get update
sudo apt-get install git
```

**Step 1:** cd to the directory you want to put the "tuplex" repository directory and run this:

```
# Clone the tuplex repository to the current directory:
git clone https://github.com/TuplexLanguage/tuplex.git
```

**Step 2:**
The following script ensures the necessary build tools are installed and then builds Tuplex.
Either copy-paste it into your console, or run as a bash script.
It is in the repo and can be run directly from the command line: `tuplex/proto/scripts/txinstall`

> Check [http://apt.llvm.org/](http://apt.llvm.org/) if you have a different Linux flavor than Ubuntu Xenial (16.04) and edit the corresponding line in the script.

```
#/bin/bash
# Ensures the necessary build tools are installed and then builds tuplex.
# This script is in the repo and can be run directly:  tuplex/proto/scripts/txinstall

set -x  # command echo on

###################
## Install the packages with the build tools needed to build the tuplex compiler:
sudo apt-get update
sudo apt-get install cmake make gcc g++ flex bison


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
```

**Step 3:** Build tuplex:

```
# Add LLVM 5.0 to the command path:
# (To make this permament for all bash sessions, it can be appended to ~/.bashrc or similar file.)
export PATH=$PATH:/usr/lib/llvm-5.0/bin

# cd to the compiler's directory:
cd tuplex/proto
cmake .
make
```

> When building Tuplex, If any tool, library or include isn't found, see if the paths in `proto/src/CMakeLists.txt` need to be adapted for your setup. After this file has been changed, you need to run `cmake .` again followed by `make`.

**Step 4:** Add command paths:

To add tuplex commands (including the `txc` compiler) and scripts to the path:
<br>(You may want to do this with absolute paths in your ~/.bashrc)

```
export PATH=$PATH:$PWD/bin:$PWD/scripts
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

### Files of interest

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