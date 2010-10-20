Fibon in a Flash
===================================================================
    $ git clone git://github.com/dmpots/fibon.git
    $ cd fibon
    $ git submodule update --init benchmarks
    $ cabal configure && cabal build
    $ ./dist/build/fibon-run/fibon-run

Introduction
===================================================================
Fibon is a set of tools for running and analysing benchmarks in
Haskell. Most importantly, it includes a set of [default benchmarks][3]
taken from the [Hackage][1] open source repository.

Benchmarks
------------------
Fibon makes it easy to use either the fibon benchmarks or your own
set of benchmarks. Benchmarks are stored in the
`benchmarks/Fibon/Benchmarks` directory. This directory is setup as
a [git submodule][2] which means you can easily grab the standard
suite or use a suite kept under your own source control.

The default suite of benchmarks is stored in the
[fibon-benchmarks][3] repository on Github.

Benchmark Groups
------------------
Benchamarks named and organized into groups based on the filesystem
organization. For example, a benchmark in the directory
`benchmarks/Fibon/Benchmarks/Hackage/Agum` will have the name `Agum`
an be in the benchmark group `Hackage`.

Executables
------------------
The fibon package builds three tools:

1. `fibon-run` - runs the benchmarks
2. `fibon-analyze` - analyzes the results of a run
3. `fibon-init` - utilty used when adding new benchmarks

Size and Tune
------------------
Fibon benchmarks can be run with two different input sizes: `Test` and
`Ref`. The `Test` size is useful to make sure that a benchmark can
run successsfully, but will not give meaningful timings.

Fibon benchmarks can be run under two different tune settings (e.g.
compiler optimization settings). The `Base` and `Peak` settings can
be configured anyway you want to make the desired comparison.

Directory Structure
--------------------
Source directories
    ./benchmarks -- benchmark code
    ./config     -- config files
    ./lib        -- common files used by several executables
    ./tools      -- source code for executables

Working directories
    ./log        -- logging output from benchmark runs
    ./run        -- working directory for benchmark runs

Getting the Benchmarks
===================================================================
The benchmarks are kept in a separate repository as a git
submodule. You can get the fibon benchmarks by updating the
submodule from within your fibon working directory

    $ git submodule update --init benchmarks

This will checkout the benchmarks from the [fibon-benchmarks][3]
repository and place them in your working copy.

Running Benchmarks
===================================================================
The available benchmarks and configurations are discovered when the
fibon package is configured. Benchmarks are searched for in the
`benchmarks/Fibon/Benchmarks` directory and configuration files are
searched for in the `config` directory. If a configuration file or
benchmark is added, you will need to re-run `cabal configure` to
make them available to the fibon-run tool.

Configuration
---------------
Fibon comes with a default configuration. The default configuration
will run all benchmarks with the `Base` setting of `-O0` and a
`Peak` setting of `-O2` on the `Ref` size. A configuration file can
be used to specify more complicated configurations.

You can get some example configuration by doing
    $ git submodule update --init config

This will checkout a repository of config files. Note that currently
these files contain some user and machine-specific configurations,
but should be a useful starting point.

You can also command line options to selectively run benchmarks,
groups, sizes, and tune settings as described below.

Running
---------------
Benchamarks are run with the `fibon-run` tool. Running `fibon-run`
with no arguments will use the default config file. An alternate
config file can be specified with the `-c` flag. Also, you can give
a list of benchmarks or groups to run on the command line. Use
`--help` to see a full list of options.

Running the benchmarks will produce some logging to standard out and
create four output files in the `log` directory.

1. `*.LOG` - the full log of the run
2. `*.SUMMARY` - the mean runtimes of each benchmark
3. `*.RESULTS` - the full results in text format (pass to `fibon-analyse`)
4. `*.BINARY`  - the full results in binary format (pass to `fibon-analyse`)

Analyzing Benchmark Results
===================================================================
Benchmarks can be analyzed by the `fibon-analyse` tool.

    $ fibon-analyse log/000.default.RESULTS
or
    $ fibon-analyse log/000.default.BINARY

The binary results file is much faster to parse.

Adding New Benchmarks
===================================================================
TODO

Benchmark Notes
===================================================================
Ghc610
  ChameneosRedux
    Does not work with -O0. Gets "thread blocked indefinitely"
    exception

  Mandelbrot
    The Test size gives different result, but the Ref size is ok.
    Think it is just some kind of floating point wibbles.

[1]: http://hackage.haskell.org
[2]: http://www.kernel.org/pub/software/scm/git/docs/user-manual.html#submodules
[3]: http://github.com/dmpots/fibon-benchmarks
