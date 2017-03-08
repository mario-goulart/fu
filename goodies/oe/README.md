# OpenEmbedded Explorer (oe)

`oe` is as tool to ease work activities in OpenEmbedded (or Yocto
Project) based projects.  It provides commands for you to easily find
files (e.g., recipes, packages) and operate on them in layers, change
to common directories, expand BitBake variables, browse documentation
etc.

## Requirements

* [fu](https://github.com/mario-goulart/fu)
* [sxml-transforms](http://wiki.call-cc.org/egg/4/sxml-transforms) and [html-parser](http://wiki.call-cc.org/egg/4/html-parser) eggs

The following optional external programs are used by `oe`:

* [lynx](http://lynx.invisible-island.net/) (for documentation)
* [wget](https://www.gnu.org/software/wget/) (for documentation)
* rpm (for rpm packages)
* rpm2cpio (for rpm packages)
* cpio (for rpm packages)
* tar (for ipk and deb packages)
* ar (for ipk and deb packages)


## Installation

`oe` is a plugin for [fu](https://github.com/mario-goulart/fu), so you
have to install `fu`.  For some features (e.g., the `cd` command) and
to actually be able to execute the `oe` command on the shell, a
wrapper is provided -- this wrapper defines a shell function called
`oe`.

There are basically two ways to run `oe`: (1) by loading it as a
plugin for `fu`; (2) by building a statically linked binary.


### Loading it as a plugin for `fu`

If you have `fu` installed and want to use `oe`, you basically have to
load `oe.scm` (`(load "path/to/oe.scm")` in you `~/.fu.conf` file) and
`source` the `oe.sh` shell wrapper:

```
$ echo '(load "path/to/oe.scm")' >> ~/.fu.conf
$ . path/to/oe.sh
```


### Building a statically linked binary

`oe` can be bundled with `fu` in a statically linked binary.  To do
this, you need the
[sxml-transforms](http://wiki.call-cc.org/egg/4/sxml-transforms) and
[html-parser](http://wiki.call-cc.org/egg/4/html-parser) eggs
installed (you can install them with `chicken-install sxml-transforms
html-parser`).

To actually build a statically linked version of `fu` that bundles
`oe`, you can use the `oe-static-build.sh` script:

```
$ cd fu # directory with fu's source code
$ ./goodies/oe/oe-static-build.sh
```

A statically linked `fu` binary is going to be created in the current
directory.  You can copy this binary around run it on compatible
platforms.  To get the actual `oe` command, source the shell wrapper:

```
$ . path/to/oe.sh
```

Make sure to copy the `fu` executable binary to some directory in your
`$PATH`.


## Usage

```
Usage: oe <command> <options>

<command>s:

find [-s] [-f] [-d <depth>] <pattern>
  Short command: f.  Find files that sloppily match <pattern> (a regular
  expression) in the sources directory.  Sloppily means <pattern> will
  be surrounded by ".*" and will be case insensitive.
    -s:          strict mode -- disable sloppy mode.
    -e <except>: remove files matching <except> (not affected by -s)
    -f:          print full paths
    -d <depth>:  limit search to <depth>
    -.        :  list files whose name start with "."

view <`find' options> [<dir>] <pattern>
  Short command: v.  Find files & view.

edit <`find' options> [<dir>] <pattern>
  Short command: e.  Find files & edit.

expand [-s] [-u] <variable> [<recipe>]
  Short command: x.  Expand <variable>.  If <recipe> is provided, expand
  <variable> in the context of <recipe>.  If -s (short) is provided, only
  the final value will be printed.  If -u is provided, the raw output of
  'bitbake -e' for the variable in question will be printed.

reverse-expand [-p] <regex> [<recipe>]
  Short command: rx.  Look for <regex> in values of variables in the BitBake
  metadata.  In other words, given a value, find which variables hold that
  value.  If -p is provided, search will me made in values before their
  expansion.

sysroot-find [-s] [-f] [-d <depth>] <pattern>
  Short command: sf.  Similar to `find', but instead of searching for files
  in the sources directory, search in the sysroot directories.

sysroot-view <`sysroot-find' options> <pattern>
  Short command: sv.  Similar to `view', but instead of searching for files
  in the sources directory, search in the sysroot directories.

sysroot-edit <`sysroot-find' options> <pattern>
  Short command: se.  Similar to `edit', but instead of searching for files
  in the sources directory, search in the sysroot directories.

doc <pattern>
  Short command: d.  Show documentation for variables matching <pattern>.

pkg-find <pattern>
  Short command: pf.  Find packages that match <pattern>.

pkg-view <pattern>
  Short command: pv.  Find packages & show their content.

pkg-info <pattern>
  Short command: pi.  Find packages & show their information data.

pkg-scripts
  Short command: ps.  Find packages & show their scripts.

pkg-extract
  Short command: px.  Find packages & extract them to the current directory.

task-log [-f] [-r] <recipe> [<task>]
  Short command: tl.  Show log file for <task> executed for <recipe>.  If
  <task> is not provided, all log files (log.do_<task>) will be displayed
  as options for selection.
  -f: format command lines.  Will break compiler command line arguments into
      individual lines, hopefully improving readability.
  -r: replace variable values.  Will replace some values by their
      corresponding variable, hopefully improving readability.

task-script <recipe> [<task>]
  Short command: ts.  Show run.do_<task> scripts for <task> executed for
  <recipe>.  If <task> is not provided, all script files will be displayed
  as options for selection.

grep [<grep options>] <pattern>
  Short command: g. Search for <pattern> in the source directories.

grep-view [<grep options>] <pattern>
  Short command: gv.  Search for <pattern> in the source directories and
  call the viewer on the selected option.

grep-edit [<grep options>] <pattern>
  Short command: ge.  Search for <pattern> in the source directories and
  call the editor on the selected option.

variable-find <pattern> [<recipe>]
  Short command: vf.  Find variables matching <pattern>.

buildhistory <subcommand> <recipe|package>
  Short command: bh.  Query the buildhistory directory.

  The BUILDHISTORY_DIR environment variable can be used to indicate
  the path to the buildhistory directory.  If it is not set, oe will use
  BUILDHISTORY_DIR from the BitBake environment.

  Subcommands:

  pkgs <recipe pattern>
    List the packages generated by recipes matching <recipe pattern>.

  latest <pkg pattern>
    Show the content of the `latest' file for packages matching
    <pkg pattern>.

  pkg-view <pkg pattern>
    Short command: pv.  Equivalent to `oe pv <pkg>', but using information
    from buildhistory instead.

  what-depends <recipe>
    Show recipes that depend on recipe <recipe>.

  what-rdepends <pkg>
    Show packages that have runtime dependency on <pkg>.

  what-rprovides <provider>
    Show packages that provide <provider> in run time.

cd <place>
  Change directory to <place>.  The available <place>s are:

    top
      Change to the directory pointed by the by the BUILDDIR variable.
      top is implicitly assumed as <place> if cd is given none.

    buildhistory
      Short: bh.  Change to the directory pointed by the BUILDHISTORY_DIR
      variable.

    deploy
      Short: dd.  Change to the directory pointed by the DEPLOY_DIR
      variable.

    work <recipe>
      Short: wd.  Change to the directory pointed by the WORKDIR
      variable for recipe <recipe>.
```


## Implementation

`oe` is implemented as a plugin for
[fu](https://github.com/mario-goulart/fu).  Being it a plugin for
`fu`, `oe` is actually a command that `fu` handles.  Example:

```
$ fu oe cd buildhistory
```

Since some features (e.g., the `cd` command) have to be executed in
the context of the running shell, a shell function called `oe` is
provided, so that you can call `oe` as regular command:

```
$ oe cd buildhistory
```


## Acknowledgments

`oe` has been heavily inspired by
[O.S. Systems' ye tool](http://doc.ossystems.com.br/ye.html).
Differently from `ye`, `oe` doesn't require any specific directory
layout -- it uses whatever BitBake is able to use.
