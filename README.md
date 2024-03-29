## fu

fu is a extensible program in [CHICKEN Scheme](http://www.call-cc.org)
to find and operate on files. fu is extensible in Scheme.

It's shipped with commands to find, view and edit files:

    $ fu -h
    Usage: fu <command> <options>
    
    f [-s] [-f] [-d <depth>] [<dir>] <pattern>
      Find files that sloppily match <pattern> (a regular expression). If
      <dir> is provided, search in it, otherwise search in the current
      directory.  Sloppily means <pattern> will be surrounded by ".*"
      and will be case insensitive.
        -s:          strict mode -- disable sloppy mode.
        -e <except>: remove files matching <except> (not affected by -s)
        -f:          print full paths
        -d <depth>:  limit search to <depth>
        -.        :  list files whose name start with "."
    
    v <f options> [<dir>] <pattern>
      Find files & view.
    
    e <f options> [<dir>] <pattern>
      Find files & edit.
    

The `f` command will only print matches to the standard output.
Examples:

    $ fu f eval
    'eval.c'
    'eval.scm'
    'manual/Unit eval'

    $ fu f /var/log auth
    '/var/log/auth.log'
    '/var/log/auth.log.1'
    '/var/log/auth.log.2.gz'
    '/var/log/auth.log.3.gz'
    '/var/log/auth.log.4.gz'

If the output is a terminal, fu will highlight matches.

The `v` and `e` commands will open the file if they find a single
match, or prompt in case of multiple matches.  Example:

    $ fu v eval
    [0] 'eval.c' [0]
    [1] 'eval.scm' [1]
    [2] 'manual/Unit eval' [2]
    Option (ENTER to abort): 


### Configuration

fu will evaluate `/etc/fu.conf` and `$HOME/.fu.conf`, in that order,
if they exist and are readable.

The following parameters can be used to configure fu:

#### fu-viewer

Default value:

    (lambda (file)
      (system (sprintf "less -R ~a" (qs file)))
      (print-selected-file file))

#### fu-editor

Default value:

    (lambda (file)
      (system (sprintf "~a ~a"
                       (or (get-environment-variable "EDITOR")
                           "emacs")
                       (qs file)))
      (print-selected-file file))

#### fu-actions

A one-argument procedure (the option user selected) which is applied
the results of commands that perform no action (e.g., `f`).

Default value:

    (lambda (selection)
      (if (terminal-port? (current-output-port))
          (let ((option (prompt '("View" "Edit") identity)))
            (if (zero? option)
                ((fu-viewer) selection)
                ((fu-editor) selection)))
          (print selection)))

#### fu-pager

Used to paginate options.

Default value:

    (case (software-type)
      ((windows) "more /s")
      (else "less"))))

Unless the environment variable `LESS` is set, `fu` will set it to
`FRXis`.

#### match-highlighter

Default value:

    (let ((tty? (terminal-port? (current-output-port))))
      (lambda (match)
        (if tty?
            ;; colorize in red
            (string-append "\x1b[31;1m" match "\x1b[0m")
          match)))

### constraints

A one-argument procedure (predicate) that will be given a file path,
and it should return `#f` or a truthy value.  `#f` specifies the file
path should be excluded from results, and a truthy value specifies the
file path should be included in results.

The default value for this parameter is `#f`, which means "no
constraint".

For example, if you want to automatically exclude files owned by root
(that's a very silly example):

    (constraints
     (conjoin (constraints)
              (lambda (path)
                (not (zero? (file-owner path))))))

The use of `(conjoin (constraints) ...)` is recommended not to clobber
other `constraints` settings made by goodies that you may happen to
use.


### Extending fu

fu can be extended through command definitions in configuration files.
The `define-command` procedure defines a command.

The syntax is:

    (define-command <command> <help> <handler>)

`<command>` is symbol.  `<help>` is a string and `<handler>` is a
procedure that must handle an arbitrary number of parameters (command
line options).

Here's an example:

    (define-command 'm
      "m <f options> <pattern>
      Find & play music."
      (let ((player (lambda (file)
                      (system (sprintf "mplayer ~a" (qs file))))))
        (fu-find/operate player dir: "/srv/music")))

The code above defines a command `m`, which looks for music files
matching `<pattern>` in `/srv/music`.

The `fu-find/operate` procedure is a helper that will handle the same
options as for the `f` command, and allows some customizations, like
the directory where to find files (`dir` -- default is the current
directory) and the one-argument procedure that will be applied to the
selected file.

Once defined, commands get integrated to fu:

    $ fu -h
    Usage: fu <command> <options>
    
    f [-s] [-f] [-d <depth>] <pattern>
      Find files that sloppily match <pattern> (a regular expression). If
      <dir> is provided, search in it, otherwise search in the current
      directory.  Sloppily means <pattern> will be surrounded by ".*"
      and will be case insensitive.
        -s:          strict mode -- disable sloppy mode.
        -e <except>: remove files matching <except> (not affected by -s)
        -f:          print full paths
        -d <depth>:  limit search to <depth>
        -.        :  list files whose name start with "."

    v <f options> [<dir>] <pattern>
      Find files & view.
    
    e <f options> [<dir>] <pattern>
      Find files & edit.
    
    m <f options> [<dir>] <pattern>
      Find & play music.

Here's another example, a command to open files based on files' extension:

    (define-command 'o
      "o <f options> <pattern>
      Find & open files."
      (let ((opener
             (lambda (file)
               (let ((ext (pathname-extension file)))
                 (if ext
                     (let* ((ext (string->symbol (string-downcase ext)))
                            (program
                             (case ext
                               ((html htm) "firefox")
                               ((avi mpg mpeg mp4 mov) "mplayer")
                               ((pdf) "mupdf")
                               (else
                                (die! "Don't know how to open ~a files." ext)))))
                       (system (sprintf "~a ~a" program (qs file))))
                     (die! "Don't know how to open ~a." file))))))
        (fu-find/operate opener)))

If you don't like fu's default commands, you can remove them and use
your own.  The `remove-command!` procedure removes commands (symbols):

    $ cat ~/.fu.conf
    ;; -*- scheme -*-
    (remove-command! 'e)


    $ fu -h
    Usage: fu <command> <options>
    
    f [-s] [-f] [-d <depth>] [<dir>] <pattern>
      Find files that sloppily match <pattern> (a regular expression). If
      <dir> is provided, search in it, otherwise search in the current
      directory.  Sloppily means <pattern> will be surrounded by ".*"
      and will be case insensitive.
        -s:          strict mode -- disable sloppy mode.
        -e <except>: remove files matching <except> (not affected by -s)
        -f:          print full paths
        -d <depth>:  limit search to <depth>
        -.        :  list files whose name start with "."

    v <f options> [<dir>] <pattern>
      Find files & view.

### Deploying fu

fu can be installed like a regular CHICKEN extension.  Just change to
the source directory and run `chicken-install`:

    $ cd fu
    $ chicken-install

If you want to use fu on a system that doesn't have CHICKEN installed,
you can build a statically linked binary:

#### With CHICKEN 5

    $ cd fu
    $ CSC_OPTIONS="-static -L -static" chicken-install -n  # just build, don't install
    $ ldd fu
            not a dynamic executable

If you have extra modules in your configuration file, the CHICKEN
runtime probably won't be able to load them, as the binary is going to
be statically linked (and they are unlikely to be available on all
systems where the statically linked binary can be used anyway).  To
compile extra modules into the statically linked binary, use
`-R extra-module1 -R extra-module2 ...` in `CSC_OPTIONS`.

Example:

    $ CSC_OPTIONS="-static -L -static -R srfi-13 -R chicken.process-context" chicken-install -n


### Credits

fu uses the [command-line](https://bitbucket.org/evhan/command-line)
command line parser by [Evan
Hanson](http://wiki.call-cc.org/users/evan-hanson).
