## fu

fu is a extensible program in [CHICKEN Scheme](http://www.call-cc.org)
to find and operate on files. fu is extensible in Scheme.

It's shipped with commands to find, view and edit files:

    $ fu -h
    Usage: fu <command> <options>
    
    f [-s] [-f] [-d <depth>] <pattern>
      Find files that sloppily match <pattern> (a regular expression).
      Sloppily means <pattern> will be surrounded by ".*" and will be case
      insensitive.
        -s:          strict mode -- disable sloppy mode.
        -e <except>: remove files matching <except> (not affected by -s)
        -f:          print full paths
        -d <depth>:  limit search to <depth>
        -.        :  list files whose name start with "."
    
    v <f options> <pattern>
      Find files & view.
    
    e <f options> <pattern>
      Find files & edit.
    

The `f` command will only print matches to the standard output.
Example:

    $ fu f eval
    'eval.scm'
    'eval.c'
    'manual/Unit eval'

If the output is a terminal, fu will highlight matches.

The `v` and `e` commands will open the file if they find a single
match, or prompt in case of multiple matches.  Example:

    $ fu v eval
    [0] 'eval.scm' [0]
    [1] 'eval.c' [1]
    [2] 'manual/Unit eval' [2]
    Option (ENTER to abort): 


### Configuration

fu will evaluate `/etc/fu.conf` and `$HOME/.fu.conf`, in that order,
if they exist and are readable.

The following parameters can be used to configure fu:

#### fu-viewer

Default value:

    (lambda (file)
      (system (sprintf "less -R ~a" (qs file))))

#### fu-editor

Default value:

    (lambda (file)
      (system (sprintf "emacs ~a" (qs file))))

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
      Find files that sloppily match <pattern> (a regular expression).
      Sloppily means <pattern> will be surrounded by ".*" and will be case
      insensitive.
        -s:          strict mode -- disable sloppy mode.
        -e <except>: remove files matching <except> (not affected by -s)
        -f:          print full paths
        -d <depth>:  limit search to <depth>
        -.        :  list files whose name start with "."

    v <f options> <pattern>
      Find files & view.
    
    e <f options> <pattern>
      Find files & edit.
    
    m <f options> <pattern>
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
    
    f [-s] [-f] [-d <depth>] <pattern>
      Find files that sloppily match <pattern> (a regular expression).
      Sloppily means <pattern> will be surrounded by ".*" and will be case
      insensitive.
        -s:          strict mode -- disable sloppy mode.
        -e <except>: remove files matching <except> (not affected by -s)
        -f:          print full paths
        -d <depth>:  limit search to <depth>
        -.        :  list files whose name start with "."

    v <f options> <pattern>
      Find files & view.

### Deploying fu

fu can be installed like a regular CHICKEN extension.  Just change to
the source directory and run `chicken-install`:

    $ cd fu
    $ chicken-install

If you want to use fu on a system that doesn't have CHICKEN installed,
you can build a static binary:

    $ cd fu
    $ FU_STATIC=1 chicken-install -n  # just build, don't install
    $ ldd fu
            not a dynamic executable

### Credits

fu uses the [command-line](https://bitbucket.org/evhan/command-line)
command line parser by [Evan
Hanson](http://wiki.call-cc.org/users/evan-hanson).
