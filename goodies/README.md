## fu goodies

This directory contains some extensions that can be `include`d (or
`load`ed) by configuration files to make fu provide some extra
features.

### grep.scm

Wrappers around `grep` and `git grep`.  Provides the `gv` and `ge`
commands:

    gv <grep options> <pattern>
      Equivalent to 'grep <grep options> <pattern>'.  If in a git repository,
      will run 'git grep <git grep options> <pattern>. The action to be applied
      to the selection is 'view'.

    ge <grep options> <pattern>
      Equivalent to 'grep <grep options> <pattern>'.  If in a git repository,
      will run 'git grep <git grep options> <pattern>. The action to be applied
      to the selection is 'edit'.

## Usage example

    $ cat ~/.fu.conf
    ;; -*- scheme -*-
    (define home
      (let ((dir (get-environment-variable "HOME")))
        (lambda (path)
          (make-pathname dir path))))

    (load (home "src/fu/goodies/grep.scm"))
