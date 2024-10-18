# Local `e`nvironment and commands

`env.scm` executes shell and local commands in a local environment.

It scans directories from the *current directory's context* to find:
- environment files - `.env` or `.cmd.env` files with environment variables
- command files - executables with names starting with `cmd-` or `.cmd-`

**Current directory context** is defined recursively as:
- all directories from the parent directory's context
- the current directory
- `.cmds/` and `.envs/` subdirectories (if exist)


> **Note** [Configuration](#configuration) section describes ways to customize
> this default behavior.


## Installation

Place `env.scm` inside a directory defined in `$PATH`.

The script requires [GNU Guile 3.0](https://www.gnu.org/software/guile) to run.

> **Note:**
>
> It's recommended to rename/link the `env.scm` script to a shorter name.
>
> The rest of the documentation assumes that `env.scm` is available under `e` name.


## Usage

Run the script without any arguments to see a short help message
with locally defined commands and environments:

```
$ e
Usage: e <command> [<args>]
...

Local commands:
 - foo                  -> /some/path/.cmd-foo.sh
...

Local environments:
 - /some/path/.env
...
```


### Run local commands

A local command is an executable located in the current directory's context.

To execute them we should use a command name derived from the file name
by removing a command prefix (e.g. `.cmd-`) and optional file extension.

E.g. to execute `.cmd-foo.sh` file we should use `foo` command with optional arguments:

```
$ e foo arg1 arg2 ...
```


### Run shell commands

You can use `!` to execute a shell commands using local environment:

```
$ e ! env               # show env after applying .env files
```

> **Tip**: use `\` or `'...'` to prevent variable substitution
> from the current environment
> or to include pipes inside a executed command:
>
> ```
> $ e ! echo \$ENV_FILE_NAMES
> $ e ! 'echo $ENV_FILE_NAMES'
> $ e ! 'env | grep ENV_'
> ```

Use `!` without any additional arguments to start shell with local environment:

```
$ e !                   # starts default shell
```


### Local environment

The current environment is updated with environment varibles
read from `.env` and `.dot.env` files located in the current context:

```
FOO=foo
BAR = value with spaces
```

'`#`' character starts a **comment**:

```
# This is a comment line
FOO = foo # this is also a comment
BAR = bar#part of value, not a comment
```

Use quotes to disable string trimming, inline comments
or if you need multiline values:

```
FOO = " after # space"
# ^ the value of FOO will be : ' after # space'.
MULTILINE = 'Lorem ipsum
dolor sit
amet'
```


## Configuration

The `env.scm` script can be configured using following environment variables:

- `ENV_FILE_NAMES`
  - environment file names
  - list of names separated by ':'
  - default value: `.env:.cmd.env`

- `ENV_CMD_PREFIXES`
  - command file prefixes
  - list of prefixes separated by ':'
  - default value: `.cmd-:cmd-`

- `ENV_CMDS`
  - custom command name to path mappings
  - list of `<name>=<path>` pairs separated by ':' (e.g. 'foo=./foo.sh:bar=mybar.py')
  - default value: (none)

- `ENV_DIR_NAMES`
  - list of context subdirectory names separated by ':'
  - default value: `.cmds:.envs`
