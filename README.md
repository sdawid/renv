# `e`nvironment commands

`e` executes commands defined in the current directory or its ancestors.

A command is any executable in the form: `[.cmds/][.]cmd-<name>.<ext>`.

Commands shares environment variables defined inside `[.cmds/][.]cmd.env` files.



## How to build it

```
$ make init
$ make
```


## Usage

```
$ e
```
... will show currenctly available commands.



Usage:

```
$ e foo a b c d
```
... will execute `foo` command script passing `a b c d` arguments.



Use `!` to execute external command in the modified environment:
```
$ e ! env               # show env after applying cmd.env files
$ e ! 'echo $FOO'       # show value of FOO after applying cmd.env files
```


or to start a new shell in the modified environment:
```
$ e !
```
