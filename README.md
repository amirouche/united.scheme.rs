# united.scheme.rs

<div align="center">
Frontend for several Scheme implementations · One CLI, many schemes ·
The best ui/ux to build portable Scheme libraries · All around finest
competition in software
</div>

<div align="center">
  <img src="https://raw.githubusercontent.com/amirouche/united.scheme.rs/hello-schemer/logo.png" />
</div>

## Usage with container

With your favorite container CLI:

1. pull the image;
2. create a container from the image;
3. enter inside the container;

Inside the container a command called `united` can be used to achieve
what is described in the next section.

## Usage

```
# united all check [DIRECTORY ...]
# united SCHEME check [DIRECTORY ...]
# united SCHEME exec [DIRECTORY ...] PROGRAM [-- EXTRA ...]
# united SCHEME repl [DIRECTORY ...]
# united SCHEME version
# united available
# united install SCHEME ...
# united prefix [DIRECTORY]
```

## Getting started

```sh
git clone git@github.com:amirouche/united.scheme.rs.git"
cd united.scheme.rs/
./venv
united.scm available | xargs united.scm install
united chibi exec tests/ tests/hello.scm -- azul 2006
united all check tests/
```
