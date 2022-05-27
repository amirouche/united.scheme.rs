# scheme-united

**work in progress**

The goal of this repository is to document "how to build portable
scheme libraries, and programs". At this time, it only provides a
script to install several Scheme implementations on Debian-like
systems, and a prebuilt Docker image where several Scheme are already
installed.

## Getting started

### Bare installation

You can compile, and install several Scheme on a Debian-like host
without docker. You will prolly need `sudo` rights. To install all
required system dependencies use something like:

```sh
% apt install --yes cmake autopoint bsdmainutils debhelper dh-autoreconf dh-strip-nondeterminism dwz flex gettext gettext-base groff-base intltool-debian libarchive-zip-perl libdebhelper-perl libelf1 libffi-dev libfile-stripnondeterminism-perl libgc-dev libglib2.0-0 libgmp-dev libgmpxx4ldbl libicu-dev libncurses-dev libncurses5-dev libpipeline1 libreadline-dev libsub-override-perl libtext-unidecode-perl gperf libuchardet0 libunistring-dev libxml-libxml-perl libxml-namespacesupport-perl libxml-sax-base-perl libxml-sax-perl libxml2 man-db pkg-config po-debconf tex-common texinfo tzdata ucf zlib1g-dev libsqlite3-dev libssl-dev git make libtool wget curl libck-dev build-essential uuid-dev
```

To install all supported Scheme in /opt/united, at the root of this
repository, do the following cli dance:

```sh
% ./venv
% scheme-united install ~/.local/opt/united/
```

You will need to adjust shell environmentt variable `PATH` to include
`~/.local/opt/united/bin`.

### Prebuilt docker image

This repository publish an image that is ready to use. You can pull it with:

```sh
% docker pull ghcr.io/amirouche/scheme-united:latest
```

Then, you can spawn a container with the following command:

```sh
% docker run -v $(PWD):/myproject -it --rm ghcr.io/amirouche/scheme-united:latest
```

In the shell that is spawned you need to enter the `/united` directory and
activate the virtual environment:

```sh
% cd /united && ./venv
```

It will update the shell environment variables in order to have access
to supported Scheme implementations. Then you can do:

```sh
% cd /myproject
% chibi myprogram.scm
```

### Build your own docker image

A `Dockerfile` is present at the root of this repository. That is the
one used to build the github image.
