# -*- mode: dockerfile; coding: utf-8 -*-
FROM ubuntu:20.04

ENV DEBIAN_FRONTEND=noninteractive

RUN apt update && apt upgrade --yes && apt install --yes git
RUN git clone https://github.com/amirouche/scheme-united.git united

RUN apt install cmake autopoint bsdmainutils debhelper dh-autoreconf dh-strip-nondeterminism dwz flex gettext gettext-base groff-base intltool-debian libarchive-zip-perl libcroco3 libdebhelper-perl libelf1 libffi-dev libfile-stripnondeterminism-perl libgc-dev libgc1c2 libglib2.0-0 libgmp-dev libgmpxx4ldbl libicu66 libncurses-dev libncurses5-dev libpipeline1 libreadline-dev libsub-override-perl libtext-unidecode-perl gperf libuchardet0 libunistring-dev libxml-libxml-perl libxml-namespacesupport-perl libxml-sax-base-perl libxml-sax-perl libxml2 man-db pkg-config po-debconf tex-common texinfo tzdata ucf zlib1g-dev libsqlite3-dev libssl-dev git make libtool wget curl libck-dev build-essential uuid-dev

RUN cd /united && ./venv $(pwd) scheme-united install

RUN rm -rf /var/cache/apt/* /tmp
