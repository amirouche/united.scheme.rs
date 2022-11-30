# -*- mode: dockerfile; coding: utf-8 -*-
FROM ubuntu:22.04

ARG UNITED_WORKER_COUNT=2
ENV DEBIAN_FRONTEND=noninteractive
ENV UNITED_PREFIX=/united/local/

RUN apt update && apt upgrade --yes && apt install --yes cmake autopoint bsdmainutils debhelper dh-autoreconf dh-strip-nondeterminism dwz flex gettext gettext-base groff-base intltool-debian libarchive-zip-perl libdebhelper-perl libelf1 libffi-dev libfile-stripnondeterminism-perl libgc-dev libglib2.0-0 libgmp-dev libgmpxx4ldbl libicu-dev libncurses-dev libncurses5-dev libpipeline1 libreadline-dev libsub-override-perl libtext-unidecode-perl gperf libuchardet0 libunistring-dev libxml-libxml-perl libxml-namespacesupport-perl libxml-sax-base-perl libxml-sax-perl libxml2 man-db pkg-config po-debconf tex-common texinfo tzdata ucf zlib1g-dev libsqlite3-dev libssl-dev make libtool wget curl libck-dev build-essential uuid-dev asciidoctor chibi-scheme

RUN git clone https://github.com/amirouche/scheme-united.git united

RUN cd /united && ./scheme-united available | sort | xargs ./scheme-united install

RUN rm -rf /var/cache/apt/* /tmp
