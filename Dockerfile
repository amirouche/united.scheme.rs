# -*- mode: dockerfile; coding: utf-8 -*-
FROM ubuntu:22.04

ARG UNITED_WORKER_COUNT=2
ENV DEBIAN_FRONTEND=noninteractive
ENV UNITED_PREFIX=/opt/united/

RUN apt update && apt upgrade --yes && apt install --yes asciidoctor autopoint bsdmainutils build-essential cdbs chibi-scheme cmake curl debhelper dh-autoreconf dh-strip-nondeterminism dh-translations dwz flex gettext gettext-base git gperf groff-base intltool intltool-debian libarchive-zip-perl libblkid-dev libbrotli-dev libcairo2-dev libck-dev libdatrie-dev libdebhelper-perl libegl-dev libelf1 libffi-dev libfile-stripnondeterminism-perl libfile-which-perl libfontconfig-dev libfontconfig1-dev libfreetype-dev libfreetype6-dev libfribidi-dev libgc-dev libgl-dev libgl1-mesa-dev libgles-dev libgles1 libglib2.0-0 libglib2.0-dev libglib2.0-dev-bin libglu1-mesa-dev libglvnd-core-dev libglvnd-dev libglx-dev libgmp-dev libgmpxx4ldbl libgraphite2-dev libharfbuzz-dev libharfbuzz-gobject0 libice-dev libicu-dev libjpeg-dev libjpeg-turbo8-dev libjpeg8-dev libmount-dev libncurses-dev libncurses5-dev libopengl-dev libpango1.0-dev libpcre2-16-0 libpcre2-dev libpcre2-posix3 libpipeline1 libpixman-1-dev libpng-dev libpthread-stubs0-dev libreadline-dev libselinux1-dev libsepol-dev libsm-dev libsqlite3-dev libssl-dev libsub-override-perl libtext-unidecode-perl libthai-dev libtool libuchardet0 libunistring-dev libx11-dev libxau-dev libxaw7-dev libxcb-render0-dev libxcb-shm0-dev libxcb1-dev libxdmcp-dev libxext-dev libxft-dev libxml-libxml-perl libxml-namespacesupport-perl libxml-sax-base-perl libxml-sax-perl libxml2 libxmu-dev libxmu-headers libxpm-dev libxrender-dev libxt-dev make man-db pango1.0-tools patchutils pkg-config po-debconf scour sqlite3 tex-common texinfo tzdata ucf uuid-dev wget x11proto-dev xorg-sgml-doctools xtrans-dev zlib1g-dev curl libcurl4

RUN ln -s /usr/lib/x86_64-linux-gnu/libcurl.so.4 /usr/lib/x86_64-linux-gnu/libcurl.so

ADD ./scheme-united.scm /usr/bin/scheme-united.scm

RUN scheme-united.scm available | sort | xargs scheme-united.scm install

RUN rm -rf /var/cache/apt/* /tmp
