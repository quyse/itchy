FROM ubuntu:16.04

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y git build-essential automake autogen libtool clang
RUN git clone https://github.com/tpoechtrager/cctools-port /root/cctools
RUN cd /root/cctools/cctools && ./autogen.sh && ./configure && make -j$(nproc) && make install
RUN apt-get install -y netbase ca-certificates unar file
RUN useradd -m -s /bin/bash runner
COPY itchy-runner /usr/bin/itchy-runner
WORKDIR /home/runner
USER runner
