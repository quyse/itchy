FROM ubuntu:16.04

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update && apt-get upgrade -y
RUN apt-get install -y netbase ca-certificates unar file
RUN useradd -m -s /bin/bash runner
COPY itchy-runner /usr/bin/itchy-runner
WORKDIR /home/runner
USER runner
