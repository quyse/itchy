FROM alpine:latest

RUN mkdir /lib64 && ln -s /lib/libc.musl-x86_64.so.1 /lib64/ld-linux-x86-64.so.2
COPY itchyd /bin/itchyd
CMD itchyd --config /etc/itchyd/itchyd.yaml
