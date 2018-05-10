FROM base/archlinux

RUN pacman -Syu --noconfirm
RUN pacman -S --noconfirm numactl docker

COPY itchyd /usr/bin/
CMD itchyd --config /var/itchy/itchy.yaml
